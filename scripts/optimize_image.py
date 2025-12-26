#!/usr/bin/env python3
"""
Image optimization script for TFS-CMMS uploads.
Resizes images to max dimension, compresses JPEG, strips EXIF.

Usage: python3 optimize_image.py <input_path> <output_path> [max_dimension] [quality]

Defaults:
  max_dimension: 1920 (pixels)
  quality: 85 (JPEG quality 1-100)

Exit codes:
  0 - Success
  1 - Error (prints error message to stderr)
"""

import sys
import os
from PIL import Image

# Default settings
DEFAULT_MAX_DIMENSION = 1920
DEFAULT_QUALITY = 85

def optimize_image(input_path, output_path, max_dimension=DEFAULT_MAX_DIMENSION, quality=DEFAULT_QUALITY):
    """
    Optimize an image:
    - Resize to fit within max_dimension (preserving aspect ratio)
    - Convert to JPEG
    - Compress with specified quality
    - Strip EXIF data
    """
    try:
        with Image.open(input_path) as img:
            # Convert to RGB if necessary (handles PNG with transparency, etc.)
            if img.mode in ('RGBA', 'LA', 'P'):
                # Create white background for transparent images
                background = Image.new('RGB', img.size, (255, 255, 255))
                if img.mode == 'P':
                    img = img.convert('RGBA')
                background.paste(img, mask=img.split()[-1] if img.mode == 'RGBA' else None)
                img = background
            elif img.mode != 'RGB':
                img = img.convert('RGB')
            
            # Get original dimensions
            original_width, original_height = img.size
            
            # Calculate new dimensions if needed
            if original_width > max_dimension or original_height > max_dimension:
                if original_width > original_height:
                    new_width = max_dimension
                    new_height = int(original_height * (max_dimension / original_width))
                else:
                    new_height = max_dimension
                    new_width = int(original_width * (max_dimension / original_height))
                
                # Resize with high-quality resampling
                img = img.resize((new_width, new_height), Image.LANCZOS)
            
            # Ensure output directory exists
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
            
            # Save as JPEG with optimization
            # Note: This strips EXIF data by default (no exif parameter)
            img.save(output_path, 'JPEG', quality=quality, optimize=True)
            
            return True
            
    except Exception as e:
        print(f"Error optimizing image: {e}", file=sys.stderr)
        return False

def main():
    if len(sys.argv) < 3:
        print("Usage: optimize_image.py <input_path> <output_path> [max_dimension] [quality]", file=sys.stderr)
        sys.exit(1)
    
    input_path = sys.argv[1]
    output_path = sys.argv[2]
    max_dimension = int(sys.argv[3]) if len(sys.argv) > 3 else DEFAULT_MAX_DIMENSION
    quality = int(sys.argv[4]) if len(sys.argv) > 4 else DEFAULT_QUALITY
    
    if not os.path.exists(input_path):
        print(f"Input file not found: {input_path}", file=sys.stderr)
        sys.exit(1)
    
    if optimize_image(input_path, output_path, max_dimension, quality):
        # Print output path on success (for Lisp to capture)
        print(output_path)
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == '__main__':
    main()
