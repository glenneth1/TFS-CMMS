#!/usr/bin/env python3
"""
PDF Generator for TFS CMMS Material Request Forms (MRF)
Generates PDF reports with consistent TF SAFE CMMS branding.
"""

import json
import sys
import os
from datetime import datetime
from reportlab.lib import colors
from reportlab.lib.pagesizes import letter, landscape
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, Image, BaseDocTemplate, PageTemplate, Frame
from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_RIGHT

# Page dimensions - landscape mode
PAGE_WIDTH, PAGE_HEIGHT = landscape(letter)

class NumberedCanvas:
    """Canvas that adds page numbers."""
    def __init__(self, canvas, doc):
        self.canvas = canvas
        self.doc = doc
        
    def __getattr__(self, name):
        return getattr(self.canvas, name)
    
    def showPage(self):
        self._draw_page_number()
        self.canvas.showPage()
        
    def save(self):
        self._draw_page_number()
        self.canvas.save()
        
    def _draw_page_number(self):
        page_num = self.canvas.getPageNumber()
        text = f"Page {page_num}"
        self.canvas.setFont('Helvetica', 9)
        self.canvas.drawRightString(PAGE_WIDTH - 0.5*inch, 0.4*inch, text)

def create_header(static_dir):
    """Create the header with both logos and title."""
    elements = []
    
    # Try to load logos
    versar_logo = os.path.join(static_dir, 'img', 'company_logo.png')
    tfs_logo = os.path.join(static_dir, 'img', 'TFS_Logo.png')
    
    # Header table with logos and title
    header_data = []
    
    # Left logo - preserve aspect ratio
    if os.path.exists(versar_logo):
        left_img = Image(versar_logo, width=1.2*inch, height=0.5*inch, kind='proportional')
    else:
        left_img = ""
    
    # Center title
    styles = getSampleStyleSheet()
    subtitle_style = ParagraphStyle('Subtitle', parent=styles['Normal'], fontSize=8, textColor=colors.gray, alignment=TA_CENTER)
    title_style = ParagraphStyle('Title', parent=styles['Heading1'], fontSize=14, alignment=TA_CENTER, spaceAfter=2)
    tagline_style = ParagraphStyle('Tagline', parent=styles['Normal'], fontSize=9, alignment=TA_CENTER)
    
    center_content = [
        Paragraph("TF SAFE CMMS - Report", subtitle_style),
        Paragraph("Materials / Tools Request", title_style),
        Paragraph("Task Force SAFE - CENTCOM AOR", tagline_style)
    ]
    
    # Right logo - preserve aspect ratio
    if os.path.exists(tfs_logo):
        right_img = Image(tfs_logo, width=1.2*inch, height=0.5*inch, kind='proportional')
    else:
        right_img = ""
    
    header_data = [[left_img, center_content, right_img]]
    
    # Landscape mode - wider header
    header_table = Table(header_data, colWidths=[2*inch, 6*inch, 2*inch])
    header_table.setStyle(TableStyle([
        ('ALIGN', (0, 0), (0, 0), 'LEFT'),
        ('ALIGN', (1, 0), (1, 0), 'CENTER'),
        ('ALIGN', (2, 0), (2, 0), 'RIGHT'),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('LINEBELOW', (0, 0), (-1, 0), 2, colors.HexColor('#1a365d')),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 10),
    ]))
    
    elements.append(header_table)
    elements.append(Spacer(1, 0.3*inch))
    
    return elements

def create_info_section(mrf_data):
    """Create the MRF information section with full-width MRF number heading."""
    elements = []
    styles = getSampleStyleSheet()
    
    # Full-width MRF Number heading
    mrf_heading_style = ParagraphStyle('MRFHeading', parent=styles['Heading1'], 
                                        fontSize=14, fontName='Helvetica-Bold', 
                                        alignment=TA_CENTER, spaceAfter=10,
                                        textColor=colors.HexColor('#1a365d'))
    elements.append(Paragraph(mrf_data.get('mrf_number', ''), mrf_heading_style))
    elements.append(Spacer(1, 0.1*inch))
    
    label_style = ParagraphStyle('Label', parent=styles['Normal'], fontSize=10, fontName='Helvetica-Bold')
    value_style = ParagraphStyle('Value', parent=styles['Normal'], fontSize=10)
    
    # Info grid - 2 columns (without MRF number, it's now a heading)
    info_data = [
        [Paragraph("<b>Date:</b>", label_style), Paragraph(mrf_data.get('request_date', ''), value_style),
         Paragraph("<b>Status:</b>", label_style), Paragraph(mrf_data.get('status', ''), value_style)],
        [Paragraph("<b>Team Number:</b>", label_style), Paragraph(mrf_data.get('team_number', ''), value_style),
         Paragraph("<b>Contract:</b>", label_style), Paragraph(mrf_data.get('contract_number', ''), value_style)],
        [Paragraph("<b>Site Support:</b>", label_style), Paragraph(mrf_data.get('site_support_location', ''), value_style),
         Paragraph("<b>Base:</b>", label_style), Paragraph(mrf_data.get('base', ''), value_style)],
        [Paragraph("<b>Camp Name:</b>", label_style), Paragraph(mrf_data.get('camp_name', 'N/A'), value_style),
         Paragraph("<b>BLD # / Name:</b>", label_style), Paragraph(mrf_data.get('building_number', ''), value_style)],
        [Paragraph("<b>Inspector:</b>", label_style), Paragraph(mrf_data.get('requestor_name', ''), value_style),
         Paragraph("", label_style), Paragraph("", value_style)],
    ]
    
    # Landscape mode - wider info table
    info_table = Table(info_data, colWidths=[1.2*inch, 3.3*inch, 1.2*inch, 3.3*inch])
    info_table.setStyle(TableStyle([
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
        ('TOPPADDING', (0, 0), (-1, -1), 4),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 4),
    ]))
    
    elements.append(info_table)
    elements.append(Spacer(1, 0.3*inch))
    
    return elements

def create_items_table(items):
    """Create the items table."""
    elements = []
    styles = getSampleStyleSheet()
    
    # Table header
    header_style = ParagraphStyle('TableHeader', parent=styles['Normal'], fontSize=9, fontName='Helvetica-Bold', textColor=colors.white)
    cell_style = ParagraphStyle('TableCell', parent=styles['Normal'], fontSize=9)
    
    table_data = [[
        Paragraph("#", header_style),
        Paragraph("Part Number", header_style),
        Paragraph("Nomenclature", header_style),
        Paragraph("Qty", header_style),
        Paragraph("UOM", header_style),
        Paragraph("Mat", header_style),
        Paragraph("Tool", header_style),
        Paragraph("Notes", header_style),
    ]]
    
    for i, item in enumerate(items, 1):
        qty = item.get('quantity', 0)
        # Format quantity as integer if it's a whole number
        if isinstance(qty, float) and qty == int(qty):
            qty = int(qty)
        
        table_data.append([
            Paragraph(str(i), cell_style),
            Paragraph(item.get('part_number', '') or '', cell_style),
            Paragraph(item.get('nomenclature', '') or '', cell_style),
            Paragraph(str(qty), cell_style),
            Paragraph(item.get('uom', '') or '', cell_style),
            Paragraph('Yes' if item.get('is_material') else 'No', cell_style),
            Paragraph('Yes' if item.get('is_tool') else 'No', cell_style),
            Paragraph(item.get('notes', '') or '', cell_style),
        ])
    
    # If no items, add empty row
    if not items:
        table_data.append([Paragraph("No items", cell_style), "", "", "", "", "", "", ""])
    
    # Landscape mode - wider columns (total ~10 inches usable width)
    items_table = Table(table_data, colWidths=[0.4*inch, 1.3*inch, 3.5*inch, 0.5*inch, 0.6*inch, 0.5*inch, 0.5*inch, 2.7*inch])
    items_table.setStyle(TableStyle([
        # Header row
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#1a365d')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('ALIGN', (0, 0), (-1, 0), 'CENTER'),
        # All cells
        ('GRID', (0, 0), (-1, -1), 0.5, colors.black),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 4),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 4),
        # Alternating row colors
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f7fafc')]),
        # Center align quantity, UOM, Mat, Tool columns
        ('ALIGN', (3, 1), (3, -1), 'CENTER'),
        ('ALIGN', (4, 1), (6, -1), 'CENTER'),
    ]))
    
    elements.append(items_table)
    
    return elements

def generate_mrf_pdf(json_path, output_path, static_dir):
    """Generate the MRF PDF from JSON data."""
    
    # Load JSON data
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    mrf_data = data.get('mrf', {})
    items = data.get('items', [])
    
    # Create PDF document in landscape mode
    doc = SimpleDocTemplate(
        output_path,
        pagesize=landscape(letter),
        rightMargin=0.5*inch,
        leftMargin=0.5*inch,
        topMargin=0.5*inch,
        bottomMargin=0.5*inch
    )
    
    elements = []
    
    # Add header
    elements.extend(create_header(static_dir))
    
    # Add info section
    elements.extend(create_info_section(mrf_data))
    
    # Add items table
    elements.extend(create_items_table(items))
    
    # Build PDF first to get total page count, then rebuild with page numbers
    # First pass - build to count pages
    from io import BytesIO
    temp_buffer = BytesIO()
    temp_doc = SimpleDocTemplate(temp_buffer, pagesize=landscape(letter),
                                  rightMargin=0.5*inch, leftMargin=0.5*inch,
                                  topMargin=0.5*inch, bottomMargin=0.5*inch)
    temp_doc.build(elements[:])  # Use copy of elements
    total_pages = temp_doc.page
    
    # Second pass - build with page numbers showing "Page X of Y"
    def add_page_number(canvas, doc):
        page_num = canvas.getPageNumber()
        text = f"Page {page_num} of {total_pages}"
        canvas.setFont('Helvetica', 9)
        canvas.drawRightString(PAGE_WIDTH - 0.5*inch, 0.4*inch, text)
    
    doc.build(elements, onFirstPage=add_page_number, onLaterPages=add_page_number)
    
    return output_path

def main():
    if len(sys.argv) < 4:
        print("Usage: generate_mrf_pdf.py <json_path> <output_path> <static_dir>")
        sys.exit(1)
    
    json_path = sys.argv[1]
    output_path = sys.argv[2]
    static_dir = sys.argv[3]
    
    try:
        result = generate_mrf_pdf(json_path, output_path, static_dir)
        print(f"PDF generated: {result}")
    except Exception as e:
        print(f"Error generating PDF: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
