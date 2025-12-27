#!/usr/bin/env python3
"""
TFS-CMMS Issue Tracker / Bug Report Form Generator

Generates a fillable PDF form for reporting bugs and unexpected behavior.
Uses the same styling as the Typst documents (logos, headers, etc.)
"""

import os
from datetime import datetime
from reportlab.lib import colors
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm, mm
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Image, Table, TableStyle
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfform
from reportlab.lib.enums import TA_CENTER, TA_LEFT

# Colors matching the Typst template
NAVY_BLUE = colors.HexColor('#003366')
GRAY = colors.HexColor('#666666')
LIGHT_GRAY = colors.HexColor('#f5f5f5')
WHITE = colors.white

# Get the base directory
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


class IssueTrackerForm:
    def __init__(self, output_path):
        self.output_path = output_path
        self.width, self.height = A4
        self.margin = 2.5 * cm
        
    def generate(self):
        """Generate the issue tracker PDF form."""
        c = canvas.Canvas(self.output_path, pagesize=A4)
        
        # Draw the form
        self._draw_header(c)
        self._draw_form_fields(c)
        self._draw_footer(c)
        
        c.save()
        print(f"Generated: {self.output_path}")
    
    def _draw_header(self, c):
        """Draw the header with logos and title."""
        # TFS Logo (top left)
        tfs_logo_path = os.path.join(BASE_DIR, 'TFS_Logo.png')
        if os.path.exists(tfs_logo_path):
            c.drawImage(tfs_logo_path, self.margin, self.height - 3.5*cm, 
                       width=4*cm, height=2.5*cm, preserveAspectRatio=True)
        
        # Company logo (top right)
        company_logo_path = os.path.join(BASE_DIR, 'company_logo.png')
        if os.path.exists(company_logo_path):
            c.drawImage(company_logo_path, self.width - self.margin - 4*cm, 
                       self.height - 3.5*cm, width=4*cm, height=2.5*cm, 
                       preserveAspectRatio=True)
        
        # Title
        c.setFont("Helvetica-Bold", 24)
        c.setFillColor(NAVY_BLUE)
        title = "TFS-CMMS"
        title_width = c.stringWidth(title, "Helvetica-Bold", 24)
        c.drawString((self.width - title_width) / 2, self.height - 4.5*cm, title)
        
        # Subtitle
        c.setFont("Helvetica", 14)
        subtitle = "Issue / Bug Report Form"
        subtitle_width = c.stringWidth(subtitle, "Helvetica", 14)
        c.drawString((self.width - subtitle_width) / 2, self.height - 5.2*cm, subtitle)
        
        # Horizontal line
        c.setStrokeColor(NAVY_BLUE)
        c.setLineWidth(2)
        c.line(self.margin, self.height - 5.8*cm, 
               self.width - self.margin, self.height - 5.8*cm)
        
        # Version and date
        c.setFont("Helvetica", 9)
        c.setFillColor(GRAY)
        version_text = f"Version 1.0 — {datetime.now().strftime('%B %Y')}"
        c.drawRightString(self.width - self.margin, self.height - 6.3*cm, version_text)
    
    def _draw_form_fields(self, c):
        """Draw the form fields."""
        y_pos = self.height - 7.5*cm
        field_height = 0.8*cm
        label_width = 4*cm
        field_width = self.width - 2*self.margin - label_width - 0.5*cm
        
        # Form styling
        c.setFont("Helvetica-Bold", 11)
        c.setFillColor(NAVY_BLUE)
        
        # Section 1: Reporter Information
        c.drawString(self.margin, y_pos, "REPORTER INFORMATION")
        y_pos -= 0.8*cm
        
        fields_section1 = [
            ("Name:", "reporter_name"),
            ("Email:", "reporter_email"),
            ("Date:", "report_date"),
            ("Role:", "reporter_role"),
        ]
        
        for label, field_name in fields_section1:
            y_pos = self._draw_text_field(c, label, field_name, y_pos, 
                                          label_width, field_width, field_height)
        
        y_pos -= 0.5*cm
        
        # Section 2: Issue Details
        c.setFont("Helvetica-Bold", 11)
        c.setFillColor(NAVY_BLUE)
        c.drawString(self.margin, y_pos, "ISSUE DETAILS")
        y_pos -= 0.8*cm
        
        # Issue Type (checkboxes)
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Issue Type:")
        
        checkbox_x = self.margin + label_width
        issue_types = ["Bug", "Unexpected Behavior", "Feature Request", "Data Issue", "Other"]
        for i, issue_type in enumerate(issue_types):
            if i > 0 and i % 3 == 0:
                y_pos -= 0.6*cm
                checkbox_x = self.margin + label_width
            c.acroForm.checkbox(
                name=f'issue_type_{i}',
                x=checkbox_x,
                y=y_pos - 0.15*cm,
                size=12,
                buttonStyle='check',
                borderColor=NAVY_BLUE,
                fillColor=WHITE,
            )
            c.setFont("Helvetica", 9)
            c.drawString(checkbox_x + 0.5*cm, y_pos, issue_type)
            checkbox_x += 3.5*cm
        
        y_pos -= 1.2*cm
        
        # Priority
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Priority:")
        
        checkbox_x = self.margin + label_width
        priorities = ["Critical", "High", "Medium", "Low"]
        for i, priority in enumerate(priorities):
            c.acroForm.checkbox(
                name=f'priority_{i}',
                x=checkbox_x,
                y=y_pos - 0.15*cm,
                size=12,
                buttonStyle='check',
                borderColor=NAVY_BLUE,
                fillColor=WHITE,
            )
            c.setFont("Helvetica", 9)
            c.drawString(checkbox_x + 0.5*cm, y_pos, priority)
            checkbox_x += 3*cm
        
        y_pos -= 1*cm
        
        # Page/Feature affected
        y_pos = self._draw_text_field(c, "Page/Feature:", "page_feature", y_pos,
                                      label_width, field_width, field_height)
        
        y_pos -= 0.3*cm
        
        # Issue Summary
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Issue Summary:")
        y_pos -= 0.5*cm
        
        # Multi-line text area for summary
        c.acroForm.textfield(
            name='issue_summary',
            x=self.margin,
            y=y_pos - 2*cm,
            width=self.width - 2*self.margin,
            height=2*cm,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
            fieldFlags='multiline',
        )
        y_pos -= 2.5*cm
        
        # Steps to Reproduce
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Steps to Reproduce:")
        y_pos -= 0.5*cm
        
        c.acroForm.textfield(
            name='steps_to_reproduce',
            x=self.margin,
            y=y_pos - 3*cm,
            width=self.width - 2*self.margin,
            height=3*cm,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
            fieldFlags='multiline',
        )
        y_pos -= 3.5*cm
        
        # Expected Behavior
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Expected Behavior:")
        y_pos -= 0.5*cm
        
        c.acroForm.textfield(
            name='expected_behavior',
            x=self.margin,
            y=y_pos - 1.5*cm,
            width=self.width - 2*self.margin,
            height=1.5*cm,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
            fieldFlags='multiline',
        )
        y_pos -= 2*cm
        
        # Actual Behavior
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Actual Behavior:")
        y_pos -= 0.5*cm
        
        c.acroForm.textfield(
            name='actual_behavior',
            x=self.margin,
            y=y_pos - 1.5*cm,
            width=self.width - 2*self.margin,
            height=1.5*cm,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
            fieldFlags='multiline',
        )
        y_pos -= 2*cm
        
        # Additional Notes
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, "Additional Notes / Screenshots Reference:")
        y_pos -= 0.5*cm
        
        c.acroForm.textfield(
            name='additional_notes',
            x=self.margin,
            y=y_pos - 2*cm,
            width=self.width - 2*self.margin,
            height=2*cm,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
            fieldFlags='multiline',
        )
    
    def _draw_text_field(self, c, label, field_name, y_pos, label_width, field_width, field_height):
        """Draw a labeled text field and return the new y position."""
        c.setFont("Helvetica-Bold", 10)
        c.setFillColor(colors.black)
        c.drawString(self.margin, y_pos, label)
        
        c.acroForm.textfield(
            name=field_name,
            x=self.margin + label_width,
            y=y_pos - 0.2*cm,
            width=field_width,
            height=field_height,
            borderColor=NAVY_BLUE,
            fillColor=LIGHT_GRAY,
            textColor=colors.black,
            fontSize=10,
        )
        
        return y_pos - field_height - 0.3*cm
    
    def _draw_footer(self, c):
        """Draw the footer."""
        c.setFont("Helvetica", 9)
        c.setFillColor(GRAY)
        
        # Line above footer
        c.setStrokeColor(GRAY)
        c.setLineWidth(0.5)
        c.line(self.margin, 2*cm, self.width - self.margin, 2*cm)
        
        # Left side - first line
        c.drawString(self.margin, 1.5*cm, 
                    "Task Force SAFE — Computerized Maintenance Management System")
        
        # Right side - second line below
        c.drawString(self.margin, 0.9*cm,
                    "Please submit completed forms to the system administrator")


def main():
    """Generate the issue tracker form."""
    output_dir = os.path.join(BASE_DIR, 'docs')
    os.makedirs(output_dir, exist_ok=True)
    
    output_path = os.path.join(output_dir, 'TFS-CMMS_Issue_Report_Form.pdf')
    
    form = IssueTrackerForm(output_path)
    form.generate()
    
    print(f"\nIssue tracker form generated successfully!")
    print(f"Output: {output_path}")


if __name__ == '__main__':
    main()
