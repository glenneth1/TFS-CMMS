#!/usr/bin/env python3
"""
PDF Generator for TFS CMMS Inspection Reports
Generates PDF reports matching the Excel template format.
"""

import json
import sys
import os
from datetime import datetime
from io import BytesIO
from reportlab.lib import colors
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, Image, PageBreak, ListFlowable, ListItem, PageTemplate, Frame, BaseDocTemplate
from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_RIGHT
from reportlab.pdfgen import canvas

try:
    from PIL import Image as PILImage
    HAS_PIL = True
except ImportError:
    HAS_PIL = False

def compress_image(image_path, max_width=400, quality=60):
    """Compress image for smaller PDF size. Returns path to compressed image or original."""
    if not HAS_PIL:
        return image_path
    try:
        with PILImage.open(image_path) as img:
            # Convert to RGB if necessary
            if img.mode in ('RGBA', 'P'):
                img = img.convert('RGB')
            # Resize if too large
            if img.width > max_width:
                ratio = max_width / img.width
                new_size = (max_width, int(img.height * ratio))
                img = img.resize(new_size, PILImage.Resampling.LANCZOS)
            # Save to temp file with compression
            temp_path = image_path + '.compressed.jpg'
            img.save(temp_path, 'JPEG', quality=quality, optimize=True)
            return temp_path
    except Exception:
        return image_path

# Page dimensions
PAGE_WIDTH, PAGE_HEIGHT = letter

# Cover sheet content
COVER_OBJECTIVE = """The objective of this service contract is for the Contractor to provide technical services including, but not limited to inspections, assessments, repairs, testing, reports, training for facilities, and operations-related projects."""

COVER_DEFINITIONS = [
    ("LHS Electrical Deficiency:", "A deficiency that is in its current state likely to bring harm or damage."),
    ("Imminent LHS Electrical Deficiency is defined as:", "A deficiency that is in its current state is certain to bring harm or damage."),
    ("Mitigated is defined as:", "The action of reducing an Imminent LHS deficiency."),
    ("RAC – Risk Assessment Code:", "To provide insight for safety personnel the severity of identified deficiencies if left as found and is meant for information purposes only."),
    ("RAC 1:", "Immediate danger to public or property with critical to catastrophic consequences."),
    ("RAC 2:", "Possible danger to public or property with significant to minor consequences."),
]

COVER_WORKFLOW = [
    "Inspections will be performed from source to end users.",
    "When a deficiency is identified, it will always be documented, at that point and time it was found.",
    "If a deficiency is found it will never be left in its current unsafe condition. A deficiency will either be repaired or mitigated.",
    "Mitigating actions could include, but are not limited to, isolating the supply, posting warning signs and/or barriers or even just making the site aware of the risk.",
    "On recording any deficiency, it will be recorded at the status as to which it was initially inspected.",
    "Actions have been put in place by TFS, in order to allow a true reflection of the condition of all inspected electrical systems and facilities.",
]

COVER_CLOSING = """The inspection report provides a simple engineering view of a facilities electrical safety condition where it can continue to be used safely, along with all LHS deficiencies being highlighted in accordance with the general scope of the TF SAFE contract. This medium can supply a brief insight to the possible risk factors in and around a specific location within the AOR either CENTCOM wide or locally for the commander at a specific FOB. The TFS team can provide specialist knowledge and experience to assist in reducing the overall risk."""

def create_cover_sheet(static_dir):
    """Create the cover sheet as the first page - all content on single page."""
    styles = getSampleStyleSheet()
    elements = []
    
    # Custom styles for cover sheet - compact to fit on one page
    heading_style = ParagraphStyle(
        'CoverHeading',
        parent=styles['Heading2'],
        fontSize=10,
        spaceBefore=4,
        spaceAfter=2,
        textColor=colors.HexColor('#2c5282'),
    )
    
    body_style = ParagraphStyle(
        'CoverBody',
        parent=styles['Normal'],
        fontSize=8,
        spaceAfter=3,
        leading=10
    )
    
    bullet_style = ParagraphStyle(
        'CoverBullet',
        parent=styles['Normal'],
        fontSize=8,
        leftIndent=15,
        spaceAfter=1,
        leading=9
    )
    
    sub_bullet_style = ParagraphStyle(
        'CoverSubBullet',
        parent=styles['Normal'],
        fontSize=8,
        leftIndent=30,
        spaceAfter=1,
        leading=9
    )
    
    title_style = ParagraphStyle(
        'CoverTitle',
        parent=styles['Heading1'],
        fontSize=16,
        alignment=TA_CENTER,
        spaceBefore=0,
        spaceAfter=8,
        textColor=colors.HexColor('#2c5282')
    )
    
    # Header with logos
    tfs_logo_path = os.path.join(static_dir, 'img/TFS_Logo.png')
    company_logo_path = os.path.join(static_dir, 'img/company_logo.png')
    
    logo_data = [[None, None, None]]
    
    if os.path.exists(tfs_logo_path):
        try:
            tfs_logo = Image(tfs_logo_path, width=1*inch, height=1*inch)
            logo_data[0][0] = tfs_logo
        except:
            logo_data[0][0] = ''
    
    if os.path.exists(company_logo_path):
        try:
            company_logo = Image(company_logo_path, width=1*inch, height=0.5*inch)
            logo_data[0][2] = company_logo
        except:
            logo_data[0][2] = ''
    
    logo_table = Table(logo_data, colWidths=[2.5*inch, 2.5*inch, 2.5*inch])
    logo_table.setStyle(TableStyle([
        ('ALIGN', (0, 0), (0, 0), 'LEFT'),
        ('ALIGN', (2, 0), (2, 0), 'RIGHT'),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
    ]))
    elements.append(logo_table)
    elements.append(Spacer(1, 0.1*inch))
    
    # Title
    elements.append(Paragraph("Task Force SAFE", title_style))
    
    # Objective
    elements.append(Paragraph("<u><b>Objective</b></u>", heading_style))
    elements.append(Paragraph(COVER_OBJECTIVE, body_style))
    
    # Definitions
    elements.append(Paragraph("<u><b>Definitions</b></u>", heading_style))
    for term, definition in COVER_DEFINITIONS:
        elements.append(Paragraph(f"• <b>{term}</b> {definition}", bullet_style))
    
    # Workflow
    elements.append(Paragraph("<u><b>Workflow</b></u>", heading_style))
    for i, item in enumerate(COVER_WORKFLOW, 1):
        elements.append(Paragraph(f"{i}. {item}", bullet_style))
    
    # Closing Summary
    elements.append(Paragraph("<u><b>Closing Summary</b></u>", heading_style))
    elements.append(Paragraph(COVER_CLOSING, body_style))
    
    # Page break after cover sheet
    elements.append(PageBreak())
    
    return elements

def create_header_table(report_data):
    """Create the header section with report details."""
    data = [
        ['Base Location', report_data.get('site_name', ''), 'Date:', report_data.get('inspection_date', '')],
        ['Base Code', report_data.get('site_code', ''), 'Team #:', report_data.get('team_number', '')],
        ['Building #', report_data.get('building_number', ''), 'System Voltage', report_data.get('system_voltage', '')],
        ['Building Type', report_data.get('building_type', ''), 'Inspection Phase', report_data.get('inspection_phase', '')],
        ['Previous Report #:', report_data.get('previous_report', 'N/A'), 'Work Order:', report_data.get('work_order_number', 'N/A')],
    ]
    
    table = Table(data, colWidths=[1.5*inch, 2*inch, 1.5*inch, 2*inch])
    table.setStyle(TableStyle([
        ('FONTNAME', (0, 0), (-1, -1), 'Helvetica'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('FONTNAME', (0, 0), (0, -1), 'Helvetica-Bold'),
        ('FONTNAME', (2, 0), (2, -1), 'Helvetica-Bold'),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ('BACKGROUND', (0, 0), (0, -1), colors.lightgrey),
        ('BACKGROUND', (2, 0), (2, -1), colors.lightgrey),
    ]))
    return table

def create_location_section(report_data):
    """Create the location description section."""
    styles = getSampleStyleSheet()
    elements = []
    
    elements.append(Paragraph("<b>Physical Building Location Description:</b>", styles['Normal']))
    elements.append(Paragraph(report_data.get('location_description', '-'), styles['Normal']))
    elements.append(Spacer(1, 0.1*inch))
    
    # Overall rating
    rating = report_data.get('overall_rating', 'Not Rated')
    rating_color = {'Good': 'green', 'Fair': 'orange', 'Poor': 'red'}.get(rating, 'black')
    elements.append(Paragraph(f"<b>Overall Building Electrical System:</b> <font color='{rating_color}'>{rating}</font>", styles['Normal']))
    
    return elements

def get_rac_color(rac_score):
    """Get color for RAC score."""
    colors_map = {1: colors.red, 2: colors.orange, 3: colors.yellow, 4: colors.green}
    return colors_map.get(rac_score, colors.grey)

def create_deficiency_block(def_data, uploads_dir):
    """Create a deficiency block with description first, then image on left, details on right."""
    styles = getSampleStyleSheet()
    elements = []
    
    def_num = def_data.get('deficiency_number', '?')
    rac = def_data.get('rac_score', 3)
    
    # Styles for wrapped text
    cell_style = ParagraphStyle('Cell', parent=styles['Normal'], fontSize=8, leading=10)
    desc_style = ParagraphStyle('Desc', parent=styles['Normal'], fontSize=9, leading=11)
    
    # Deficiency header
    header_data = [
        [f"Deficiency #{def_num}", f"RAC {rac}", f"Occurrences: {def_data.get('num_occurrences', 1)}"]
    ]
    header_table = Table(header_data, colWidths=[2*inch, 0.8*inch, 1.5*inch])
    header_table.setStyle(TableStyle([
        ('FONTNAME', (0, 0), (-1, -1), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('BACKGROUND', (1, 0), (1, 0), get_rac_color(rac)),
        ('TEXTCOLOR', (1, 0), (1, 0), colors.white if rac <= 2 else colors.black),
        ('ALIGN', (1, 0), (1, 0), 'CENTER'),
        ('BOX', (0, 0), (-1, -1), 0.5, colors.black),
    ]))
    elements.append(header_table)
    elements.append(Spacer(1, 0.05*inch))
    
    # Description FIRST - so you read what the deficiency is before details
    desc_text = str(def_data.get('description', '-'))
    elements.append(Paragraph(f"<b>Description:</b> {desc_text}", desc_style))
    elements.append(Spacer(1, 0.08*inch))
    
    # Build image element if exists
    img_element = None
    image_path = def_data.get('image_path')
    if image_path:
        full_path = os.path.join(uploads_dir, image_path)
        if os.path.exists(full_path):
            try:
                img_element = Image(full_path, width=2*inch, height=1.5*inch)
            except:
                pass
    
    # Details with Paragraph for text wrapping on ALL fields
    code_text = f"{def_data.get('code_source', '-')} {def_data.get('code_reference', '')}"
    details_data = [
        ['Location:', Paragraph(str(def_data.get('location_description', '-')), cell_style)],
        ['Category:', Paragraph(str(def_data.get('deficiency_category', '-')), cell_style)],
        ['Equipment:', Paragraph(str(def_data.get('equipment_category', '-')), cell_style)],
        ['Imminent:', def_data.get('imminent_danger', 'No')],
        ['Action:', Paragraph(str(def_data.get('action_taken', 'N/A') or 'N/A'), cell_style)],
        ['Code:', Paragraph(code_text, cell_style)],
        ['Status:', def_data.get('deficiency_status', '-')],
    ]
    
    details_table = Table(details_data, colWidths=[0.7*inch, 3*inch])
    details_table.setStyle(TableStyle([
        ('FONTNAME', (0, 0), (0, -1), 'Helvetica-Bold'),
        ('FONTNAME', (1, 0), (1, -1), 'Helvetica'),
        ('FONTSIZE', (0, 0), (-1, -1), 8),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
        ('TOPPADDING', (0, 0), (-1, -1), 2),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 2),
        ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
    ]))
    
    # Layout: image on left, details on right
    if img_element:
        layout_data = [[img_element, details_table]]
        layout_table = Table(layout_data, colWidths=[2.1*inch, 3.8*inch])
        layout_table.setStyle(TableStyle([
            ('VALIGN', (0, 0), (-1, -1), 'TOP'),
            ('LEFTPADDING', (0, 0), (-1, -1), 2),
            ('RIGHTPADDING', (0, 0), (-1, -1), 2),
        ]))
        elements.append(layout_table)
    else:
        elements.append(details_table)
    
    elements.append(Spacer(1, 0.25*inch))
    
    return elements

def create_signature_section(report_data):
    """Create the signatures section."""
    styles = getSampleStyleSheet()
    elements = []
    
    elements.append(Paragraph("<b>Signatures:</b>", styles['Heading3']))
    
    sig_data = [
        ['Inspector 1:', report_data.get('inspector1_name', ''), 'Signed:', report_data.get('inspector1_signed_at', '')],
        ['Inspector 2:', report_data.get('inspector2_name', '-'), 'Signed:', report_data.get('inspector2_signed_at', '-')],
        ['QC:', report_data.get('qc_name', ''), 'Signed:', report_data.get('qc_signed_at', '')],
    ]
    
    sig_table = Table(sig_data, colWidths=[1*inch, 2.5*inch, 1*inch, 2*inch])
    sig_table.setStyle(TableStyle([
        ('FONTNAME', (0, 0), (-1, -1), 'Helvetica'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('FONTNAME', (0, 0), (0, -1), 'Helvetica-Bold'),
        ('FONTNAME', (2, 0), (2, -1), 'Helvetica-Bold'),
        ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
    ]))
    elements.append(sig_table)
    
    return elements

def create_summary_section(report_data):
    """Create the summary of findings section."""
    styles = getSampleStyleSheet()
    elements = []
    
    elements.append(Paragraph("<b>Summary of Findings:</b>", styles['Heading3']))
    summary = report_data.get('summary_of_findings', 'No summary provided.')
    elements.append(Paragraph(summary, styles['Normal']))
    
    return elements

class NumberedCanvas(canvas.Canvas):
    """Canvas that tracks page count for 'Page X of Y' numbering and adds header."""
    def __init__(self, *args, **kwargs):
        canvas.Canvas.__init__(self, *args, **kwargs)
        self._saved_page_states = []

    def showPage(self):
        self._saved_page_states.append(dict(self.__dict__))
        self._startPage()

    def save(self):
        num_pages = len(self._saved_page_states)
        for state in self._saved_page_states:
            self.__dict__.update(state)
            self.draw_header()
            self.draw_page_number(num_pages)
            canvas.Canvas.showPage(self)
        canvas.Canvas.save(self)

    def draw_header(self):
        """Draw 'TF SAFE CMMS - Report' header at top of each page."""
        self.saveState()
        self.setFont('Helvetica', 8)
        self.setFillColor(colors.HexColor('#666666'))
        self.drawCentredString(PAGE_WIDTH / 2, PAGE_HEIGHT - 0.25 * inch, "TF SAFE CMMS - Report")
        self.restoreState()

    def draw_page_number(self, page_count):
        self.setFont('Helvetica', 8)
        text = f"Page {self._pageNumber} of {page_count}"
        self.drawCentredString(PAGE_WIDTH / 2, 0.3 * inch, text)

def generate_pdf(report_data, deficiencies, output_path, uploads_dir, static_dir=None):
    """Generate the PDF report."""
    doc = SimpleDocTemplate(
        output_path,
        pagesize=letter,
        rightMargin=0.3*inch,
        leftMargin=0.3*inch,
        topMargin=0.4*inch,
        bottomMargin=0.5*inch
    )
    
    styles = getSampleStyleSheet()
    elements = []
    
    # Cover sheet first
    if static_dir:
        elements.extend(create_cover_sheet(static_dir))
    
    # Title
    title_style = ParagraphStyle(
        'Title',
        parent=styles['Heading1'],
        fontSize=14,
        alignment=TA_CENTER,
        spaceAfter=12
    )
    elements.append(Paragraph(report_data.get('report_number', 'Inspection Report'), title_style))
    elements.append(Spacer(1, 0.1*inch))
    
    # Header
    elements.append(create_header_table(report_data))
    elements.append(Spacer(1, 0.2*inch))
    
    # Location and rating
    elements.extend(create_location_section(report_data))
    elements.append(Spacer(1, 0.2*inch))
    
    # Building image if exists - smaller to save space
    building_image = report_data.get('building_image_path')
    if building_image:
        full_path = os.path.join(uploads_dir, building_image)
        if os.path.exists(full_path):
            try:
                elements.append(Paragraph("<b>Building Picture:</b>", styles['Normal']))
                img = Image(full_path, width=2.5*inch, height=2*inch)
                img.hAlign = 'LEFT'
                elements.append(img)
                elements.append(Spacer(1, 0.1*inch))
            except:
                pass
    
    # Deficiencies - sort by deficiency_number ascending
    if deficiencies:
        sorted_defs = sorted(deficiencies, key=lambda d: d.get('deficiency_number', 0))
        elements.append(Paragraph(f"<b>Deficiencies ({len(sorted_defs)}):</b>", styles['Heading2']))
        elements.append(Spacer(1, 0.05*inch))
        
        # Start deficiencies on new page for cleaner layout
        elements.append(PageBreak())
        
        for i, def_data in enumerate(sorted_defs):
            elements.extend(create_deficiency_block(def_data, uploads_dir))
            # Add page break every 3 deficiencies
            if (i + 1) % 3 == 0 and i < len(sorted_defs) - 1:
                elements.append(PageBreak())
    
    # Summary page
    elements.append(PageBreak())
    elements.append(Paragraph(report_data.get('report_number', 'Inspection Report'), title_style))
    elements.append(Spacer(1, 0.1*inch))
    elements.append(create_header_table(report_data))
    elements.append(Spacer(1, 0.2*inch))
    elements.extend(create_summary_section(report_data))
    elements.append(Spacer(1, 0.3*inch))
    elements.extend(create_signature_section(report_data))
    
    # Build PDF with page numbers (Page X of Y)
    doc.build(elements, canvasmaker=NumberedCanvas)
    return output_path

def main():
    if len(sys.argv) < 3:
        print("Usage: generate_pdf.py <json_data_file> <output_pdf_path> [uploads_dir] [static_dir]")
        sys.exit(1)
    
    json_file = sys.argv[1]
    output_path = sys.argv[2]
    uploads_dir = sys.argv[3] if len(sys.argv) > 3 else 'data/uploads'
    static_dir = sys.argv[4] if len(sys.argv) > 4 else 'static'
    
    with open(json_file, 'r') as f:
        data = json.load(f)
    
    report_data = data.get('report', {})
    deficiencies = data.get('deficiencies', [])
    
    generate_pdf(report_data, deficiencies, output_path, uploads_dir, static_dir)
    print(f"PDF generated: {output_path}")

if __name__ == '__main__':
    main()
