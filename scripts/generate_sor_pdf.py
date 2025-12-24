#!/usr/bin/env python3
"""
PDF Generator for TFS CMMS SOR (Statement of Requirement) Reports
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
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, Image
from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_RIGHT

# Page dimensions - landscape mode for wide tables
PAGE_WIDTH, PAGE_HEIGHT = landscape(letter)

def create_header(static_dir, report_title="SOR Report"):
    """Create the header with both logos and title."""
    elements = []
    
    # Try to load logos
    versar_logo = os.path.join(static_dir, 'img', 'company_logo.png')
    tfs_logo = os.path.join(static_dir, 'img', 'TFS_Logo.png')
    
    # Left logo
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
        Paragraph(report_title, title_style),
        Paragraph("Task Force SAFE - CENTCOM AOR", tagline_style)
    ]
    
    # Right logo
    if os.path.exists(tfs_logo):
        right_img = Image(tfs_logo, width=1.2*inch, height=0.5*inch, kind='proportional')
    else:
        right_img = ""
    
    header_data = [[left_img, center_content, right_img]]
    header_table = Table(header_data, colWidths=[2*inch, PAGE_WIDTH - 5*inch, 2*inch])
    header_table.setStyle(TableStyle([
        ('ALIGN', (0, 0), (0, 0), 'LEFT'),
        ('ALIGN', (1, 0), (1, 0), 'CENTER'),
        ('ALIGN', (2, 0), (2, 0), 'RIGHT'),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 10),
    ]))
    
    elements.append(header_table)
    elements.append(Spacer(1, 0.1*inch))
    
    # Divider line
    divider = Table([['']], colWidths=[PAGE_WIDTH - 1*inch])
    divider.setStyle(TableStyle([
        ('LINEBELOW', (0, 0), (-1, -1), 2, colors.HexColor('#1a365d')),
    ]))
    elements.append(divider)
    elements.append(Spacer(1, 0.2*inch))
    
    return elements

def create_report_meta(data):
    """Create the report metadata section."""
    elements = []
    styles = getSampleStyleSheet()
    
    meta_style = ParagraphStyle('Meta', parent=styles['Normal'], fontSize=10, spaceAfter=3)
    bold_style = ParagraphStyle('MetaBold', parent=styles['Normal'], fontSize=10, spaceAfter=3)
    
    meta_items = []
    if data.get('site_name'):
        meta_items.append(f"<b>Site:</b> {data['site_name']}")
    if data.get('contract_week'):
        meta_items.append(f"<b>Contract Week:</b> {data['contract_week']}")
    if data.get('date_from') and data.get('date_to'):
        meta_items.append(f"<b>Date Range:</b> {data['date_from']} to {data['date_to']}")
    if data.get('repair_by'):
        meta_items.append(f"<b>Repair By:</b> {data['repair_by']}")
    if data.get('status'):
        meta_items.append(f"<b>Status:</b> {data['status']}")
    meta_items.append(f"<b>Generated:</b> {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    
    for item in meta_items:
        elements.append(Paragraph(item, meta_style))
    
    elements.append(Spacer(1, 0.2*inch))
    return elements

def create_summary_section(data):
    """Create the summary cards section."""
    elements = []
    styles = getSampleStyleSheet()
    
    section_title = ParagraphStyle('SectionTitle', parent=styles['Heading2'], fontSize=12, 
                                    textColor=colors.HexColor('#1a365d'), spaceAfter=10)
    elements.append(Paragraph("Summary", section_title))
    
    summary = data.get('summary', {})
    total = summary.get('total', 0)
    open_count = summary.get('open_count', 0)
    closed_count = summary.get('closed_count', 0)
    reinsp_count = summary.get('reinsp_count', 0)
    
    # Summary table
    summary_data = [
        ['Total SORs', 'Open', 'Closed', 'Re-Inspection'],
        [str(total), str(open_count), str(closed_count), str(reinsp_count)]
    ]
    
    summary_table = Table(summary_data, colWidths=[2*inch, 2*inch, 2*inch, 2*inch])
    summary_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#1a365d')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('FONTSIZE', (0, 1), (-1, 1), 14),
        ('FONTNAME', (0, 1), (-1, 1), 'Helvetica-Bold'),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 10),
        ('TOPPADDING', (0, 0), (-1, -1), 10),
        ('GRID', (0, 0), (-1, -1), 1, colors.HexColor('#ddd')),
        ('BACKGROUND', (0, 1), (0, 1), colors.HexColor('#ebf8ff')),  # Total - blue
        ('BACKGROUND', (1, 1), (1, 1), colors.HexColor('#fed7d7')),  # Open - red
        ('BACKGROUND', (2, 1), (2, 1), colors.HexColor('#c6f6d5')),  # Closed - green
        ('BACKGROUND', (3, 1), (3, 1), colors.HexColor('#fefcbf')),  # ReInsp - yellow
    ]))
    
    elements.append(summary_table)
    elements.append(Spacer(1, 0.3*inch))
    return elements

def create_responsibility_section(data):
    """Create the by repair responsibility section."""
    elements = []
    styles = getSampleStyleSheet()
    
    by_responsibility = data.get('by_responsibility', [])
    if not by_responsibility:
        return elements
    
    section_title = ParagraphStyle('SectionTitle', parent=styles['Heading2'], fontSize=12, 
                                    textColor=colors.HexColor('#1a365d'), spaceAfter=10)
    elements.append(Paragraph("By Repair Responsibility", section_title))
    
    # Table header
    table_data = [['Repair By', 'Total', 'Open', 'Closed', 'Re-Insp']]
    
    for resp in by_responsibility:
        table_data.append([
            resp.get('repair_by', '-'),
            str(resp.get('total', 0)),
            str(resp.get('open_count', 0)),
            str(resp.get('closed_count', 0)),
            str(resp.get('reinsp_count', 0))
        ])
    
    resp_table = Table(table_data, colWidths=[3*inch, 1.5*inch, 1.5*inch, 1.5*inch, 1.5*inch])
    resp_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#1a365d')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('ALIGN', (0, 0), (0, -1), 'LEFT'),
        ('ALIGN', (1, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
        ('TOPPADDING', (0, 0), (-1, -1), 6),
        ('GRID', (0, 0), (-1, -1), 1, colors.HexColor('#ddd')),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f8f9fa')]),
    ]))
    
    elements.append(resp_table)
    elements.append(Spacer(1, 0.3*inch))
    return elements

def create_by_site_section(data):
    """Create the by site breakdown section."""
    elements = []
    styles = getSampleStyleSheet()
    
    by_camp = data.get('by_camp', [])
    if not by_camp:
        return elements
    
    section_title = ParagraphStyle('SectionTitle', parent=styles['Heading2'], fontSize=12, 
                                    textColor=colors.HexColor('#1a365d'), spaceAfter=10)
    elements.append(Paragraph("By Site", section_title))
    
    table_data = [['Site', 'Country', 'Total', 'Open', 'Closed', 'Re-Insp']]
    
    for camp in by_camp:
        table_data.append([
            camp.get('camp_name', '-'),
            camp.get('country', '-'),
            str(camp.get('total', 0)),
            str(camp.get('open_count', 0)),
            str(camp.get('closed_count', 0)),
            str(camp.get('reinsp_count', 0))
        ])
    
    site_table = Table(table_data, colWidths=[2.5*inch, 1.5*inch, 1.2*inch, 1.2*inch, 1.2*inch, 1.2*inch])
    site_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#1a365d')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('ALIGN', (0, 0), (1, -1), 'LEFT'),
        ('ALIGN', (2, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
        ('TOPPADDING', (0, 0), (-1, -1), 6),
        ('GRID', (0, 0), (-1, -1), 1, colors.HexColor('#ddd')),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f8f9fa')]),
    ]))
    
    elements.append(site_table)
    elements.append(Spacer(1, 0.3*inch))
    return elements

def create_details_section(data):
    """Create the detailed records section."""
    elements = []
    styles = getSampleStyleSheet()
    
    details = data.get('details', [])
    if not details:
        return elements
    
    section_title = ParagraphStyle('SectionTitle', parent=styles['Heading2'], fontSize=12, 
                                    textColor=colors.HexColor('#1a365d'), spaceAfter=10)
    elements.append(Paragraph(f"Detailed Records ({len(details)} shown)", section_title))
    
    # Smaller font for details table
    cell_style = ParagraphStyle('Cell', parent=styles['Normal'], fontSize=7, leading=9)
    
    table_data = [['Site', 'Bldg', 'Def #', 'Category', 'Repair By', 'SOR #', 'Status', 'RAC', 'Date']]
    
    for d in details[:200]:  # Limit to 200 for PDF size
        table_data.append([
            Paragraph(str(d.get('camp_name', '-'))[:20], cell_style),
            str(d.get('building_number', '-'))[:10],
            str(d.get('deficiency_number', '-'))[:15],
            Paragraph(str(d.get('def_category', '-'))[:25], cell_style),
            str(d.get('repair_by', '-'))[:15],
            str(d.get('om_sor_number', '-'))[:15],
            str(d.get('deficiency_status', '-'))[:15],
            str(d.get('rac', '-')),
            str(d.get('inspection_date', '-'))[:10]
        ])
    
    details_table = Table(table_data, colWidths=[1.1*inch, 0.6*inch, 0.9*inch, 1.3*inch, 1*inch, 1*inch, 1*inch, 0.4*inch, 0.8*inch])
    details_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#1a365d')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('ALIGN', (7, 0), (7, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 8),
        ('FONTSIZE', (0, 1), (-1, -1), 7),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 4),
        ('TOPPADDING', (0, 0), (-1, -1), 4),
        ('GRID', (0, 0), (-1, -1), 0.5, colors.HexColor('#ddd')),
        ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f8f9fa')]),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
    ]))
    
    elements.append(details_table)
    return elements

def generate_sor_pdf(data, output_path, static_dir):
    """Generate the SOR report PDF."""
    doc = SimpleDocTemplate(
        output_path,
        pagesize=landscape(letter),
        rightMargin=0.5*inch,
        leftMargin=0.5*inch,
        topMargin=0.5*inch,
        bottomMargin=0.5*inch
    )
    
    elements = []
    
    # Header
    report_title = "SOR Report"
    if data.get('site_name'):
        report_title = f"SOR Report - {data['site_name']}"
    elements.extend(create_header(static_dir, report_title))
    
    # Report metadata
    elements.extend(create_report_meta(data))
    
    # Summary
    elements.extend(create_summary_section(data))
    
    # By Repair Responsibility
    elements.extend(create_responsibility_section(data))
    
    # By Site (if all sites)
    elements.extend(create_by_site_section(data))
    
    # Details
    elements.extend(create_details_section(data))
    
    # Build PDF
    doc.build(elements)
    return output_path

def main():
    if len(sys.argv) < 3:
        print("Usage: generate_sor_pdf.py <json_data_file> <output_pdf_path> [static_dir]", file=sys.stderr)
        sys.exit(1)
    
    json_file = sys.argv[1]
    output_path = sys.argv[2]
    static_dir = sys.argv[3] if len(sys.argv) > 3 else os.path.join(os.path.dirname(__file__), '..', 'static')
    
    with open(json_file, 'r') as f:
        data = json.load(f)
    
    generate_sor_pdf(data, output_path, static_dir)
    print(f"PDF generated: {output_path}")

if __name__ == "__main__":
    main()
