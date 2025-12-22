#!/usr/bin/env python3
"""Generate DAR PDF with naming convention: DAR_SITE_DATE_TEAMNUMBER.pdf"""

import sqlite3
import sys
import os
from datetime import datetime
from weasyprint import HTML, CSS
from weasyprint.text.fonts import FontConfiguration

# Get the project root directory
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
DB_PATH = os.path.join(PROJECT_DIR, 'data', 'tfs-cmms.db')
REPORTS_DIR = os.path.join(PROJECT_DIR, 'reports', 'dar')

def get_dar_data(dar_id):
    """Fetch DAR data from database."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    # Get DAR header
    cursor.execute('''
        SELECT d.*, u.full_name as submitter_name
        FROM daily_activity_reports d
        JOIN users u ON d.submitted_by = u.id
        WHERE d.id = ?
    ''', (dar_id,))
    dar = cursor.fetchone()
    
    if not dar:
        conn.close()
        return None
    
    # Get activities
    cursor.execute('SELECT * FROM dar_activities WHERE dar_id = ? ORDER BY activity_time', (dar_id,))
    activities = cursor.fetchall()
    
    # Get reports submitted
    cursor.execute('SELECT * FROM dar_reports_submitted WHERE dar_id = ? ORDER BY id', (dar_id,))
    reports = cursor.fetchall()
    
    conn.close()
    
    return {
        'dar': dict(dar),
        'activities': [dict(a) for a in activities],
        'reports': [dict(r) for r in reports]
    }

def format_date_display(date_str):
    """Format YYYY-MM-DD as DD-MMM-YY."""
    if not date_str:
        return '-'
    try:
        dt = datetime.strptime(date_str[:10], '%Y-%m-%d')
        return dt.strftime('%d-%b-%y').upper()
    except:
        return date_str

def format_date_filename(date_str):
    """Format YYYY-MM-DD as DDMMMYY for filename."""
    if not date_str:
        return 'NODATE'
    try:
        dt = datetime.strptime(date_str[:10], '%Y-%m-%d')
        return dt.strftime('%d%b%y').upper()
    except:
        return 'NODATE'

def get_site_code(camp_location):
    """Get short site code from camp location."""
    # Map common locations to codes
    site_codes = {
        'ERBIL': 'EAB',
        'GREEN VILLAGE': 'GV',
        'AL ASAD': 'AAS',
        'AL UDEID': 'AUB',
        'CAMP ARIFJAN': 'ARJ',
        'ARIFJAN': 'ARJ',
        'BUEHRING': 'BUE',
        'ALI AL SALEM': 'AAS',
    }
    upper_loc = (camp_location or '').upper()
    for name, code in site_codes.items():
        if name in upper_loc:
            return code
    # Default: first 3 letters
    return upper_loc[:3] if upper_loc else 'UNK'

def generate_dar_filename(dar):
    """Generate filename: DAR_SITE_DATE_TEAMNUMBER.pdf"""
    site_code = get_site_code(dar['camp_location'])
    date_str = format_date_filename(dar['report_date'])
    team = dar['team_number'] or 'XXX'
    return f"DAR_{site_code}_{date_str}_{team}.pdf"

def generate_html(data):
    """Generate HTML for the DAR PDF."""
    dar = data['dar']
    activities = data['activities']
    reports = data['reports']
    
    date_display = format_date_display(dar['report_date'])
    
    activities_html = ''
    for act in activities:
        time = act['activity_time'] or ''
        activity = act['activity_description'] or ''
        notes = act['notes'] or ''
        activities_html += f'''
        <tr>
            <td style="width: 60px;">{time}</td>
            <td>{activity}</td>
            <td>{notes}</td>
        </tr>
        '''
    
    reports_html = ''
    for rpt in reports:
        tag = rpt['report_tag_id'] or ''
        submitted = 'Yes' if rpt['submitted'] else 'No'
        reports_html += f'''
        <tr>
            <td>{tag}</td>
            <td style="width: 80px; text-align: center;">{submitted}</td>
        </tr>
        '''
    
    html = f'''
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <style>
            @page {{
                size: A4;
                margin: 1.5cm;
            }}
            body {{
                font-family: Arial, sans-serif;
                font-size: 10pt;
                line-height: 1.4;
            }}
            .header {{
                text-align: center;
                margin-bottom: 20px;
            }}
            .header h1 {{
                font-size: 16pt;
                margin: 0;
                color: #003366;
            }}
            .header h2 {{
                font-size: 14pt;
                margin: 5px 0;
                font-weight: normal;
            }}
            .info-grid {{
                display: table;
                width: 100%;
                margin-bottom: 15px;
            }}
            .info-row {{
                display: table-row;
            }}
            .info-cell {{
                display: table-cell;
                padding: 3px 10px 3px 0;
            }}
            .info-label {{
                font-weight: bold;
                width: 120px;
            }}
            table {{
                width: 100%;
                border-collapse: collapse;
                margin-bottom: 15px;
            }}
            th, td {{
                border: 1px solid #ccc;
                padding: 5px 8px;
                text-align: left;
            }}
            th {{
                background-color: #003366;
                color: white;
                font-weight: bold;
            }}
            .section-title {{
                font-size: 12pt;
                font-weight: bold;
                color: #003366;
                margin: 15px 0 10px 0;
                border-bottom: 2px solid #003366;
                padding-bottom: 3px;
            }}
            .checkbox {{
                display: inline-block;
                width: 12px;
                height: 12px;
                border: 1px solid #333;
                margin-right: 5px;
                vertical-align: middle;
            }}
            .checkbox.checked {{
                background-color: #003366;
            }}
            .signature-section {{
                margin-top: 30px;
                page-break-inside: avoid;
            }}
            .signature-row {{
                display: flex;
                justify-content: space-between;
                margin-bottom: 10px;
            }}
            .signature-line {{
                border-bottom: 1px solid #333;
                width: 200px;
                display: inline-block;
            }}
            .footer {{
                margin-top: 20px;
                font-size: 8pt;
                color: #666;
                text-align: center;
            }}
        </style>
    </head>
    <body>
        <div class="header">
            <h1>TaskForce SAFE CENTCOM</h1>
            <h2>Daily Activity Report</h2>
        </div>
        
        <div class="info-grid">
            <div class="info-row">
                <div class="info-cell info-label">Date:</div>
                <div class="info-cell">{date_display}</div>
                <div class="info-cell info-label">Camp / Location:</div>
                <div class="info-cell">{dar['camp_location'] or '-'}</div>
            </div>
            <div class="info-row">
                <div class="info-cell info-label">Team Member Name:</div>
                <div class="info-cell">{dar['team_member_name'] or '-'}</div>
                <div class="info-cell info-label">Team #:</div>
                <div class="info-cell">{dar['team_number'] or '-'}</div>
            </div>
            <div class="info-row">
                <div class="info-cell info-label">Team Mate Name:</div>
                <div class="info-cell">{dar['team_mate_name'] or '-'}</div>
                <div class="info-cell info-label"></div>
                <div class="info-cell"></div>
            </div>
            <div class="info-row">
                <div class="info-cell info-label">Toolbox Topic:</div>
                <div class="info-cell">{dar['toolbox_topic'] or '-'}</div>
                <div class="info-cell info-label">Safety Brief:</div>
                <div class="info-cell">{'Yes' if dar['attended_safety_brief'] else 'No'}</div>
            </div>
            <div class="info-row">
                <div class="info-cell info-label">Site Lead:</div>
                <div class="info-cell">{dar['site_lead'] or '-'}</div>
                <div class="info-cell info-label"></div>
                <div class="info-cell"></div>
            </div>
        </div>
        
        <div class="section-title">Activity Record</div>
        <table>
            <thead>
                <tr>
                    <th style="width: 60px;">TIME</th>
                    <th>Activity</th>
                    <th>Notes</th>
                </tr>
            </thead>
            <tbody>
                {activities_html if activities_html else '<tr><td colspan="3" style="text-align: center;">No activities recorded</td></tr>'}
            </tbody>
        </table>
        
        <div class="section-title">Safety</div>
        <p>
            <span class="checkbox {'checked' if dar['hecp_submitted'] else ''}"></span>
            HECP Package Submitted to Site Lead
        </p>
        
        <div class="section-title">Reports Submitted to QA/QC</div>
        <table>
            <thead>
                <tr>
                    <th>Report TAG ID</th>
                    <th style="width: 80px;">Submitted</th>
                </tr>
            </thead>
            <tbody>
                {reports_html if reports_html else '<tr><td colspan="2" style="text-align: center;">No reports submitted</td></tr>'}
            </tbody>
        </table>
        
        <div class="signature-section">
            <p><strong>Full Name:</strong> <span class="signature-line">{dar['team_member_name'] or ''}</span></p>
            <p><strong>Full Name:</strong> <span class="signature-line">{dar['team_mate_name'] or ''}</span> 
               <strong style="margin-left: 50px;">Date:</strong> <span class="signature-line">{date_display}</span></p>
            <p style="font-size: 9pt; font-style: italic;">
                By entering my full name and date above; I hereby acknowledge that the information contained on this form is complete and accurate.
            </p>
        </div>
        
        <div class="footer">
            Task Force SAFE 'Daily Activity Report' | Generated by TFS-CMMS
        </div>
    </body>
    </html>
    '''
    return html

def generate_pdf(dar_id, output_dir=None):
    """Generate PDF for a DAR."""
    data = get_dar_data(dar_id)
    if not data:
        print(f"Error: DAR {dar_id} not found")
        return None
    
    # Generate filename
    filename = generate_dar_filename(data['dar'])
    
    # Set output directory
    if output_dir is None:
        output_dir = REPORTS_DIR
    os.makedirs(output_dir, exist_ok=True)
    
    output_path = os.path.join(output_dir, filename)
    
    # Generate HTML and convert to PDF
    html_content = generate_html(data)
    font_config = FontConfiguration()
    html = HTML(string=html_content)
    html.write_pdf(output_path, font_config=font_config)
    
    print(f"Generated: {output_path}")
    return output_path

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: generate_dar_pdf.py <dar_id> [output_dir]")
        sys.exit(1)
    
    dar_id = int(sys.argv[1])
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    
    result = generate_pdf(dar_id, output_dir)
    if result:
        print(f"Success: {result}")
    else:
        sys.exit(1)
