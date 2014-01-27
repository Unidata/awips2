##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
#    Name:
#       XDefaults.py
#       GFS1-NHD:A6647.0000-SCRIPT;1.18
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.18 (DELIVERED)
#         Updated:  21-FEB-2008 12:53:57      OBERFIEL
#           Removed Globals.Colors resource as a default setting.
#         Created:  20-FEB-2008 10:01:07      OBERFIEL
#           Updated Globals.Colors list to reflect user's changes.
#       
#       Revision 1.17 (REVIEW)
#         Created:  18-JAN-2008 08:07:29      GILMOREDM
#           Included predefined default values for forecaster defined
#           colors for status window
#       
#       Revision 1.16 (DELIVERED)
#         Created:  19-NOV-2007 20:31:26      OBERFIEL
#           Removed carriage return characters in files
#       
#       Revision 1.15 (DELIVERED)
#         Created:  26-OCT-2007 09:59:55      GILMOREDM
#           added entry for numHoursAll
#       
#       Revision 1.14 (DELIVERED)
#         Created:  22-MAY-2007 10:11:43      GILMOREDM
#           Added impactPlacement which is configurable through the
#           options interface and code changes that allow the
#           configuration of placement of impact statements within
#       
#       Revision 1.13 (INITIALIZE)
#         Created:  18-APR-2007 12:53:46      SOLSON
#           Removed CR characters from the previous rev of this item.
#       
#       Revision 1.12 (DELIVERED)
#         Created:  06-DEC-2006 14:23:21      OBERFIEL
#           Update to include new resource
#       
#       Revision 1.11 (DELIVERED)
#         Created:  02-MAY-2006 09:22:40      TROJAN
#           SPR 7131: changed font for Balloon
#       
#       Revision 1.10 (DELIVERED)
#         Created:  02-MAY-2006 08:15:03      TROJAN
#           SPR 7136: changed font for Balloon
#       
#       Revision 1.9 (DELIVERED)
#         Created:  06-JUL-2005 20:50:40      TROJAN
#           spr 6910
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:40:26      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  04-APR-2005 14:59:38      TROJAN
#           spr 6776
#       
#       Revision 1.6 (DELIVERED)
#         Created:  30-SEP-2004 18:56:04      TROJAN
#           stdr 874
#       
#       Revision 1.5 (APPROVED)
#         Created:  19-AUG-2004 21:01:34      OBERFIEL
#           code change
#       
#       Revision 1.4 (APPROVED)
#         Created:  01-JUL-2004 15:00:03      OBERFIEL
#           Update
#       
#       Revision 1.3 (DELIVERED)
#         Created:  17-MAR-2004 19:40:12      TROJAN
#           sprs for 2.1
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:36      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:46:32      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7361
#       	Action Date:       06-JUN-2008 13:40:48
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: AvnWatch GUI is not Section 508 compliant
#       
#

import Globals

_AlertOptions = ['disabled'] + [('alertLevel%d' % i) for i in xrange(2,len(Globals.Colors))]

# list of options needed in AvnFPS
# each entry is a tuple:
#   X resource as specified in X resource file
#   item type: 'font', 'entry', 'color', 'toggle', 'option' as needed in 
#       ResourceDialog, '' if not editable
#   default value
#   description, displayed in ResourceDialog
#   optional set of values, needed if item type is 'option'

Defaults = [
    ('*font', '-adobe-helvetica-bold-r-normal-*-*-100-*-*-p-*-iso8859-9',
        'font', 'Default font'),
    ('*background', 'grey70', 'color', 'Default background'),
    ('*foreground', 'black', 'color', 'Default foreground'),
    ('*Text.font', 'fixed', 'font', 'Text window font'),
    ('*Text.background', '#2f3f6f', 'color', 'Text window background'),
    ('*Text.foreground', '#ffffff', 'color', 'Text window foreground'),
    ('*Text.width', '80', 'entry', 'Text window width'),
    ('*Text.height', '24', 'entry', 'Text window height'),
    ('*Text.cursor', 'xterm', 'option', 'Mouse cursor in Text window',
        ['xterm', 'arrow', 'gumby', 'hand1', 'hand2', 'pencil']),
    ('*textEditor.font', 'fixed', 'font', 'Forecast Editor font'),
    ('*textEditor.background', '#2f3f6f', 'color',
        'Forecast Editor background'),
    ('*textEditor.foreground', '#ffffff', 'color',
        'Forecast Editor foreground'),
    ('*textEditor.width', '70', 'entry', 'Forecast Editor window width'),
    ('*textEditor.height', '20', 'entry',
        'Forecast Editor window height'),
    ('*textEditor.cursor', 'xterm', 'option',
        'Mouse cursor in Forecast Editor window',
        ['xterm', 'arrow', 'gumby', 'hand1', 'hand2', 'pencil']),
    ('*textEditor.insertWidth', '2', 'entry', 'Insertion cursor width'),
    ('*insertBackground', 'white', 'color', 'Insertion cursor color'),
    ('*textViewer.width', '74', 'entry', 'Text Viewer window width'),
    ('*textViewer.height', '12', 'entry', 'Text Viewer window height'),
    ('*orientation', 'horizontal', 'option', 'Forecast Editor layout',
        ['vertical', 'horizontal']),
    ('*Listbox.font', 'fixed', 'font', 'Listbox font'),
    ('*Entry.font', 'fixed', 'font', 'Entry field font'),
    ('*Entry.background', 'white', 'color', 'Entry field background'),
    ('*MessageBar*Entry.background', 'grey85', 'color',
        'Message Bar (on the bottom of windows) background'),
    ('*Balloon.Label.font', 'fixed', 'font',
        'Balloon message (i.e. this one) font'),
    ('*impactPlacement', 'top', 'option', 'Placement of impact messages',
        ['top','split','bottom']),	
    ('*transientDialogs', '1', 'toggle',
        'May change behavior of dialog windows placement'),
    ('*confirmClose', '0', 'toggle',
        'Ask for confirmation while closing editor'),
    ('*confirmSend', '1', 'toggle',
        'Ask for confirmation while sending amendments and corrections'),
    ('*alertLevel0', 'green3', 'color', 'AvnWatch Status OK'),
    ('*alertLevel1', 'grey', 'color', 'AvnWatch Status Missing'),
    ('*alertLevel2', 'pale green', 'color', 'AvnWatch Lowest Alert'),
    ('*alertLevel3', 'yellow', 'color', ''),
    ('*alertLevel4', 'orange', 'color', ''),
    ('*alertLevel5', 'red', 'color', ''),
    ('*alertLevel6', 'purple', 'color', 'AvnWatch Highest Alert'),
    ('*notifyDeiconify', _AlertOptions[2], 'option',
        'Alert level to deiconify monitor GUI', _AlertOptions),
    ('*notifyRaise', _AlertOptions[3], 'option',
        'Alert level to raise monitor GUI', _AlertOptions),
    ('*notifyPlay', _AlertOptions[4], 'option',
        'Alert level to play file', _AlertOptions),
#   ('*notifyTalk', 'color', _AlertOptions[2],
#       'Alert level to send voice message', _AlertOptions),
    ('*playFile', '', 'file', 'Sound to play on TAF alert'),
    ('*blink', '0', 'toggle', 'Blink on new notification'),
    ('*disallowSend', 'error', 'option',
        'Disallow transmission error level',
        ['always', 'warning', 'error', 'fatal']),
    ('*loadOrder', 'merge', 'option', 'How to initialize forecast',
        ['template', 'merge', 'latest']),
    ('*autosave', '1', 'toggle', 'Autosave bulletin in a backup file'),
    ('*updatetimes', '1', 'toggle', 'Update issue and valid time on QC'),
    ('*autoprint', '0', 'toggle', 'Print transmitted forecast'),
    ('*insert', 1, 'toggle', 'Insert/Overwrite'),
    ('*wrap', 'none', 'option', 'Wrap long lines', ['none', 'word']),
    ('*showUnique', '0', 'toggle', 'Remove duplicate METARs and TAFs from TWEB Editor'),
    ('*amdbuttons', 1, 'toggle', 'Show editor shortcuts'),
    ('*numTafs', '1', 'option', 'Number of TAFs to display',
        ['1', '2', '3', '99']),
    ('*numHours', '6', 'option', 'Number of hours of METARs to display',
        ['1', '3', '6', '12', '24', '99']),
    ('*numHoursAll', '1', 'option', 'Number of hours of METARs to display when All selected',
	['1', '3', '6', '12', '24', '99']),
    ('*showHeaders', '1', 'toggle',
        'Show WMO headers while displaying TAFs and Metars'),
    ('*showDecoded', '1', 'toggle', 'Show decoded METARs ARONET style'),
    ('*showProbs', '1', 'toggle',
        'Show category probabilities for MOS reports'),
    ('*showFormatted', 'raw', 'option', 'Format guidance reports',
        ['raw', 'long', 'short']),
    ('*highlightFlightCat', '0', 'toggle',
        'Highlight flight categories in TAF editor display window'),
    ('*lifrColor', 'white', 'color', 'background color for LIFR'),
    ('*ifrColor', 'grey85', 'color', 'background color for IFR'),
    ('*mvfrColor', 'grey70', 'color', 'background color for MVFR'),
    ('*vfrColor', 'grey55', 'color', 'background color for VFR'),
    # break here in ResourceDialog: if item type is an empty string
    ('*collective', '0'),
    ('*wrapLength', '0')
]
