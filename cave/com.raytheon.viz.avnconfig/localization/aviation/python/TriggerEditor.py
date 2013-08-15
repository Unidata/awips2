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
#       TriggerEditor.py
#       GFS1-NHD:A8059.0000-SCRIPT;16
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 16 (DELIVERED)
#         Created:  09-JUL-2007 14:42:40      OBERFIEL
#           Removed reference to TWEBs in Items and NNN variables
#       
#       Revision 15 (DELIVERED)
#         Created:  03-MAY-2007 09:42:52      OBERFIEL
#           Removed references to XTF product, INFORMIX (sniffle) and
#           update the Help dialog content to be
#           grammatically correct and accurate.
#       
#       Revision 14 (DELIVERED)
#         Created:  05-JUN-2006 09:02:37      OBERFIEL
#           Removed references to path with version number in it
#       
#       Revision 13 (DELIVERED)
#         Created:  29-MAR-2006 11:11:52      OBERFIEL
#           Updates and minor fixes needed since deployment to ATAN
#           sites
#       
#       Revision 12 (DELIVERED)
#         Created:  23-FEB-2006 08:24:45      TROJAN
#           moved paths to text database commands to avnenv.sh
#       
#       Revision 11 (DELIVERED)
#         Created:  19-SEP-2005 15:13:07      TROJAN
#           spr 7011
#       
#       Revision 10 (APPROVED)
#         Created:  07-SEP-2005 12:57:39      TROJAN
#           spr 7010
#       
#       Revision 9 (DELIVERED)
#         Created:  07-JUL-2005 13:05:01      TROJAN
#           spr 6905
#       
#       Revision 8 (DELIVERED)
#         Created:  01-JUN-2005 17:41:28      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 7 (DELIVERED)
#         Created:  07-MAY-2005 11:39:40      OBERFIEL
#           Added Item Header Block
#       
#       Revision 6 (DELIVERED)
#         Created:  18-APR-2005 17:32:30      OBERFIEL
#           Changes to support gamin
#       
#       Revision 5 (DELIVERED)
#         Created:  15-MAR-2005 18:39:31      OBERFIEL
#           Fixed spelling errors
#       
#       Revision 4 (DELIVERED)
#         Created:  28-FEB-2005 19:37:20      TROJAN
#           spr 6685
#       
#       Revision 3 (APPROVED)
#         Created:  28-FEB-2005 19:04:55      TROJAN
#           spr 6685
#       
#       Revision 2 (BUILD_RELEASE)
#         Created:  07-DEC-2004 18:58:59      TROJAN
#           spr 6452
#       
#       Revision 1 (APPROVED)
#         Created:  30-SEP-2004 18:20:40      TROJAN
#           rewrite of avnsetup
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7318
#       	Action Date:       20-JUL-2007 10:05:35
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Remove references to TWEB in configuration GUI
#       
#
import logging, os
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, Busy

_Help = {
    'title': 'Trigger Editor Help',
    'content': """
This dialog is used to create a trigger file to get various text
products from the FXA text decoder that are important to the proper
functioning of AvnFPS monitoring capabilities.

The contents of latest trigger file is loaded when this dialog is
displayed.  Use the"Update" button to update any missing entries from
AWIPS's afos2awips text file.

You can modify or add missing PILs in any of the text fields as needed.
Pressing the"Make" button will write the triggers to disk and update
the watchwarn table in the fxatext database.

The "Close" button quits this application.

The "Help" button displays this help dialog.
"""
}

_Logger = logging.getLogger(__name__)

RootDir = os.environ['ROOT_DIR']
TopDir = os.environ['TOP_DIR']
A2AFile = os.path.join(os.environ['FXA_HOME'], 'data', 'afos2awips.txt')
TriggerFile = os.path.join(RootDir, 'etc', 'triggerTemplate')
TriggerScript = os.path.join(RootDir, 'bin', 'avntrigger.sh')
SQLFile = os.path.join(TopDir, 'tmp', 'sqlcommand')
Database = 'fxatext'
Table = 'watchwarn'

SQL = os.path.join(os.environ['PSQL'], 'bin', 'psql')
SQLCommands = """DELETE FROM %s WHERE script LIKE '%%bin/avntrigger.sh%%';
\copy %s FROM '%s' USING DELIMITERS '|'
\q
""" % (Table, Table, TriggerFile)

##############################################################################
class Editor(AvnDialog.Dialog):
    Items = ['taf', 'metar', 'ccfp']
    NNN = {'metar': 'MTR', 'taf': 'TAF', 'ccfp': 'CFP'}
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)

        self.title('Trigger Editor')
        self.wm_iconname(newName='TEditor')

        # from AvnDialog
        self.createMessageBar(self.interior(), True)

        self.__getAfosIds()
        self.__getTriggerFile()
        self.ids = AvnParser.getAllSiteIds()
        # add CCFP entries
        self.ids.update({'ccfp': ['01', '02', '03']})
        numrows = max([len(self.ids.get(tag, [])) for tag in self.Items]) + 10

        interior = self.interior()
        frame = Frame(interior, relief='ridge', bd=2)
        bbox = Pmw.ButtonBox(frame)
        btn = bbox.add('Update', command=self.__update)
        Balloon().bind(btn, 'Updates missing values using the afos2awips file')
        btn = bbox.add('Make', command=self.__make)
        Balloon().bind(btn, 
            'Creates trigger file and updates watchwarn table')
        btn = bbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = bbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        bbox.alignbuttons()
        bbox.grid(row=0, column=0, columnspan=len(self.Items), sticky='news')
        self.item = dict.fromkeys(self.Items)
        validator = {'validator': 'alphanumeric', 'min': 8, 'max': 9, \
            'minstrict': 0}
        for n, tag in enumerate(self.Items):
            sf = Pmw.ScrolledFrame(frame,
                labelpos='n',
                label_text=tag.upper(),
                horizflex='elastic',
                )
            self.item[tag] = []
            
            for k in range(numrows):
                entry = Pmw.EntryField(sf.interior(),
                    labelpos='w',
                    label_text='',
                    label_anchor='e',
                    label_width=6,
                    entry_width=9,
                    validate=validator,
                    )
                entry.grid(row=k, column=0, sticky='news')
                self.item[tag].append(entry)
            w = entry.component('entry').winfo_reqwidth() + \
                entry.component('label').winfo_reqwidth() + \
                sf.component('vertscrollbar').winfo_reqwidth()
            sf.configure(clipper_width=w)
            sf.grid(row=1, column=n, sticky='news')
        frame.pack(side='top', expand='yes', fill='x')

        self.initialiseoptions(Editor)
        self.setGeometry()

    def __getAfosIds(self):
        self.pils = {}
        try:
            for line in file(A2AFile):
                try:
                    afos = line.split()[0]
                except:
                    continue    # blank line
                nnn = afos[3:6]
                if nnn not in ['TAF', 'TWB']:
                    # don't bother with METARs, will use TAFs CCC and XXX
                    continue
                if afos[3:] == 'TWBSYN':
                    self.pils[afos[:6]] = afos
                else:
                    self.pils[afos[3:]] = afos
                    if nnn == 'TAF':
                        # make MTR entry
                        tmp = afos[:3] + 'MTR' + afos[6:]
                        self.pils[tmp[3:]] = tmp
            # add ccfp entries
            for n in range(1,4):
                self.pils['CFP%02d'%n] = 'MKCCFP%02d'%n
        except:
            msg = 'Cannot access %s file' % A2AFile
            _Logger.exception(msg)

    def __getTriggerFile(self):
        self.triggers = []
        try:
            self.triggers = [line.split(None, 1)[0] \
                for line in file(TriggerFile)]
        except Exception:
            msg = 'Cannot read %s' % TriggerFile
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)

    def __update(self):
        for tag in self.Items:
            for n, ident in enumerate(self.ids[tag]):
                # only update empty fields
                if self.item[tag][n].getvalue():
                    continue
                key = self.NNN[tag] + ident[-3:]
                t = self.pils.get(key, '')
                self.item[tag][n].setvalue(t)

    def __make(self):
        try:
            fh = file(TriggerFile, 'w')
        except Exception:
            msg = 'Cannot open %s for writing' % TriggerFile
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)
            return
        for tag in self.Items:
            for n, ident in enumerate(self.ids[tag]):
                entry = self.item[tag][n]
                if not entry.getvalue().strip():
                    continue
                elif not entry.valid():
                    msg = 'Invalid %s entry for %s' % (tag.upper(), ident)
                    self.messagebar().message('usererror', msg)
                    break
                pil = entry.getvalue().upper().strip()
                print >> fh, pil, '|', TriggerScript
        fh.close()
        try:
            self.__updateTable()
            msg = 'Trigger file created successfully'
            _Logger.info(msg)
            self.messagebar().message('systemevent', msg)
        except Exception, e:
            msg = 'Failed to make triggers: %s' % e
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)

    def __updateTable(self):
        file(SQLFile, 'w').write(SQLCommands)
        cmd = '%s %s -f "%s"' % (SQL, Database, SQLFile)
        if os.system(cmd):
            raise Avn.AvnError('os.system() failed on %s' % cmd)

    def listTriggers(self):
        for tag in self.Items:
            for n, ident in enumerate(self.ids.get(tag, [])):
                try:
                    self.item[tag][n].configure(label_text=ident)
                    key = self.NNN[tag] + ident[-3:]
                    for t in self.triggers:
                        if t[3:] == key:
                            self.item[tag][n].setvalue(t)
                            break
                except IndexError:
                    msg = 'Too many new triggers\n.Restart avnsetup'
                    Busy.showerror(msg, self.interior())
                    break
