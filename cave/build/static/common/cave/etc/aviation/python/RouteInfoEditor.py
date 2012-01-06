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
#       RouteInfoEditor.py
#       GFS1-NHD:A8058.0000-SCRIPT;9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 9 (DELIVERED)
#         Created:  29-JUN-2006 07:50:31      TROJAN
#           spr 7181: modified "Update" action logic
#       
#       Revision 8 (DELIVERED)
#         Created:  27-JUL-2005 18:35:32      TROJAN
#           spr 6948
#       
#       Revision 7 (APPROVED)
#         Created:  06-JUL-2005 20:50:39      TROJAN
#           spr 6910
#       
#       Revision 6 (DELIVERED)
#         Created:  07-MAY-2005 11:37:44      OBERFIEL
#           Added Item Header Block
#       
#       Revision 5 (DELIVERED)
#         Created:  04-APR-2005 14:16:29      TROJAN
#           spr 6777
#       
#       Revision 4 (APPROVED)
#         Created:  21-MAR-2005 15:45:20      TROJAN
#           spr 6732
#       
#       Revision 3 (APPROVED)
#         Created:  21-MAR-2005 15:14:37      TROJAN
#           spr 6732
#       
#       Revision 2 (DELIVERED)
#         Created:  19-JAN-2005 14:47:13      TROJAN
#           spr 6563
#       
#       Revision 1 (APPROVED)
#         Created:  30-SEP-2004 18:19:22      TROJAN
#           rewrite of avnsetup
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7181
#       	Action Date:       26-FEB-2007 09:50:36
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: Update button for TAF and TWEB site editor GUIs does not work
#       
#
# RouteInfoEditor.py
# Aviation setup editor 
# Author: George Trojan, SAIC/MDL, September 2004
# last update: 06/28/06

import logging, os, re
import ConfigParser
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, EditDialog

_Help = {
    'title': 'Route Setup Help',
    'content': """
This dialog is used to define TWEB route information.

To add a new site, enter site id and press "Update". Edit displayed
entries and press "Save". Create default template files.

To load current configuration, enter site id and press <Enter> 
or use the "Load" button".

To create template files, press "Make" in the "Templates" area.

To edit template file, press "Edit" in the "Templates" area.

You can use "Update" button to extract information from AWIPS
configuration files. Only non-empty fields will be overwritten.
"""
}

_A2A = '%s/data/afos2awips.txt' % os.environ['FXA_HOME']
_Logger = logging.getLogger(__name__)

def _validateAFOS(text):
    text = text.strip()
    length = len(text)
    if length < 8:
        return Pmw.PARTIAL
    value = text.upper()
    if re.match('^[A-Z]{3,3}TWB[(0-9)|(A-Z)]{2,3}$', value):
        return Pmw.OK
    else:
        if length == 9:
            return Pmw.ERROR
        else:
            return Pmw.PARTIAL

def _validateWMO(text):
    text = text.strip()
    length = len(text)
    if length < 11:
        return Pmw.PARTIAL
    value = text.upper()
    if re.match('^[A-Z]{4}[0-9]{2} [A-Z]{4}$', value):
        return Pmw.OK
    else:
        return Pmw.ERROR

def _validateIdent(text):
    for x in text.split(','):
        n = len(x)
        if n == 0:
            continue
        elif n > 4:
            return Pmw.ERROR
        elif n < 4:
            return Pmw.PARTIAL
        if Pmw.alphanumericvalidator(x) == Pmw.ERROR:
            return Pmw.ERROR
    return Pmw.OK

def getHeaders(ident):
    tid = 'TWB'+ident[1:]
    for line in file(_A2A):
        if line[3:9] == tid:
            return line.rstrip().split(None, 1)
    else:
        return None

##############################################################################
class Editor(AvnDialog.Dialog):
    Sites = ['METARS', 'TAFS']
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' TWEB Route Info')

        # from AvnDialog
        self.createMessageBar(self.interior(), True)

        interior = self.interior()
        frame = Frame(interior, relief='ridge', bd=2)
        self.site = Pmw.EntryField(frame,
            labelpos='w',
            label_text='Route',
            entry_width=4,
            validate={'validator': 'alphanumeric',
                'min': 3, 'max': 3, 'minstrict': 0},
            command=self.__load,
            )
        self.site.pack(side='left', expand='no', padx=5)
        bbox = Pmw.ButtonBox(frame)
        btn = bbox.add('Load', command=self.__load)
        Balloon().bind(btn, 'Retrieves data for selected site')
        btn = bbox.add('Update', command=self.__update)
        Balloon().bind(btn, 'Retrieves site info from fxa data files')
        btn = bbox.add('Save', command=self.__save)
        Balloon().bind(btn, 'Saves route info to a file')
        btn = bbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = bbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        bbox.alignbuttons()
        bbox.pack(side='right', expand='no')
        frame.pack(side='top', expand='yes', fill='x')

        group = Pmw.Group(interior, tag_text='Headers')
        self.wmo = Pmw.EntryField(group.interior(),
            labelpos='w',
            label_text='WMO',
            entry_width=12,
            validate=_validateWMO,
            )
        self.wmo.grid(row=0, column=0, sticky='ew', padx=20)
        self.afos = Pmw.EntryField(group.interior(),
            labelpos='w',
            label_text='AFOS',
            entry_width=9,
            validate=_validateAFOS,
            )
        self.afos.grid(row=0, column=1, sticky='ew', padx=20)
        group.pack(side='top', expand='yes', fill='x', pady=5)

        group = Pmw.Group(interior, tag_text='Associated ids')
        self.ident = {}
        for n, tag in enumerate(self.Sites):
            self.ident[tag] = Pmw.EntryField(group.interior(),
                labelpos='w',
                label_text=tag,
                entry_width=64,
                validate=_validateIdent,
                )
            self.ident[tag].grid(row=n, column=0, sticky='e')
        group.pack(side='top', expand='yes', fill='x', pady=5)

        group = Pmw.Group(interior, tag_text='Template')
        bbox = Pmw.ButtonBox(group.interior())
        btn = bbox.add('Edit', command=self.__editTemplate)
        Balloon().bind(btn, 'Template editor')
        btn = bbox.add('Make', command=self.__makeTemplate)
        Balloon().bind(btn, 'Makes default template')
        bbox.alignbuttons()
        bbox.pack(side='top')
        group.pack(side='top', expand='yes', fill='x', pady=5)

        self.initialiseoptions(Editor)
        self.setGeometry()

    def __clear(self):
        self.wmo.clear()
        self.afos.clear()
        for tag in self.Sites:
            self.ident[tag].clear()

    def __validate(self):
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        for tag in self.Sites:
            if not self.ident[tag].valid():
                return False
        return True

    def __load(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        cfg = AvnParser.getTwbSiteCfg(ident)
        if cfg is None:
            self.__clear()
            msg = 'Failed to retrieve configuration for %s' % ident
            self.messagebar().message('systemerror', msg)
            return
        self.wmo.setvalue(cfg['headers']['wmo'])
        self.afos.setvalue(cfg['headers']['afos'])
        for tag in self.Sites:
            # FIXME: it's a list! And strip trailing 's'
            ids = cfg['sites'][tag.lower()[:-1]]
            self.ident[tag].setvalue(','.join(ids))

    def __update(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        hdr = getHeaders(ident)
        if hdr:
            self.afos.setvalue(hdr[0])
            self.wmo.setvalue(hdr[1])
        else:
            msg = 'Failed to find headers for %s' % ident
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)

    def __save(self):
        self.messagebar().resetmessages('systemerror')
        if not self.__validate():
            msg = 'Invalid entry'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        path = os.path.join('etc', 'twbs', ident, 'info.cfg')
        try:
            cp = ConfigParser.ConfigParser()
            cp.add_section('headers')
            wmo = self.wmo.getvalue().strip().upper()
            cp.set('headers', 'wmo', wmo)
            afos = self.afos.getvalue().strip().upper()
            cp.set('headers', 'afos', afos)

            cp.add_section('sites')
            for tag in self.Sites:
                s = self.ident[tag].getvalue().strip().upper()
                cp.set('sites', tag, s)
            try:
                if not os.path.exists(os.path.dirname(path)):
                    os.mkdir(os.path.dirname(path))
                fp = file(path, 'w')
                fp.write('# %s\n\n' % path) 
                cp.write(fp)
            finally:
                fp.close()
            msg = 'Updated %s' % path
            self.messagebar().message('systemevent', msg)
        except Exception, e:
            msg = 'Failed to update %s' % path
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)

    def __makeTemplate(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        path = os.path.join('etc', 'twbs', ident, 'template')
        if os.path.exists(path):
            return
        try:
            if not os.path.exists(os.path.dirname(path)):
                os.mkdir(os.path.dirname(path))
            file(path, 'w').write('%s TWEB DDHHMM \n\n' % ident)
            msg = 'Template files created'
            self.messagebar().message('userevent', msg)
        except IOError:
            msg = 'Cannot make template %s' % path
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)

    def __editTemplate(self):
        if not hasattr(self, '_textEditor'):
            self._textEditor = EditDialog.Editor(self.interior())
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        path = os.path.join('etc', 'twbs', ident, 'template')
        if os.path.exists(path):
            self._textEditor.getFile(path)
        self._textEditor.setworkdir(os.path.dirname(path))
        self._textEditor.display()
