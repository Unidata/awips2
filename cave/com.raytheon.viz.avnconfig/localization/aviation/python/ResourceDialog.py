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
#       ResourceDialog.py
#       GFS1-NHD:A4934.0000-SCRIPT;20
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 20 (DELIVERED)
#         Created:  21-FEB-2008 13:54:46      OBERFIEL
#           Changed so that Globals.Colors resource is modified.
#       
#       Revision 19 (REVIEW)
#         Created:  01-FEB-2008 14:06:24      GILMOREDM
#           Added functionality so that the "notify" items are updated
#           with proper colors when status indicator colors are
#           changed.
#       
#       Revision 18 (INITIALIZE)
#         Updated:  18-APR-2007 12:49:14      SOLSON
#           Removed CR characters from the previous rev of this item.
#         Created:  18-APR-2007 12:48:35      SOLSON
#           Removed CR characters from the previous rev of this item.
#       
#       Revision 17 (DELIVERED)
#         Created:  06-DEC-2006 14:15:40      BLI
#           Modified for making xmit configurable
#       
#       Revision 16 (DELIVERED)
#         Created:  23-MAY-2006 08:47:14      TROJAN
#           spr 7152: fixed select days matching criteria - round time, 
#           added history button in TWEB Editor's statusbar, 
#           fixed spelling
#       
#       Revision 15 (DELIVERED)
#         Created:  16-MAY-2006 10:50:31      TROJAN
#           spr 7146: added history button in TWEB Editor's statusbar,
#           fixed spelling
#       
#       Revision 14 (DELIVERED)
#         Created:  06-JUL-2005 20:50:39      TROJAN
#           spr 6910
#       
#       Revision 13 (DELIVERED)
#         Created:  07-MAY-2005 11:37:34      OBERFIEL
#           Added Item Header Block
#       
#       Revision 12 (DELIVERED)
#         Created:  04-APR-2005 14:59:38      TROJAN
#           spr 6776
#       
#       Revision 11 (DELIVERED)
#         Created:  15-FEB-2005 18:23:19      TROJAN
#           spr 6529
#       
#       Revision 10 (APPROVED)
#         Created:  19-AUG-2004 20:54:33      OBERFIEL
#           Code change
#       
#       Revision 9 (APPROVED)
#         Created:  09-JUL-2004 19:11:20      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 8 (APPROVED)
#         Created:  01-JUL-2004 14:59:49      OBERFIEL
#           Update
#       
#       Revision 7 (DELIVERED)
#         Created:  05-NOV-2003 19:09:45      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 6 (DELIVERED)
#         Created:  24-APR-2003 14:54:50      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 5 (DELIVERED)
#         Created:  28-FEB-2003 12:38:23      TROJAN
#           spr 4818 4823
#       
#       Revision 4 (DELIVERED)
#         Created:  02-AUG-2002 14:08:48      PCMS
#           Implementing autoprint of forecasts on send
#       
#       Revision 3 (DELIVERED)
#         Created:  09-JUL-2002 21:07:08      PCMS
#           Fixed missing line break in error message and invalid path
#           when workstation uses automounter.
#       
#       Revision 2 (DELIVERED)
#         Created:  11-JUN-2002 19:11:04      PCMS
#           Prevented wrapping lines in error dialogs.  Enabled ability
#           to change cursor color.
#       
#       Revision 1 (DELIVERED)
#         Created:  29-MAY-2002 22:13:55      PCMS
#           Initial version
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
import logging, os
import Globals
from Tkinter import *
import Pmw, tkColorChooser
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, Busy, FontChooser, Globals, XDefaults

_Help = {
    'title': 'Resource Editor Help',
    'content': """
This is an editor for the private resources file for AvnFPS.

The name of the resource is listed first. A short description in a
balloon popup window by pointing mouse cursor at the name. Value of
the resource is displayed on the right.  Depending on the resource
type it is:

Toggle button:	selected or not
Option menu:	allows selection from predefined values
Entry:		allows to type in values, such as width and height
Dialogs:
    File selection dialog:  to select audio file, resource *playFile
    Color chooser dialog:   to define colors
    Font chooser dialog:    to define fonts

When the editor starts, it tries to access configuration file
'awips/adapt/avnfps/etc/app-resources/X.n' where n is forecaster
number. If the file does not exist, a warning dialog is displayed and
the default file is read. If the default file does not exist,
hardcoded values are used.

Press 'Save' to write displayed values into your configuration file.
To restore default configuration, press 'Restore' for all resources,
or the 'Default' button to the left of each resource name to reset
them individually.
"""
}

_Logger = logging.getLogger(__name__)

class _Menu:
    def __init__(self, parent, name, msg, values=None):
        self.id = name
        self.parent = parent
        self.frame = Frame(parent, relief='groove', bd=2)
        
	bbox = Pmw.ButtonBox(self.frame,pady=0)
	btn = bbox.add('Reset', command=self.restore)
	Balloon().bind(btn, 'Reset to default value')
	bbox.pack(side='left', expand='no', fill='x')

        label = Label(self.frame,
            text=name,
            anchor='w',
            width=30,
            )
        label.pack(side='left', expand='no', fill='x')
        Balloon().bind(label, msg)
        self.finish(values)
        self.frame.pack(side='top', expand='yes', fill='x', padx=2, pady=0)
            
    def finish(self, values):
        pass

    def restore(self):
        pass

class ToggleMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self.tkValue = IntVar()
        self.tkValue.set(default)
        self._factorySetting = default
        _Menu.__init__(self, parent, name, msg, values)

    def finish(self, values):
        self.toggle = Checkbutton(self.frame, variable=self.tkValue)
        self.toggle.pack(side='left', expand='no', fill='x', padx=20)

    def get(self):
        return self.tkValue.get()

    def set(self, value):
        self.tkValue.set(value)

    def restore(self):
        self.set(self._factorySetting)        

class EntryMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self._default = default
        _Menu.__init__(self, parent, name, msg, values)

    def finish(self, values):
        self.entry = Pmw.EntryField(self.frame,
            entry_width=len(self._default)+1,
            value=self._default,
            )
        self.entry.pack(side='left', expand='no', padx=20)

    def get(self):
        return self.entry.get().strip()

    def set(self, value):
        self.entry.setentry(value)

    def restore(self):
        self.set(self._default)

class OptionMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self.tkValue = StringVar()
        self.tkValue.set(default)
        self._factorySetting = default
        _Menu.__init__(self, parent, name, msg, values)

    def _showCursor(self, name=None):
        if self.id.endswith('cursor'):
            self.menu.component('menubutton').configure(cursor=self.get())

    def finish(self, values):
        self.menu = Pmw.OptionMenu(self.frame,
            menubutton_textvariable=self.tkValue,
            menubutton_width=20,
            items=values,
            command=self._showCursor,
            )
        self.menu.pack(side='left', expand='no', fill='x', padx=20)

    def get(self):
        return self.tkValue.get()

    def getitems(self):
	return self.menu._optionInfo['items'][1]

    def setitems(self, items):
	self.menu.setitems(items)

    def set(self, value):
        self.tkValue.set(value)
        self._showCursor()

    def restore(self):
        self.set(self._factorySetting)

class FontMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self._factorySetting = self._font = default
        _Menu.__init__(self, parent, name, msg)

    def finish(self, values):
        self.sample = Button(self.frame,
            text=self._font,
            anchor='w',
            font=self._font,
            command=self._showFontDialog,
            )
        self.sample.pack(side='left', expand='no', fill='x', padx=20)

    def _showFontDialog(self):
        font = FontChooser.askfont(font=self._font,
            parent=self.frame, title='Font Browser')
        self.set(font)

    def get(self):
        return self._font

    def set(self, value):
        self._font = value
        self.sample.configure(text=self._font, font=self._font)

    def restore(self):
        self.set(self._factorySetting)

class ColorMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self._factorySetting = self._color = default
        _Menu.__init__(self, parent, name, msg)

    def finish(self, values):
        self.sample = Button(self.frame,
            text=self._color,
            anchor='w',
            background=self._color,
            command=self._showColorDialog,
            )
        self.sample.pack(side='left', expand='no', fill='x', padx=20)

    def _showColorDialog(self):
        color = tkColorChooser.askcolor(self._color, parent=self.frame, 
            title='Color Browser')
        self.set(color[1])

    def get(self):
        return self._color

    def set(self, value):
	if value == None or self._color == str(value):
            return
        #
        # Update Global Colors list
        if self.id.startswith('*alertLevel'):
            try:
                _idx = int(self.id[-1])
                if Globals.Colors[_idx] != str(value):
                    Globals.Colors[_idx] = str(value)
            except:
                raise
        
	oldcolor, self._color = str(self._color), str(value)
	self.sample.configure(background=value, text=value)

    def restore(self):
        self.set(self._factorySetting)
	    
class FileMenu(_Menu):
    def __init__(self, parent, name, default, msg, values=None):
        self._factorySetting = self._file = default
        _Menu.__init__(self, parent, name, msg)

    def finish(self, values):
        self.sample = Button(self.frame,
            text=self._file,
            anchor='w',
            command=self._showFileDialog,
            )
        self.sample.pack(side='left', expand='no', fill='x', padx=20)

    def _showFileDialog(self):
        file = Busy.askopenfilename(self.frame,
            initialdir='%s/etc/sounds' % os.environ['TOP_DIR'],
            filetypes=[('SUN', '.au'), ('M$', '.wav')])
        if not file:
            return
        try:
            self.set(file)
        except IOError:
            msg = 'Cannot access %s' % file
            _Logger.exception(msg)
            Busy.showerror(msg, self.frame)

    def get(self):
        return self._file.strip()

    def set(self, value):
        self._file = value
        self.sample.configure(text=self._file)

    def restore(self):
        self.set(self._factorySetting)

class ResourceEditor(AvnDialog.Dialog):
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' Resource Editor')

        # cannot get sizes configured automatically
        self.option_add('*topwidth', '512', 30)
        self.option_add('*topheight', '512', 30)

        self.parent = parent
        label = Label(self.interior(),
            relief='groove',
            text='Resource configuration for %s' % Globals.Forecaster,
            )
        label.pack(side='top', padx=5, pady=5)
        buttonbox = Pmw.ButtonBox(self.interior())
        btn = buttonbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = buttonbox.add('Save', command=self.__save)
        Balloon().bind(btn, 'Save displayed configuration')
        btn = buttonbox.add('Restore', command=self.__restore)
        Balloon().bind(btn, 'Restore default configuration')
        button = buttonbox.add('Help', 
            command=lambda x=_Help: self.showHelp(x))
        Balloon().bind(button, 'Show help')

        buttonbox.alignbuttons()
        buttonbox.pack(side='top', expand='no', fill='x')

        scrolledframe = Pmw.ScrolledFrame(self.interior(),
            vscrollmode='static',
            clipper_width=int(self.option_get('topwidth', '')),
            clipper_height=int(self.option_get('topheight', '')),
            )
        scrolledframe.pack(side='top', expand='yes', fill='both')
        self.frame = scrolledframe.interior()

        self.menu = filter(None, map(self.__makelist, XDefaults.Defaults))
        self.createMessageBar(self.interior())
        self.getResources()
        
    def __makelist(self, resource):
        if len(resource) < 4:
            return None
        
        name, default, type, msg = resource[:4]
        
        try:
            values = resource[4]
        except IndexError:
            values = None
        if type == 'toggle':
            return ToggleMenu(self.frame, name, default, msg, values)
        elif type == 'option':
            return OptionMenu(self.frame, name, default, msg, values)
        elif type == 'color':
            return ColorMenu(self.frame, name, default, msg, values)
        elif type == 'font':
            return FontMenu(self.frame, name, default, msg, values)
        elif type == 'file':
            return FileMenu(self.frame, name, default, msg, values)
        elif type == 'entry':
            return EntryMenu(self.frame, name, default, msg, values)
        else:
            return None

    def __restore(self):
        if not Busy.askokcancel("""This will delete your configuration.
Really want to continue?""", self.interior()):
            return
        id = AvnParser.getForecasters().get(Globals.Forecaster, '')['id']
        filename = 'etc/app-resources/X.%s' % id
        try:
            os.unlink(filename)
            self.getResources()
        except OSError:
            msg = 'Cannot remove %s' % filename
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())

    def __save(self):
        ident = AvnParser.getForecasters().get(Globals.Forecaster, '')['id']
        filename = 'etc/app-resources/X.%s' % ident
        lines = ['!! file X.%s\n' % Globals.Forecaster]
        for m in self.menu:
            v = m.get()
            if v in (None, ''):
                msg = 'Missing entry for %s' % m.id
                _Logger.error(msg)
                Busy.showerror(msg, self.interior())
                return
            lines.append('%s:     %s\n' % (m.id, v))
        lines.extend(self.fresources)
        try:
            file(filename, 'w').writelines(lines)
            Busy.showinfo('Restart GUI for changes to take effect',
                self.interior())
        except IOError:
            msg = 'Cannot create %s' % filename
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
        self.withdraw()

    def getResources(self):
        # Reads application resources file
        def _setResource(line):
        # returns true if line defines resource and is not in menu
            if line[0] == '!':
                return 0
            name, value = line.split(None, 1)
            for m in self.menu:
                if m.id == name[:-1]:       # skip ':'
                    m.set(value[:-1])   # skip '\n'
                    return 0
            return 1
        
        filename = 'etc/app-resources/X'
        id = AvnParser.getForecasters().get(Globals.Forecaster, '')['id']
        ffilename = '%s.%s' % (filename, id)
        if not os.path.isfile(ffilename):
            Busy.showwarning('Cannot access resources file\n' + \
                'starting with defaults', self.parent) 
            ffilename = filename
        try:
            self.fresources = filter(_setResource, file(ffilename))
        except IOError:
            msg = 'Cannot access resources file'
            _Logger.exception(msg)
            Busy.showerror(msg, self.parent)
            self.fresources = []
            return

if __name__ == '__main__':
    
    import AppShell
    
    try:
        os.chdir(TopDir)
        
        shell = AppShell.AppShell()
        shell.root.option_add('*transientDialogs','0')
        
        resourceEditor = ResourceEditor(shell.interior())
        resourceEditor.setGeometry()
        resourceEditor.display()
        try:
            resourceEditor.mainloop()
        except KeyboardInterrupt:
            print 'Globals.Colors',Globals.Colors
        
    except SystemExit:
        raise
