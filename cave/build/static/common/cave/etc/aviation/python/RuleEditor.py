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
#       RuleEditor.py
#       GFS1-NHD:A8057.0000-SCRIPT;13
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 13 (DELIVERED)
#         Created:  24-SEP-2008 14:37:36      OBERFIEL
#           Fixed display to make use of RMK in observations optional
#           when applying rules.
#       
#       Revision 12 (DELIVERED)
#         Created:  14-MAR-2008 10:28:48      OBERFIEL
#           Removed spurious character in the Help documentation.
#       
#       Revision 11 (DELIVERED)
#         Created:  06-FEB-2006 10:09:28      TROJAN
#           several coding errors related to change of alternate site
#           list format
#       
#       Revision 10 (APPROVED)
#         Created:  01-FEB-2006 12:58:59      TROJAN
#           Rule arguments were not saved correctly
#       
#       Revision 9 (APPROVED)
#         Created:  29-JAN-2006 14:05:22      TROJAN
#           stdr 956
#       
#       Revision 8 (DELIVERED)
#         Created:  07-SEP-2005 12:57:39      TROJAN
#           spr 7010
#       
#       Revision 7 (DELIVERED)
#         Created:  06-JUL-2005 20:50:39      TROJAN
#           spr 6910
#       
#       Revision 6 (DELIVERED)
#         Created:  07-MAY-2005 11:37:53      OBERFIEL
#           Added Item Header Block
#       
#       Revision 5 (DELIVERED)
#         Created:  04-APR-2005 14:16:29      TROJAN
#           spr 6777
#       
#       Revision 4 (DELIVERED)
#         Created:  24-JAN-2005 15:51:13      TROJAN
#           spr 6259
#       
#       Revision 3 (APPROVED)
#         Created:  10-NOV-2004 18:04:29      OBERFIEL
#           Fixed typos for LLWS
#       
#       Revision 2 (APPROVED)
#         Created:  08-NOV-2004 19:01:24      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1 (APPROVED)
#         Created:  30-SEP-2004 18:18:29      TROJAN
#           rewrite of avnsetup
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7383
#       	Action Date:       06-NOV-2008 15:25:22
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Lack of customization in QC check
#       

import logging, os
import ConfigParser
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, Busy
import MonitorP, MetarMonitorP, LtgMonitor, RadLtgMonitor, CCFPMonitor
import GridMonitor, LLWSMonitor

_Help = {
    'title': 'Rule Editor Help',
    'content': """
This dialog is used to define and configure TAF monitoring rules.

To load current configuration, enter site id and press <Enter> or use 
the "Load" button. Site id XXXX loads default rules.

Select a tab for the monitoring module you wish to modify the rules. 
The top list displays current rules: severity levels (colors) and 
associated messages. The list is sorted with respect to the severity
level. 
To view detailed rule description, select an item on this list. 
All rule parameters will be displayed in the "Rule Editor" window. 
You may modify editable parameters. Press "Replace" when finished. 
To remove a rule from the list, press "Remove".
To add a new rule, first select one of the available methods, then
modify rule parameters as desired. You must enter a message. Press
"Add" to add the rule to the list.
NOTE: 
1. argument types and values are not verified by the editor.
2. if an argument is a list, the separators are commas. *There has 
   to be a comma, even if the list contains one item.*
   
Press 'Save' when finished.

To restore default rules for a TAF Site and a currently selected
monitoring module, use the "Delete" button.
"""
}

_Logger = logging.getLogger(__name__)

_Monitors = ['mtrs', 'ltg', 'rltg', 'ccfp', 'grids', 'llws']
_Module = { \
    'mtrs': MetarMonitorP, \
    'ltg': LtgMonitor, \
    'rltg': RadLtgMonitor, \
    'ccfp': CCFPMonitor, \
    'grids': GridMonitor, \
    'llws': LLWSMonitor, \
}

##############################################################################
class Editor(AvnDialog.Dialog):
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' Monitoring Criteria')

        self._editor = dict.fromkeys(_Monitors)
        # for each page
        self.slist = {}
        self.methods = {}
        self.active_rules = {}
        self.current_rule = None
        self.label = {}
        self.severity = {}
        self.type = {}
        self.tkUnique = {}
        self.msg = {}
        self.arg = {}
        self.doc = {}
        try:
            self.color = AvnParser.getGuiCfg()['colors']
        except Exception:
            # use default values
            self.color = ['green3', 'grey', 'pale green', \
                'yellow', 'orange', 'red', 'purple']
            _Logger.error('Cannot access etc/gui.cfg')

        # from AvnDialog
        self.createMessageBar(self.interior(), True)

        interior = self.interior()
        frame = Frame(interior, relief='ridge', bd=2)
        self.siteentry = Pmw.EntryField(frame,
            labelpos='w',
            label_text='Site Id',
            entry_width=4,
            validate={'validator': 'alphabetic',
                'min': 0, 'max': 4},
            command=self.load,
            )
        self.siteentry.pack(side='left', expand='no', padx=5)
        bbox = Pmw.ButtonBox(frame)
        btn = bbox.add('Load', command=self.load)
        Balloon().bind(btn, 'Retrieves rule set for selected site')
        btn = bbox.add('Save', command=self.__save)
        Balloon().bind(btn, 'Saves rule set to a file')
        btn = bbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = bbox.add('Delete', command=self.__delete)
        Balloon().bind(btn, 'Deletes site-specific rules')
        btn = bbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        bbox.alignbuttons()
        bbox.pack(side='right', expand='no')
        frame.pack(side='top', expand='yes', fill='both')
        self.editpager = Pmw.NoteBook(interior,
            raisecommand=self.load,
            )
        self.editpager.pack(side='top', fill='both', expand='yes')
        for name in _Monitors:
            self.__createPage(name)
        self.editpager.setnaturalsize()

        self.initialiseoptions(Editor)
        self.setGeometry()
        self._ready = True

    def __createPage(self, name):
        page = self.editpager.add(name, tab_width=10)
        self.slist[name] = Pmw.ScrolledListBox(page,
            labelpos='nw',
            label_text='  %-24s %-32s %s' % ('Color', 'Method', 'Message'),
            vscrollmode='static',
            hscrollmode='dynamic',
            listbox_exportselection=0,
            selectioncommand=self.__editCurrent,
            )
        self.slist[name].pack(side='top', expand='yes', fill='both')
        
        frame = Frame(page, relief='ridge', bd=2)
        sf = self.methods[name] = Pmw.ScrolledFrame(frame,
            labelpos='n',
            label_text='Available methods',
            horizflex='elastic',
            )
        # create sorted list of methods
        tmp = [(k.__name__, k) for k in _Module[name].__dict__.values() \
            if type(k) == type(MonitorP.Rule) and issubclass(k, MonitorP.Rule)]
        tmp.sort()
        sfi = self.methods[name].interior()
        for label, klass in tmp:
            btn = Button(sfi, 
                text=label, 
                command=Avn.curry(self.__editNew, name, klass),
            )
            btn.pack(side='top', expand='yes', fill='x')
            Balloon().bind(btn, klass.__doc__)
        w = sf.component('label').winfo_reqwidth() + \
            sf.component('vertscrollbar').winfo_reqwidth()
        sf.configure(clipper_width=w)
        sf.pack(side='left', expand='yes', fill='y', pady=5)
        group = Pmw.Group(frame, tag_text='Rule Editor')
        bbox = Pmw.ButtonBox(group.interior(),
            labelpos='n',
            label_text='',
            frame_borderwidth=2,
            frame_relief='groove',
            )
        btn = bbox.add('Add', command=self.__add)
        Balloon().bind(btn, 'Adds new rule')
        btn = bbox.add('Replace', command=self.__replace)
        Balloon().bind(btn, 'Replaces currently selected rule')
        btn = bbox.add('Remove', command=self.__remove)
        Balloon().bind(btn, 'Removes selected rule')
        bbox.alignbuttons()
        bbox.pack(side='top', expand='yes', fill='x', padx=5)

        f = Frame(group.interior())
        self.label[name] = Label(f, text='Method')
        self.label[name].pack(side='left', padx=5)
        self.tkUnique[name] = IntVar()
        cb = Checkbutton(f,
            text='Unique',
            variable=self.tkUnique[name],
            )
        cb.pack(side='right', padx=5)
        self.type[name] = Pmw.OptionMenu(f,
            labelpos='w',
            label_text='Type',
            items=('vsby', 'wind', 'sky', 'wx', 'cat'),
            )
        self.type[name].pack(side='right', padx=5)
        self.severity[name] = Pmw.OptionMenu(f,
            labelpos='w',
            label_text='Severity',
            items=self.color[2:],
            )
        self.severity[name].pack(side='right', padx=5)
        f.pack(side='top', expand='yes', fill='x')
        self.msg[name] = Pmw.EntryField(group.interior(),
            labelpos='nw',
            label_text='Message',
            entry_width=72,
            )
        self.msg[name].pack(side='top', expand='yes', fill='x', padx=5)
        f = Frame(group.interior())
        self.arg[name] = [None]*4
        for n in range(4):
            self.arg[name][n] = Pmw.EntryField(f,
                labelpos='w',
                label_text='arg',
                entry_width=16,
                )   
            self.arg[name][n].grid(row=0, column=n, padx=5, pady=5)
            self.arg[name][n].grid_remove()
        f.pack(side='top', expand='yes', fill='x')
        self.doc[name] = Label(group.interior(), 
            text='', 
            justify='left', 
            height=4,
            )
        self.doc[name].pack(side='top', expand='yes', fill='both')
        group.pack(side='left', expand='yes', fill='both', padx=5, pady=5)
        frame.pack(side='top', expand='yes', fill='both')

    def __getActiveRules(self, page, id_):
        namespace = _Module[page].Monitor.Namespace
        try:
            rules = MonitorP.getActiveRules(id_.upper(), namespace, page)
            # sort w.r.t. severity
            tmp = [(int(r.severity), r) for r in rules]
            tmp.sort()
            tmp.reverse()
            return [x[1] for x in tmp]
        except Exception:
            msg = 'Cannot load %s rules for %s' % (page, id_)
            _Logger.exception(msg)
            self.messagebar().message('usererror', msg)
            return None

    def __display(self, page, select=False):
        if self.active_rules[page] is None:
            self.slist[page].clear()
            return
        if select:
            n = self.slist[page].index('active')
        items = ['%-12s %-16s %-s' % (self.color[r.severity], \
            r.__class__.__name__, r.msg or 'System generated') \
            for r in self.active_rules[page]]
        self.slist[page].setlist(items)
        if select:
            self.slist[page].selection_set(n)
            self.slist[page].see(n)

    def __delete(self):
        id_ = self.siteentry.getvalue().strip().upper()
        if id_.upper() == 'XXXX':
            Busy.showerror('Cannot delete default rules', self.interior())
            return
        page = self.editpager.getcurselection()
        if not Busy.askyesno('Delete %s rules for %s?' % (page, id_), 
            self.interior()):
            return
        try:
            path = os.path.join('etc', 'tafs', id_, page+'.cfg')
            os.unlink(path)
            self.load()
            self.messagebar().message('userevent', 'Restored default rules')
        except OSError:
            msg = 'Cannot remove %s rules for %s' % (page, id_)
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)

    def __getRule(self, page):
        if self.current_rule is None:
            return None
        r = self.current_rule
        r.severity = self.color.index(self.severity[page].getvalue())
        r.type = self.type[page].getvalue()
        r.unique = self.tkUnique[page].get()
        r.msg = self.msg[page].getvalue()
        for w in self.arg[page]:
            opt = w.cget('label_text')
            if opt:
                r.args[opt] = w.getvalue().strip()
        return r

    def __showRule(self, page, r):
        self.label[page].configure(text=r.args['method'])
        self.severity[page].invoke(int(r.severity)-2)
        self.type[page].invoke(r.type)
        self.tkUnique[page].set(r.unique)
        self.msg[page].setvalue(r.msg)
        n = -1
        for arg in r.args:
            if arg in ['method', 'type', 'unique']:
                continue
            n += 1
            w = self.arg[page][n]
            w.configure(label_text=arg)
            v = r.args[arg]
            if type(v) == type([]):
                w.setvalue(','.join([str(x) for x in v+['']]))
            else:
                w.setvalue(str(v))
            w.grid()
        while n < 3:
            n += 1
            w = self.arg[page][n]
            w.configure(label_text='')
            w.grid_remove()
        self.doc[page].configure(text=r.__doc__)
        self.current_rule = r

    def __editCurrent(self):
        page = self.editpager.getcurselection()
        n = self.slist[page].index('active')
        self.__showRule(page, self.active_rules[page][n])

    def __editNew(self, page, klass):
        rule = klass()
        rule.args['method'] = rule.__class__.__name__
        self.__showRule(page, rule)

    def __add(self):
        page = self.editpager.getcurselection()
        rule = self.__getRule(page)
        if not rule:
            return
        self.active_rules[page].append(rule)
        self.__display(page)

    def __replace(self):
        page = self.editpager.getcurselection()
        n = self.slist[page].index('active')
        rule = self.__getRule(page)
        if not rule:
            return
        self.active_rules[page][n] = rule
        self.__display(page, True)

    def __remove(self):
        page = self.editpager.getcurselection()
        n = self.slist[page].index('active')
        self.active_rules[page].pop(n)
        self.__display(page)

    def __save(self):
        id_ = self.siteentry.getvalue().strip().upper()
        if not id_:
            return
        page = self.editpager.getcurselection()
        path = os.path.join('etc', 'tafs', id_, page+'.cfg')
        try:
            cp = ConfigParser.ConfigParser()
            cp.add_section('rules')
            cp.set('rules', 'active', ','.join([str(i) for i in \
                range(len(self.active_rules[page]))]))
            for n, r in enumerate(self.active_rules[page]):
                tag = 'rule_%d' % n
                cp.add_section(tag)
                for key in r.__dict__:
                    if key == 'args':
                        for k in r.__dict__[key]:
                            v = r.__dict__[key][k]
                            if type(v) == type([]):
                                cp.set(tag, k, 
                                    ','.join([str(x) for x in v+['']]))
                            else:
                                cp.set(tag, k, str(v))
                    else:
                        cp.set(tag, key, r.__dict__[key])
            fp = file(path, 'w')
            try:
                fp.write('# %s\n\n' % path) 
                cp.write(fp)
            finally:
                fp.close()
            msg = 'Updated %s' % path
            self.messagebar().message('systemevent', msg)
        except Exception:
            msg = 'Failed to update %s' % path
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)

    def load(self, page=None):
        if not hasattr(self, '_ready'):
            return
        if page is None:
            page = self.editpager.getcurselection()
        id_ = self.siteentry.getvalue().strip().upper() or 'XXXX'
        self.siteentry.setvalue(id_)
        self.active_rules[page] = self.__getActiveRules(page, id_)
        self.__display(page)
