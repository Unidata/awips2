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
#       AppShell.py
#       GFS1-NHD:A3642.0000-SCRIPT;26
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 26 (DELIVERED)
#         Created:  06-JUL-2005 20:50:37      TROJAN
#           spr 6910
#       
#       Revision 25 (DELIVERED)
#         Created:  07-MAY-2005 11:53:26      OBERFIEL
#           Added Item Header Block
#       
#       Revision 24 (DELIVERED)
#         Created:  04-APR-2005 15:51:01      TROJAN
#           spr 6775
#       
#       Revision 23 (DELIVERED)
#         Created:  11-MAR-2005 14:37:37      TROJAN
#           spr 6716
#       
#       Revision 22 (UNDER WORK)
#         Created:  11-MAR-2005 14:35:48      TROJAN
#           spr 6716
#       
#       Revision 21 (DELIVERED)
#         Created:  15-FEB-2005 18:23:18      TROJAN
#           spr 6529
#       
#       Revision 20 (APPROVED)
#         Created:  16-NOV-2004 20:07:09      PCMS
#           Restoring history
#       
#       Revision 19 (DELIVERED)
#         Created:  13-APR-2004 17:08:39      OBERFIEL
#           Updated build and date information
#       
#       Revision 18 (DELIVERED)
#         Created:  17-MAR-2004 19:40:07      TROJAN
#           sprs for 2.1
#       
#       Revision 17 (DELIVERED)
#         Created:  09-JAN-2004 15:27:53      PCMS
#           Updating for code cleanup
#       
#       Revision 16 (REVIEW)
#         Created:  08-JAN-2004 21:39:44      PCMS
#           Updating for code cleanup
#       
#       Revision 15 (APPROVED)
#         Created:  05-NOV-2003 19:04:08      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 14 (DELIVERED)
#         Created:  19-JUN-2003 14:45:31      TROJAN
#           spr 4867
#       
#       Revision 13 (DELIVERED)
#         Created:  15-MAY-2003 14:06:47      TROJAN
#           spr 5151
#       
#       Revision 12 (DELIVERED)
#         Created:  24-APR-2003 14:54:41      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 11 (DELIVERED)
#         Updated:  10-APR-2003 15:26:52      TROJAN
#           spr 4997
#         Created:  16-MAR-2003 17:48:07      TROJAN
#           spr 4931
#       
#       Revision 10 (BUILD_RELEASE)
#         Created:  10-MAR-2003 13:38:58      TROJAN
#           sprs 4904 - 4908
#       
#       Revision 9 (BUILD_RELEASE)
#         Created:  28-FEB-2003 12:49:59      TROJAN
#           always updated to reflect release date
#       
#       Revision 8 (DELIVERED)
#         Created:  24-OCT-2002 16:37:51      PCMS
#           Fixing more changes related to new NWS rules (Tempo group)
#       
#       Revision 7 (DELIVERED)
#         Created:  21-OCT-2002 22:00:05      PCMS
#           Fixed 'about' dialog to display version and date of current
#           release.
#       
#       Revision 6 (DELIVERED)
#         Created:  17-JUL-2002 13:38:16      PCMS
#           Updating with TWEB QC and reorganized directory structure
#           to allow for multiple versions.
#       
#       Revision 5 (DELIVERED)
#         Created:  09-JUL-2002 21:07:00      PCMS
#           Fixed missing line break in error message and invalid path
#           when workstation uses automounter.
#       
#       Revision 4 (DELIVERED)
#         Created:  13-MAY-2002 22:30:33      PCMS
#           Fixed placement of dialog boxes.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 18:18:40      PCMS
#           Updated HTML help display
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:49:10      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:28:02      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6910
#       	Action Date:       09-AUG-2005 14:09:26
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: code cleanup
#       
#
# AppShell.py
# Generic application shell
# Based on: John E. Grayson, 'Python and Tkinter Programming', 
#   Manning, 2000, Chapter 8
# last update: 07/03/05

import os, webbrowser 
from Tkinter import *
import Pmw
import Avn, Balloon, Busy, MessageBar

class AppShell(Pmw.MegaWidget):
    appname = Avn.Name
    appversion = Avn.Version
    appcontact = Avn.Contact

    def __init__(self, **kw):
        optiondefs = ( \
            ('background', 'grey', None), \
            ('msghistory', 0, Pmw.INITOPT), \
        )
        kw2 = {}
        for k in kw:
            if k in ['background', 'msghistory']:
                kw2[k] = kw[k] 
        self.defineoptions(kw2, optiondefs)

        self.root = Tk()
        self.root.withdraw()
        self.__initializeTk()
        Pmw.initialise(self.root, useTkOptionDb=1)

        # set window manager hints
        self.root.wm_title(self.appname)
        self.root.wm_protocol('WM_DELETE_WINDOW', self.exit)
        swidth = self.root.winfo_screenwidth()
        sheight = self.root.winfo_screenheight()
        self.root.wm_maxsize(width=swidth-40, height=sheight-40)

        # instantiate BusyManager
        Busy.instantiate(self.root)

        # Initialize the base class
        Pmw.MegaWidget.__init__(self, parent=self.root)

        # Initialize the application
        self.appInit(**kw)

        # Create the interface
        self.__createInterface()

        # Pack the container and set focus to ourselves
        self._hull.pack(side='top', fill='both', expand='yes')
        self.focus_set()
        self.initialiseoptions(AppShell)

        # finish
        self.appFinish()

    def __createAboutBox(self):
        Pmw.aboutversion(self.appversion)
        Pmw.aboutcontact(self.appcontact)
        self.about = Pmw.AboutDialog(self._hull, applicationname=self.appname)
        self.about.withdraw()
        self.about.transient(self._hull)

    def __createDataArea(self):
        self.__dataarea = Frame(self._hull,
            relief='groove',
            bd=2,
            )
        self.__dataarea.pack(side='top', fill='both', expand='yes')
            
    def __createMenuBar(self):
        self.menubar = Pmw.MenuBar(self._hull,
            hull_relief='raised',
            hull_borderwidth=1,
            )
        self.menubar.pack(fill='x')
        self.menubar.addmenu('Help', 'About %s' % self.appname, side='right')
        self.menubar.addmenu('File', 'File commands and Quit')

    def __createMessageBar(self):
        frame = Frame(self._hull,
            relief='sunken',
            )
        self.__messagebar = MessageBar.MessageBar(frame,
            entry_relief='sunken',
            entry_bd=1,
            history=self['msghistory'],
            )
        self.__messagebar.pack(side='left', expand='yes', fill='x', padx=3)
        frame.pack(side='bottom', expand='no', fill='x')

    def __createInterface(self):
        self.__createMenuBar()
        self.__createMessageBar()
        self.__createDataArea()
        self.__createAboutBox()

        self.createMenuBar()
        self.createInterface()

    def __initializeTk(self):
        Pmw.Color.setscheme(self.root, background=self['background'])
        self.root.option_add('*EntryField.Entry.background', 'white')
        self.root.option_add('*MessageBar.Entry.background', 'grey85')
        self.root.option_add('*Listbox*background', 'white')
        self.root.option_add('*Listbox*selectBackground', 'dark slate blue')
        self.root.option_add('*Listbox*selectForeground', 'white')
        self.root.option_add('*confirmClose', '0')
        self.initializeTk()

    def appInit(self, **kw):
        # Called before interface is created
        pass

    def appFinish(self):
        # Called after interface is created
        pass

    def createInterface(self):
        pass

    def createMenuBar(self):
        pass
        
    def initializeTk(self):
        pass

    def interior(self):
        return self.__dataarea

    def messagebar(self):
        return self.__messagebar

    def exit(self):
        if int(self.option_get('confirmClose', '')):
            if Busy.askokcancel('Do you really want to quit?', self.interior()):
                self.quit()
        else:
            self.quit()

    def run(self):
        self.pack()
        self.root.deiconify()
        self.mainloop()
    
    def showAbout(self):
        self.about.activate(geometry='first+%d+%d' % \
            (self.winfo_rootx()+30, self.winfo_rooty()+30))
    
    def showHTML(self):
        url = 'file:%s/doc/html/index.html' % os.environ['TOP_DIR']
        webbrowser.open(url)
