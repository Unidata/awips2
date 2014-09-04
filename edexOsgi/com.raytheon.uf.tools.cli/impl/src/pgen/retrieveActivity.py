#!/usr/bin/env python

##
# This script is used to extract PGEN products from EDEX.
# It can be run in batch mode by specifying the "-l", "-t", "-d", "-st", "-n", and
# "-p" options on the command line.  Optionally, users can run it in interactive 
# mode by invoking it with no argument.
# 
# Users can override the default EDEX server and port name by specifying them
# in the $DEFAULT_HOST and $DEFAULT_PORT shell environment variables.
# 
##

import os
import logging
import xml.etree.ElementTree as ET
from Tkinter import *

from ufpy import UsageArgumentParser
import lib.CommHandler as CH
import ProductRetriever
import ActivityUtil

logger = None
def __initLogger():
    global logger
    logger = logging.getLogger("retrieveActivity")
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Uncomment line below to enable debug-level logging
    #ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
#
#  Parses command line input and store in "options".
#
def __parseCommandLine():
    parser = UsageArgumentParser.UsageArgumentParser(prog='retrieveActivity',description="Retrieve PGEN Activities from EDEX.  When invoked without any arguments, retrieveActivity is run in interactive mode.")
    bgroup = parser.add_argument_group(title='batch',description='For running in scripts and/or batch mode.')

    bgroup.add_argument("-l*", action="store", dest="label", 
                      help="Activity Label being requested", 
                      required=False, metavar="label")
    bgroup.add_argument("-t*", action="store", dest="type", 
                      help="Activity Type being requested",
                      required=False, metavar="type")
    bgroup.add_argument("-st", action="store", dest="subtype", 
                      help="Activity Subtype being requested",
                      required=False, metavar="subtype")
    bgroup.add_argument("-d*", action="store", dest="reftime", 
                      help="Activity Ref Time being requested (YYYY-MM-DD_HH:MM)",
                      required=False, metavar="reftime")
    bgroup.add_argument("-n*", action="store", dest="name", 
                      help="Activity Name being requested",
                      required=False, metavar="name")
    bgroup.add_argument("-f", action="store", dest="fullpath", 
                      help="Write out XML with full path? (Yes/No)",
                      required=False, metavar="fullpath")
    
    bgroup = parser.add_argument_group(title='Note',description='Pattern match with "*" is allowed for -l, -t, -d, and -n. E.g, -l "*CCFP*3*" will match any activities whose label contains CCFP and 3.')
    
    options = parser.parse_args()
    
    options.interactive = False
    if (options.label == None and options.type == None and 
        options.reftime == None and options.subtype == None and 
        options.fullpath == None and options.name == None):
            options.interactive = True
    else:
        if (options.label == None and options.type == None and 
            options.reftime == None and options.name == None):
            print "Must enter values for at least one of -l, -t, -d, or -n"
            exit(0)
       
    logger.debug("Command-line arguments: " + str(options))
    return options

#
#  Main program.
#
def main():
    __initLogger()
    logger.info("Starting retrieveActivity.")
    options = __parseCommandLine()
    
    if options.interactive :
        # Launch interactive GUI
        logger.info("Running in interactive mode.")
        root = Tk()
        root.title("Retrieve Activity")
        app = RetrieveGui(master=root)
        app.mainloop()
        root.destroy()
    else:
        # Retrieve all activities abnd build a map of record using
        # type(subtype) as key.
        mu = ActivityUtil.ActivityUtil()
        activityMap = mu.getActivityMap()
    
        reqtype = None
        if ( options.type != None ):
            reqtype = options.type;
            if ( options.subtype != None ) :
                reqtype = options.type + "(" + options.subtype + ")"
                     
        records = []
        for key in activityMap.iterkeys():
            recs = activityMap[key]
            for rec in recs:
                if ( mu.stringMatcher(options.label, rec["activityLabel"]) and
                     mu.stringMatcher(reqtype, key ) and 
                     mu.stringMatcher(options.name, rec["activityName"] ) ):
                         #Remove sec.msec from record's refTime
                         dbRefTime = rec["dataTime.refTime"]
                         dotIndex = dbRefTime.rfind(":")
                         if ( dotIndex > 0 ):
                             shortTime = dbRefTime[:dotIndex]
                         else:
                             shortTime = dbRefTime
                         
                         #Replace the "_" with a whitespace in reftime.
                         optionTime = options.reftime.replace("_", " ")
                 
                         if ( mu.stringMatcher( optionTime, shortTime ) ):
                             records.append( rec )    
                                               
        for rec in records:
            pr = ProductRetriever.ProductRetriever(rec["dataURI"], rec["activityLabel"])
            if options.fullpath != None and options.fullpath.upper().startswith("Y"):
                pr.setFullpath(True)
            pr.getProducts()

    logger.info("retrieveActivity is complete.")

#
#  Interactive GUI for PGEN activity retrieval 
#
class RetrieveGui(Frame):
    """ Interactive GUI for PGEN activity retrieval """
    
    def __init__(self, master=None):
        """ Initialize Frame and create widgets """
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def getProducts(self):

        # if an activity type and label have been selected, get products and write them out.
        if len(self.typeList.curselection()) != 0 and len(self.nameList.curselection()) != 0:
            type = self.typeList.get(self.typeList.curselection())
            
            items = self.nameList.curselection()
            for i in items :
                idx = int(i)
                label = self.nameList.get(idx)
                dataURI = self.activityMap[type][idx]['dataURI']

                pr = ProductRetriever.ProductRetriever(dataURI, label)
                pr.getProducts()        
        
    def createWidgets(self):
        activityType = Label(self)
        activityType["text"] = "Activity Type"
        activityType.pack()

        # Activity Type list section
        frame = Frame(self)
        vscrollbar = Scrollbar(frame, orient=VERTICAL)
        hscrollbar = Scrollbar(frame, orient=HORIZONTAL)
        self.typeList = Listbox(frame,selectmode=BROWSE,yscrollcommand=vscrollbar.set,xscrollcommand=hscrollbar.set,exportselection=0,width=50,height=15,bg="white")
        vscrollbar.config(command=self.typeList.yview)
        hscrollbar.config(command=self.typeList.xview)
        vscrollbar.pack(side=RIGHT, fill=Y)
        hscrollbar.pack(side=BOTTOM, fill=BOTH)

        self.typeList.pack(side=LEFT,fill=BOTH,expand=1)
        frame.pack()
        self.typeList.insert(END,"Loading...")  # Temporary item while data are being requested from EDEX

        activityLabel = Label(self)
        activityLabel["text"] = "Activity Label"
        activityLabel.pack()

        # Activity Label list section
        frame2 = Frame(self)
        vscrollbar2 = Scrollbar(frame2, orient=VERTICAL)
        hscrollbar2 = Scrollbar(frame2, orient=HORIZONTAL)
        self.nameList = Listbox(frame2,yscrollcommand=vscrollbar2.set,xscrollcommand=hscrollbar2.set,exportselection=0, width=50,height=15,bg="white", selectmode=EXTENDED)
        vscrollbar2.config(command=self.nameList.yview)
        hscrollbar2.config(command=self.nameList.xview)
        vscrollbar2.pack(side=RIGHT, fill=Y)
        hscrollbar2.pack(side=BOTTOM, fill=BOTH)
        self.nameList.pack()
        frame2.pack()

        self.QUIT = Button(self)
        self.QUIT["text"] = "QUIT"
        self.QUIT["fg"]   = "red"
        self.QUIT["command"] =  self.quit

        self.QUIT.pack({"side": "right"})

        self.retrieve = Button(self)
        self.retrieve["text"] = "Retrieve",
        self.retrieve["command"] = self.getProducts

        self.retrieve.pack({"side": "left"})

        #
        #  Get all Activity Types and Labels from EDEX for use in selection ListBoxes.
        #  Insert list of Types in Type Listbox
        #
        self.activityMap = ActivityUtil.ActivityUtil().getActivityMap()
        self.typeList.delete(0,END)
        for key in self.activityMap.iterkeys():
            self.typeList.insert(END,key)
        self.current = None
        self.poll()

    #
    #  Continuously polls for user selection changes in the Activity Type ListBox
    #
    def poll(self):
        now = self.typeList.curselection()
        if len(now) == 0:
            self.after(250, self.poll)
            return
        
        if now != self.current:
            self.typeList_has_changed(now)
            self.current = now
        self.after(250, self.poll)

    #
    #  Replace the list of Activity Labels in the Label Listbox
    #  with those associated with the current Activity Type selection
    #
    def typeList_has_changed(self, index):
        self.nameList.delete(0,END)
        for label in self.activityMap[ self.typeList.get(index) ]:
            self.nameList.insert(END, label['activityLabel'])


if __name__ == '__main__':
    main()