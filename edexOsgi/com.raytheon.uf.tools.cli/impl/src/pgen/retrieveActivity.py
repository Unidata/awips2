#!/usr/bin/env python

##
# This script is used to extract PGEN products from EDEX.
# It can be run in batch mode by specifying the "-l" and "-t" options on the
# command line.  Optionally, users can run it in interactive mode by invoking it
# with no argument.
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
    
def __parseCommandLine():
    parser = UsageArgumentParser.UsageArgumentParser(prog='retrieveActivity',description="Retrieve PGEN Activities from EDEX.  When invoked without any arguments, retrieveActivity is run in interactive mode.")
    bgroup = parser.add_argument_group(title='batch',description='For running in scripts and/or batch mode.')

    bgroup.add_argument("-l", action="store", dest="label", 
                      help="Activity Label being requested", 
                      required=False, metavar="label")
    bgroup.add_argument("-t", action="store", dest="type", 
                      help="Activity Type being requested",
                      required=False, metavar="type")
    options = parser.parse_args()
    
    options.interactive = False
    if options.label == None and options.type == None :
        options.interactive = True
    elif options.label == None or options.type == None :
        print "Must enter values for both arguments -l and -t"
        exit(0)

    logger.debug("Command-line arguments: " + str(options))
    return options

#
#  This method sends a CatalogQuery request to the EDEX uEngine
#  for the dataURI associated with the given activity type and label
#
def __getDataURI( type, label):
    script='''import CatalogQuery
query = CatalogQuery.CatalogQuery("pgen")
query.addConstraint("activityType","{0}","=")
query.addConstraint("activityLabel","{1}","=")
query.addReturnedField("dataURI")
query.addReturnedField("dataTime.refTime")
return query.execute()'''.format(type,label)

    service = '/services/pyproductjaxb'
    host = os.getenv("DEFAULT_HOST", "localhost")
    port = os.getenv("DEFAULT_PORT", "9581")
    connection=str(host+":"+port)
    ch = CH.CommHandler(connection,service)
    ch.process(script)

    if not ch.isGoodStatus():
        print ch.formatResponse()
        exit(1)

    logger.debug( ch.getContents() )
    return __parseResponse( ch.getContents() )

#
#  Parses the XML response from the uEngine and extracts
#  the value for the dataURI field.  If multiple are returned, the last
#  one is used.
#
def __parseResponse(xml):
    tree = ET.fromstring(xml)
    for attr in tree.iter('attributes'):
        if attr.attrib['field'] == 'dataURI':
            duri = attr.attrib['value']

    return duri

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
        # Retrieve products for given activity type and label
        logger.info("looking for Product: " + options.type + " - " + options.label)
        dataURI = __getDataURI(options.type, options.label)
        logger.debug("Found dataURI = " + dataURI)

        pr = ProductRetriever.ProductRetriever(dataURI, options.label)
        outdir = os.getcwd() + str(os.sep) + options.type + str(os.sep) + options.label + str(os.sep)
        #pr.setOutputDir(outdir)
        pr.getProducts()

    #print "Products were written to directory: " + outdir
    logger.info("retrieveActivity is complete.")

class RetrieveGui(Frame):
    """ Interactive GUI for PGEN product retrieval """
    
    def __init__(self, master=None):
        """ Initialize Frame and create widgets """
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def getProducts(self):

        # if an activity type and label have been selected, get products and write them out.
        if len(self.typeList.curselection()) != 0 and len(self.nameList.curselection()) != 0:
            type = self.typeList.get(self.typeList.curselection())
            label = self.nameList.get(self.nameList.curselection())
            labelindex = int(self.nameList.curselection()[0])
            dataURI = self.activityMap[type][labelindex]['dataURI']

            pr = ProductRetriever.ProductRetriever(dataURI, label)
            #outdir = os.getcwd() + str(os.sep) + options.type + str(os.sep) + options.label + str(os.sep)
            #pr.setOutputDir(outdir)
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
        self.nameList = Listbox(frame2,yscrollcommand=vscrollbar2.set,xscrollcommand=hscrollbar2.set,exportselection=0, width=50,height=15,bg="white")
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
        self.activityMap = self.__getActivityMap()
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
            #print label
            self.nameList.insert(END, label['activityLabel'])

    #
    #  Sends a CatalogQuery to the EDEX uEngine to get a list of 
    #  PGEN Activity TYpes, Labels, and associated dataURIs 
    # in the pgen database tables.
    #
    def __getActivityMap(self):
        script='''import CatalogQuery
query = CatalogQuery.CatalogQuery("pgen")
query.addReturnedField("activityType")
query.addReturnedField("activityLabel")
query.addReturnedField("dataURI")
return query.execute()'''

        service = '/services/pyproductjaxb'
        host = os.getenv("DEFAULT_HOST", "localhost")
        port = os.getenv("DEFAULT_PORT", "9581")
        connection=str(host+":"+port)
        ch = CH.CommHandler(connection,service)
        ch.process(script)

        if not ch.isGoodStatus():
            print ch.formatResponse()
            exit(1)

        logger.debug( ch.getContents() )
        return self.__generateMap( ch.getContents() )

    #
    #  Generates a map of activity types, label, and dataURIs from 
    #  the XML returned from EDEX uEngine for use in the activity type and label
    #  Listboxes.
    #
    #  The map is a dictionary (dict) of Activity Types whose values are a list of dicts
    #  which have keys "activityType", "activityLabel", and "dataURI".
    #
    def __generateMap(self, xml):
        aMap = dict()
        tree = ET.fromstring(xml)
        for item in tree.iter('items'):
            #print item.attrib['key']
            record = dict()
            for attr in item.iter('attributes'):
                record.update( {attr.attrib['field'] : attr.attrib['value'] } )
                #print record

            atype = record['activityType']
            if aMap.has_key(atype):
                aMap[atype].append(record)
            else:
                aMap.update( {atype: [record]} )

        return aMap


if __name__ == '__main__':
    main()
