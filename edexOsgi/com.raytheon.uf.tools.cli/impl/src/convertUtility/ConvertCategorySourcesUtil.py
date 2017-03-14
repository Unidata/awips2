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
from matplotlib.colors import cnames
import os
import string
import sys
from UserString import MutableString

##############################################################################
# Class implementing a merge utility for combining the Master Category and 
# Master Sources and related Priorities Command Line Interface (CLI)
# tool.
#
# Configuration Files:
#
# Example Usage:
#     mergeCategorySourcesUtil Category_filename Soures_filename output_filename
#
#     echo "This is a test." | textdb.py write CCCNNNXX
#
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/21/2011      5853          cjeanbap       Initial Creation.
##############################################################################

CONSTANT_XML_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
CONSTANT_ALERT_HEADER = "<alertConfiguration name=\"DEFAULT\">"
CONSTANT_ALERT_FOOTER = "</alertConfiguration>"
CONSTANT_GLOBAL_CONFIG = "<globalConfiguration height=\"37\" width=\"-1\" yPosition=\"-1\" xPosition=\"-1\" logLength=\"%(log_length)s\" audioDuration=\"%(audio_duration)s\" blinkDuration=\"%(blink_duration)s\" expandedPopup=\"%(expanded_popup)s\" categoryShown=\"%(category_shown)s\" sourceKeyShown=\"%(source_key_shown)s\" priorityShown=\"%(priority_shown)s\" mode=\"%(layout_style)s\"/>"
CONSTANT_CATEGORY = "<category textBox=\"%(textbox_cell)s\" longName=\"%(long_name)s\" categoryName=\"%(category_name)s\" locked=\"%(locked_flag)s\"/>"
CONSTANT_SOURCE_HEADER = "<source monitor=\"%(monitor_name)s\" locked=\"%(locked_flag)s\" name=\"%(short_name)s\" longName=\"%(long_name)s\">"
CONSTANT_SOURCE_FOOTER = "</source>"
CONSTANT_CONFIGURATION_ITEM_HEADER = "<configurationItem forced=\"%(forced_flag)s\">"
CONSTANT_CONFIGURATION_ITEM_METADATA = "<metadata foreground=\"%(foreground_color)s\" background=\"%(background_color)s\" pythonEnabled=\"%(python_enabled_flag)s\" log=\"%(log_flag)s\" priority=\"%(priority_flag)s\" popup=\"%(popup_flag)s\" blink=\"%(blink_flag)s\" text=\"%(text_string)s\" audioEnabled=\"%(audio_enabled_flag)s\"/>"
CONSTANT_CONFIGURATION_ITEM_FOOTER = "</configurationItem>"
CONSTANT_CONFIGURATION_MONITOR_HEADER = "<configurationMonitor>"
CONSTANT_MONTITOR_METADATA_EMPTY = "<metadata />"
CONSTANT_MONTITOR_METADATA = "<metadata imageFile=\"%(image_file)s\" omitted=\"%(omitted_flag)s\" tclscript=\"%(tcl_script)s\" interval=\"%(interval)s\"/>" 
CONSTANT_CONFIGURATION_MONITOR_FOOTER = "</configurationMonitor>"
CONSTANT_NEWLINE = '\n'

CONSTANT_TRUE_STR = "true"
CONSTANT_FALSE_STR = "false"

CONSTANT_FLASH_FLOOD = "FlashFlood.png"
CONSTANT_FOG = "Fog.png"
CONSTANT_SNOW = "Snow.png"
CONSTANT_SCAN = "Scan.png"
CONSTANT_SAFE_SEAS = "SS.png"

__omittedMonitors = []
__categoryTextBox = []

class ConvertCategorySourcesUtil:

    # initializer.
    def __init__(self):
        self.commands = None
        self.xml = ""
        
    def __usage(self):
        print ' '
        print 'usage statement:'
        print './mergeCategorySourceUtil Category_filename.gcf Source_Filename.dat Output_filename.xml'
        print ' '
        print '\tCategory_filename includes the absolute path to the Category file, (filename.gcf).'    
        print '\tSource_filename includes the absolute path to the Source file, (filename.dat).'
        print '\tOutput_filename includes the absolute path to the Output file, (filename.xml).'
        print ' '

    # generate a Category XML element; write directly to output file
    def __processCategory(self, outputfile, words):
        #print "entered processCategory"        
                # use the global categoryTextBox to determine which text box the category 
        # is placed.
        textbox_cell = "0"
        categoryname = words[0].strip() 
        if categoryname in self.__categoryTextBox[0]:
            textbox_cell = 1
        elif categoryname in self.__categoryTextBox[1]:
            textbox_cell = 2
        elif categoryname in self.__categoryTextBox[2]:
            textbox_cell = 3
        elif categoryname in self.__categoryTextBox[3]:
            textbox_cell = 4
                
        # intentionally leaving in (special) and/or (Announcer); don't understand usage
        # 
        # 'special' Categories are handled specifically and specially in Guardian.
        # 'Announcer' Categories are direct translation from the Announcer application.
        # if (special) and/or (Announcer) need to be removed then uncomment the lines
        # below. 
        
        #if long_name.endswith("(special) (Announcer)"):
        #    wordLen = len(long_name)            
        #    long_name = long_name[0: (wordLen - len("(special) (Announcer)"))]
        #elif "(special)" in long_name:
        #    wordLen = len(long_name) 
        #    long_name = long_name[0: (wordLen - len("(special)"))]
        #elif "(Announcer)" in long_name:
        #    wordLen = len(long_name) 
        #    long_name = long_name[0: (wordLen - len("(Announcer)"))]
        long_name = words[2].strip()
        if len(long_name.strip()) == 0:
            long_name = words[0].strip() 
        
        if len(words) == 3:
            locked = CONSTANT_FALSE_STR
            if words[1] == '0':
                locked = CONSTANT_TRUE_STR
        
        result = '\t' + CONSTANT_CATEGORY % {"textbox_cell": textbox_cell, "long_name": long_name, "category_name": words[0].strip(), \
                                             "locked_flag": locked}
        self.__write_result(outputfile, result + CONSTANT_NEWLINE)
        
        #print "exiting processCategory"
        
    # convert the color string into a HEX color code.
    #
    # return:
    #    the Hexadecimal code value of the the color string    
    def __determineColorCode(self, colorString):
        
        result = "#000000"
        
        if colorString in cnames:
            result = cnames.get(colorString)       
             
        return result
    
    def __determinePriority(self, priority):
        
        priorityFlag = "VERBOSE"
        if priority == '0':
            priorityFlag = "CRITICAL"
        elif priority == '1':
            priorityFlag = "SIGNIFICANT"
        elif priority == '2':
            priorityFlag = "PROBLEM"
        elif priority == '3':
            priorityFlag = "EVENTA"
        elif priority == '4':
            priorityFlag = "EVENTB"    
    
        return priorityFlag
        
    # generate the Configuration Metadata Item XML element.
    #
    # return:
    #    Configuration Item XML element
    def __processConfigurationMetadataItem(self, words):
        #print "entered __processConfigurationMetadataItem"
        
        foreground = self.__determineColorCode(words[7].strip())        
        background = self.__determineColorCode(words[6].strip())

        audioEnabled = CONSTANT_FALSE_STR
        if words[4].strip() == '1':
            audioEnabled = CONSTANT_TRUE_STR     
        
        popupFlag = CONSTANT_FALSE_STR
        if words[3].strip() <> '0':
            popupFlag = CONSTANT_TRUE_STR

        blinkFlag = CONSTANT_FALSE_STR
        if words[2].strip() == '1':
            blinkFlag = CONSTANT_TRUE_STR

        pythonEnabled = CONSTANT_FALSE_STR
        if words[5].strip() == '1':
            pythonEnabled = CONSTANT_TRUE_STR
            
        textFlag = CONSTANT_FALSE_STR
        if words[1].strip() == '1':
            textFlag = CONSTANT_TRUE_STR
            
        priorityFlag = self.__determinePriority(words[0].strip())

        result = '\t' + CONSTANT_CONFIGURATION_ITEM_METADATA % \
            {"foreground_color": foreground, "background_color": background, "python_enabled_flag": pythonEnabled, \
             "log_flag": "false", "priority_flag": priorityFlag, "popup_flag": popupFlag, "blink_flag": blinkFlag, \
             "text_string": textFlag, "audio_enabled_flag": audioEnabled}        
        
        #print "exiting __processConfigurationMetadataItem"
        
        return result
    
    # generate the Configuration Monitor xml element.
    #
    # return:
    #    Configuration Monitor XML element    
    def __processConfigurationMonitor(self, words):
        #print "entered __processConfigurationMonitor"    
        
        result = MutableString()
        
        result += '\t\t' 
        result += '\n\t\t'
        
        #if words[1] == '0'
        if len(words[4]) == 0:
            result +=  CONSTANT_CONFIGURATION_MONITOR_HEADER + CONSTANT_NEWLINE + '\t\t\t' + \
                CONSTANT_MONTITOR_METADATA_EMPTY + CONSTANT_NEWLINE + '\t\t' + \
                CONSTANT_CONFIGURATION_MONITOR_FOOTER 
        else:
            omittedMonitorFlag = CONSTANT_FALSE_STR
            if words[0].strip() in self.__omittedMonitors:
                omittedMonitorFlag = CONSTANT_TRUE_STR            
                
            imageFile = ""
            if words[4].strip() == "scti_FlashFlood.xbm":
                imageFile = CONSTANT_FLASH_FLOOD
            elif words[4].strip() == "FOGbutton.xbm":
                imageFile = CONSTANT_FOG
            elif words[4].strip() == "SSbutton.xbm":
                imageFile = CONSTANT_SAFE_SEAS
            elif words[4].strip() == "SNOWbutton.xbm":
                imageFile = CONSTANT_SNOW
            elif words[4].strip() == "SCANbutton.xbm":
                imageFile = CONSTANT_SCAN
            #elif words[4].strip() == "scti_SPCguidance.xbm":
                #imageFile =
                
            result_0 = CONSTANT_CONFIGURATION_MONITOR_HEADER + CONSTANT_NEWLINE            
            result_1 = '\t\t\t' + \
                CONSTANT_MONTITOR_METADATA % {"image_file": imageFile, \
                                              "omitted_flag": omittedMonitorFlag, "tcl_script": words[2].strip(), \
                                              "interval": words[3].strip()} + CONSTANT_NEWLINE #
                                               
            result_2 = '\t\t' + CONSTANT_CONFIGURATION_MONITOR_FOOTER    
            result += result_0 + result_1 + result_2
    
        result += CONSTANT_NEWLINE
    
        #print "exiting __processConfigurationMonitor"
    
        return result
    
    # generate the Source Header XML element.
    #
    # return:
    #    Source Header XML element
    def __processSource(self, words):
    
        result = '\t' + CONSTANT_SOURCE_HEADER % {"monitor_name": "false", "locked_flag": "true", \
                                                  "short_name": words[0].strip(), "long_name": words[0].strip()}     
    
        return result
    
    # generate the Global Configuration XML element.
    #
    # return:
    #    Global Configuration XML element
    def __processGlobalConfiguration(self, outputfile, words):
        #print "entered __processGlobalConfiguration"
        result = '\t' + \
            CONSTANT_GLOBAL_CONFIG % {"log_length": words[5].strip(), "audio_duration": words[10].strip(), \
                                      "blink_duration": words[9].strip(), "expanded_popup": words[11].strip(), \
                                      "category_shown": words[8].strip(), "source_key_shown": words[7].strip(), \
                                      "priority_shown": words[6].strip(), "layout_style": words[1].strip()}    
        self.__write_result(outputfile, result + CONSTANT_NEWLINE)
        
        self.__categoryTextBox = self.__splitLine(words[2], ',')
        self.__omittedMonitors = self.__splitLine(words[3], ',')
        #print "exiting __processGlobalConfiguration"
    
    # split the input line based upon a character separator.
    #
    # return:
    #    a parsed array based upon the separator value.
    def __splitLine(self, line, sep):            
        return line.split(sep)
    
    # write result to output file
    def __write_result(self, outputfilename, outputline):
        file = open(outputfilename, "a+")    
        file.write(outputline)    
        file.close()
    
    # parse the merged temp file
    def __processInputFile(self, inputfile, outputfile):
        #print "entered processInputFile"        
        fileHandle = open(inputfile)
        
        x = 0
        
        constructingSourceXMLElement = 0
        
        result = MutableString()
        tmpResult = MutableString()    
        
        #for line in open(inputfile).readlines():
        for line in fileHandle.readlines():
            line = line.strip(CONSTANT_NEWLINE)    
            words = self.__splitLine(line, '|')
            if len(words) == 1:
                continue
            if len(words) == 3:
                self.__processCategory(outputfile, words)                
            elif len(words) == 12:   
                self.__processGlobalConfiguration(outputfile, words)
            else:
                # found a source element 
                if len(words) == 5:                    
                    if constructingSourceXMLElement == 1:
                        # end constructing of Configuration Item XML Element
                        # end constructing of Source XML Element 
                        constructingSourceXMLElement = 0
                        tmpResult += '\t\t' + \
                            CONSTANT_CONFIGURATION_ITEM_FOOTER + CONSTANT_NEWLINE + '\t'+ \
                            CONSTANT_SOURCE_FOOTER                        
                        self.__write_result(outputfile, str(tmpResult) + CONSTANT_NEWLINE)                        
                        tmpResult = MutableString() 
                      
                    # start constructing a new Source XML Element
                    result_0 = self.__processSource(words)
                    # start constructing a new Configuration Monitor XML Element
                    result_1 = self.__processConfigurationMonitor(words)
                    result_2 = '\t\t' + CONSTANT_CONFIGURATION_ITEM_HEADER % {"forced_flag": "false"}
                    result = result_0 + result_1 + result_2 + CONSTANT_NEWLINE                
                    self.__write_result(outputfile, str(result))
                 
                    constructingSourceXMLElement = 1
                else:
                    # construct the Configuration Metadata Item XML Element.
                    if len(line) == 0:
                        continue
                    
                    words = self.__splitLine(line, '|')    
                    if words[0] == "t":
                        continue
                    
                    tmpResult += '\t\t' + self.__processConfigurationMetadataItem(words) + CONSTANT_NEWLINE
         
        # end constructing of Configuration Item XML Element
        # end constructing of Source XML Element 
        result = tmpResult 
        result += '\t\t' + CONSTANT_CONFIGURATION_ITEM_FOOTER                        
        self.__write_result(outputfile, str(result) + CONSTANT_NEWLINE)
        self.__write_result(outputfile, '\t' + CONSTANT_SOURCE_FOOTER + CONSTANT_NEWLINE)
                        
        #print "exiting processInputFile"
    
    # merge both files into a simple single file
    def __mergeCategorySources(self, outputfile, inputfilenames=[]):
        #print "entered __mergeCategorySources"
        readLine1 = 0         
        
        #print "before for-loop"        
        for line in open(inputfilenames[1]).readlines():
            if (len(line.strip('\r\n')) == 0) or (len(line.strip(CONSTANT_NEWLINE)) == 0):
                continue
    
            if line.startswith("#"): 
                continue    
            else:                
                #print "before if-statement"                
                if readLine1 == 0:   
                    readLine1 = 1
                    #print "print first line of input file 2"   
                    self.__write_result(outputfile, line + CONSTANT_NEWLINE)
                    
                    self.__write_result(outputfile, '\n\n')                  
                        
                    #print "before for-loop"
                    for line in open(inputfilenames[0]).readlines():
                        if (len(line.strip('\r\n')) == 0) or (len(line.strip(CONSTANT_NEWLINE)) == 0):
                            continue
                        elif line.startswith("#"):  
                            continue
                        else:
                            self.__write_result(outputfile, line)
                              
                    self.__write_result(outputfile, CONSTANT_NEWLINE)
                else:
                    self.__write_result(outputfile, line + CONSTANT_NEWLINE)
                    
        #print "exiting mergeCategorySources"
    
    def execute(self):
        #print "assigning input files and output file name"
        inputfilenames, outputfilename = sys.argv[1:-1], sys.argv[-1]
    
        #check args
        if len(sys.argv) < 4:
            self.__usage()
            return 1 
        else:
            try:
                if not '.dat' in inputfilenames[0] or not os.path.exists(inputfilenames[0]):
                    print ' '
                    print 'Category file, %(inputfile)s, has the wrong extension(.dat) or does not exist!' % {"inputfile": inputfilenames[0]}
                    print ' '
                    self.__usage()
                    return 1
            except:
                print 'Provided Category file, %(inputfile)s, does not exist!' % {"inputfile": inputfilenames[0]}
                return 1
            
            try:
                if not '.gcf' in inputfilenames[1] or not os.path.exists(inputfilenames[1]):
                    print ' '
                    print 'Source file, %(inputfile)s, has the wrong extension(.gcf) or does not exist!' % {"inputfile": inputfilenames[1]}
                    print ' '
                    self.__usage()
                    return 1
            except:
                print 'Provided Sources file, %(inputfile)s, does not exist!' % {"inputfile": inputfilenames[0]}
                return 1
                
            temporaryfilename = os.getpid()
                                
            temporaryfilename = "./" + str(temporaryfilename)
            file = open(outputfilename, 'w')
            file.close()
            
            self.__mergeCategorySources(temporaryfilename, inputfilenames)
            
            self.__write_result(outputfilename, CONSTANT_XML_HEADER + CONSTANT_NEWLINE + \
                                 CONSTANT_ALERT_HEADER + CONSTANT_NEWLINE)
            
            self.__processInputFile(temporaryfilename, outputfilename)
            
            self.__write_result(outputfilename, CONSTANT_ALERT_FOOTER + CONSTANT_NEWLINE)
            
            os.remove(temporaryfilename)
    
            return 0
  
##############################################################################
# default execution; allows the class to be run as an application
##############################################################################
if __name__ == "__main__":
    ccsu = ConvertCategorySourcesUtil()
    status = ccsu.execute()
    if status == 0:
        print "DONE, merging the Categories and Sources files ..."
   
    exit(status)