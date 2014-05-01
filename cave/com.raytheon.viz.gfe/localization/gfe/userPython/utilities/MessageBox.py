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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MessageBox.py
#
# Author: wdougherty
# ----------------------------------------------------------------------------
import jep
import JUtil
from org.eclipse.jface.dialogs import MessageDialog
from org.eclipse.swt import SWT
from com.raytheon.viz.gfe.dialogs import ThreadedMessageDialog
##
# This class is set up to make it easy for the user to create quick message
# dialogs in SWT from Python. 
#
class MessageBox(object):
    # SWT constants, so Python clients don't need to import SWT
    OK = SWT.OK # the OK button style/return value
    CANCEL = SWT.CANCEL # the CANCEL button style/return value
    
    # Icons to show with the message
    ICON_ERROR = SWT.ICON_ERROR
    ICON_INFORMATION = SWT.ICON_INFORMATION
    ICON_QUESTION = SWT.ICON_QUESTION
    ICON_WARNING = SWT.ICON_WARNING
    ICON_WORKING = SWT.ICON_WORKING
    
    ##
    # The constructor for a MessageBox. This just invokes the constructor
    # for the Java messageBox, setting defaults if the user hasn't specified
    # a shell or style.
    #
    # @param shell: only included for backwards compatibility. Ignored.
    # @type shell: N/A
    # @param style: Style codes for this MessageBox.
    # @type style: int
    def __init__(self, shell=None, style=None):
        if style is None:
            style = SWT.OK
        self.__messageBox = ThreadedMessageDialog()
        # Convert SWT constants to JFace constants
        labels = ["Ok"]
        if (style & MessageBox.ICON_ERROR) != 0:
            dstyle = MessageDialog.ERROR
        elif (style & MessageBox.ICON_QUESTION) != 0:
            dstyle = MessageDialog.QUESTION
            labels.append("Cancel")
        elif (style & MessageBox.ICON_INFORMATION) != 0:
            dstyle = MessageDialog.INFORMATION
        elif (style & MessageBox.ICON_WARNING) != 0:
            dstyle = MessageDialog.WARNING
        else:
            dstyle = MessageDialog.NONE
        if (style & MessageBox.CANCEL) != 0:
            if not "Cancel" in labels:
                labels.append("Cancel")
        self.__messageBox.setStyle(dstyle)
        labels = JUtil.pyValToJavaObj(labels)
        self.__messageBox.setDialogButtonLabels(labels)    
        
    def getText(self):
        return self.__messageBox.getTitle()
    
    def getMessage(self):
        return self.__messageBox.getMessage()
    
    def getParent(self):
        return None;
    
    def open(self):
        val = self.__messageBox.open()
        return val
    
    def setMessage(self, message):
        self.__messageBox.setMessage(message)
        
    def setText(self, text):
        self.__messageBox.setTitle(text)

    def setButtonLabels(self, labels):
        labels = JUtil.pyValToJavaObj(labels)
        self.__messageBox.setDialogButtonLabels(labels)
        
    def getButtonLabels(self):
        return self.__messageBox.getDialogButtonLabels()
    
    def setDefaultIndex(self, idx):
        self.__messageBox.setDefaultIndex(idx)
        
    def getDefaultIndex(self):
        return self.__messageBox.getDefaultIndex()