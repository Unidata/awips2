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
# MyDialog.py
# Template class for creating a message or dialog
#
# Author: hansen
# ----------------------------------------------------------------------------

import Tkinter

# To use this dialog:
#  -- Create "MyDialog" as a Utility in the Edit Actions Dialog
#     Utilities window.
#  -- At the beginning of your file, include:
#      import MyDialog
#  -- In your procedure or smart tool (preProcessTool section):
#      dialog = MyDialog.MyDialog(None, "Title", "Message")
#  -- When you want to close the dialog (e.g. at end of the procedure,
#     or in postProcessTool):
#      dialog.destroy()
#
# For example, here is a Procedure that displays an "In Progress"
#  dialog:
#
##  import MyDialog
##  import SmartScript 

##  class Procedure (SmartScript.SmartScript): 
##      def __init__(self, dbss): 
##          SmartScript.SmartScript.__init__(self, dbss) 

##      def execute(self, editArea, timeRange, varDict):
##          # Put up In Progress dialog
##          dialog = MyDialog.MyDialog(None,"Status","Procedure in Progress")

##          self.copy(elements, 120, 240, 'MRF') 
##          self.copy(elements, 61, 120, 'AVN') 
##          self.copy(elements, begintime, 60, 'NAM')

##          # Destroy In Progress dialog
##          dialog.top().destroy()
  
class MyDialog(Tkinter.Tk):

  def __init__(self, parent, title, message, callback=None):
      
      Tkinter.Tk.__init__(self, parent)
      
      self.transient(parent)
      self.__parent = parent
      self.withdraw()
      
      # Create the dialog         
      self.__top = Tkinter.Toplevel(self.__parent)
      self.__top.title(title)
      
      # Center the dialog on the screen
      xoff = (self.__top.winfo_screenwidth() / 2) - (self.__top.winfo_reqwidth() / 2)
      yoff = (self.__top.winfo_screenheight() / 2) - (self.__top.winfo_reqheight() / 2)
      self.__top.wm_geometry("+%d+%d" % (xoff, yoff))

      # Create Label text in the dialog
      labelFont =  "-b&h-helvetica-bold-i-*-18-*"
      self.__l = Tkinter.Label(self.__top, text=message, font=labelFont)
      self.__l.pack(padx=5)

      #Include these lines if you want your dialog to have
      # an entry field and Ok button
      #The callback (set up by your calling program) will be
      # called when the OK button is clicked. It can be used to
      # send the results of the user entries back to your procedure
      # or smart tool.
      
##      self.__callback = callback
##      self.__e = Tkinter.Entry(top)
##      self.__e.pack(padx=5)
##      b = Tkinter.Button(self.__top, text="OK", command=self.ok)
##      b.pack(pady=5)

      self.protocol("WM_DELETE_WINDOW", self.closeCB)
      
      self.__top.update_idletasks()
      
  def ok(self):
      print "value is", self.__e.get()
      # Set up a variable dictionary and notify the
      # the calling program of the results
      if self.__callback is not None:
        varDict = {}
        varDict["Entry Value"] = self.__e.get()
        self.__callback(varDict)
      self.destroy()
  
  def closeCB(self):
      self.destroy()
      self.quit()

def myCB(varDict):
  entry = varDict["Entry Value"]
  print "Entry value", entry

# The following is executed when testing this dialog by entering
# the following from the command line:
#  python MyDialog.py
  
if __name__ == "__main__":
    d = MyDialog(None, "Status","Procedure In Progress", callback=myCB)
    x = input("Enter 0 to Close Dialog: ")
    d.destroy()
    
