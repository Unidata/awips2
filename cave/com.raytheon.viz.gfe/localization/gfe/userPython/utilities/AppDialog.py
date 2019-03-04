##
##

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import Tkinter
#
##
# We have a system in which "short" Python scripts are invoked from a GUI
# written in another language. Some of the scripts need to pop up dialogs
# to obtain user input. tkSimpleDialog.Dialog does not work well as a base
# class for these dialogs because it assumes that a Tk root window is present,
# which is typically not the case for these embedded scripts. 
#
class AppDialog(Tkinter.Tk):
    "A top-level dialog with its own Tcl interpreter."
    
    def __init__(self, **kw):
        "Constructor."
        Tkinter.Tk.__init__(self, **kw)
        body = self.body(self)
        body.focus_set()
        self.buttonbox()
        self.protocol("WM_DELETE_WINDOW", self.cancel)

    def apply(self):
        """Process the data.

This method is called automatically to process the data, *after*
the dialog is destroyed. By default, it does nothing."""
        pass
    
    def body(self, master):
        """Create dialog body.

Return the widget that should have initial focus.
This method should be overridden, and is called
by the __init__ method."""        
        frame = Tkinter.Frame(master)
        label = Tkinter.Label(frame, text="Body")
        label.pack()
        frame.pack(side=Tkinter.TOP)
        return frame
    
    def buttonbox(self):
        box = Tkinter.Frame(self)
        okButton = Tkinter.Button(box, text="Ok", command=self.ok)
        okButton.pack(side=Tkinter.LEFT, pady=5, padx=10)
        cancelButton = Tkinter.Button(box, text="Cancel", command=self.cancel)
        cancelButton.pack(side=Tkinter.LEFT, pady=5, padx=10)
        box.pack(side=Tkinter.BOTTOM)
    
    def cancel(self, event=None):
        """Process the Cancel button.
This method is also invoked when the dialog is closed by the control bar."""      
        self.destroy()
    
    def ok(self, event=None):
        """Process the Ok button."""
        if self.validate():
            self.destroy()
            self.apply()
    
    def validate(self):
        """Validate the data.
This method is called automatically to process the data before 
the dialog is destroyed. By default, it always returns True.
Override to perform validation."""
        return True
