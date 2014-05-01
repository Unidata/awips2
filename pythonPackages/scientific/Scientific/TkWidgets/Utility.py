# Various useful small widgets
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# Last revision: 2008-8-18
#

import Tkinter, Dialog, FileDialog
import copy, os, string


class FilenameEntry(Tkinter.Frame):

    """
    Filename entry widget

    A FilenameEntry widget consists of three parts: an identifying
    label, a text entry field for the filename, and a button labelled
    'browse' which call a file selection dialog box for picking a file
    name.
    """

    def __init__(self, master, text, pattern = '*', must_exist = True,
                 **attr):
        """
        @param master: the parent widget
        @param text: the label in front of the filename box
        @type text: C{str}
        @param pattern: the filename matching pattern that determines the
                        file list in the file selection dialog
        @type pattern: C{str}
        @param must_exist: if C{True}, allow only names of existing files
        @type must_exist: C{bool}
        """
        self.pattern = pattern
        self.must_exist = must_exist
        newattr = copy.copy(attr)
        newattr['text'] = text
        Tkinter.Frame.__init__(self, master)
        apply(Tkinter.Label, (self,), newattr).pack(side=Tkinter.LEFT)
        self.filename = Tkinter.StringVar()
        Tkinter.Button(self, text="Browse...",
                       command=self.browse).pack(side=Tkinter.RIGHT)
        newattr = copy.copy(attr)
        newattr['textvariable'] = self.filename
        entry = apply(Tkinter.Entry, (self,), newattr)
        entry.pack(side=Tkinter.RIGHT, expand=1, fill=Tkinter.X)
        entry.icursor("end")

    def browse(self):
        if self.must_exist:
            file = FileDialog.LoadFileDialog(self).go(pattern=self.pattern)
        else:
            file = FileDialog.SaveFileDialog(self).go(pattern=self.pattern)
        if file:
            self.filename.set(file)

    def get(self):
        """
        @returns: the current filename
        @rtype: C{str}
        @raises ValueError: if must_exist is C{True} and the name does not
                            refer to an existing file
        """
        filename =  self.filename.get()
        if self.must_exist and not os.path.exists(filename):
            Dialog.Dialog(self, title='File not found',
                          text='The file "' + filename + '" does not exist.',
                          bitmap='warning', default=0,
                          strings = ('Cancel',))
            raise ValueError
        return filename


class FloatEntry(Tkinter.Frame):

    """
    Entry field for float numbers

    A FloatEntry widget consists of a label followed by a text entry
    field. 
    """
    
    def __init__(self, master, text, init = None, lower=None, upper=None,
                 name = None, **attr):
        """
        @param master: the parent widget
        @param text: the label in front of the entry field
        @type text: C{str}
        @param init: an optional initial value (default: blank field)
        @type init: number
        @param upper: an optional upper limit for the value
        @type upper: number
        @param lower: an optional lower limit for the value
        @type lower: number
        """
        self.text = text
        self.lower = lower
        self.upper = upper
        if name is None:
            name = text
        self.name = name
        newattr = copy.copy(attr)
        newattr['text'] = text
        Tkinter.Frame.__init__(self, master)
        apply(Tkinter.Label, (self,), newattr).pack(side=Tkinter.LEFT)
        self.value = Tkinter.DoubleVar()
        if init is not None:
            self.value.set(init)
        newattr = copy.copy(attr)
        newattr['textvariable'] = self.value
        self.entry = apply(Tkinter.Entry, (self,), newattr)
        self.entry.pack(side=Tkinter.RIGHT, anchor=Tkinter.E,
                        expand=1, fill=Tkinter.X)
        self.entry.icursor("end")

    def bind(self, sequence=None, func=None, add=None):
        self.entry.bind(sequence, func, add)

    def set(self, value):
        """
        Set the value displayed in the field

        @param value: the new value
        @type value: C{float}
        """
        return self.value.set(value)

    def get(self):
        """
        @returns: the current value displayed in the field
        @rtype: C{float}
        @raises ValueError: if the current value is not a valid
                            number or is not within the specified limits
        """
        try:
            value = self.value.get()
        except (Tkinter.TclError, ValueError):
            Dialog.Dialog(self, title='Illegal value',
                          text='The value of "' + self.name +
                               '" must be a number.',
                          bitmap='warning', default=0,
                          strings = ('Cancel',))
            raise ValueError
        range_check = 0
        if self.lower is not None and value < self.lower:
            range_check = -1
        if self.upper is not None and value > self.upper:
            range_check = 1
        if range_check != 0:
            text = 'The value of "' + self.name + '" must not be '
            if range_check < 0:
                text = text + 'smaller than ' + `self.lower` + '.'
            else:
                text = text + 'larger than ' + `self.upper` + '.'
            Dialog.Dialog(self, title='Value out of range', text=text,
                          bitmap='warning', default=0,
                          strings = ('Cancel',))
            raise ValueError
        return value


class IntEntry(FloatEntry):

    """
    Entry field for integer numbers

    A IntEntry widget consists of a label followed by a text entry
    field. 
    """
    
    def get(self):
        """
        @returns: the current value displayed in the field
        @rtype: C{int}
        @raises ValueError: if the current value is not a valid
                            number or is not within the specified limits
        """
        value = FloatEntry.get(self)
        ivalue = int(value)
        if ivalue != value:
            Dialog.Dialog(self, title='Illegal value',
                          text='The value of "' + self.name +
                               '" must be an integer.',
                          bitmap='warning', default=0,
                          strings = ('Cancel',))
            raise ValueError
        return ivalue

class ButtonBar(Tkinter.Frame):

    """
    Horizontal array of buttons
    """

    def __init__(self, master, left_button_list, right_button_list):
        """
        @param master: the parent widget
        @param left_button_list: a list of (text, action) tuples specifying the
                                 buttons on the left-hand side of the button bar
        @param right_button_list: a list of (text, action) tuples specifying the
                                  buttons on the right-hand side of the button
                                  bar
        """
        Tkinter.Frame.__init__(self, master, bd=2, relief=Tkinter.SUNKEN)
        for button, action in left_button_list:
            Tkinter.Button(self, text=button,
                           command=action).pack(side=Tkinter.LEFT)
        for button, action in right_button_list:
            Tkinter.Button(self, text=button,
                           command=action).pack(side=Tkinter.RIGHT)


class StatusBar(Tkinter.Frame):

    """
    Status bar

    A status bar can be used to inform the user about the status of an
    ongoing calculation. A message can be displayed with set() and
    removed with clear(). In both cases, the StatusBar object makes
    sure that the change takes place immediately. While a message
    is being displayed, the cursor form is changed to a watch.
    """

    def __init__(self, master):
        """
        @param master: the parent widget
        """
        Tkinter.Frame.__init__(self, master, bd=2, relief=Tkinter.RAISED)
        self.text = Tkinter.Label(self, text='')
        self.text.pack(side=Tkinter.LEFT, expand=Tkinter.YES)

    def set(self, text):
        """
        Set a message to be displayed in the status bar
        
        @param text: the text of the message
        """
        self.text.configure(text = text)
        self.text.update_idletasks()
        self.master.configure(cursor='watch')
        self.update()
        self.update_idletasks()

    def clear(self):
        """
        Clear any message displayed in the status bar
        """
        self.text.configure(text = '')
        self.text.update_idletasks()
        self.master.configure(cursor='top_left_arrow')
        self.update_idletasks()


#
# The following class was taken from the Pythonware Tkinter introduction
#
class ModalDialog(Tkinter.Toplevel):

    def __init__(self, parent, title = None):

        Tkinter.Toplevel.__init__(self, parent)
        self.transient(parent)
        
        if title:
            self.title(title)

        self.parent = parent

        self.result = None

        body = Tkinter.Frame(self)
        self.initial_focus = self.body(body)
        body.pack(padx=5, pady=5)

        self.buttonbox()

        self.grab_set()

        if not self.initial_focus:
            self.initial_focus = self

        self.protocol("WM_DELETE_WINDOW", self.cancel)

        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))

        self.initial_focus.focus_set()

        self.wait_window(self)

    #
    # construction hooks

    def body(self, master):
        # create dialog body.  return widget that should have
        # initial focus.  this method should be overridden

        pass

    def buttonbox(self):
        # add standard button box. override if you don't want the
        # standard buttons

        box = Tkinter.Frame(self)

        w = Tkinter.Button(box, text="OK", width=10,
                           command=self.ok, default=Tkinter.ACTIVE)
        w.pack(side=Tkinter.LEFT, padx=5, pady=5)
        w = Tkinter.Button(box, text="Cancel", width=10, command=self.cancel)
        w.pack(side=Tkinter.LEFT, padx=5, pady=5)

        self.bind("<Return>", self.ok)
        self.bind("<Escape>", self.cancel)

        box.pack()

    #
    # standard button semantics

    def ok(self, event=None):

        if not self.validate():
            self.initial_focus.focus_set() # put focus back
            return

        self.withdraw()
        self.update_idletasks()

        self.apply()

        self.cancel()

    def cancel(self, event=None):

        # put focus back to the parent window
        self.parent.focus_set()
        self.destroy()

    #
    # command hooks

    def validate(self):

        return 1 # override

    def apply(self):

        pass # override


if __name__ == '__main__':
    
    class MyDialog(ModalDialog):

        def body(self, master):

            Tkinter.Label(master, text="First:").grid(row=0)
            Tkinter.Label(master, text="Second:").grid(row=1)

            self.e1 = IntEntry(master, '', 0, 0, 10, fg='red')
            self.e2 = Tkinter.Entry(master)

            self.e1.grid(row=0, column=1)
            self.e2.grid(row=1, column=1)
            return self.e1 # initial focus

        def apply(self):
            first = string.atoi(self.e1.get())
            second = string.atoi(self.e2.get())
            self.result = first, second

    root = Tkinter.Tk()
    Tkinter.Button(root, text="Hello!").pack()
    root.update()
    d = MyDialog(root)
    print d.result
