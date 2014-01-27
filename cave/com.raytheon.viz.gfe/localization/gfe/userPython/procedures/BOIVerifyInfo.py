# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerifyInfo - version 2.0.5
#
#   Allows changes to forecaster numbers - and changes any current records
#   to match the new numbers.  Allows deletion of grids in the archived
#   database (i.e. to remove bad grids)
#
# Author: Tim Barker - SOO BOI
#   2007/11/06 - version 2.0 - Original Documented Implementation
#   2008/05/28 - version 2.0.5 - added ability to show and delete grids
#                    from obs models
#
#
#   2010/04/23  ryu  Initial port to AWIPS II.
#
# ----------------------------------------------------------------------------
#
MenuItems = ["Verify"]

from numpy import *
from math import *
import Tkinter,tkFont,tkMessageBox
import TkDefaults
import time
import SmartScript
import BOIVerifyUtility

PROGNAME="BOIVerifyInfo" # you can change it if you dont like BOI.  Shame on you!

MONS=["DUM","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
DAYS=["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]
COLORLIST=["Cyan","Orange","PaleGreen","Red","Blue","Magenta","ForestGreen","Sienna",
           "Pink","Green","purple","Yellow","Tan","Turquoise","SteelBlue",
           "chartreuse","Gold","tomato","Violet","aquamarine","Coral"]
#=====================================================================
#  The dummy procedure - which does nothing more than start the
#  non-modal BOIVerifyInfo dialog box.
#
class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      self._dbss=dbss
      SmartScript.SmartScript.__init__(self, dbss)
      return
   #------------------------------------------------------------------
   #  execute - simply starts the non-modal "Info" dialog box
   #
   def execute(self):
      self.statusBarMsg("Starting %s"%PROGNAME,"R")

      tk = Tkinter.Tk()
      sw = tk.winfo_screenwidth()
      sh = tk.winfo_screenheight()
      tk.geometry("%dx%d+0+0" % (sw,sh))
      self.root = tk

      TkDefaults.setDefaults(tk)

      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.dialog=VerifInfo(self.VU, parent=tk,
                            name="BOIVerify Grid Archive Info",
                            callbackMethod=self.doneInfo,
                            modal=0,
                            )
      tk.withdraw()
      tk.mainloop()

      self.statusBarMsg("Finished starting %s"%PROGNAME,"R")
      return
   #------------------------------------------------------------------
   #  doneInfo - called when the Info dialog is closed (with the button
   #             name as an argument
   #
   def doneInfo(self,buttonType):
      self.root.destroy()
      return
#=====================================================================
#
#  Basic Class for a dialog - similar to IFPDialog.Dialog
#
class Dialog(Tkinter.Toplevel):
   def __init__(self,parent=None,title=None,modal=0):
      if parent is None:
         return
      Tkinter.Toplevel.__init__(self,parent)
      #self.transient(parent)
      if title:
         self.title(title)
      self.parent=parent
      self.result=None
      self.buttonbox()
      bodyFrame=Tkinter.Frame(self)
      self.initial_focus=self.body(bodyFrame)
      bodyFrame.pack(padx=5,pady=5,fill=Tkinter.BOTH,expand=1)
      bodyFrame.pack_propagate(1)
      #
      if not self.initial_focus:
         self.initial_focus=self
      self.protocol("WM_DELETE_WINDOW", self.cancel)
      self.geometry("+%d+%d"%(parent.winfo_rootx()+50,
                              parent.winfo_rooty()+50))
      self.initial_focus.focus_set()
      
      self.wait_visibility()
      self.update_idletasks()
      geom=self.geometry()
      (wh,rest)=geom.split("+",1)
      (wid,hgt)=wh.split("x",1)
      self.minsize(wid,hgt)
      
      if modal==1:
         self.grab_set()
         self.wait_window(self)
         return self.result
      else:
         return self
   #------------------------------------------------------------------
   #  body - normally overridden with the stuff you want to display
   #        in the dialog box
   #
   def body(self,master):
      pass
   #------------------------------------------------------------------
   #  buttonbox - displays the buttonbox at the bottom of the dialog.
   #              Normally has OK and Cancel buttons - but can be
   #              overridden to have any buttons desired
   #
   def buttonbox(self):
      box=Tkinter.Frame(self)
      w=Tkinter.Button(box,text="Ok",width=10,command=self.ok,
                  default=Tkinter.ACTIVE)
      w.pack(side=Tkinter.LEFT,padx=5,pady=5)
      w=Tkinter.Button(box,text="Cancel",width=10,command=self.cancel)
      w.pack(side=Tkinter.LEFT,padx=5,pady=5)
      box.pack(side=Tkinter.BOTTOM)
   #------------------------------------------------------------------
   #  ok - called when the OK button is pressed.  Calls validate to
   #       see if the input is OK.  If the input is OK it removes
   #       the dialog and does the action specified in apply
   #       If the input has some problem - it returns to the dialog
   #
   def ok(self,event=None):
      if not self.validate():
         self.initial_focus.focus_set()
         return
      self.withdraw()
      self.update_idletasks()
      self.apply()
      self.cancel()
   #------------------------------------------------------------------
   #  cancel - called when the Cancel button is pressed - and when
   #           everything else is done.  Destroys the dialog
   #
   def cancel(self,event=None):
      self.parent.focus_set()
      self.destroy()
   #------------------------------------------------------------------
   #  validate - normally overridden with stuff that checks the input
   #             on the dialog box.  Should return 1 if input is OK,
   #             and 0 if not.
   #
   def validate(self):
      return 1
   #------------------------------------------------------------------
   #  apply - normally overridden with stuff that needs to be done
   #          when the dialog input has been validated and it is
   #          OK to proceed.
   #
   def apply(self):
      pass
#=====================================================================
#  ChangeCancelDialog - a Dialog  to change the forecaster number,
#                       forecaster ID, or forecater name
#
class ChangeCancelDialog(Dialog):
   def __init__(self, VU, numberVar, idVar, nameVar,
                parent=None, name="Edit Forecaster", callbackMethod=None,
                modal=1):

      self.__parent = parent
      self.__name = name
      self.__modal = modal
      self.__callbackMethod = callbackMethod
      self.__VU=VU
      self.__numberVar=numberVar
      self.__idVar=idVar
      self.__nameVar=nameVar
      self.__oldNum=self.__numberVar.get()
      self.__oldID=self.__idVar.get()
      self.__oldName=self.__nameVar.get()
      self.__dialog=Dialog.__init__(self,parent=self.__parent,
                                    title=self.__name,
                                    modal=self.__modal)
      return
   #-----------------------------------------------------------------
   # buttonbox - special buttonbox with Change and Cancel buttons
   #
   def buttonbox(self):
      buttonFrame = Tkinter.Frame(self)
      Tkinter.Button(buttonFrame, text="Change", width=7,
        command=self.changeCB).pack(side=Tkinter.LEFT, pady=5, padx=10)
      Tkinter.Button(buttonFrame, text="Cancel",width=7,
        command=self.cancelCB).pack(side=Tkinter.LEFT,pady=5, padx=10)
      buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
   #------------------------------------------------------------------
   # body - special body with the current number, username, and
   #        display name shown
   #
   def body(self, master):
      Tkinter.Label(master,text="Number:").grid(column=0,row=0,sticky=Tkinter.E)
      Tkinter.Label(master,text="Username:").grid(column=0,row=1,sticky=Tkinter.E)
      Tkinter.Label(master,text="Display Name:").grid(column=0,row=2,sticky=Tkinter.E)
      self.numEntry=Tkinter.Entry(master,textvariable=self.__numberVar,width=2)
      self.numEntry.grid(column=1,row=0,sticky=Tkinter.W)
      self.idEntry=Tkinter.Entry(master,textvariable=self.__idVar,width=8)
      self.idEntry.grid(column=1,row=1,sticky=Tkinter.W)
      self.nameEntry=Tkinter.Entry(master,textvariable=self.__nameVar,width=25)
      self.nameEntry.grid(column=1,row=2,sticky=Tkinter.W)
   #------------------------------------------------------------------
   #  changeCB - called when they click on Change.  Need to validate
   #             everything to make sure the changes are OK.
   #
   def changeCB(self):
      #
      #  Check forecaster number for just a number, between 1 and 99
      #
      newNumStr=self.__numberVar.get().strip()      
      try:
         num=int(newNumStr)
      except:
         tkMessageBox.showerror("Error","Forecaster Number needs to be an integer",parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      if ((num<0)or(num>99)):
         tkMessageBox.showerror("Error","Forecater Number needs to be between 1 and 99",
                       parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      if (num==0):
         tkMessageBox.showerror("Error","Forecaster Number 0 cannot be used",
                       parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      #
      #  If changing numbers - make sure the new number is not already
      #  in use.
      #
      if num!=int(self.__oldNum):
         curNums=self.__VU.getFcstrNums()
         for testNum in curNums:
            if int(testNum)==num:
               tkMessageBox.showerror("Errror","Forecaster Number %d is already in use"%num,
                             parent=self)
               self.numEntry.selection_range(0,Tkinter.END)
               self.numEntry.focus_set()
               return
      #
      #  Must provide a username
      #
      newID=self.__idVar.get().strip()
      if len(newID)==0:
         tkMessageBox.showerror("Error","You must provide a username",
                       parent=self)
         self.idEntry.selection_range(0,Tkinter.END)
         self.idEntry.focus_set()
         return
      #
      #  username cannot be the same as any other current username
      #
      if newID!=self.__oldID:
         curIDs=self.__VU.getFcstrIDs()
         for testID in curIDs.values():
            if testID==newID:
               tkMessageBox.showerror("Error","Username %s is already in use"%newID,
                             parent=self)
               self.idEntry.selection_range(0,Tkinter.END)
               self.idEntry.focus_set()
               return
      #
      #  Must provide a display name
      #
      newName=self.__nameVar.get().strip()
      if len(newName)==0:
         tkMessageBox.showerror("Error","You must provide a display name",
                       parent=self)
         self.nameEntry.selection_range(0,Tkinter.END)
         self.nameEntry.focus_set()
         return
      #
      #  If everything the same as when we started - treat this
      #  the same as a cancel
      #
      if ((num==int(self.__oldNum))and(newID==self.__oldID)and(newName==self.__oldName)):
         self.cancel()
         return
      #
      #  If number changes - need to do a lot more stuff - so ask them
      #  if they are sure - and do it if they say yes
      #
      if (num!=int(self.__oldNum)):
         oldnum=int(self.__oldNum)
         text=     "It will take a while to change forecasts "
         text+="attributed to old forecaster number %d "%oldnum
         text+="to new forecaster number %d.\n"%num
         text+="\n"
         text+="Are you sure you want to proceed?"
         ynDiag=tkMessageBox.askyesno("Are you sure?",text,
                            parent=self,default=tkMessageBox.NO)
         if not ynDiag:
            self.__callbackMethod("Change")
            self.cancel()
            return
      #
      #  OK - all input is valid and user wants to proceed
      #  so remove the 'change' dialog and actually make
      #  the changes
      #
      self.withdraw()
      self.update_idletasks()
      #
      #  make number changes (could take some time)
      #
      if (num!=int(self.__oldNum)):
         self.changeNumbers()
      #
      #  Change IDs and names - should be fast
      #
      Names=self.__VU.getFcstrNames()
      IDs=self.__VU.getFcstrIDs()
      numStr="%2.2d"%num
      Names[numStr]=newName
      IDs[numStr]=newID
      self.__VU.setFcstrs(Names,IDs)
      self.__VU.saveFcstrNums()
      #
      if self.__callbackMethod is not None:
         self.__callbackMethod("Change")
      self.cancel()
   #------------------------------------------------------------------
   #  cancelCB - called when they click on Cancel when making
   #             changes
   #
   def cancelCB(self):
      if self.__callbackMethod is not None:
         self.__callbackMethod("Cancel")
      self.cancel()
   #------------------------------------------------------------------
   #  changeNumbers - gets called if they REALLY want to change
   #                  numbers for a forecaster.  Goes through all
   #                  Official datafiles and changes all grids
   #                  associated with old number to new number
   #
   def changeNumbers(self):
      newNumStr=self.__numberVar.get().strip()
      num=int(newNumStr)
      oldnum=int(self.__oldNum)
      for parm in self.__VU.getVerParms():
         datatype=self.__VU.getVerParmType(parm)
         if not self.__VU.checkFile(parm,"Official",modify=1,datatype=datatype):
            continue
         fnc=self.__VU.fncFcstr[:,:]
         count=add.reduce(add.reduce(equal(fnc,oldnum)))
         nnc=where(equal(fnc,oldnum),num,fnc)
         self.__VU.fncFcstr[:,:]=nnc[:,:].astype(int8)
         self.__VU.closeFcstFile()
      newID=self.__idVar.get().strip()
      newName=self.__nameVar.get().strip()
      Names=self.__VU.getFcstrNames()
      IDs=self.__VU.getFcstrIDs()
      del Names[self.__oldNum]
      del IDs[self.__oldNum]
      newNumStr="%2.2d"%num
      Names[newNumStr]=newName
      IDs[newNumStr]=newID
      self.__VU.setFcstrs(Names,IDs)
      self.__VU.saveFcstrNums()
      return
#=====================================================================
#  AddCancelDialog - for when they want to add a forecaster
#
class AddCancelDialog(Dialog):
   def __init__(self, VU, numberVar, idVar, nameVar,
                parent=None, name="Add Forecaster", callbackMethod=None,
                modal=1):

      self.__parent = parent
      self.__name = name
      self.__modal = modal
      self.__callbackMethod = callbackMethod
      self.__VU=VU
      self.__numberVar=numberVar
      self.__idVar=idVar
      self.__nameVar=nameVar
      
      self.__dialog=Dialog.__init__(self,parent=self.__parent,
                                    title=self.__name,
                                    modal=self.__modal)
      return
   #------------------------------------------------------------------
   #  buttonbox - special buttonbox with Add and Cancel buttons
   #
   def buttonbox(self):
      buttonFrame = Tkinter.Frame(self)
      Tkinter.Button(buttonFrame, text="Add", width=7,
        command=self.addCB).pack(side=Tkinter.LEFT, pady=5, padx=10)
      Tkinter.Button(buttonFrame, text="Cancel",width=7,
        command=self.cancelCB).pack(side=Tkinter.LEFT,pady=5, padx=10)
      buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
   #------------------------------------------------------------------
   # body - special body with the current number, username, and
   #        display name shown
   #
   def body(self, master):
      Tkinter.Label(master,text="Number:").grid(column=0,row=0,sticky=Tkinter.E)
      Tkinter.Label(master,text="Username:").grid(column=0,row=1,sticky=Tkinter.E)
      Tkinter.Label(master,text="Display Name:").grid(column=0,row=2,sticky=Tkinter.E)
      self.numEntry=Tkinter.Entry(master,textvariable=self.__numberVar,width=2)
      self.numEntry.grid(column=1,row=0,sticky=Tkinter.W)
      self.idEntry=Tkinter.Entry(master,textvariable=self.__idVar,width=8)
      self.idEntry.grid(column=1,row=1,sticky=Tkinter.W)
      self.nameEntry=Tkinter.Entry(master,textvariable=self.__nameVar,width=25)
      self.nameEntry.grid(column=1,row=2,sticky=Tkinter.W)
   #------------------------------------------------------------------
   #  addCB - called when they click on Add.  Need to validate
   #          everything to make sure the changes are OK.
   #
   def addCB(self):
      #
      #  Check forecaster number for just a number, between 1 and 99
      #
      newNumStr=self.__numberVar.get().strip()
      try:
         num=int(newNumStr)
      except:
         tkMessageBox.showerror("Error","Forecaster Number needs to be an integer number",
                       parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      if ((num<0)or(num>99)):
         tkMessageBox.showerror("Error","Forecater Number needs to be between 1 and 99",
                       parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      if (num==0):
         tkMessageBox.showerror("Error","Forecaster Number 0 cannot be used",
                       parent=self)
         self.numEntry.selection_range(0,Tkinter.END)
         self.numEntry.focus_set()
         return
      #
      #  Make sure the new number is not already
      #  in use.
      #
      curNums=self.__VU.getFcstrNums()
      for testNum in curNums:
         if int(testNum)==num:
            tkMessageBox.showerror("Error","Forecaster Number %d is already in use"%num,
                          parent=self)
            self.numEntry.selection_range(0,Tkinter.END)
            self.numEntry.focus_set()
            return
      #
      #  Must provide a username
      #
      newID=self.__idVar.get().strip()
      if len(newID)==0:
         tkMessageBox.showerror("Error","You must provide a username",
                       parent=self)
         self.idEntry.selection_range(0,Tkinter.END)
         self.idEntry.focus_set()
         return
      #
      #  username cannot be the same as any other current username
      #
      curIDs=self.__VU.getFcstrIDs()
      for testID in curIDs.values():
         if testID==newID:
            tkMessageBox.showerror("Error","Username %s is already in use"%newID,
                          parent=self)
            self.idEntry.selection_range(0,Tkinter.END)
            self.idEntry.focus_set()
            return
      #
      #  Must provide a display name
      #
      newName=self.__nameVar.get().strip()
      if len(newName)==0:
         tkMessageBox.showerror("Error","You must provide a display name",
                       parent=self)
         self.nameEntry.selection_range(0,Tkinter.END)
         self.nameEntry.focus_set()
         return
      #
      #  No number changes - but change IDs and names
      #
      self.withdraw()
      self.update_idletasks()
      Names=self.__VU.getFcstrNames()
      IDs=self.__VU.getFcstrIDs()
      numStr="%2.2d"%num
      Names[numStr]=newName
      IDs[numStr]=newID
      self.__VU.setFcstrs(Names,IDs)
      self.__VU.saveFcstrNums()
      #
      if self.__callbackMethod is not None:
         self.__callbackMethod("Add")
      self.cancel()
   #------------------------------------------------------------------
   #  cancelCB - called when they click on Cancel when making
   #             changes
   #   
   def cancelCB(self):
      if self.__callbackMethod is not None:
         self.__callbackMethod("Cancel")
      self.cancel()
#==============================================================================
# doneDialog - a generic dialog class with a single DONE button at the bottom
#
class doneDialog(Dialog):
   def __init__(self, parent=None, name="nonModal Dialog", callbackMethod=None,
                modal=1):

      self.__parent = parent
      self.__name = name
      self.__modal = modal
      self.__callbackMethod = callbackMethod
      self.__dialog=Dialog.__init__(self,parent=self.__parent,
                                    title=self.__name,
                                    modal=self.__modal)
      return self.__dialog
   def buttonbox(self):
      buttonFrame = Tkinter.Frame(self)
      Tkinter.Button(buttonFrame, text="Done", width=10,
        command=self.doneCB).pack(side=Tkinter.RIGHT, pady=5, padx=10)
      buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
   def body(self, master):
      bodylabel=Tkinter.Label(master,text="This is the body of doneDialog")
      bodylabel.pack(side=Tkinter.BOTTOM)
   def doneCB(self):
      if self.__callbackMethod is not None:
         self.__callbackMethod("Done")
      self.cancel()
#=======================================================================
# ListDialog - shows all the forecasts made by the specified
#              forecaster number
#
class ListDialog(doneDialog):
   def __init__(self, VU, numstr, idstr, namestr, parent=None,
                name="List of forecasts", callbackMethod=None, modal=1):
      self.__VU=VU
      self.__numstr=numstr
      self.__idstr=idstr
      self.__namestr=namestr
      self.__parent=parent
      doneDialog.__init__(self,parent=parent,name=name,callbackMethod=callbackMethod,
                          modal=modal)
   def body(self,master):
      sb=Tkinter.Scrollbar(master=master)
      sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      self.cf=tkFont.Font(family="Courier",size=-12)
      txt=Tkinter.Text(master=master,width=60,height=30,yscrollcommand=sb.set,
                       font=self.cf)
      txt.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      sb.config(command=txt.yview)
      text=""
      maxparmwidth=0
      totalgrids=0
      oldnum=int(self.__numstr)
      fcstsMade={}
      for parm in self.__VU.getVerParms():
         maxparmwidth=max(maxparmwidth,len(parm))
         datatype=self.__VU.getVerParmType(parm)
         if not self.__VU.checkFile(parm,"Official",modify=0,datatype=datatype):
            continue
         fnc=self.__VU.fncFcstr[:,:]
         involved=logical_or.reduce(equal(fnc,oldnum),1)
         recsUsed=compress(involved,self.__VU.fncRecs)
         for i in xrange(recsUsed.shape[0]):
            rec=int(recsUsed[i])
            totalgrids+=1
            basekey="%d"%self.__VU.fncBtime[rec]
            if basekey not in fcstsMade.keys():
               fcstsMade[basekey]={}
            if parm not in fcstsMade[basekey].keys():
               fcstsMade[basekey][parm]=(1,self.__VU.fncStime[rec],self.__VU.fncEtime[rec])
            else:
               (num,start,end)=fcstsMade[basekey][parm]
               num+=1
               start=min(self.__VU.fncStime[rec],start)
               end=max(self.__VU.fncEtime[rec],end)
               fcstsMade[basekey][parm]=(num,start,end)
         self.__VU.closeFcstFile()
      #
      #  Display the data
      #
      cbb=tkFont.Font(family="Courier",size=-14,weight=tkFont.BOLD)
      cb=tkFont.Font(family="Courier",size=-12,weight=tkFont.BOLD)
      txt.tag_config("title",font=cbb)
      txt.tag_config("date",font=cb)
      txt.insert(Tkinter.END,"%s Grids:\n\n"%self.__namestr,("title"))
      if totalgrids==0:
         txt.insert(Tkinter.END,"   NONE\n\n",("date"))
      bases=fcstsMade.keys()
      bases.sort()
      bases.reverse()
      fmtn="  %%%ds: %%3d grids made from %%3d to %%3d hours\n"%(maxparmwidth)
      fmt1="  %%%ds: %%3d grid  made from %%3d to %%3d hours\n"%(maxparmwidth)
      for base in bases:
         (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(int(base))
         txt.insert(Tkinter.END,"%s %s %d, %4.4d %2.2dZ:\n"%(DAYS[gwda],
                    MONS[gmon],gday,gyea,ghou),("date"))
         made=fcstsMade[base]
         parms=made.keys()
         parms.sort()
         for parm in parms:
            (num,start,end)=made[parm]
            starthr=int((start-int(base))/3600.)
            endhr=int((end-int(base))/3600.)
            fmt=fmtn
            if num==1:
               fmt=fmt1
            txt.insert(Tkinter.END,fmt%(parm,num,starthr,endhr))
         txt.insert(Tkinter.END,"\n")
      txt.config(state=Tkinter.DISABLED)
      return
#=======================================================================
#  InfoDialog - shows info about the specified grid
#
class InfoDialog(doneDialog):
   def __init__(self, VU, model, parm, record, parent=None,
                name="Grid Info", callbackMethod=None, modal=1):
      self.__VU=VU
      self.__model=model
      self.__parm=parm
      self.__record=record
      self.__parent=parent
      doneDialog.__init__(self,parent=parent,name=name,callbackMethod=callbackMethod,
                          modal=modal)
   def body(self,master):
      sb=Tkinter.Scrollbar(master=master)
      sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      self.cf=tkFont.Font(family="Courier",size=-12)
      txt=Tkinter.Text(master=master,width=60,height=20,yscrollcommand=sb.set,
                       font=self.cf)
      txt.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      sb.config(command=txt.yview)
      obsmodels=self.__VU.getCFG('OBSMODELS')
      text=""
      #
      if not self.__VU.checkFile(self.__parm,self.__model,modify=0):
         text+="\n\nCould not read info for %s %s grid! \n\n"%(self.__model,self.__parm)
      else:
         if self.__model in obsmodels:
            btime=self.__VU.oncBtime[self.__record]
            stime=self.__VU.oncStime[self.__record]
            etime=self.__VU.oncEtime[self.__record]
            vtime=self.__VU.oncVtime[self.__record]
            fnums=[]
         else:
            btime=self.__VU.fncBtime[self.__record]
            stime=self.__VU.fncStime[self.__record]
            etime=self.__VU.fncEtime[self.__record]
            vtime=self.__VU.fncVtime[self.__record]
            fnums=self.__VU.fncFcstr[self.__record,:]
   
         datatype=self.__VU.getVerParmType(self.__parm)
         gridData=self.__VU.readRecord(self.__parm,self.__model,self.__record)
         if datatype!=1:
            minval=minimum.reduce(minimum.reduce(gridData))
            maxval=maximum.reduce(maximum.reduce(gridData))
            sum=add.reduce(add.reduce(gridData))
            sumsqr=add.reduce(add.reduce(gridData*gridData))
         else:
            (mag,direc)=gridData
            minval=minimum.reduce(minimum.reduce(mag))
            maxval=maximum.reduce(maximum.reduce(mag))
            sum=add.reduce(add.reduce(mag))
            sumsqr=add.reduce(add.reduce(mag*mag))
         numpts=add.reduce(add.reduce(ones(self.__VU._empty.shape)))
         avg=sum/numpts
         std=sqrt((sumsqr/numpts)-(avg*avg))
         self.__VU.closeFcstFile()

         prec=self.__VU.getParmPrecision(self.__model,self.__parm)
         if prec>0:
            fmt1="%%.%df"%prec
            fmt2="%%.%df"%(prec+1)
         else:
            fmt1="%d"
            fmt2="%.1f"
         (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(btime)
         (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(stime)
         (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(etime)
         (vyea,vmon,vday,vhou,vmin,vsec,vwda,vyda,vdst)=time.gmtime(vtime)
         if self.__model in obsmodels:
            if (sday==eday):
               text+="\n%s %s grid from %2.2d-%2.2dZ %4.4d/%2.2d/%2.2d\n"%(self.__model,
                          self.__parm,shou,ehou,syea,smon,sday)
            else:
               if (smon==emon):
                  text+="\n%s %s grid from %2.2d %2.2dZ through %2.2d %2.2dZ  %4.4d/%2.2d\n"%(self.__model,
                             self.__parm,sday,shou,eday,ehou,syea,smon)
               else:
                  text+="\n%s %s grid from %4.4d/%2.2d/%2.2d %2.2dZ through %4.4d/%2.2d/%2.2d %2.2dZ\n"%(self.__model,
                             self.__parm,syea,smon,sday,shou,eyea,emon,eday,ehou)
         else:
            text+="\n%s %s grid from %2.2dZ run %4.4d/%2.2d/%2.2d\n"%(self.__model,
                      self.__parm,bhou,byea,bmon,bday)
         text+="\n"
         #
         #  Show forecast hour and valid time
         #
         if self.__model not in obsmodels:
            fhr=int((stime-btime)/3600.0)
            text+="  %d-hr forecast\n"%fhr
            if (sday==eday):
               text+="    Valid: %2.2d-%2.2dZ %4.4d/%2.2d/%2.2d\n"%(shou,ehou,
                         syea,smon,sday)
            else:
               if (smon==emon):
                  text+="    Valid: %2.2d %2.2dZ through %2.2d %2.2dZ  %4.4d/%2.2d\n"%(sday,
                            shou,eday,ehou,syea,smon)
               else:
                  text+="    Valid: %4.4d/%2.2d/%2.2d %2.2dZ through %4.4d/%2.2d/%2.2d %2.2dZ\n"%(syea,
                            smon,sday,shou,eyea,emon,eday,ehou)
            text+="\n"
         #
         #  Show archive time
         #
         text+="  Archived at %2.2d:%2.2dZ %4.4d/%2.2d/%2.2d\n"%(vhou,vmin,vyea,vmon,vday)
         text+="\n"
         #
         #  Show forecasters
         #
         if self.__model=="Official":
            text+="  Forecasters:\n"
            for j in xrange(fnums.shape[0]):
               if fnums[j]>0:
                  text+="    %2.2d - %s \n"%(fnums[j],self.__VU.getFcstrName(fnums[j]))
            text+="\n"
         #
         #  Show stats
         #
         minvalStr=fmt1%minval
         maxvalStr=fmt1%maxval
         avgStr=fmt2%avg
         stdStr=fmt2%std
         text+="  Minimum: %s\n"%minvalStr
         text+="  Maximum: %s\n"%maxvalStr
         text+="  Average: %s\n"%avgStr
         text+="  Std Dev: %s\n"%stdStr
         text+="\n"
      #
      #
      #
      txt.insert(Tkinter.END,text)
      txt.config(state=Tkinter.DISABLED)
      return
#=======================================================================
#
#  The main BOIVerify Info dialog box
#
class VerifInfo(doneDialog):
   def __init__(self, VU, parent=None, name="nonModal Dialog",
                callbackMethod=None, modal=1):
      self.__parent=parent
      self.__VU=VU
      self.fontHeight=18 # in pixels (well, not quite, but close)
      self.boxWidth=7 # width of hour, in pixels
      self.hourWidth=self.boxWidth+3
      self.rowH=self.fontHeight+5 # 5 pixels to surround box and allow sep line
      self.scrollIncY=self.rowH/3
      self.scrollIncX=self.scrollIncY*2
      self.scrbuffer=10 # within this many pixels of edge - it auto-scrolls
      self.yoff=2 # to space down past border
      self.cfb=tkFont.Font(family="Arial",size=-self.fontHeight,weight=tkFont.BOLD)
      self.cf=tkFont.Font(family="Arial",size=-10)
      self.fcstrNames=self.__VU.getFcstrNames()
      self.fcstrIDs=self.__VU.getFcstrIDs()
      self.fcstrNums=self.__VU.getFcstrNums()
      self.usedFcstrs=[]
      self.fcbstates=[]
      dialog=doneDialog.__init__(self,parent=parent,name=name,
                                 callbackMethod=callbackMethod,
                                 modal=modal)
      #
      #  Now that dialog exists - set minimum size on dialog box - then expand
      #
      geom=dialog.geometry()
      (wh,rest)=geom.split("+",1)
      (wid,hgt)=wh.split("x",1)
      #
      #  Now make it a more reasonable width
      #
      iwid=int(wid)+300
      geom="%dx%s+%s"%(iwid,hgt,rest)
      dialog.geometry(geom)
      #
      #  Setup dialog for latest date
      #
      self.displayDate()
      return
   #------------------------------------------------------------------
   # newModel - called when a new model is chosen from the list of
   #            models.  Have to read in all the model basetimes and
   #            find the basetime closest to the currently displayed
   #            basetime
   #
   def newModel(self):
      #
      #  Get new model name and setup message while working
      #
      model=self.ml.getCurrentSelection()
      msgWindow=messageWindow("Searching %s Grids"%model,self)
      try:
         #
         #  Get new parms and put them in order
         #
         parmList=self.__VU.listModelParms(model)
         self.parmList=self.orderParms(parmList)
         self.drawParmNames()
         #
         #  Get time being shown for current model...will
         #  try to match this time for the new model
         #
         timeindex=self.tl.getCurrentIndex()
         oldbasetime=self.times[timeindex]
         #
         #  Get new basetimes for this new model - want to search through
         #  parm that has the least grids.  We'll guess MaxT - but if
         #  there is no MaxT for this model - use the first parm and
         #  get all the basetimes for that parm
         #
         self.times=[]
         if "MaxT" in self.parmList:
            self.times=self.__VU.getBases("MaxT",model)
         else:
            self.times=self.__VU.getBases(self.parmList[0],model)
         #
         #  Search through basetimes trying to find the index
         #  with the basetime closest to the basetime we had
         #  before
         #
         self.times.sort()
         self.times.reverse()
         self.timestrs=[]
         defentry=0
         defindex=0
         mindiff=abs(self.times[0]-oldbasetime)
         for i in xrange(len(self.times)):
            btime=self.times[i]
            (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(btime)
            self.timestrs.append("%2.2d/%2.2d/%4.4d %2.2dZ"%(gmon,gday,gyea,ghou))
            diff=abs(btime-oldbasetime)
            if diff<mindiff:
               mindiff=diff
               defindex=i
         #  
         #  Now set the basetime to the new defindex basetime
         #
         defentry=self.timestrs[defindex] # ?? what if no entries in list?
         self.tl.newEntries(self.timestrs,defentry)
      except:
         pass
      #
      #  Close the message window...we're done working
      #
      msgWindow.destroy()
      #
      #  Now show the data for the new model for the default basetime
      #  just selected
      #
      self.displayDate()
      #
      return
   #------------------------------------------------------------------
   #
   #  For a set of inputParms, put them in the order specified by
   #  PARMINFO_ORDER.  If there are other parms - put them alphabetically
   #  after the parms in PARMINFO_ORDER.
   #
   def orderParms(self,inputParms):
      inputParms.sort()
      parmOrder=self.__VU.getCFG('PARMINFO_ORDER')
      outputParms=[]
      for parm in parmOrder:
         if parm in inputParms:
            outputParms.append(parm)
            inputParms.remove(parm)
      for parm in inputParms:
         outputParms.append(parm)
      return outputParms
   #------------------------------------------------------------------
   #  redisplayDate - re-draw timeblocks...but scroll back to where
   #                  it is currently displayed
   #
   def redisplayDate(self):
      #
      #  Get current canvas location and percentage within
      #  the scrolling areas
      #
      cx=self.cGrd.canvasx(0)
      cy=self.cGrd.canvasy(0)
      sr=self.cGrd.cget("scrollregion")
      (xmin,ymin,xmax,ymax)=sr.split()
      xp=(cx-int(xmin))/(int(xmax)-int(xmin))
      yp=(cy-int(ymin))/(int(ymax)-int(ymin))
      #
      #  Re-draw date
      #
      self.displayDate()
      #
      #  Scroll back to old location
      #
      self.cGrd.xview("moveto",xp)
      self.cTim.xview("moveto",xp)
      self.cGrd.yview("moveto",yp)
      self.cLab.yview("moveto",yp)
      return
   #------------------------------------------------------------------
   #  changeDate - possibly change to a new date - but if the new
   #               date is the same as that currently displayed - then
   #               just re-display the current date (this keeps it
   #               from scrolling off to a different location within
   #               the curent dates forecasts from being shown).
   #
   def changeDate(self):
      index=self.tl.getCurrentIndex()
      basetime=self.times[index]
      if basetime==self.baseDisplayed:
         self.redisplayDate()
      else:
         self.displayDate()
      return
   #------------------------------------------------------------------
   #  displayDate - Display timeblocks for the date currently selected
   #                in the "tl" timelist (self.times)
   #
   def displayDate(self):
      #
      #  Get the basetime and setup message while working
      #
      index=self.tl.getCurrentIndex()
      basetime=self.times[index]
      model=self.ml.getCurrentSelection()
      obsmodels=self.__VU.getCFG('OBSMODELS')
      if model in obsmodels:
         msgWindow=messageWindow("Reading %s Grids"%model,self)
      else:
         msgWindow=messageWindow("Reading %s Grids"%self.timestrs[index],self)
      try:
         self.usedFcstrs=[]
         model=self.ml.getCurrentSelection()
         #
         #  Stop any timing events for the current canvas
         #
         if self.screvent is not None:
            self.cGrd.after_cancel(self.screvent)
         self.screvent=None
         #
         #  Clear the canvas
         #
         self.cGrd.delete(Tkinter.ALL)
         self.cTim.delete(Tkinter.ALL)
         #
         #
         #
         minpix=0
         mintime=basetime
         maxpix=0
         maxtime=basetime
         fnums=self.__VU.getFcstrNums()
         fnums.sort()
         colorgroups=[]
         #
         #  Loop over parms to display
         #
         for i in xrange(len(self.parmList)):
            parm=self.parmList[i]
            #
            #  Loop over records for this basetime for
            #  the parm/model
            #
            if model not in obsmodels:
               recs=self.__VU.getFcstRecords(parm,model,basetime)
            else:
               ret=self.__VU.checkFile(parm,model)
               recs=list(self.__VU.oncRecs)
            for rec in recs:
               rec = int(rec)
               #
               #  Get times for this record and keep track of
               #  mintime/maxtime
               #
               if model not in obsmodels:
                  stime=self.__VU.fncStime[rec]
                  etime=self.__VU.fncEtime[rec]
               else:
                  stime=self.__VU.oncStime[rec]
                  etime=self.__VU.oncEtime[rec]
               #
               #  'deleted' records have a 'zero' start/end time
               #  we don't want to display those - we ignore them
               #
               if ((stime<5000)or(etime<5000)):
                  continue
               maxtime=max(maxtime,etime)
               mintime=min(mintime,stime)
               #
               #  For Official model..find color/pattern based
               #  on forecaster numbers
               #
               if model=="Official":
                  fcstrs=self.__VU.fncFcstr[rec,:]
                  flist=list(fcstrs)
                  flist.sort()
                  flist.reverse()
                  somebody=greater(fcstrs,0)
                  numFcstrs=add.reduce(somebody) # number of forecasters who touched grid
                  if numFcstrs==0: # no forecasters - use unknown 'white' color
                     colorfill="white"
                     stippletype=""
                  elif numFcstrs==1:
                     colorfill="white"
                     stippletype=""
                     fnum="%2.2d"%compress(somebody,fcstrs)
                     if fnum not in self.usedFcstrs:
                        self.usedFcstrs.append(fnum)
                     for j in xrange(len(fnums)):
                        if fnum==fnums[j]:
                           colorfill=COLORLIST[j%len(COLORLIST)]
                           break
                  else:
                     involved=compress(somebody,fcstrs)
                     for f in involved:
                        fadd="%2.2d"%f
                        if fadd not in self.usedFcstrs:
                           self.usedFcstrs.append(fadd)
                     stippletype="gray50"
                     match=0
                     if len(colorgroups)>0:
                        for colorgroup in colorgroups:
                           (fcstrlist,color)=colorgroup
                           match=1
                           for j in xrange(len(fcstrlist)):
                              if fcstrlist[j]!=flist[j]:
                                 match=0
                                 break
                           if match==1:
                              colorfill=color
                              break
                     if match==0:
                        colorfill=COLORLIST[len(colorgroups)%len(COLORLIST)]
                        newgroup=(flist,colorfill)
                        colorgroups.append(newgroup)
               else: # anything but Official...has white/solid timeblocks
                  colorfill="white"
                  stippletype=""
               #
               #  Setup tags with rec:(record number),
               #                 parm:(parm name),
               #                  col:(original color)
               #
               tagtuple=("grid","rec:%d"%rec,"parm:%s"%parm,"col:%s"%colorfill)
               #
               #  find coordinates of box based on time and row (i)
               #
               shr=(stime-basetime)/3600
               ehr=(etime-basetime)/3600
               x1=shr*(self.hourWidth)+2
               x2=ehr*(self.hourWidth)-2
               y1=(i*self.rowH)+self.yoff+2
               y2=y1+self.fontHeight-1
               #
               #  Make the timeblock box
               #
               self.cGrd.create_polygon(x1,y1,x1,y2,x2,y2,x2,y1,fill=colorfill,outline=colorfill,
                                        stipple=stippletype,width=1,tags=tagtuple)
               #
               #  Keep track of max/min times displayed
               #
               maxpix=max(maxpix,ehr*(self.hourWidth))
               minpix=min(minpix,shr*(self.hourWidth))
         #
         #  Setup bindings for popups on the grid boxes
         #
         self.cGrd.tag_bind("grid","<Button-3>",self.postPopGrid)
         if model=="Official":
            self.cGrd.tag_bind("grid","<Button-2>",self.extract)
            self.cGrd.bind("<Button-1>",self.buttonstart)
            self.cGrd.bind("<B1-Motion>",self.drag)
            self.cGrd.bind("<ButtonRelease-1>",self.buttonstop)
         else:
            self.cGrd.tag_unbind("grid","<Button-2>")
            self.cGrd.unbind("<Button-1>")
            self.cGrd.unbind("<B1-Motion>")
            self.cGrd.unbind("<ButtonRelease-1>")
         #
         #  Setup scrolling regions for grid canvas and timelabel canvas
         #
         self.cGrd.configure(scrollregion=(minpix,0,maxpix,self.parmHeight))
         self.cTim.configure(scrollregion=(minpix,0,maxpix,50))
         #
         #  Horizontally move to the start of this basetime
         #  (for Official add 12 hours)
         #
         offset=0
         if model=="Official":
            offset=12*self.hourWidth
         x0=float(offset-minpix)/float(maxpix-minpix)
         self.cGrd.xview("moveto",x0)
         self.cTim.xview("moveto",x0)
         #
         #  Make time marks from mintime to maxtime
         #     tick marks at hourly intervals
         #     hash marks through grid canvas at 6 hourly intervals
         #     label centerred above 12Z each day
         #
         for jtim in xrange(mintime,maxtime,3600):
            (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(jtim)
            fhr=(jtim-basetime)/3600
            x=fhr*self.hourWidth
            if ghou==0:
               ywid=30
               self.cGrd.create_line(x,0,x,50*self.rowH,fill="blue",stipple="gray50",tags="hash")
            elif (ghou%6==0):
               ywid=10
               self.cTim.create_text(x,30-ywid,anchor=Tkinter.S,fill="white",font=self.cf,text="%2.2d"%ghou)
               self.cGrd.create_line(x,0,x,50*self.rowH,fill="blue",stipple="gray50",tags="hash")
               if ghou==12:
                  self.cTim.create_text(x,30-ywid-10,anchor=Tkinter.S,
                                       fill="white",font=self.cf,
                                       text="%s %d (%s)"%(MONS[gmon],gday,DAYS[gwda]))
            else:
               ywid=5
            self.cTim.create_line(x,30,x,30-ywid,fill="white")
         self.cGrd.lower("hash")
         #
         #  Update the color boxes next to forecaster names...based on
         #  the forecasters active making the grids currently displayed
         #
         self.updateFcstrButtons()
         #
         #  Check to see if the 'set combo' button can be made active
         #
         self.checkSetButton()
         #
         #  set the baseDisplayed time...so future changes in date
         #  can know what is on the screen now...
         #
         self.baseDisplayed=basetime
      except:
         pass
      #
      #  Close the message window - we're done displaying this
      #  basetime
      #
      msgWindow.destroy()
      return
   #-----------------------------------------------------------------
   #  body - custom body that has GridManager-like qualities.  It
   #         displays gridblocks with forecaster numbers associated
   #         with each grid
   #
   def body(self, master):
      #
      #
      #
      self.screvent=None # grid canvas timing events
      self.baseDisplayed=0
      #
      self.modelstrs=self.__VU.listModels() # what if no models ?
      obsmodels=self.__VU.getCFG('OBSMODELS') # what if no obs models ?
      for model in obsmodels:
         if model not in self.modelstrs:
            self.modelstrs.append(model)
      self.modelstrs.sort()
      if "Official" in self.modelstrs:
         defmodel="Official"
      else:
         defmodel=self.modelstrs[0]
      #
      parmList=self.__VU.listModelParms(defmodel)
      self.parmList=self.orderParms(parmList)
      #
      #  Popup Menu for forecaster actions
      #
      self.popFcstr=Tkinter.Menu(master=master,tearoff=0)
      self.popFcstr.add_command(label="Edit",command=self.editFcstr)
      self.popFcstr.add_command(label="Delete",command=self.deleteFcstr)
      self.popFcstr.add_command(label="List Forecasts",command=self.listFcstr)
      #
      #  Popup Menu for grid button-3 actions
      #
      self.popGrid=Tkinter.Menu(master=master,tearoff=0)
      self.popGrid.add_command(label="Display Info",command=self.gridInfo)
      self.popGrid.add_separator()
      self.popGrid.add_command(label="Delete Grid",command=self.gridDelete)
      #
      #  Get base times of model
      #
      self.times=self.__VU.getBases(self.parmList[0],defmodel)
      self.times.sort()
      self.times.reverse()
      self.timestrs=[]
      for i in self.times:
         print "time:", i
         (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(i)
         self.timestrs.append("%2.2d/%2.2d/%4.4d %2.2dZ"%(gmon,gday,gyea,ghou))
      defentry=self.timestrs[0] # ?? what if no entries in list?
      #
      pwid=0
      for i in xrange(len(self.parmList)):
         parm=self.parmList[i]
         pwid=max(pwid,self.cfb.measure(parm))
      parmWidth=pwid+(4*2) # four of the 2-pixel spacers
      self.parmHeight=(len(self.parmList)*self.rowH)+(self.yoff*2)
      
      topframe=Tkinter.Frame(master)
      lab=Tkinter.Label(topframe,text="Model:")
      lab.pack(side=Tkinter.LEFT)
      self.ml=comboList(defmodel,self.modelstrs,parent=topframe,
                        callbackMethod=self.newModel)
      self.ml.cf.pack(side=Tkinter.LEFT,fill=Tkinter.NONE,expand=0)
      fr=Tkinter.Frame(topframe,width=10)
      fr.pack(side=Tkinter.LEFT)
      lab=Tkinter.Label(topframe,text="Date/Cycle:")
      lab.pack(side=Tkinter.LEFT,padx=10,anchor=Tkinter.W)
      self.prevBut=Tkinter.Button(topframe,text="<<",pady=0,padx=0,command=self.prevCycle)
      self.prevBut.pack(side=Tkinter.LEFT)
      self.tl=comboList(defentry,self.timestrs,parent=topframe,
                   callbackMethod=self.changeDate)
      self.tl.cf.pack(side=Tkinter.LEFT,fill=Tkinter.NONE,expand=0)
      self.nextBut=Tkinter.Button(topframe,text=">>",pady=0,padx=0,command=self.nextCycle)
      self.nextBut.pack(side=Tkinter.LEFT)
      topframe.pack(side=Tkinter.TOP,anchor="w")

      botframe=Tkinter.Frame(master)
      self.sHor=Tkinter.Scrollbar(botframe,orient=Tkinter.HORIZONTAL)
      self.sVer=Tkinter.Scrollbar(botframe,orient=Tkinter.VERTICAL)
      self.cLab=Tkinter.Canvas(botframe,relief=Tkinter.SUNKEN,width=parmWidth,
                               height=50,scrollregion=(0,0,parmWidth,self.parmHeight),
                               bg="black",
                          bd=2,yscrollcommand=self.sVer.set,
                          yscrollincrement=self.scrollIncY)
      self.cTim=Tkinter.Canvas(botframe,relief=Tkinter.SUNKEN,width=200,
                          height=30,bd=2,xscrollcommand=self.sHor.set,
                               bg="black",
                               scrollregion=(0,0,400,30),
                          xscrollincrement=self.scrollIncX)
      self.cGrd=Tkinter.Canvas(botframe,relief=Tkinter.SUNKEN,width=200,
                          height=50,bd=2,xscrollcommand=self.sHor.set,
                               bg="black",
                               scrollregion=(0,0,400,self.parmHeight),
                          yscrollcommand=self.sVer.set,
                          xscrollincrement=self.scrollIncX,yscrollincrement=self.scrollIncY)
      self.cGrd.bind("<Button-1>",self.buttonstart)
      self.cGrd.bind("<B1-Motion>",self.drag)
      self.cGrd.bind("<ButtonRelease-1>",self.buttonstop)
      self.fFcs=Tkinter.Frame(botframe,width=100,height=50,relief=Tkinter.SUNKEN,
                              bd=2)
      self.updateFcstrButtons()
      #
      #
      #
      self.sHor.config(command=self.scrollBothX)
      self.sVer.config(command=self.scrollBothY)

      self.sHor.grid(row=0,column=1,sticky=Tkinter.W+Tkinter.E)
      self.cTim.grid(row=1,column=1,sticky=Tkinter.W+Tkinter.E)
      self.cLab.grid(row=2,column=0,sticky=Tkinter.N+Tkinter.S)
      self.cGrd.grid(row=2,column=1,sticky=Tkinter.W+Tkinter.E+Tkinter.N+Tkinter.S)
      self.sVer.grid(row=2,column=2,sticky=Tkinter.N+Tkinter.S)
      self.fFcs.grid(row=2,column=3,sticky=Tkinter.N+Tkinter.S)

      but=Tkinter.Button(master=botframe,text="Add New Forecaster",
                         command=self.addFcstr,)
      but.grid(row=1,column=3)
      self.scb=Tkinter.Button(master=botframe,text="Set Forecasters for Selected Grids",
                              command=self.setCombo,)
      self.scb.grid(row=3,column=3,sticky=Tkinter.N+Tkinter.S)

      botframe.grid_rowconfigure(0,weight=0)
      botframe.grid_rowconfigure(1,weight=0)
      botframe.grid_rowconfigure(2,weight=1,minsize=50)
      botframe.grid_rowconfigure(3,weight=0)
      botframe.grid_columnconfigure(0,weight=0,minsize=50)
      botframe.grid_columnconfigure(1,weight=1,minsize=50)
      botframe.grid_columnconfigure(2,weight=0)
      botframe.grid_columnconfigure(3,weight=0,minsize=100)
      
      botframe.pack(side=Tkinter.TOP,expand=1,fill=Tkinter.BOTH)
      self.dlgtop=botframe.winfo_toplevel()
      self.drawParmNames()
      return
   #------------------------------------------------------------------
   #  setCombo - set the forecaster number info on the currently
   #             selected grids with the forecasters currently "ON"
   #             in the checkboxes
   def setCombo(self):
      #
      #  scan thorugh fcbstates to get forecaster numbers of
      #  those that are are "ON".  fnums is a list of numbers
      #
      fnums=[]
      for i in xrange(len(self.fcstrNums)):
         if self.fcbstates[i].get()>0:
            fnums.append(self.fcstrNums[i])
      #
      #  If too many forecasters in the combination...give them
      #  an error message
      #
      maxForecasters=self.__VU.getCFG("MAXFCSTRS")
      if len(fnums)>maxForecasters:
         tkMessageBox.showerror("Error","No more than %d forecasters on any grid"%maxForecasters,
                        parent=self)
         return
      #
      #  Loop through selected grids
      #
      selItems=self.cGrd.find_withtag("selected")
      if selItems is not None:
         for item in selItems:
            grid=0
            record=-1
            parm=""
            tags=self.cGrd.gettags(item)
            for tag in tags:
               if tag[0:4]=="grid":
                  grid=1
               elif tag[0:4]=="rec:":
                  record=int(tag[4:])
               elif tag[0:5]=="parm:":
                  parm=tag[5:]
            if grid==1:
               gridModel=self.ml.getCurrentSelection()
               if self.__VU.checkFile(parm,gridModel,modify=1):
                  fcstrs=self.__VU.fncFcstr[record,:]
                  #print "  %s %s %d fcstrs=%s"%(gridModel,parm,record,str(fcstrs))
                  for i in xrange(self.__VU.getCFG('MAXFCSTRS')):
                     self.__VU.fncFcstr[record,i]=0
                  for i in xrange(len(fnums)):
                     self.__VU.fncFcstr[record,i]=int(fnums[i])
                  fcstrs=self.__VU.fncFcstr[record,:]
                  #print "   changed to %s"%str(fcstrs)
         self.__VU.closeFcstFile()
         self.redisplayDate()
      return
   #------------------------------------------------------------------
   #  updateFcstrButtons - update the display of forecaster buttons
   #                       with new list of forecasters.
   #
   def updateFcstrButtons(self):
      #
      #  get the current on/off states for each number
      #
      state={}
      if len(self.fcbstates)>0:
         for i in xrange(len(self.fcstrNums)):
            num=self.fcstrNums[i]
            state[num]=self.fcbstates[i].get()
      stateKeys=state.keys()
      #
      #  Delete any widgets currently in the frame
      #  the one that caused the callback will not
      #  be deleted (this is a memory leak!)
      #
      widgets=self.fFcs.pack_slaves()
      if widgets is not None:
         for widget in widgets:
            widget.pack_forget()
            del widget
      #
      #  get the updated names/nums/IDs
      #
      self.fcstrNames=self.__VU.getFcstrNames()
      self.fcstrIDs=self.__VU.getFcstrIDs()
      self.fcstrNums=self.__VU.getFcstrNums()
      #
      #
      maxwid=0
      for num in self.fcstrNums:
         maxwid=max(maxwid,len(self.fcstrNames[num]))
      #
      self.fcbstates=[]
      for i in xrange(len(self.fcstrNums)):
         num=self.fcstrNums[i]
         label="%s - %s"%(num,self.fcstrNames[num])
         rowframe=Tkinter.Frame(master=self.fFcs,name="f%s"%num)
         var=Tkinter.IntVar()
         if num in stateKeys:
            var.set(state[num])
         else:
            var.set(0)
         self.fcbstates.append(var)
         if i==0:
            color="white"
         else:
            color=COLORLIST[i%len(COLORLIST)]
         cb=Tkinter.Checkbutton(master=rowframe,text=label,indicatoron=1,
                                   variable=var,padx=0,pady=0,name="c%s"%num,
                                command=self.checkSetButton)
         #print "checking",num,"against usedFcstrs:",self.usedFcstrs
         if ((i==0)or(num in self.usedFcstrs)):
            mb=Tkinter.Button(master=rowframe,relief=Tkinter.FLAT,
                              command=cb.toggle,width=5,
                              text=" ",padx=0,pady=0,borderwidth=0,
                              background=color,foreground='white',name="b%s"%num,
                              activebackground=color,activeforeground='white')
         else:
            bgcol=rowframe.cget("bg")
            mb=Tkinter.Button(master=rowframe,
                              relief=Tkinter.FLAT,command=cb.toggle,width=5,
                              text=" ",padx=0,pady=0,borderwidth=0,
                              background=bgcol,foreground='white',name="b%s"%num,
                              activebackground=bgcol,activeforeground='white')
         mb.pack(side=Tkinter.LEFT)
         if i!=0:
            mb.bind("<Button-3>",self.postPopFcstr)
         
         cb.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
         rowframe.pack(side=Tkinter.TOP,fill=Tkinter.X,expand=1)
         if i!=0:
            rowframe.bind("<Button-3>",self.postPopFcstr)
            cb.bind("<Button-3>",self.postPopFcstr)
      
      #
      #
      #
      df=Tkinter.Frame(master=self.fFcs).pack(side=Tkinter.TOP,fill=Tkinter.BOTH,expand=1)
      #
      #  give the dialog a chance to update size on its own
      #
      tl=self.fFcs.winfo_toplevel()
      tl.update_idletasks()
      #
      #  Set new minimum size -based on requested width/height
      #
      rwid=tl.winfo_reqwidth()
      rhgt=tl.winfo_reqheight()
      tl.minsize(rwid,rhgt)
      #
      #  If height of current grid is not as big as the minimum
      #  size, then make that change manually (if they have
      #  modified the size earlier - the automatic propagate wont
      #  make it bigger)
      #
      geom=tl.geometry()
      (wh,rest)=geom.split("+",1)
      (wid,hgt)=wh.split("x",1)
      if int(hgt)<int(rhgt):
         tl.geometry("%sx%d"%(wid,int(rhgt)))
      return
   #------------------------------------------------------------------
   #  nextCycle - move to the next basetime in the "tl" self.times
   #              list of basetimes
   #
   def nextCycle(self):
      timeindex=self.tl.getCurrentIndex()
      if timeindex>0:
         self.tl.setCurrentIndex(timeindex-1)
         self.changeDate()
      return
   #------------------------------------------------------------------
   #  prevCycle - move to the previous basetime in the "tl" self.times
   #              list of basetimes
   #
   def prevCycle(self):
      timeindex=self.tl.getCurrentIndex()
      if timeindex<len(self.times)-1:
         self.tl.setCurrentIndex(timeindex+1)
         self.changeDate()
      return
   #------------------------------------------------------------------
   #  postPopGrid - post the popup that allows them to get info on
   #                a grid, or delete a grid
   #
   def postPopGrid(self,event):
      curgrid=self.cGrd.find_withtag(Tkinter.CURRENT)
      grtags=self.cGrd.gettags(curgrid)
      for tag in grtags:
         if tag.find("rec")>-1:
            self.gridRecord=int(tag[4:])
         if tag.find("parm")>-1:
            self.gridParm=tag[5:]
      self.gridModel=self.ml.getCurrentSelection()
      self.popGrid.post(event.x_root,event.y_root)
      self.popGrid.grab_set()
   #------------------------------------------------------------------
   #  gridInfo - post the dialog with info about the particular grid
   #             called from the popGrid popup menu
   #
   def gridInfo(self):
      InfoDialog(self.__VU,self.gridModel,self.gridParm,self.gridRecord,parent=self)
      return
   #==================================================================
   #  gridDelete - delete the specified grid (but give them a chance
   #               to back out of it first)
   #               called from the popGrid popup menu
   #
   def gridDelete(self):
      #
      #  Give them a chance to back out of deleting an archived grid.
      #
      obsmodels=self.__VU.getCFG('OBSMODELS')
      model=self.gridModel
      parm=self.gridParm
      record=self.gridRecord
      text=""
      #
      #  Make sure we can open this file
      #
      if not self.__VU.checkFile(parm,model,modify=0):
         text+="Cant delete this grid"
         tkMessageBox.showerror("Error",text,parent=self)
         return
      #
      #  Make different warning message depending on whether
      #  it is a forecast grid or an observed grid
      #
      if model in obsmodels:
         stime=self.__VU.oncStime[record]
         etime=self.__VU.oncEtime[record]
         (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(stime)
         (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(etime)
         text+="Are you sure you want to delete the %s %s grid "%(model,parm)
         text+="from %2.2dZ %4.4d/%2.2d/%2.2d through %2.2dZ %4.4d/%2.2d/%2.2d\n\n"%(shou,
                    syea,smon,sday,ehou,eyea,emon,eday)
      else:
         btime=self.__VU.fncBtime[self.gridRecord]
         (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(btime)
         stime=self.__VU.fncStime[self.gridRecord]
         fhr=int((stime-btime)/3600.0)
         text+="Are you sure you want to delete the %d-hr %s forecast "%(fhr,self.gridParm)
         text+="from the %2.2dZ %4.4d/%2.2d/%2.2d run from %s?\n\n"%(bhou,
                     byea,bmon,bday,self.gridModel)
      #
      #  But in all cases...give DIRE warning in messsage
      #  so that they think this through
      #
      text+="Once deleted it cannot be retreived!\n\n"
      text+="CAREFULLY CONSIDER WHAT YOU ARE DOING!"
      #
      #  Make sure that they want to continue
      #
      ynDiag=tkMessageBox.askyesno("Are you sure?",text,
                         parent=self,default=tkMessageBox.NO)
      if not ynDiag:
         return
      #
      #  Delete the grid...and if there is an error doing that
      #  tell them 
      #
      reclist=[record,]
      if not self.__VU.deleteRecord(parm,model,reclist):
         tkMessageBox.showerror("Error","Could not delete grid",parent=self)
         return
      #
      #  Finally...redisplay the grids for the current date
      #
      self.redisplayDate()
      return
   #------------------------------------------------------------------
   # postPopFcstr - post the popup menu that allows them to edit info
   #                about a forecaster, list forecasts made by a
   #                forecaster, or delete a forecaster
   #
   def postPopFcstr(self,event):
      self.editFnum=str(event.widget)[-2:]
      self.editFID=self.fcstrIDs[self.editFnum]
      self.editFname=self.fcstrNames[self.editFnum]
      self.popFcstr.post(event.x_root,event.y_root)
      self.popFcstr.grab_set()
   #-----------------------------------------------------------------
   # editFcstr - post the dialog where the forecaster number/id/name
   #             can be changed.
   #             This is called from the popFcstr popup menu
   #
   def editFcstr(self):
      self.numVar=Tkinter.StringVar()
      self.numVar.set(self.editFnum)
      self.idVar=Tkinter.StringVar()
      self.idVar.set(self.editFID)
      self.nameVar=Tkinter.StringVar()
      self.nameVar.set(self.editFname)      
      ChangeCancelDialog(self.__VU, self.numVar,self.idVar,self.nameVar,
                         parent=self)
      self.updateFcstrButtons()
      return
   #------------------------------------------------------------------
   #  deleteFcstr - delete a forecaster from the list of forecasters
   #                (give them a chance to back out of it first).
   #                Any grids currently attributed to this number
   #                will be changed into the 'unknown' forecaster
   #               
   def deleteFcstr(self):
      #
      #  Don't let them delete the 'unknown' forecaster
      #
      num=int(self.editFnum)
      name=self.editFname
      if num==0:
         tkMessageBox.showerror("Error","You cannot delete the Unknown user",
                        parent=self)
         return
      #
      #  see how many grids this number is attributed to
      #
      msgWindow=messageWindow("Checking on forecaster %2.2d"%num,self)
      try:
         self.totalgrids=0
         for parm in self.__VU.getVerParms():
            datatype=self.__VU.getVerParmType(parm)
            if not self.__VU.checkFile(parm,"Official",modify=0,datatype=datatype):
               continue
            fnc=self.__VU.fncFcstr[:,:]
            numrec=add.reduce(add.reduce(equal(fnc,num)))
            self.totalgrids+=numrec
            self.__VU.closeFcstFile()
      except:
         pass
      msgWindow.destroy()
      #
      #  Give them a chance to back out of it.
      #
      if self.totalgrids>0:
         text=     "There are %d archived grids made by "%self.totalgrids
         text+="forecaster number %d : %s.\n"%(num,name)
         text+="\n"
         text+="Are you sure you want to delete %s and "%name
         text+="associate all those grids with the Unknown "
         text+="forecaster?"
      else:
         text=     "Are you sure you want to delete forecaster "
         text+="number %d : %s ?"%(num,name)
      ynDiag=tkMessageBox.askyesno("Are you sure?",text,
                         parent=self,default=tkMessageBox.NO)
      if not ynDiag:
         return
      #
      #  setup a message window because this may take a while...
      #
      text="Deleting Forecaster #%d"%num
      msgWindow=messageWindow(text,self)
      try:
         if self.totalgrids>0:
            for parm in self.__VU.getVerParms():
               #print "deleting #%d from %s"%(num,parm)
               datatype=self.__VU.getVerParmType(parm)
               if not self.__VU.checkFile(parm,"Official",modify=1,datatype=datatype):
                  #print "Could not open %s file for Official"%parm
                  continue
               fnc=self.__VU.fncFcstr[:,:]
               involved=logical_or.reduce(equal(fnc,num),1)
               recsUsed=compress(involved,self.__VU.fncRecs)
               for i in xrange(recsUsed.shape[0]):
                  rec=recsUsed[i]
                  fcstrs=fnc[rec,:]
                  #print "  record %d has %s"%(rec,fcstrs)
                  numfcstrs=add.reduce(greater_equal(fcstrs,0))
                  if numfcstrs==1:
                     fcstrs[equal(fcstrs,num)] = 0
                  else:
                     fcstrs[equal(fcstrs,num)] = -127
                  #print "     changed to %s"%fcstrs
                  fnc[rec,:]=fcstrs
               self.__VU.fncFcstr[:,:]=fnc[:,:].astype(int8)
               self.__VU.closeFcstFile()
         numstr="%2.2d"%num
         Names=self.__VU.getFcstrNames()
         IDs=self.__VU.getFcstrIDs()
         del Names[numstr]
         del IDs[numstr]
         self.__VU.setFcstrs(Names,IDs)
         self.__VU.saveFcstrNums()
      except:
         tkMessageBox.showerror("Error","Could not delete forecaster #%d"%num,
                         parent=self)
      msgWindow.destroy()
      #
      #  re-draw list of forecaster buttons
      #
      self.updateFcstrButtons()
      return
   #------------------------------------------------------------------
   #  listFcstr - post the dialog where we display all the forecast
   #              made for this forecaster.
   #              Called by the PopFcstr popup menu
   #
   def listFcstr(self):
      ListDialog(self.__VU,self.editFnum,self.editFID,
                 self.editFname,parent=self)
      return
   #------------------------------------------------------------------
   #  addFcstr - post the dialog where we can add a forecaster.
   #             called by the popFcstr popup menu
   #
   def addFcstr(self):
      self.numVar=Tkinter.StringVar()
      self.numVar.set("")
      self.idVar=Tkinter.StringVar()
      self.idVar.set("")
      self.nameVar=Tkinter.StringVar()
      self.nameVar.set("")      
      AddCancelDialog(self.__VU, self.numVar,self.idVar,self.nameVar,
                         parent=self)
      self.updateFcstrButtons()
      return
   #------------------------------------------------------------------
   #  drawParmNames - clear the parm name list - and draw text with new
   #                  names
   #
   def drawParmNames(self):
      self.cLab.delete(Tkinter.ALL)
      #
      #  Fill in parameter names
      #
      for i in xrange(len(self.parmList)):
         parm=self.parmList[i]
         yrow=i*(self.rowH)+self.yoff
         self.cLab.create_text(5,yrow+3,anchor=Tkinter.NW,fill="white",
                               font=self.cfb,text=parm)
      return
   #------------------------------------------------------------------
   #  scrollBothX - horizontally scrolls timebar and grid canvas -
   #                unless all of the X-scrollregion is already visible
   #
   def scrollBothX(self,*args):
      sr=self.cGrd.cget('scrollregion').split()
      sw=int(sr[2])-int(sr[0])
      wd=self.cGrd.winfo_width()
      if wd>=sw: # abort scross/moves if all of xscrollregion already visible
         return None
      apply(self.cTim.xview,args)
      apply(self.cGrd.xview,args)
      return None
   #------------------------------------------------------------------
   #  scrollBothY - vertically scrolls parm lables and grid canvas -
   #                unless all of the Y-scrollregion is already visible
   #
   def scrollBothY(self,*args):
      sr=self.cGrd.cget('scrollregion').split()
      sh=int(sr[3])-int(sr[1])
      hg=self.cGrd.winfo_height()
      if hg>=sh:  # abort scrolls/moves if all of yscrollregion already visible
         return None
      apply(self.cLab.yview,args)
      apply(self.cGrd.yview,args)
      return None
   #------------------------------------------------------------------
   #  buttonstart - button 1 is pushed down.  Store current location in
   #                xx,yy and store the starting location in xcstart,ycstart
   #                setup to call 'scrtest' (to test for auto-scrolling)
   #                if button is still down in a few milliseconds
   #
   def buttonstart(self,event):
      self.xx=event.x
      self.yy=event.y
      self.xcstart=self.cGrd.canvasx(self.xx)
      self.ycstart=self.cGrd.canvasy(self.yy)
      self.screvent=self.cGrd.after(200,self.scrtest)
      #
      #  If any grid boxes are 'selected' now - turn them off and
      #  set their color back to their original color
      #
      selItems=self.cGrd.find_withtag("selected")
      if selItems is not None:
         for  item in selItems:
            tags=self.cGrd.gettags(item)
            for tag in tags:
               if tag[0:4]=="col:":
                  oldcolor=tag[4:]
            self.cGrd.itemconfigure(item,fill=oldcolor,outline=oldcolor,stipple="")
      self.cGrd.dtag(Tkinter.ALL,"selected")
      #
      return "break"
   #------------------------------------------------------------------
   #  drag - button is held down while moving. Get new location in xx,yy
   #         and convert to new canvas location in xcnow,ycnow.  Draw
   #         selection box from xcstart,ycstart to xcnow,ycnow.
   #
   def drag(self,event):
      self.xx=event.x
      self.yy=event.y
      self.xcnow=self.cGrd.canvasx(self.xx)
      self.ycnow=self.cGrd.canvasy(self.yy)
      self.cGrd.delete('areasel')
      self.cGrd.create_rectangle(self.xcstart,self.ycstart,
                                 self.xcnow,self.ycnow,
                                 outline="cyan",tags='areasel')
      #
      #  Get selected grids, and any item inside the selection box
      #
      selItems=self.cGrd.find_withtag("selected")
      inItems=self.cGrd.find_overlapping(self.xcstart,self.ycstart,
                                 self.xcnow,self.ycnow)
      #
      #  Check for grid items inside the selection box that are NOT
      #  currently in the selected list.  For these - set them to
      #  selected and set their color to the highlight color
      #
      if inItems is not None:
         for item in inItems:
            tags=self.cGrd.gettags(item)
            if "grid" in tags:
               if item not in selItems:
                  newtags=list(tags)
                  newtags.append("selected")
                  self.cGrd.itemconfigure(item,fill="yellow",outline="yellow",stipple="gray12")
                  self.cGrd.itemconfigure(item,tags=tuple(newtags))
      #
      #  Check currently selected items...and if no longer in the
      #  selection box, then turn their color back to their original color
      #
      if selItems is not None:
         for item in selItems:
            if item not in inItems:
               tags=self.cGrd.gettags(item)
               if "grid" in tags:
                  for tag in tags:
                     if tag[0:4]=="col:":
                        oldcolor=tag[4:]
                  self.cGrd.itemconfigure(item,fill=oldcolor,outline=oldcolor,stipple="")
               self.cGrd.dtag(item,"selected")
      #
      #  Finally check for status of 'set selected' button
      #
      self.checkSetButton()
      self.cGrd.update_idletasks()
      return "break"
   #------------------------------------------------------------------
   # scrtest - while button is down but not moving - check to see if
   #           pointer is in the auto-scrolling zone (within scrbuffer
   #           of edge of canvas) and scroll if so.  If we scroll -
   #           then update the selection box.
   #
   def scrtest(self):
      hg=self.cGrd.winfo_height()
      wd=self.cGrd.winfo_width()
      scrollflag=0
      if self.xx<self.scrbuffer:
         self.scrollBothX('scroll','-1','units')
         scrollflag=1
      if self.xx>(wd-self.scrbuffer):
         self.scrollBothX('scroll','1','units')
         scrollflag=1
      if self.yy<self.scrbuffer:
         self.scrollBothY('scroll','-1','units')
         scrollflag=1
      if self.yy>(hg-self.scrbuffer):
         self.scrollBothY('scroll','1','units')
         scrollflag=1
      #
      #  If we scrolled - update the area that is highlighted
      #
      if scrollflag==1:
         self.xcnow=self.cGrd.canvasx(self.xx)
         self.ycnow=self.cGrd.canvasy(self.yy)
         self.cGrd.delete('areasel')
         self.cGrd.create_rectangle(self.xcstart,self.ycstart,
                                  self.xcnow,self.ycnow,
                                  fill='',outline="cyan",tags='areasel')
         self.cGrd.update_idletasks()
      #
      #  Check again for scrolling in a few milliseconds
      #
      self.screvent=self.cGrd.after(50,self.scrtest)
   #------------------------------------------------------------------
   #  buttonstop - button 1 is released - save final position in xcnow,
   #               ycnow.  Remove the selection box.
   #
   def buttonstop(self,event):
      if self.screvent is not None:
         self.cGrd.after_cancel(self.screvent)
      self.screvent=None
      self.xx=event.x
      self.yy=event.y
      self.xcnow=self.cGrd.canvasx(self.xx)
      self.ycnow=self.cGrd.canvasy(self.yy)
      self.cGrd.delete('areasel')
      #
      #  Get selected grids, and any item inside the selection box
      #
      selItems=self.cGrd.find_withtag("selected")
      inItems=self.cGrd.find_overlapping(self.xcstart,self.ycstart,
                                 self.xcnow,self.ycnow)
      #
      #  Check for grid items inside the selection box that are NOT
      #  currently in the selected list.  For these - set them to
      #  selected and set their color to the highlight color
      #
      if inItems is not None:
         for item in inItems:
            tags=self.cGrd.gettags(item)
            if "grid" in tags:
               if item not in selItems:
                  newtags=list(tags)
                  newtags.append("selected")
                  self.cGrd.itemconfigure(item,fill="yellow",outline="yellow",stipple="gray12")
                  self.cGrd.itemconfigure(item,tags=tuple(newtags))
      #
      #  Check currently selected items...and if no longer in the
      #  selection box, then turn their color back to their original color
      #
      if selItems is not None:
         for item in selItems:
            if item not in inItems:
               tags=self.cGrd.gettags(item)
               if "grid" in tags:
                  for tag in tags:
                     if tag[0:4]=="col:":
                        oldcolor=tag[4:]
                  self.cGrd.itemconfigure(item,fill=oldcolor,outline=oldcolor,stipple="")
               self.cGrd.dtag(item,"selected")
      self.checkSetButton()
      self.cGrd.update_idletasks()
      return "break"
   #------------------------------------------------------------------
   #  checkSetButton - check to see if the "Set Forecasters for Selected
   #                   Grids" button can be enabled.  There have to be
   #                   some selected grids - AND - there have to be
   #                   some selected forecasters
   #
   def checkSetButton(self):
      someFcstrs=0
      for i in xrange(len(self.fcstrNums)):
         if self.fcbstates[i].get()>0:
            someFcstrs=1
            break
      #
      if someFcstrs==1:
         selItems=self.cGrd.find_withtag("selected")
         if selItems is not None:
            if len(selItems)>0:
               self.scb.configure(state=Tkinter.NORMAL)
               return
      #
      self.scb.configure(state=Tkinter.DISABLED)
      return
   #------------------------------------------------------------------
   #
   #  extract forecasters for this grid into the currently selected
   #  forecasters.
   #
   def extract(self,event):
      curgrid=self.cGrd.find_withtag(Tkinter.CURRENT)
      grtags=self.cGrd.gettags(curgrid)
      for tag in grtags:
         if tag.find("rec")>-1:
            self.gridRecord=int(tag[4:])
         if tag.find("parm")>-1:
            self.gridParm=tag[5:]
      self.gridModel=self.ml.getCurrentSelection()
      
      if self.__VU.checkFile(self.gridParm,self.gridModel,modify=0):
         fcstrs=self.__VU.fncFcstr[self.gridRecord,:]
         self.fcstrNums=self.__VU.getFcstrNums()
         for i in xrange(len(self.fcstrNums)):
            self.fcbstates[i].set(0)
         for i in xrange(fcstrs.shape[0]):
            fnum=fcstrs[i]
            if fnum>0:
               fnumstr="%2.2d"%fnum
               if fnumstr in self.fcstrNums:
                  idx=self.fcstrNums.index(fnumstr)
                  self.fcbstates[idx].set(1)
      self.checkSetButton()
#=====================================================================
#
#  Custom comboList widget
#
#  User sees currently selected entry from list of entries, and a
#  pulldown button.  When pulldown is activated the list is shown -
#  with scrollbars (if needed) and the user can click on the entry
#  desired.  The callbackMethod is called when the user chooses an
#  entry - and you can get the currentEntry with getCurrentEntry method
#  and currentIndex with getCurrentIndex method.
#
class comboList(Tkinter.Frame):
   def __init__(self,defaultEntry,entryList,parent=None,callbackMethod=None,
                width=0,height=5):
      if defaultEntry not in entryList:
         return
      Tkinter.Frame.__init__(self,parent)
      self.__callbackMethod=callbackMethod
      self.entries=[]
      for entry in entryList:
         self.entries.append(entry)
      self.currentIndex=self.entries.index(defaultEntry)
      self.currentSelection=defaultEntry
      
      if width==0:
         for entry in self.entries:
            width=max(len(entry),width)
         width+=1
      #
      #  Make the popup chooser
      #
      self.opop=Tkinter.Toplevel()
      self.opop.withdraw()
      of=Tkinter.Frame(self.opop)
      if len(self.entries)>height:
         os=Tkinter.Scrollbar(of,orient=Tkinter.VERTICAL)
         self.ol=Tkinter.Listbox(of,width=width,height=height,
                            yscrollcommand=os.set,
                            selectmode=Tkinter.SINGLE,
                            exportselection=0)
         os.config(command=self.ol.yview)
         os.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      else:
         self.ol=Tkinter.Listbox(of,width=width,height=height)
      for entry in self.entries:
         self.ol.insert(Tkinter.END,entry)
      self.ol.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      of.pack(side=Tkinter.TOP)
      self.ol.bind("<ButtonRelease-1>",self.removePopup)
      self.opop.transient(parent)
      self.opop.overrideredirect(1)
      self.opop.update_idletasks()
      popwidth=self.opop.winfo_reqwidth()
      if (len(self.entries)<=height):
         popwidth+=21
      popheight=self.opop.winfo_reqheight()
      hpl=popheight/height
      #
      #  Make the display of current entry and pulldown button
      #
      self.cf=Tkinter.Frame(parent,width=popwidth)
      self.cl=Tkinter.Listbox(self.cf,width=width,height=1,
                         selectmode=Tkinter.SINGLE,
                         exportselection=0)
      self.cl.insert(Tkinter.END,defaultEntry)
      self.cl.pack(side=Tkinter.LEFT)
      self.cl.update_idletasks()
      cw=self.cl.winfo_reqwidth()
      ch=self.cl.winfo_reqheight()
      canw=popwidth-cw-6
      canh=ch-6
      bw=2 # border width
      cc=Tkinter.Canvas(self.cf,width=canw,height=canh,
                        relief=Tkinter.RAISED,bd=bw)
      tsize=min(canw,canh)
      toffx=((canw-tsize)/2.0)+bw+bw
      toffy=((canh-tsize)/2.0)+bw+bw
      twid=tsize-bw-bw
      x0=toffx
      y0=toffy
      x1=toffx+twid
      y1=toffy
      x2=toffx+(twid/2)
      y2=toffy+twid
      cp=cc.create_polygon(x0,y0,x1,y1,x2,y2,fill="black")
      cc.pack(side=Tkinter.LEFT)
      #self.cf.pack(side=Tkinter.TOP)
      self.cl.bind("<Button-1>",self.postPopup)
      cc.bind("<Button-1>",self.postPopup)
      return
   def postPopup(self,event):
      curgeom=self.cf.winfo_geometry()
      (wh,rest)=curgeom.split("+",1)
      (w,h)=wh.split("x",1)
      iw=int(w)
      ih=int(h)
      x=self.cf.winfo_rootx()
      y=self.cf.winfo_rooty()
      newgeom="+%d+%d"%(x,y+ih)
      self.opop.geometry(newgeom)
      self.opop.deiconify()
      self.ol.select_clear(0,Tkinter.END)
      self.ol.select_set(self.currentIndex)
      self.ol.see(self.currentIndex)
      popgeom=self.opop.geometry()
      (wh,rest)=popgeom.split("+",1)
      (iw,ih)=wh.split("x",1)
      self.iw=int(iw)
      self.ih=int(ih)
      self.opop.grab_set_global() # once you get here - you MUST choose
      #self.opop.grab_set()
      self.opop.focus_set()
      self.opop.bind("<FocusOut>",self.closePopup)
      self.opop.bind("<Button-1>",self.popClick)
      return "break"
   #------------------------------------------------------------------
   #  popClick - Test if they are clicking in the list - if so - they
   #             might release inside the list - and that will get
   #             captured in removePopup.  If not - then close the
   #             popup without choosing
   #
   def popClick(self,event):
      x=event.x
      y=event.y
      if ((x>self.iw)or(x<1)):
         return self.closePopup(event)
      if ((y>self.ih)or(y<1)):
         return self.closePopup(event)
      return "break"
   #------------------------------------------------------------------
   #  removePopup - called when they choose one in the list
   #
   def removePopup(self,event):
      selectIndex=int(self.ol.nearest(event.y))
      self.currentIndex=selectIndex
      newEntry=self.entries[selectIndex]
      self.currentSelection=newEntry
      self.opop.grab_release()
      self.cl.delete(0)
      self.cl.insert(0,newEntry)
      self.cf.focus_set()
      self.opop.withdraw()
      self.__callbackMethod()
      return
   #------------------------------------------------------------------
   #  closePopup - called when they dont pick from the list - but
   #               need to close the popup
   #
   def closePopup(self,event):
      #self.cf.focus_set()
      self.opop.grab_release()
      self.opop.unbind("<Button-1>")
      self.opop.withdraw()
      return
   #------------------------------------------------------------------
   #  getCurrentSelection - tells you the currently selected text
   #
   def getCurrentSelection(self):
      return self.currentSelection
   #------------------------------------------------------------------
   #  getCurrentIndex - tells you the index of the currently selected
   #                    text
   #
   def getCurrentIndex(self):
      return self.currentIndex
   #------------------------------------------------------------------
   #  delIndex - delete the specified index from the entries
   #
   def delIndex(self,index):
      if ((index<0)or(index>=len(self.entries))):
         return
      curSel=self.currentSelection
      self.ol.delete(index)
      del self.entries[index]
      if curSel in self.entries:
         self.currentIndex=self.entries.index[curSel]
      else:
         self.currentIndex=min(self.currentIndex,len(self.entries)-1)
         self.currentSelection=self.entries[self.currentIndex]
      self.cl.delete(0,Tkinter.END)
      self.cl.insert(Tkinter.END,self.currentSelection)
      return
   #------------------------------------------------------------------
   #  setCurrentIndex - set the selected entry to the specified index
   #                    in entries
   #
   def setCurrentIndex(self,index):
      if ((index<0)or(index>=len(self.entries))):
         return
      self.currentIndex=index
      self.currentSelection=self.entries[index]
      self.cl.delete(0,Tkinter.END)
      self.cl.insert(Tkinter.END,self.currentSelection)
      return
   #------------------------------------------------------------------
   #  setCurrentSelection - set the selected entry to the specified
   #                        entry
   #
   def setCurrentSelection(self,selection):
      if selection not in self.entries:
         return
      index=self.entries.index[selection]
      self.currentIndex=index
      self.currentSelection=self.entries[index]
      self.cl.delete(0,Tkinter.END)
      self.cl.insert(Tkinter.END,self.currentSelection)
      return
   #------------------------------------------------------------------
   #  delValue - delete the specified entry
   #
   def delValue(self,value):
      if value in self.entries:
         indexdel=self.entries.index(value)
         self.delIndex(indexdel)
      return
   #------------------------------------------------------------------
   #  newEntries - replace all the entries with a new list of entries.
   #               If the currently selected entry is in the new
   #               list of entries - then select it in the new list
   #               otherwise select the first entry
   #
   def newEntries(self,newList,newDef):
      if len(newList)>0:
         self.ol.delete(0,Tkinter.END)
         self.entries=[]
         for entry in newList:
            self.entries.append(entry)
            self.ol.insert(Tkinter.END,entry)
         if newDef in newList:
            self.setCurrentIndex(newList.index(newDef))
         else:
            self.setCurrentIndex(0)
      return
#=====================================================================
#  Create a basic 'message window' indicating that something
#  is happening.  Must be careful to destroy this...because there
#  is no way for the user to destroy this if something goes wrong.
#
def messageWindow(message,parent=None):
   if parent is None:
      return
   pwid=parent.winfo_width()
   phgt=parent.winfo_height()
   px=parent.winfo_rootx()
   py=parent.winfo_rooty()
   msgWindow=Tkinter.Toplevel(master=parent)
   msgWindow.resizable(0,0)
   msgWindow.transient()
   msgWindow.overrideredirect(1)
   msgLab=Tkinter.Label(master=msgWindow,text=message,
                        relief=Tkinter.RIDGE,height=5,width=29,borderwidth=4)
   msgLab.pack(side=Tkinter.TOP)
   msgWindow.update_idletasks()
   wid=msgWindow.winfo_width()
   hgt=msgWindow.winfo_height()
   nx=int(px+(pwid/2.0)-(wid/2.0))
   ny=int(py+(phgt/2.0)-(hgt/2.0))
   msgWindow.geometry("%dx%d+%d+%d"%(wid,hgt,nx,ny))
   msgWindow.update_idletasks()
   return msgWindow
#=====================================================================      
#
#  stuff to support a callback with a pre-known variable
#        
def GenericCallback(callback, *firstArgs, **firstKWArgs):
    if firstKWArgs:
        return GC(callback, *firstArgs, **firstKWArgs)
    else:
        return GCNoKWArgs(callback, *firstArgs)
#=====================================================================
#
#  Classes for callbacks
#
class GC:
    def __init__(self,callback,*firstArgs, **firstKWArgs):
        self.__callback=callback
        self.__firstArgs=firstArgs
        self.__firstKWArgs=firstKWArgs
    def __call__(self, *lastArgs, **kwArgs):
        if kwArgs:
            netKWArgs=self.__firstKWArgs.copy()
            netKWArgs.update(self.__kwArgs)
        else:
            netKWArgs=self.__firstKWArgs
        return self.__callback (*(self.__firstArgs+lastArgs),**netKWArgs)
class GCNoKWArgs:
    def __init__(self, callback, *firstArgs):
        self.__callback=callback
        self.__firstArgs=firstArgs
    def __call__(self, *args, **kwArgs):
        return self.__callback (*(self.__firstArgs+args),**kwArgs)
