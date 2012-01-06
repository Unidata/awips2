      Subroutine SetQsmooth(npass,smthwgt)
      Implicit  None
      Integer*4 passes,npass
      Real*4    smoothness,smthwgt
      Common   /qsmthcmn/passes,smoothness
      Data      passes,smoothness/0,0.5/
      passes=npass
      smoothness=smthwgt
      Return
      End
