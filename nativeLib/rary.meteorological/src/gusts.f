      subroutine gusts(p,t,td,np,gstpot)
      implicit none
C###############################################################################
C Statement of purpose.
C ---------------------
C Determine the convective gust potential, after Western region technical
C attachment 76-14 (June, 1976).
C
C History.
C --------                    
C D. Baker    25 Mar 87     Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------   .             .
C
C On output:
C ----------  .             .
C
C###############################################################################
C Input arguments.
      integer np
      real p(np),t(np),td(np)
C Output argument.
      integer gstpot
C Local variables.
      real t700,td700,dd700,dd500,t500,td500,t400,t300,tp400,tp300,
     & tcb,pcb,plift,ui,thdpar,eptpar,etpar,plev1,plev2
      integer i,ier,flag,g,gstsave
C Local constants.
      parameter (flag=99999)
C External functions.
C      real tlcl,pcon
      real temp_of_te
C Make sure calculation is possible.
      gstpot=flag
      gstsave=0
      if (p(1).lt.700. .or. p(np).gt.300.) go to 9999
C Compute the 700 mb dew point depression.
      call pvalue(700.,p,np,t,t700)
      call pvalue(700.,p,np,td,td700)
      dd700=t700-td700
C Loop from 470 to 530 mb every 5 mb and choose the largest convective
C gust potential.  
      do 150 i=470,530,5
       plift=float(i)
       call pvalue(plift,p,np,t,t500)
       call pvalue(plift,p,np,td,td500)
       dd500=t500-td500
       plev1=plift-100.
       plev2=plift-200.
C Compute the LCL of the 500 mb parcel.
C       tcb=tlcl(t500,td500)
C       pcb=pcon(plift,t500,tcb)
       call tplcl(t500,td500,plift,tcb,pcb,ier)
C Compute the dry and moist adiabats through the LCL of the 500 mb parcel.
       call deftrk(tcb,pcb,thdpar,eptpar)
C Compute the parcel temperature at 400 mb.
       if (pcb.lt.plev1) then
        tp400=thdpar*((plev1/1000.0)**0.286) 
       else
        etpar=eptpar*((plev1/1000.0)**0.286)
        tp400=temp_of_te(etpar,plev1)
       end if
C Compute the environmental temperature at 400 mb.
       call pvalue(plev1,p,np,t,t400)
C Compute the parcel temperature at 300 mb.
       if (pcb.lt.plev2) then
        tp300=thdpar*((plev2/1000.0)**0.286)
       else
        etpar=eptpar*((plev2/1000.0)**0.286)
        tp300=temp_of_te(etpar,plev2)
       end if
C Compute the environmental temperature at 300 mb.
       call pvalue(plev2,p,np,t,t300)
C Compute the ui index.
       ui=(t400-tp400)+(t300-tp300)
C      Type *,'plift,dd700,tcb,pcb,tpar,epar,t400,tp400,t300,tp300,ui:'
C     *,plift,dd700,tcb,pcb,thdpar,eptpar,t400,tp400,t300,tp300,ui
C     Compute the gust potential.
       call cvgust(dd700,ui,g)
C Save the largest value.
       if (g.gt.gstsave) gstpot=g
       gstsave=g
150   continue
C Return to calling program.
      return
9999  end
