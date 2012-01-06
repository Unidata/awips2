      subroutine deftrk(tcb,pcb,thdpar,eptpar)
      implicit none

C##############################################################################
C Statement of purpose.
C ---------------------
C This routine computes the values of the dry adiabat from the initial parcel
C pressure to the LCL and the moist adiabat above the LCL.  These two curves 
C define the path of a lifted parcel.
C
C History.
C --------
C D. Baker    12 May 85     Original version.
C D. Perry    11 Feb 97     Updated function calls.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C tcb         Real          Temperature at the LCL (K).
C pcb         Real          Pressure at the LCL (mb).
C
C On output:
C ----------
C thdpar      Real          Dry adiabat (potential temperature)
C                           intersecting the LCL (K).
C eptpar      Real          Moist adiabat (equivalent potential temperature)
C                           intersecting the LCL (K).
C##############################################################################

C**** Input arguments.

      real tcb,pcb

C**** Output arguments.

      real thdpar,eptpar

C**** Internal variable
      real etpar

C**** External function.

      real adiabatic_te

C**** Compute dry and moist adiabats 

      thdpar=tcb*((1000.0/pcb)**0.286)
      etpar=adiabatic_te(tcb,pcb)
      eptpar=etpar*((1000.0/pcb)**0.286)
C      eptpar=mytw(tcb,tcb,pcb) 

C**** Exit.

      return
      end
