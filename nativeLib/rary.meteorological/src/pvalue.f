      subroutine pvalue(pres,p,np,param,value)
      implicit none
C###############################################################################
C Statement of purpose.
C ---------------------
C Compute the value of some parameter in a sounding at a given pressure level
C using log pressure interpolation.
C
C History.
C --------                    
C D. Baker    25 Mar 87     Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C pres        Real          Pressure level where parameter value desired.
C p           Real Array    Sounding pressures (mb).
C np          Integer       Number of pressure levels input.
C param       Real Array    Values of parameter at each pressure level.
C
C On output:
C ----------               
C value       Real          Value of parameter at desired pressure level.
C###############################################################################
C Input arguments.
      real p(1),param(1)
      real pres
      integer np
C Output argument.
      real value
C Local variables.
      integer i
      real p1,p2,p3
C Local constants.
      real flag
      parameter (flag=99999.)
C External functions.
      real interp1
C Make sure that the computation is possible.
      value=flag
      if (pres.lt.p(np) .or. pres.gt.p(1)) go to 9999
C Check and see if the lowest pressure is equal to the desired pressure.
      if (abs(p(1)-pres).lt.0.1) then
       value=param(1)
       go to 9999
      end if
C Determine value of parameter at level "pres" in the sounding.  
      do 100 i=2,np
       if (p(i).le.pres) then
        p1=alog(p(i-1))
        p2=alog(pres)
        p3=alog(p(i))
        value=interp1(param(i-1),param(i),p1,p2,p3)
        go to 9999
       end if
100   continue
C Return to calling program.
9999  return
      end
