
      real function thetawa(temp,dwpt,pres,iw,ier)
      implicit none

c#######################################################################
c      
c Statement of purpose: Calculates the wet bulb potential temperature 
c via the adiabatic method.  
c
c Date: 13 June 1997
c
c Input:
c
c  temp - temperature (degrees Celsius)
c  dwpt - dew point temperature (degrees Celsius)
c  pres - pressure (mb)
c  iw   - > 0 for mixing ratio with respect to water
c         < 0 for mixing ratio with respect to ice
c
c Output:
c
c  thetawa  - adiabatic wet bulb potential temperature (degrees Celsius)
c  ier      - > 0 routine successful
c             < 0 routine failed
c
c######################################################################
c declare formal arguments
      real    temp, dwpt, pres
      integer iw, ier
c
      real tl, pl, zl
      integer n
      parameter (n=100)
      real soln(0:n)
      
c
c Calculate the temperature, pressure and height at the lifting 
c condensation level.
c      
      call tpzlcl(temp,dwpt,pres,iw,tl,pl,zl,ier)
      if (ier.lt.0) then 
          thetawa= -999.0
          return
      endif
      soln(0)= tl
      
c
c Calculate the adiabatic wet bulb potential temperature by bringing
c the parcel down pseudo-moist adiabatically from the LCL to 1000 mb.
c
      call pseudolift(n,pl,1000.0,soln)
      thetawa = soln(n)
      
      return
      end
