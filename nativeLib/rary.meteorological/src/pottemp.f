
      real function pottemp(temp,dwpt,pres,iw)

c########################################################################
c
c Statement of purpose: Calculates the potential temperature using Eq. 7
c in Bolton, D., 1980: The computation of equivalent potential temperature.
c Monthly Weather Review, 108, 1046-1053.  
c
c Date: 5 June 1997
c
c Input:
c
c  temp  - temperature in degrees Kelvin
c  dwpt  - dew point temperature in degrees Kelvin
c  pres  - pressure in millibars
c  iw    - >0 for mixing ratio with respect to water
c          <0 for mixing ratio with respect to ice
c
c Output:
c
c  tpot  - potential temperature in degrees Kelvin
c
c########################################################################

      real temp, dwpt, pres, dkappa
      integer iw

C
C Function declaration
C      
      real dmixr
c
c Compute the potential temperature.
c
        
      dkappa= .2854d0*(1.0d0-(.28d-3*dmixr(dwpt,pres,iw)))
      pottemp= temp*((1000.0d0/pres)**dkappa)
      return
      end





