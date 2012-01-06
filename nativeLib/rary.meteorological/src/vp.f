
      real function vp(tk,iw)

c######################################################################
c
c Statement of purpose: Calculates the saturation vapor pressure in the 
c pure phase with respect to either water (Eq. 2) or ice (Eq. 4) using 
c formulae from Sonntag, D. 1990: Important new values of the physical 
c constants of 1986, vapour pressure formulations based on the ITS-90, 
c and psychrometer formulae. Z. Meteorol., 40, 340-344.
c
c Date: 5 June 1997
c
c Input:
c
c  t  - temperature in degrees Kelvin
c  iw - >0 for vapor pressure with respect to water
c       <0 for vapor pressure with respect to ice
c
c Output:
c
c  vp - vapor pressure in Pascals
c
c######################################################################

      implicit none
c declare formal arguments
      real tk
      integer iw

c
c Compute vapor pressure.
c

c
c with respect to ice:
c
      if (iw.lt.0) then
          vp= exp(((-6024.5282)/tk)+29.32707
     &            +(1.0613868d-2*tk)-(1.3198825d-5*(tk**2))
     &            -(.49382577*log(tk)))

c
c with respect to water:
c

      else
          vp= exp(((-6096.9385)/tk)+21.2409642-(2.711193d-2*tk)
     &            +(1.673952d-5*(tk**2))+(2.433502*log(tk)))
      endif
      return
      end

