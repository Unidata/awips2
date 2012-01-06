
      real function dmixr(temp,pres,iw)
      implicit none

c########################################################################
c
c Statement of purpose: Calculates the water vapor mixing ratio with
c respect to either water or ice.  Values of physical constants from 
c Sonntag, D. 1990: Important new values of the physical constants of 
c 1986, vapour pressure formulations based on the ITS-90, and psychrometer
c formulae. Z. Meteorol., 40, 340-344.
c
c Date: 5 June 1997
c
c Input:
c
c  temp  - temperature in degrees Kelvin
c  pres  - pressure in millibars
c  iw    - >0 for mixing ratio with respect to water
c          <0 for mixing ratio with respect to ice
c
c Output:
c
c  dmixr - mixing ratio in grams per kilogram
c
c########################################################################


      real temp, pres, rd, rv, e
      integer iw

C
C Function declaration
C      
      real vp
c
c Input physical constants. 
c
c rd = dry air gas constant
c rv = water vapor gas constant
c
 
      rd= 287.0586
      rv= 461.525

c
c Calculate actual vapor pressure and the mixing ratio. 
c

      e= vp(temp,iw)
      dmixr= (1.0d3*rd/rv*e)/((pres*100.0)-e)
      return
      end
