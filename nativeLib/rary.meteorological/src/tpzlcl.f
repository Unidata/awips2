
      subroutine tpzlcl(tk,tdk,pinit,iw,tl,pl,zl,ier)
      implicit none
      
c#########################################################################
c
c Statement of purpose: Calculates the temperature at the lifted
c condensation level using Eq. 14 in Bolton, D., 1980: The computation of 
c equivalent potential temperature. Monthly Weather Review, 108, 1046-1053.
c This equation is solved iteratively using Newton's method.  
c Also calculates the pressure at the LCL and the vertical distance to the
c LCL from the initial lifting level.
c
c Date: 5 June 1997
c
c Input:
c
c  tk    - parcel temperature in Kelvin
c  td    - parcel dew point temperature in Kelvin
c  pinit - initial pressure level of the parcel in millibars
c  iw    - >0 for mixing ratio with respect to water
c          <0 for mixing ratio with respect to ice
c 
c Output:
c
c  tl    - temperature (Kelvin) at the LCL 
c  pl    - pressure (mb) at the LCL
c  zl    - distance (m) between the initial level and the LCL
c  ier   - >0 if successful completion
c          <0 if failure
c
c########################################################################

      real tk, tdk, pinit, pl, tl, zl
      real p0, theta, tlold, fprime, dtl
      integer iw, ier, i
C
C Function declarations
C      

      real pottemp, dmixr
      
c
c Set up some parameters. 
c

      i   = 0
      ier = -1
      p0  = 1000.0
      tl  = 0.0
      pl  = 0.0

c        
c Convert temperature/dewpoint to Kelvin and compute 
c initial potential temperature.
c

      theta = pottemp(tk,tdk,pinit,iw)

c
c Use Newton's method to iteratively solve for the temperature
c at the LCL. 
c

      tlold = tdk
   10 fprime = (.001266/tlold) - (1.d0/(tlold**2))
      dtl = (1.0/tlold) - (1.0/tdk) + (.001266*log(tlold/tdk)) 
     &      - (.000514*log(tk/tdk))
      dtl = tlold - (dtl/fprime)

c      
c If we have converged to a solution, compute the pressure at and
c the lifting distance to the LCL and return, otherwise iterate again.
c

      if (abs(dtl-tlold).le.1.0d-8) then
          tlold = 1.0d0/(.2854*(1.0-(.28d-3*dmixr(tdk,pinit,iw))))
          pl = p0 / ((theta/dtl)**tlold) 
          tl = dtl
          zl = (tk-tl)*1005.7/9.80665
          ier = 1
          return
      else
          tlold = dtl
          i= i+1
          if (i.lt.20) goto 10
      endif
      return  
      end
