      subroutine tplcl(tk,td,pinit,tl,pl,ier)
      implicit none
c
c##################################################################
c Statement of purpose:
c ---------------------
c Calculate the temperature and pressure at the LCL given the 
c parcel's initial temperature, dew point and pressure.
c
c History:
c  E. Thaler 13 Mar 1997 - original version
c
c On input:
c
c  tk    = parcel temperature in Celsius
c  td    = parcel dew point temperature in Celsius
c  pinit = initial pressure level of the parcel
c 
c On output:
c
c  tl    = temperature (K) at the LCL 
c  pl    = pressure (mb) at the LCL
c  ier   = >0 if successful completion, <0 if failure
c
c##################################################################
c       
c Input arguments.
c      
      real tk,td,pinit
c
c Output arguments.
c      
      real tl,pl
      integer ier
c
c Local variables.
c      
      integer i
      real*8 p0,tkk,tdk,theta,tlold,fprime,dtl
c        
c Set up some parameters. 
c
      i   = 0
      ier = -1
      p0  = 1000.d0
      tl  = 0.
      pl  = 0.
c        
c Convert temperature/dewpoint to K and compute 
c initial potential temperature.
c
      tkk   = dble(tk) + 273.15d0
      tdk   = dble(td) + 273.15d0
      theta = tkk * ((p0/dble(pinit))**.2854d0)
c
c Use Newton's method to compute the temperature
c at the LCL using Eq. (14) in Bolton (MWR 1980)
c
      tlold = tdk
   10 fprime = (.001266d0/tlold) - (1.d0/(tlold**2))
      dtl = (1.d0/tlold) - (1.d0/tdk) + (.001266d0*dlog(tlold/tdk)) 
     &      - (.000514d0*dlog(tkk/tdk))
      dtl = tlold - (dtl/fprime)
c      
c If we have converged to a solution, compute the pressure at
c the LCL and return, otherwise iterate again.
c
      if (dabs(dtl-tlold).le.1.d-7) then
          pl  = p0 * (((dtl/theta)**(1.d0/.2854d0)))
          tl  = dtl
          ier = 1
          return
      else
          tlold = dtl
          i= i+1
          if (i.lt.20) goto 10
      endif
      return  
      end
