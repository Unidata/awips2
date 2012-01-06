
      subroutine pseudolift(n,pstart,pfinish,soln)
      implicit none

c#######################################################################
c      
c Statement of purpose: Calculates the temperature along a pseudo-moist
c adiabat by solving, as an initial value problem, the differential 
c equation describing the pseudo-moist adiabat.  Uses a fourth order 
c Adams predictor-corrector scheme started with a Runge-Kutta fourth 
c order scheme. The methods are based on those in Burden, R.L. and J.D. 
c Faires, 1985: Numerical Analysis 3rd Edition, PWS Publishers, 676 pp. 
c
c Date: 14 October 1997
c
c Input:
c
c  n       - number of mesh points on the pressure axis between
c            start and finish
c  pstart  - initial pressure [mb] (start integration here)
c  pfinish - final pressure [mb] (end integration here)
c  soln    - array containing the initial value of the temperature 
c            (degrees K) in soln(0) at the starting pressure 
c
c Output:
c
c  soln    - array containing the temperatures (degrees K) at each grid
c            point along the pseudo-moist adiabat that runs through
c            starting pressure and initial temperature in soln(0)
c
c######################################################################

      real pstart, pfinish, start, finish, h
      real*8 dk1, dk2, dk3, dk4, tt, wt
      integer i, j, k, n
      real w(0:3),t(0:3),soln(0:n)

C
C Function declaration
C      
      real*8 pmalrrhs 

      w(0) = soln(0)
c
c Check to see that beginning and ending points of integration 
c interval differ from one another.  If not, set solution array
c to initial value and return.
c
      
      if (abs(pstart-pfinish).lt.1.0d-6) then
          do 25 k= 1,n
             soln(k)= soln(0)
   25     continue
          return
      endif
      
c
c Set up initial value for the pressure and compute the step size.
c

      t(0) = pstart*1.0d2
      start= t(0)
      finish= pfinish*1.0d2
      h= (finish-start)/dble(n)
      
c
c Use the 4th order Runge-Kutta one-step scheme to get the solution
c at the first three pressure increments beyond the initial pressure.
c

      do 50 i= 1,3
         dk1= h*pmalrrhs(real(t(i-1)),real(w(i-1)))
         dk2= h*pmalrrhs(real(t(i-1)+(h/2.0d0)),
     +                   real(w(i-1)+(dk1/2.0d0)))
         dk3= h*pmalrrhs(real(t(i-1)+(h/2.0d0)),
     +                   real(w(i-1)+(dk2/2.0d0)))
         dk4= h*pmalrrhs(real(t(i-1)+h),real(w(i-1)+dk3))
         w(i)= w(i-1)+real((dk1+dk2+dk2+dk3+dk3+dk4)/6.0)
         t(i)= start+(i*h)
         soln(i)= w(i)
   50 continue

c
c Use the Adams four step, 4th order predictor-corrector scheme to 
c find the rest of the solution.
c
 
   60 tt= start+(real(i)*h)
      if ((start.lt.finish).and.(tt.gt.finish)) return
      if ((start.gt.finish).and.(tt.lt.finish)) return
      if (start.eq.finish) return
      
c
c Prediction step.
c

      wt= w(3)+((h/24.d0)*((55.0d0*pmalrrhs(real(t(3)),real(w(3))))
     &                    -(59.0d0*pmalrrhs(real(t(2)),real(w(2))))
     +                    +(37.0d0*pmalrrhs(real(t(1)),real(w(1))))
     &                    -( 9.0d0*pmalrrhs(real(t(0)),real(w(0))))))

c
c Correction step.
c

      wt= w(3)+((h/24.0d0)*(( 9.0d0*pmalrrhs(real(tt),  real(wt)))
     &                     +(19.0d0*pmalrrhs(real(t(3)),real(w(3))))
     +                     -( 5.0d0*pmalrrhs(real(t(2)),real(w(2))))
     &                     +( 1.0d0*pmalrrhs(real(t(1)),real(w(1))))))
      soln(i)= wt

c
c Advance to the next value of the independent variable.
c   
      
      do 70 j= 0,2 
         t(j)= t(j+1)
         w(j)= w(j+1)
   70 continue
      t(3)= tt
      w(3)= wt
      i=i+1
      goto 60
      end

c----------------------------------------------------------------------

      real*8 function pmalrrhs(p,tk)
      implicit none

c######################################################################
c
c Statement of purpose: Calculates the right hand side of the ODE which
c defines the pseudo-moist adiabatic lapse rate.  This is Eq. A.1.24 in  
c Young, K.C., 1993: Microphysical Processes in Clouds. Oxford University
c Press, 427 pp.  The latent heat of vaporization relationship is Eq. 2
c in Bolton, D., 1980: The computation of equivalent potential temperature. 
c Monthly Weather Review, 108, 1046-1053.  Physical constants from 
c Sonntag, D. 1990: Important new values of the physical constants of 1986, 
c vapour pressure formulations based on the ITS-90, and psychrometer 
c formulae. Z. Meteorol., 40, 340-344.
c
c Input:
c
c  p - pressure in millibars
c  t - temperature in degrees Kelvin
c
c Output:
c
c  pmalrrhs - value of pseudo-moist adiabatic lapse rate in degrees
c             Kelvin per Pascal (C Pa^-1)
c  
c######################################################################

      real p, tk
      real*8 dmv, dmd, rv, rd, dlv, cp, es, ftop, fbot

C
C Function declaration
C      
      real vp
c
c Input physical constants.
c
c  dmv - molar mass of water vapour (kg mol^-1)
c  dmd - molar mass of dry air (kg mol^-1)
c  rv  - gas constant of water vapour (J kg^-1 K^-1)
c  rd  - gas constant of dry air (J kg^-1 K^-1)
c  dlv - latent heat of vaporization (J kg^-1)
c  cp  - specific heat of air at constant pressure (J kg^-1 K^-1)
c

      dmv = .01801528d0
      dmd = .0289645d0
      rv  = 461.525d0
      rd  = 287.0586d0
      dlv= (2.501d0-(.00237d0*tk))*1.0d6
      cp  = 1005.7d0

c      
c Convert temperature to Kelvin.  Calculate vapor pressure. 
c

      es = vp(tk,1)

c
c Calculate right hand side.
c

      ftop = (rd*tk/p)+(dmv*es*dlv/p/p/dmd)
      fbot = cp+(dlv*dlv*dmv*es/dmd/rv/p/tk/tk)
      pmalrrhs = ftop/fbot
      return
      end
