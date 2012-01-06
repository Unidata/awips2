      integer function cgp(tempip,dwptip,presip,thetawip,sfcpres,
     &                 toppres,iw,deltap)
      implicit none
c########################################################################
c
c Statement of purpose: Calculates the convective gust potential based 
c on Western Region Technical Attachment 76-??.
c
c Date: 05 February 1998 
c
c Input:
c
c  tempip(0:)   - temperature (degrees Kelvin) on the uniform pressure
c                 grid
c  dwptip(0:)   - dew point temperature (degrees Kelvin) on the 
c                 uniform pressure grid
c  presip(0:)   - pressure (mb) on the uniform pressure grid 
c  thetawip(0:) - wet bulb potential temperature (degrees Kelvin) on
c                 the uniform pressure grid
c  sfcpres      - presip(sfcpres) contains the surface pressure (mb)
c  toppres      - presip(toppres) contains the last pressure (mb) on
c                 the uniform pressure grid
c  iw           - >0 for mixing ratio with respect to water
c                 <0 for mixing ratio with respect to ice
c  deltap       - pressure increment (mb) of the uniform pressure 
c                 grid
c
c Output:
c
c  icgp         - convective gust potential (1,2,3,4)
c
c########################################################################

      real twmax, dd, delta, deltap, sfcpres, toppres
      real top300, bot300, top400, bot400, top500, bot500
      real top700, bot700, tl, pl, zl, usi, t4
      integer iw, itwmax, it4, it3, i, ier
      integer n
      parameter (n=400)
      real tempip(n), dwptip(n), presip(n), thetawip(n), soln(0:n) 

c
c Initialize some variables.
c

      cgp= -999
      itwmax= -999
      it4= -999
      it3= -999
      twmax= -999.0d0
      dd= -999.0d0
      delta= deltap/2.0d0

c
c The original algorithm lifted the 500 mb parcel and used the 700 mb dew 
c point depression.  Here we find the most unstable parcel "near" 500 mb
c and the driest parcel "near" 700 mb.  We also need the environmental
c temperature "near" 300 mb and 400 mb.  "Near" at 500 and 700 mb means a
c 60 mb interval (470-530 mb, 670-730 mb) or a deltap interval if the 
c interpolated data is too coarse.  "Near" at 300 and 400 mb means a 20 
c mb interval (290-310 mb, 390-410 mb) or a deltap interval if interpolated
c data is too coarse.  This section determines the intervals. 
c

      top300= min(300.0d0-delta,290.0d0)
      bot300= max(300.0d0+delta,310.0d0)
      top400= min(400.0d0-delta,390.0d0)
      bot400= max(400.0d0+delta,410.0d0)
      top500= min(500.0d0-delta,470.0d0)
      bot500= max(500.0d0+delta,530.0d0)
      top700= min(700.0d0-delta,670.0d0)
      bot700= max(700.0d0+delta,730.0d0)

c
c Now that the intervals to find things have been determined, find the most
c unstable parcel defined as that parcel with the highest wet bulb potential
c temperature.  Also, find the parcel with the largest dew point depression
c and the pressure levels corresponding to the near 300 and near 400 mb 
c temperatures.       
c      
      
      do 10 i= sfcpres,toppres
c          print *,presip(i),thetawip(i)
         if ((presip(i).ge.top500).and.(presip(i).le.bot500)) then
              if (thetawip(i).gt.twmax) then
                  twmax= thetawip(i)
                  itwmax= i
              endif
         endif 
         if ((presip(i).ge.top700).and.(presip(i).le.bot700)) then
              if ((tempip(i)-dwptip(i)).gt.dd) then
                   dd= tempip(i)-dwptip(i)
              endif
         endif
         if ((presip(i).ge.top400).and.(presip(i).le.bot400)) then
              it4= i
         endif
         if ((presip(i).ge.top300).and.(presip(i).le.bot300)) then
              it3= i
         endif
   10 continue
     
c
c Lift the most unstable parcel found above to its LCL.  Then lift it 
c pseudo-moist adiabatically up to "near" 400 mb then from there up to 
c "near" 300 mb.  Compare these lifted parcel temperatures to the 
c environmental temperatures at these levels to calculate the upper 
c stability index.
c
      
      call tpzlcl(tempip(itwmax),dwptip(itwmax),presip(itwmax),
     &            iw,tl,pl,zl,ier)
      soln(0)= tl
c       print *,'lifting',presip(itwmax),tempip(itwmax),dwptip(itwmax)
c       print *,'lcl',tl,pl
c       print *,'going into pseudolift',pl,presip(it4),soln(0)
      call pseudolift(100,pl,presip(it4),soln)
      t4= soln(100)
c       print *,'parcel temp at ',presip(it4),t4
      soln(0)= t4
      call pseudolift(100,presip(it4),presip(it3),soln)
c       print *,'parcel temp at ',presip(it3),soln(100)
c       print *,'4,3',tempip(it4),tempip(it3)
      usi= (tempip(it4)-t4)+(tempip(it3)-soln(100))     
c       print *,usi,dd
       
c
c Check for area 1 on the nomogram.
c
 
      if ((dd.le.10.0d0).and.(dd+(2.0d0*usi).le.20.0d0)) then
          cgp= 1
 
c
c Check for area 2 on the nomogram.
c
 
      elseif ((dd-(2.0d0*usi).le.15.0d0).and.(usi.ge.5.0d0).and.
     &        (dd+(2.0d0*usi).gt.20.0d0)) then
          cgp= 2
 
c
c Check for area 4 on the nomogram.
c
 
      elseif (((dd.ge.15.0d0).and.(usi.le.4.0d0)).or.((dd.gt.25.0d0)
     &    .and.(dd-(2.5d0*usi).ge.15.0d0).and.(usi.gt.4.0d0))) then
          cgp= 4
 
c
c Check for area 3 on the nomogram.
c
 
      else
          cgp= 3
      endif
      return
      end
