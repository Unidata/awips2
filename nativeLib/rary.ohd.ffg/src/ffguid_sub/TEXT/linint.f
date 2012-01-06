c  =====================================================================
c  pgm:  linint (iconv,npro,pro,p,ro)
c
c   in: iconv  .... conversion type:
c                     0 = precipitation to runoff
c                     1 = runoff to precipitation
c   in: npro   .... number of values in array pro
c   in: pro    .... array containing rainfall runoff values
c  i/o: p      .... precipitation:
c                     input  when iconv = 0
c                     output when iconv = 1
c  o/i: ro     .... runoff:
c                     output when iconv = 0
c                     input  when iconv = 1
c  =====================================================================
c
      subroutine linint (iconv,npro,pro,p,ro)
c
c.......................................................................
c
c  linear interpolation and extrapolation
c
c.......................................................................
c  subroutine initially written by
c       Tim Sweeney, HRL - January 1992
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      dimension pro(npro)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/linint.f,v $
     . $',                                                             '
     .$Id: linint.f,v 1.3 2004/01/30 17:49:52 scv Exp $
     . $' /
C    ===================================================================
C
c
c  define functions
      slfunc(p1,p2,ro1,ro2)=(ro2-ro1)/(p2-p1)
      varf(var,var1,basv)=basv+slope*(var-var1)
c
      call prbug ('linint',1,5,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iud,*) 'in linit - iconv=',iconv,
     +   ' npro=',npro,' pro=',(pro(i),i=1,npro)
c
      iex = 0
c
      if (iconv.eq.1) go to 40
c
c  precip to runoff ............................................
c
c  determine bracket
      do 10 i=3,npro,2
         if (p.le.pro(i)) go to 20
10       continue
c
c  upper end extrapolation
      i = npro-1
      base = pro(npro)
      iex = 1
      go to 30
c
c  interpolation and low end extrapolation
20    base = pro(i-1)
c
c  create two point rating
30    p1  = pro(i-2)
      p2  = pro(i)
      ro1 = pro(i-1)
      ro2 = pro(i+1)
      slope = slfunc(p1,p2,ro1,ro2)
      if (iex.eq.0) then
         ro = varf(p,p1,base)
         else
            ro = varf(p,p2,base)
         endif
      go to 80
c
c  runoff to precip ........................................
c
c  determine bracket
40    do 50 i=4,npro,2
         if (ro.le.pro(i)) go to 60
50       continue
c
c  upper end extrapolation
      i = npro
      base = pro(npro-1)
      iex = 1
      go to 70
c
c  interpolation and low end extrapolation
60    base = pro(i-3)
c
c  create two point rating
70    p1  = pro(i-3)
      p2  = pro(i-1)
      ro1 = pro(i-2)
      ro2 = pro(i)
      slope = 1.0/slfunc(p1,p2,ro1,ro2)
      if (iex.eq.0) then
         p = varf(ro,ro1,base)
         else
            p = varf(ro,ro2,base)
         endif
c
80    if (ibug.eq.1) then
         write (iud,100) p1,ro1,p2,ro2,i,base,slope
100   format (' in linint - p1=',f8.2,' ro1=',f8.2,' p2=',f8.2,
     +   ' ro2=',f8.2,' i=',i2,' base=',f8.2,' slope=',f7.3)
         write (iud,90) p,ro
90    format (' in linint - p=',f7.3,' ro=',f7.3)
         endif
c
      return
c
      end
