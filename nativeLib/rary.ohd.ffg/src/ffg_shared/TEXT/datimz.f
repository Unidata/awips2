c  =====================================================================
c  pgm:  datimz (lcptz,ictz,kmo,kda,kyr,khr,kmn,kzmo,kzda,kzyr,
c                kzhr,kzmn,ndawk,ldayl,mxday,julda)
c
c   in: lcptz  .... conversion direction
c                     = 0, Z time to local time
c                     > 0, local time to Z time
c   in: ictz   .... hours between local time and Z time
c   in: kmo    .... month
c   in: kda    .... day (1 to mon(imo) )
c   in: kyr    .... year with century
c   in: khr    .... hour (00 to 23)
c   in: kmn    .... minute (00 to 59)
c  out: kzmo   .... month (Z time)
c  out: kzda   .... day (Z time)
c  out: kzyr   .... year (Z time)
c  out: kzhr   .... hour (Z time)
c  out: kzmn   .... minute (Z time)
c  out: ndawk  .... day of week (1 = Sun, etc.)
c  out: ldayl  .... time switch
c                     = 0, standard time
c                     = 1, daylight time
c  out: mxday  .... days in year
c  out: julda  .... julian date (Jan 1 = 1)
c  =====================================================================
c
      subroutine datimz (lcptz,ictz,kmo,kda,kyr,khr,kmn,kzmo,kzda,kzyr,
     *                   kzhr,kzmn,ndawk,ldayl,mxday,julda)
c
c  Convert Z time to local time (lcptz=0) or local time to Z time (lcptz
c  not equal to 0)
c
c  Initially written by - Tim Sweeney, HRL - Apr 1992
c
      character*10 cdawk

      dimension idate(6)
      integer mon(12)
     *   /31,28,31,30,31,30,31,31,30,31,30,31/
C
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/datimz.f,v $
     . $',                                                             '
     .$Id: datimz.f,v 1.4 2005/07/07 19:47:20 xfan Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('datimz  ',1,1,ibug)
c
c  set Feb to 29 if leap year
      kx = ((kyr/4)*4)/kyr
      mon(2) = 28 + kx
      mxday = 365 + kx
c
      if (ibug.eq.1) write (iud,*) 'in datimz - lcptz=',lcptz
c
      if (lcptz.eq.0) then
         kzmo = kmo
         kzda = kda
         kzyr = kyr
         kzhr = khr
         kzmn = kmn
         call datimi (-ictz,kmo,kda,kyr,khr,kmn,imo,ida,iyr,ihr,imn,mon)
         else
            imo = kmo
            ida = kda
            iyr = kyr
            ihr = khr
            imn = kmn
          endif
      ihrmn = khr*100 + kmn
C
c  calculate Julian date
      julda = 0
         do 1015 i=1,imo
         julda = julda + mon(i)
 1015    continue
      julda = julda - mon(imo) + ida
c
c  get day of week
      call ddgjw (julda,iyr,ndawk,cdawk)
c
c check if daylight savings or standard time
c
cfan
cfan  HSD bug r26-38: The times being included in the header of FFG products 
cfan                  are 1 hour sooner than they should be.
cfan                  (For MBRFC, [cpzone=Z] they use Z time, so lday=0)
cfan
cfan      call umemov ('ANY ',clkzon,1)
      call umemov (cpzone,clkzon,1)             !cfan 07/2005 Hsd bug r26-38
cfan
      iudatl=0
      idate(3)=imo
      idate(4)=ida
      idate(5)=ihrmn
      idate(1)=iyr
      call hckdls (iudatl,idate,ldayl)
      if (ibug.eq.1) write (iud,*) 'in datimz - from hckdls - ',
     *   'ldayl=',ldayl 
c
      if (lcptz.eq.0) then
c     convert Z time to local - adjust for savings time
         call datimi (ldayl,imo,ida,iyr,ihr,imn,kmo,kda,kyr,khr,kmn,mon)
         else
c     convert local to Z time
            jhr = ictz - ldayl
            call datimi (jhr,imo,ida,iyr,ihr,imn,kzmo,kzda,kzyr,kzhr,
     *                   kzmn,mon)
         endif

c
      return
c
      end
