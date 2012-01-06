c  =====================================================================
c  pgm:  int2xt (inthr,inda,inhr,imo,ida,iyr,ihr,noutz,noutds,code)
c
c   in: inthr  .... internal hour
c  out: inda   .... Julian day (internal clock)
c  out: inhr   .... hour (internal clock)
c  out: imo    .... month
c  out: ida    .... day
c  out: iyr    .... year (4 digits)
c  out: ihr    .... hour
c   in: noutz  .... time zone number
c   in: noutds .... daylight savings time switch
c   in: code   .... time zone code
c  =====================================================================
c
      subroutine int2xt (inthr,inda,inhr,imo,ida,iyr,ihr,
     +                   noutz,noutds,code)
c
c.......................................................................
c
c  Convert internal time to external time.  Convert hour 24 to hour 0.
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL                                Feb 1999
c.......................................................................
c
      character*4 code
c
      integer ndays(12)/31,28,31,30,31,30,31,31,30,31,30,31/
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/int2xt.f,v $
     . $',                                                             '
     .$Id: int2xt.f,v 1.4 2004/01/30 17:49:45 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('int2xt',1,2,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iutw,*) 'in int2xt - inthr=',inthr,
     +   ' noutz=',noutz,' noutds=',noutds,' code=',code
c
      if (inthr.lt.0) inthr = 0
      inda = inthr/24 + 1
      inhr = mod(inthr,24)
      if (ibug.eq.1) write (iutw,*) 'in int2xt - inda=',inda,
     +   ' inhr=',inhr
      call mdyh1 (inda,inhr,jmo,jda,jyr,jhr,noutz,noutds,code)
      if (jmo.eq.2.and.((jyr/4)*4.eq.jyr)) ndays(2) = 29
      if (ibug.eq.1) write (iutw,*) 'in int2xt - jmo=',jmo,
     +   ' jda=',jda,' jyr=',jyr,
     +   ' ndays(2)=',ndays(2)
c
c  convert jhr = 24 to jhr = 0
      inchr = 0
      jmn = 0
      call datimi (inchr,jmo,jda,jyr,jhr,jmn,imo,ida,iyr,ihr,
     +             imn,ndays)
      if (ibug.eq.1) write (iutw,*) 'in int2xt - imo=',imo,
     +   ' ida=',ida,' iyr=',iyr,' ihr=',ihr,' imn=',imn
c
      return
c
      end
