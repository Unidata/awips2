c  =====================================================================
c  pgm datimi (nhr,imo,ida,iyr,ihr,imn,jmo,jda,jyr,jhr,
c              jmn,mon)
c
c   in: nhr    .... number of hours to increment or decrement
c   in: imo    .... month
c   in: ida    .... day (1 to mon(imo) 0
c   in: iyr    .... year with century
c   in: ihr    .... hour (00 to 23)
c   in: imn    .... minute (00 to 59)
c  out: jmo    .... month
c  out: jda    .... day
c  out: jyr    .... year
c  out: jhr    .... hour
c  out: jmn    .... minute
c   in: mon    .... days in each month
c  =====================================================================
c
      subroutine datimi (nhr,imo,ida,iyr,ihr,imn,jmo,jda,jyr,jhr,
     +                   jmn,mon)
c
c.......................................................................
c  routine increments or decrements time
c
c
c.......................................................................
c  Initially written by
c           Tim Sweeney, HRL - Apr 1993
c.......................................................................
c
      dimension mon(12)
c
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/datimi.f,v $
     . $',                                                             '
     .$Id: datimi.f,v 1.2 2004/01/30 17:46:26 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('datimi',1,1,ibug)
c
      jmo = imo
      jda = ida
      jyr = iyr
      jhr = ihr
      jmn = imn
c
      if (nhr.lt.0) then
c     decrement hour
         jhr = jhr + nhr
         if (jhr.lt.0) then
c        decrement day
            jda = jda -1
            jhr = 24 + jhr
            if (jda.lt.1) then
c           decrement month
               jmo = jmo - 1
               if (jmo.lt.1) then
                  jmo = 12
                  jyr = jyr - 1
                  endif
               jda = mon(jmo)
               endif
            endif
         else
c        increment hour
            jhr = jhr + nhr
c        increment day
            jda = jda + jhr/24
            jhr = jhr - (jhr/24)*24
c        increment month
            mth = jmo + jda/(1+mon(jmo))
            jda = jda - (jda/(1+mon(jmo)))*mon(jmo)
            jmo = mth - (mth/13)*12
c        increment year
            jyr = jyr + (mth/13)
         endif
c
      if (ibug.gt.0) write (iud,10) nhr,imo,ida,iyr,ihr,
     +   jmo,jda,jyr,jhr
10    format (' nhr=',i2,' imo=',i2,' ida=',i2,' iyr=',i4,' ihr=',i2,
     +   'jmo=',i2,' jda=',i2,' jyr=',i4,' jhr=',i2)
c
      return
c
      end
