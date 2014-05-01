c  =====================================================================
c
      subroutine tzcnum(tzone,izonum)
c
c.......................................................................
c
c  Determine time zone number (time difference from Z time to local)
c  from time zone code
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                               Dec 1997
c......................................................................
c
      character*4 tzone,tzcode(18)
c
      dimension itznum(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/tzcnum.f,v $
     . $',                                                             '
     .$Id: tzcnum.f,v 1.2 2004/01/30 17:52:55 scv Exp $
     . $' /
C    ===================================================================
C
      data tzcode/'E   ','e   ','C   ','c   ','M   ','m   ',
     +            'P   ','p   ','A   ','a   ','H   ','h   ',
     +            'N   ','n   ','Z   ','z   ','    ','    '/
      data itznum/-5,-6,-7,-8,-9,-10,-11,0,0,0/
c
c
      call prbug ('gpprod',1,1,ibug)
c
c  time zone designator
      do 10 i=1,18
         if (tzcode(i).eq.tzone) go to 20
10       continue
c
20    jc = i/2 + mod(i,2)
      izonum = -1*itznum(jc)
c
      return
c
      end
