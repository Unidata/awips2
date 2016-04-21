c  =====================================================================
c  pgm:  prbug (sname,itrace,idb,ibug)
c
c   in: sname  .... name of input routine
c   in: itrace .... trace flag
c   in: idb    .... debug level of calling routine
c  out: ibug   .... debug output switch
c  =====================================================================
c
      subroutine prbug (sname,itrace,idb,ibug)
c
c.......................................................................
c
c  routine prints trace output and sets debug indicator
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                    Mar 1997
c.......................................................................
c
      character*(*) sname
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/prbug.f,v $
     . $',                                                             '
     .$Id: prbug.f,v 1.3 2004/01/30 17:51:20 scv Exp $
     . $' /
C    ===================================================================
C
c
ccc      write (iutw,10) sname
      if (igtrac.ge.itrace) write (iud,10) sname
10    format (' ENTER ',a )
c
      ibug = 0
      if (igdbug.ge.idb) ibug = igdbug
c
      return
c
      end
