C MODULE FPRCPU
C-----------------------------------------------------------------------
C
      SUBROUTINE FPRCPU
C
C  THIS ROUTINE CALLS URTIMR AND PRINTS A MESSAGE WITH ELAPSED AND 
C  TOTAL CPU TIMES SINCE THE LAST CALL TO URTIMR.
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcpuck'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fprcpu.f,v $
     . $',                                                             '
     .$Id: fprcpu.f,v 1.2 2001/06/13 12:14:40 mgm Exp $
     . $' /
C    ===================================================================
C
C
      CALL URTIMR (LAPSE,ICPUT)
C
      ELAPSE=LAPSE/100.
      XICPUT=ICPUT/100.
C
      WRITE (IODBUG,10) ELAPSE,XICPUT
10    FORMAT (' ELAPSED CPU TIME = ',F6.2,' SECONDS',3X,
     *   'TOTAL CPU TIME = ',F6.2,' SECONDS')
C
      RETURN
C
      END
