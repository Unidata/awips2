C MODULE FCKILL
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/16/95.10:31:37 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE KILLFN (FUNNAM)
C
C  THIS ROUTINE KILLS A FUNCTION.
C
      DIMENSION FUNNAM(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/killcd'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/killfn.f,v $
     . $',                                                             '
     .$Id: killfn.f,v 1.2 1996/01/16 23:06:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,20)
C
      DO 10 I=1,5
         WRITE (IPR,30)
         IF (IOERR.NE.IPR) WRITE (IOERR,30)
10       CONTINUE
C
      WRITE (IPR,40)
      IF (IOERR.NE.IPR) WRITE (IOERR,40)
C
      WRITE (IPR,50) FUNNAM
      IF (IOERR.NE.IPR) WRITE (IOERR,50) FUNNAM
C
      ISTOP=16
      NKILLS=NKILLS+1
C
C  PRINT REST OF SUMMARY MESSAGE
      IFN=1
      CALL FKSMSG (IFN,ISTOP,FUNNAM)
C
      RETURN
C
C-----------------------------------------------------------------------
C
20    FORMAT (' *** ENTER KILLFN')
30    FORMAT (' ')
40    FORMAT ('0',132('*'))
50    FORMAT ('0',15X,'FUNCTION ',2A4,' TERMINATED DUE TO A FATAL ',
     *   'ERROR.')
C
      END
