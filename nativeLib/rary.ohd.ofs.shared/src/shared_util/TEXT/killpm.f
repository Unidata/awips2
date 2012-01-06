C MODULE FCKILL
C-----------------------------------------------------------------------
C
      SUBROUTINE KILLPM
C
C  THIS ROUTINE KILLS THE PROGRAM.
C
      CHARACTER*8 FUNNAM
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/killcd'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/killpm.f,v $
     . $',                                                             '
     .$Id: killpm.f,v 1.3 2001/06/13 09:39:22 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER KILLPM'
C
      DO 10 I=1,3
         WRITE (IPR,30)
         IF (IOERR.NE.IPR) WRITE (IOERR,30)
10       CONTINUE
C
      WRITE (IPR,40)
      IF (IOERR.NE.IPR) WRITE (IOERR,40)
C
      WRITE (IPR,50)
      IF (IOERR.NE.IPR) WRITE (IOERR,50)
C
      ISTOP=16
      NKILLS=NKILLS+1
C
C  PRINT REST OF MESSAGE
      IFN=0
      CALL FKSMSG (IFN,ISTOP,FUNNAM)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' ')
40    FORMAT ('0',132('*'))
50    FORMAT ('0',15X,'RUN TERMINATED DUE TO A FATAL ERROR.')
C
      END
