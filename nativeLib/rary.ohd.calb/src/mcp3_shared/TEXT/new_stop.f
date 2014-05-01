C MODULE NEW_STOP
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/97          BY DWS
C
      SUBROUTINE NEW_STOP()
C
C  THIS ROUTINE STOPS THE PROGRAM.
C
      DIMENSION FUNNAM(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/new_stop.f,v $
     . $',                                                             '
     .$Id: new_stop.f,v 1.1 1997/04/06 13:22:52 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,30)
C
      DO 10 I=1,5
         WRITE (IPR,40)
         IF (IOERR.NE.IPR) WRITE (IOERR,40)
10       CONTINUE
C
      WRITE (IPR,50)
      IF (IOERR.NE.IPR) WRITE (IOERR,50)
C
C  SET COMPLETION CODE
      IF (NERRS.GT.0) THEN
         ISTOP=8
         GO TO 20
         ENDIF
      IF (NWARN.GT.0) THEN
         ISTOP=4
         GO TO 20
         ENDIF
      ISTOP=0
C
C  PRINT REST OF MESSAGE
20    IFN=0
      CALL NEW_FKSMSG (IFN,ISTOP,FUNNAM)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER NEW_STOP')
40    FORMAT (' ')
50    FORMAT ('0',132('*'))
C
      END
