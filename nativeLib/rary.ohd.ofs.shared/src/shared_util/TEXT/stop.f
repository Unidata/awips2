C MODULE STOP
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET THE STOP VALUE.
C
      SUBROUTINE STOP
C
      CHARACTER*8 FUNNAM
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/killcd'
      INCLUDE 'common/toterz'

C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/stop.f,v $
     . $',                                                             '
     .$Id: stop.f,v 1.5 2004/05/03 21:41:44 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER STOP'
      
      CALL OFSCLN
C
      DO 20 I=1,5
         WRITE (IPR,10)
         IF (IOERR.NE.IPR) WRITE (IOERR,10)
10    FORMAT (' ')
20       CONTINUE
C
      WRITE (IPR,30)
      IF (IOERR.NE.IPR) WRITE (IOERR,30)
30    FORMAT ('0',132('*'))
C
C  SET COMPLETION CODE
      ISTOP=0
      IF (NERRST.GT.0) THEN
         ISTOP=8
         GO TO 40
         ENDIF
      IF (NWARNT.GT.0) THEN
         ISTOP=4
         GO TO 40
         ENDIF
C
40    IF (ISTOP.EQ.0.AND.KLCODE.GT.0) ISTOP=KLCODE
C
C  PRINT REST OF MESSAGE
      IFN=0
      CALL FKSMSG (IFN,ISTOP,FUNNAM)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT STOP'
C
      RETURN
C
      END
