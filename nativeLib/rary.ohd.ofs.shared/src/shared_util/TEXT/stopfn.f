C MODULE STOPFN
C-----------------------------------------------------------------------
C
      SUBROUTINE STOPFN (FUNNAM)
C
C  THIS ROUTINE STOPS A FUNCTION.
C
      CHARACTER*8 FUNNAM
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/stopfn.f,v $
     . $',                                                             '
     .$Id: stopfn.f,v 1.3 1999/04/23 19:59:39 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,30)
C
      DO 10 I=1,5
         WRITE (IPR,40)
         IF (IOERR.NE.IPR) WRITE (IOERR,40)
10       CONTINUE
      WRITE (IPR,50)
      IF (IOERR.NE.IPR) WRITE (IOERR,50)
C
C  SET COMPLETION CODE.
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
20    IFN=1
      CALL FKSMSG (IFN,ISTOP,FUNNAM)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER STOPFN')
40    FORMAT (' ')
50    FORMAT ('0',132('*'))
C
      END
