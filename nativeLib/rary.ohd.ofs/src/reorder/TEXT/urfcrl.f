C MODULE URFCRL
C----------------------------------------------------------------------
C
C  ROUTINE TO ADD RATING CURVE NAME TO LIST OF RATING CURVES.
C
      SUBROUTINE URFCRL (MLSTRC,LSTRC,NLSTRC,MP,P,MT,T,IER)
C
C  THIS ROUTINE ADDS TO THE ORDERED LIST OF RATING CURVES. THE ORDER
C  OF THEIR APPEARANCE FOR THE COPY AND REORDER PROCESS IS DICTATED BY
C  THE ORDER OF THE RATING CURVES USED BY THE REORDERED SEGMENTS.
C
C   ARGUMENT LIST:
C    IER - STATUS CODE
C           0=NO ERRORS
C           1=MAXIMUM RATING CURVES EXCEEDED
C
C
      DIMENSION P(MP),T(MT)
      DIMENSION LSTRC(2,MLSTRC)
      DIMENSION LCOPY(2,50)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urfcrl.f,v $
     . $',                                                             '
     .$Id: urfcrl.f,v 1.3 1999/01/20 15:00:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IER=0
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) '*** ENTER URFCRL'
C
      IBUG=IFBUG('RTCV')
C
      IF (IBUG.GE.1) WRITE (IODBUG,20) NLSTRC,
     *   ((LSTRC(K,J),K=1,2),J=1,NLSTRC)
C
C  FIND ALL OCCURRENCES OF RATING CURVE USE FOR THIS SEGMENT
      CALL FRCTP (LCOPY,NCOPY,T,MT,P,MP,IERC)
      IF (IBUG.GE.1) WRITE (IODBUG,30) NCOPY,NLSTRC,
     *   ((LCOPY(J,K),J=1,2),K=1,NCOPY)
      IF (IERC.EQ.0) GO TO 40
         IER=1
         GO TO 140
C
40    IF (NCOPY.EQ.0) GO TO 140
      IF (NLSTRC.EQ.0) GO TO 70
C
C  REMOVE ANY DUPLIATE NAMES
      DO 60 I=1,NCOPY
         NDO=NLSTRC
         DO 50 J=1,NDO
            IF (LCOPY(1,I).EQ.LSTRC(1,J).AND.LCOPY(2,I).EQ.LSTRC(2,J))
     *         GO TO 60
50          CONTINUE
         NLSTRC=NLSTRC+1
         IF (NLSTRC.GT.MLSTRC) GO TO 110
            LSTRC(1,NLSTRC)=LCOPY(1,I)
            LSTRC(2,NLSTRC)=LCOPY(2,I)
60          CONTINUE
      GO TO 140
C
C  NO RATING CURVES IN GLOBAL LIST, JUST DO COPY
70    NC=0
80    NC=NC+1
      IF (NC.GT.NCOPY) GO TO 140
      IF (NLSTRC.EQ.0) GO TO 100
      DO 90 I=1,NLSTRC
         IF (LSTRC(1,I).EQ.LCOPY(1,NC).AND.LSTRC(2,I).EQ.LCOPY(2,NC))
     *      GO TO 80
90       CONTINUE
C
100   NLSTRC=NLSTRC+1
      IF (NLSTRC.GT.MLSTRC) GO TO 110
         LSTRC(1,NLSTRC)=LCOPY(1,NC)
         LSTRC(2,NLSTRC)=LCOPY(2,NC)
         GO TO 80
C
110   WRITE (IPR,120) MLSTRC
      CALL SUERRS (IPR,2,-1)
      IER=1
      GO TO 140
C
140   IF (IBUG.GE.1) WRITE (IODBUG,150) NLSTRC,
     *   ((LSTRC(K,J),K=1,2),J=1,NLSTRC)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) '*** EXIT URFCRL'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' NLSTRC=',I4,3X,
     *   'RATING CURVES=',(20(1X,2A4)/))
120   FORMAT ('0*** ERROR - IN URFCRL - ONLY ',I4,' RATING CURVES CAN ',
     *   'BE PROCESSED.')
30    FORMAT (' NCOPY=',I4,3X,'NLSTRC=',I4,3X,
     *   'RATING CURVES=',(20(1X,2A4)/))
150   FORMAT (' NLSTRC=',I4,3X,
     *   'RATING CURVES=',(20(1X,2A4)/))
C
      END
