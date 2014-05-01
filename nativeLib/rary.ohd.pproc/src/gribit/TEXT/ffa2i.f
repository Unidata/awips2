C MEMBER FFA2I
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  ROUTINE TO CONVERT CHARACTERS TO INTEGER*4 VALUES
C
      SUBROUTINE FFA2I (IBUF,IPOS,IWIDTH,NUM,LOC,ISTAT)
C
      CHARACTER*1 IBUF(1)
      CHARACTER*6 FMT
      CHARACTER*100 TEMP
C
      INTEGER*4 LOC(1)
C
CC    INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/ffa2i.f,v $
     . $',                                                             '
     .$Id: ffa2i.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
C
C
CC    IF (ICMTRC.GT.0) THEN
CC       CALL ULINE (ICMPRU,1)
CC       WRITE (ICMPRU,*) '*** ENTER FFA2I'
CC       ENDIF
C
CC    IF (ICMDBG.GT.0) THEN
CC       CALL ULINE (ICMPRU,1)
CC       WRITE (ICMPRU,*)
CC   *      ' IPOS=',IPOS,
CC   *      ' IWIDTH=',IWIDTH,
CC   *      ' (IBUF(I),I=IPOS,IWIDTH)=',(IBUF(I),I=IPOS,IWIDTH),
CC   *      ' NUM=',NUM,
CC   *      ' '
CC       ENDIF
C
      ISTAT=0
C
      IF (NUM.LT.1) THEN
         ISTAT=1
         GO TO 70
         ENDIF
C
      IF (IWIDTH.LT.1.OR.IWIDTH.GT.LEN(TEMP)) THEN
         ISTAT=1
         GO TO 70
         ENDIF
C
C  SET FORMAT FOR CONVERING VALUES
      FMT='(I   )'
      WRITE (FMT(3:5),'(I3)') IWIDTH
C
C  PROCESS EACH NUMBER
      TEMP=' '
      IE=IPOS-1
      DO 60 I=1,NUM
         IS=IE+1
         IE=IE+IWIDTH
         J=1
         DO 10 K=IS,IE
            TEMP(J:J)=IBUF(K)
            J=J+1
10          CONTINUE
         DO 20 J=1,IWIDTH
C        CHECK FOR BLANK
CCC         IF (TEMP(J:J).EQ.' ') TEMP(J:J)='0'
            IF (TEMP(J:J).EQ.' ') GO TO 20
C        CHECK FOR NON-NUMERIC CHARACTER
            IF (TEMP(J:J).LT.'0'.OR.TEMP(J:J).GT.'9') THEN
C           CHECK FOR '+' OR '-'
               IF (TEMP(J:J).EQ.'-'.OR.TEMP(J:J).EQ.'+') THEN
                  ELSE
                     TEMP(J:J)='0'
                     ISTAT=1
                  ENDIF
               ENDIF
20          CONTINUE
C     CHECK IF NEED TO MOVE A '+' OR '-' SIGN TO FIRST CHARACTER
         IF (IWIDTH.EQ.1) GO TO 40
         IF (TEMP(1:1).NE.'0') GO TO 40
         DO 30 J=2,IWIDTH
            IF (TEMP(J:J).EQ.'0') GO TO 30
C        CHECK FOR '+' OR '-'
            IF (TEMP(J:J).EQ.'-'.OR.TEMP(J:J).EQ.'+') THEN
               TEMP(1:1)=TEMP(J:J)
               TEMP(J:J)='0'
               ENDIF
            GO TO 40
30          CONTINUE
C     CONVERT TO NUMERIC VALUE
40       CONTINUE
C
CC       IF (ICMDBG.GT.0) THEN
CC          CALL ULINE (ICMPRU,1)
CC          WRITE (ICMPRU,*)
CC   *         ' IWIDTH=',IWIDTH,
CC   *         ' TEMP(1:IWIDTH)=',TEMP(1:IWIDTH),
CC   *         ' FMT=',FMT,
CC   *         ' '
CC          ENDIF
C
         READ (TEMP(1:IWIDTH),FMT,ERR=50) LOC(I)
         GO TO 60
C     ERROR CONVERTING VALUE
50       LOC(I)=0
         ISTAT=1
60       CONTINUE
C
70    CONTINUE
C
CC    IF (ICMTRC.GT.0) THEN
CC       CALL ULINE (ICMPRU,1)
CC       WRITE (ICMPRU,*) '*** EXIT FFA2I -',
CC   *      ' ISTAT=',ISTAT,
CC   *      ' '
CC       ENDIF
C
      RETURN
C
      END
