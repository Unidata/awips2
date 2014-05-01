C MEMBER FFA2F
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT CHARACTERS TO REAL*4 VALUES
C
      SUBROUTINE FFA2F (IBUF,IPOS,IWIDTH,IDEC,NUM,XLOC,IERR)
C
      CHARACTER*1 IBUF(1)
      CHARACTER*3 CHAR1,CHAR2
      CHARACTER*10 FMT
      CHARACTER*100 TEMP
C
      REAL*4 XLOC(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/ffa2f.f,v $
     . $',                                                             '
     .$Id: ffa2f.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
C
C
      IERR=0
      TEMP=' '
C
      IF (NUM.LT.1) GO TO 900
C
C  CHECK IF VALID NUMBER OF DECIMAL PLACES
      IF (IDEC.GT.-1.AND.IDEC.LE.IWIDTH) GO TO 5
         IDEC=0
         IERR=0
C
C  FILL FMT WITH PROPER VALUE BASED ON IWIDTH AND IDEC
 5    IF (IWIDTH.LT.1.OR.IWIDTH.GT.LEN(TEMP)) GO TO 900
      WRITE (CHAR1,'(I3)') IWIDTH
      WRITE (CHAR2,'(I3)') IDEC
      FMT='(F'//CHAR1//'.'//CHAR2//')'
C
C  LOOP FOR EACH NUMBER
      IE=IPOS-1
      DO 100 I=1,NUM
         IS=IE+1
         IE=IE+IWIDTH
C     COPY ONE NUMBER FROM IBUF INTO TEMP
         J=1
         DO 10 K=IS,IE
            TEMP(J:J)=IBUF(K)
            J=J+1
10          CONTINUE
C     CHANGE ALL BLANKS TO ZEROES
         DO 15 J=1,IWIDTH
            IF (TEMP(J:J).EQ.' ') TEMP(J:J)='0'
            IF (TEMP(J:J).GT.'9'.OR.TEMP(J:J).LT.'0') GO TO 12
C        NUMERIC VALUE - CONTINUE
            GO TO 15
C        NON-NUMERIC VALUE - SEE IF '+', '-' OR '.'
 12         IF (TEMP(J:J).EQ.'-'.OR.TEMP(J:J).EQ.'+') GO TO 15
            IF (TEMP(J:J).EQ.'.') GO TO 15
C        SET VALUE TO ZERO AND SET ERROR FLAG TO ONE
            TEMP(J:J)='0'
            IERR=1
 15         CONTINUE
C     SEE IF NEED TO MOVE A '+' OR '-' SIGN TO FIRST CHARACTER IN TEMP
         IF (IWIDTH.EQ.1) GO TO 19
         IF (TEMP(1:1).NE.'0') GO TO 19
C     SCAN TEMP FOR '+' OR '-'
         DO 18 J=2,IWIDTH
            IF (TEMP(J:J).EQ.'0') GO TO 18
            IF (TEMP(J:J).EQ.'-'.OR.TEMP(J:J).EQ.'+') GO TO 17
C        FIRST NONZERO CHARACTER IS NOT '+' OR '-' SKIP TO CONVERSION
            GO TO 19
C        '+' OR '-' FOUND - MOVE TO FIRST CHARACTER
 17         TEMP(1:1)=TEMP(J:J)
            TEMP(J:J)='0'
            GO TO 19
 18      CONTINUE
C     CONVERT TEMP TO NUMERIC VALUE - IF ERROR SET NUMERIC VALUE TO 0
 19      READ (TEMP(1:IWIDTH),FMT,ERR=20) XLOC(I)
         GO TO 100
 20      XLOC(I)=0.
         IERR=1
 100     CONTINUE
C
      GO TO 999
C
 900  IERR=1
C
 999  RETURN
C
      END
