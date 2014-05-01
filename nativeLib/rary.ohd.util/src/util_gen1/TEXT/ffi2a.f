C MEMBER FFI2A
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT FULL WORD INTEGER VALUES TO CHARACTERS
C
      SUBROUTINE FFI2A (IBUF,IPOS,IWIDTH,NUM,LOC)
C
      CHARACTER*1 IBUF(1)
      CHARACTER*6 FMT/'(I   )'/
      CHARACTER*100 TEMP
C
      DIMENSION LOC(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/ffi2a.f,v $
     . $',                                                             '
     .$Id: ffi2a.f,v 1.1 1995/09/17 19:02:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (NUM.LT.1) GO TO 50
C
      IF (IWIDTH.LT.1.OR.IWIDTH.GT.LEN(TEMP)) GO TO 50
C
C  FILL FMT WITH PROPER VALUE BASED ON IWIDTH
      WRITE (FMT(3:5),'(I3)') IWIDTH
C
C  LOOP FOR EACH NUMBER
      IE=IPOS-1
      DO 40 I=1,NUM
         IS=IE+1
         IE=IE+IWIDTH
         WRITE (TEMP(1:IWIDTH),FMT,ERR=20) LOC(I)
         J=1
         DO 10 K=IS,IE
            IBUF(K)=TEMP(J:J)
            J=J+1
10          CONTINUE
         IF (IBUF(IS).NE.'$'.AND.IBUF(IS).NE.'=') GO TO 40
C     FORMAT OVERFLOWED FIELD - FILL WITH ASTERISKS
 20      DO 30 J=IS,IE
            IBUF(J)='*'
 30         CONTINUE
40       CONTINUE
C
50     RETURN
C
      END
