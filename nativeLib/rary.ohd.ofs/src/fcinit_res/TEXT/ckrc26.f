C MEMBER CKRC26
C  (from old member FCCKRC26)
C
C DESC CHECK IF RATING CURVE IS ALLOWED TO BE INPUT
C-------------------------------------------------------------------
      SUBROUTINE CKRC26(NUM,RID,IER)
C--------------------------------------------------------------------
C  ROUTINE TO CHECK IF A RATING CURVE WITH A DIFFERENT NAME THAN 'ID'
C  HAS BEEN ENTERED FOR TYPE 'NUM'. DIFFERENT TYPES OF RATING CURVE USES
C  ARE :
C       1) TAIL WATER RATING CURVE (ONLY ONE ALLOWED)
C       2) UPSTREAM CONTROL POINT RATING CURVES (ONLY ONE ALLOWED),
C       3) DOWNSTREAM CONTROL POINT RATING CURVES (UP TO TWO ALLOWED).
C------------------------------------------------------------
C  JTOSTROWSKI - HRL - APRIL 1983
C---------------------------------------------------------
C
      INCLUDE 'common/rc26'
      INCLUDE 'common/comn26'
      INCLUDE 'common/fdbug'
C
      DIMENSION RID(2),NALLOW(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/ckrc26.f,v $
     . $',                                                             '
     .$Id: ckrc26.f,v 1.2 1997/09/22 15:58:28 page Exp $
     . $' /
C    ===================================================================
C
      DATA NALLOW/1,1,2,7*0/
C
      IER = 0
      NPOSS = NALLOW(NUM)
      IF (NRC26(NUM).EQ.0) GO TO 100
C
C  LOOK FOR NON MATCHING NAME FOR TYPE 'NUM'
C
      N = NRC26(NUM)
      DO 50 I=1,N
      NST = (I-1)*2 + 1
      IF (IUSAME(RID,RCID26(NST,NUM),2).EQ.1) GO TO 9999
ccc      IF (I .LE. NPOSS) GO TO 100
C
ccc      CALL STER26(73,1)
ccc      IER = 1
ccc      GO TO 9999
   50 CONTINUE
      IF (N .LE. NPOSS) GO TO 100
      CALL STER26(73,1)
      IER = 1
      GO TO 9999
C
C  STORE AS THE RATING CURVE FOR TYPE 'NUM'.
C
  100 CONTINUE
      NRC26(NUM) = NRC26(NUM) + 1
      N = NRC26(NUM)
      NST = (N-1)*2 + 1
      RCID26(NST,NUM) = RID(1)
      RCID26(NST+1,NUM) = RID(2)
      NUMRC = NUMRC + 1
      IF (N .LE. NPOSS) GO TO 9999
      CALL STER26(73,1)
      IER = 1
C
 9999 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1610) NUM,RID,NUMRC
 1610 FORMAT(' IN CKRC26 -- RC TYPE = ',I2,' ID = ',2A4,' TOTAL NO.',
     . ' OF RC''S IN THIS OP = ',I2)
      RETURN
      END


