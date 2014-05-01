C MEMBER PPIKOF
C  (from old member PRDRPRD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/27/95.12:18:25 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PPIKOF (IDTKEY,IMULF,JHOUR,BUF)
C
C          ROUTINE:  PPIKOF
C
C             VERSION:  1.0.0
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CONVERTS DATA FROM ONE TIME STEP TO ANOTHER BY
C    PICKING OFF, SUMMING, OR AVERAGING OF VALUES BASED ON THE DATA
C    TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IDTKEY     I     I     1     KEY TO DATA MANIPULATION
C                                       1=PICKOFF VALUE
C                                       2=SUM VALUES
C                                       3=AVERAGE
C       IMULF      I     I     1     MULTIPLICATION FACTOR
C       JHOUR      I     I     1     FIRST HOUR OF DATA DESIRED
C       BUF        R    I/O    ?     TIME SERIES RECORD TO MODIFY
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IWKBUF(22),BUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/ppikof.f,v $
     . $',                                                             '
     .$Id: ppikof.f,v 1.1 1995/09/17 18:45:44 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,170)
C
      CALL UMEMOV (BUF,IWKBUF,LENHED)
      IMUL=IMULF-1
      NVPTS=IWKBUF(3)
      INDEX=(JHOUR-IWKBUF(14))/IWKBUF(2)
      IF (INDEX.LT.0) INDEX=-INDEX*IMUL
      INDX=MOD(INDEX,IMULF)
      IF (IDTKEY.NE.1) INDX=INDX+1
      INDX=MOD(INDX,IMULF)
      L=INDX*NVPTS
      IF (L.LT.0) L=-L
      NVALS=NVPTS*IWKBUF(5)
      NTS=(NVALS-L)/(IMULF*NVPTS)
      IF (IDTKEY.EQ.1) NTS=(NVALS-L+(NVPTS*IMULF-1))/(IMULF*NVPTS)
      IVALS=NTS*NVPTS
      IF (NTS.LE.0) GO TO 140
C
C  CHANGE IWKBUF TO NEW TIME STEP
      IF (IDTKEY.EQ.1) GO TO 60
      J=IWKBUF(6)
      NUM=J+L
C
      DO 50 II=1,NTS
         DO 40 JJ=1,NVPTS
            K=0
            BUF(J)=BUF(NUM)
            IF (BUF(NUM).EQ.ZAPR) GO TO 30
               DO 10 I=1,IMUL
                  K=K+NVPTS
                  IF (BUF(NUM+K).EQ.ZAPR) GO TO 20
                     BUF(J)=BUF(J)+BUF(NUM+K)
10                   CONTINUE
               IF (IDTKEY.EQ.2) GO TO 30
               BUF(J)=BUF(J)/IMULF
               GO TO 30
20          BUF(J)=ZAPR
30          J=J+1
            NUM=NUM+1
40          CONTINUE
          NUM=NUM+NVPTS*IMUL
50        CONTINUE
      GO TO 90
C
C  SET UP IWKBUF FOR IDTKEY OF 1
60    K=L+IWKBUF(6)
      J=IWKBUF(6)
      DO 80 I=1,NTS
          DO 70 LOOP=1,NVPTS
             BUF(J)=BUF(K)
             J=J+1
             K=K+1
70           CONTINUE
          K=K+NVPTS*IMUL
80        CONTINUE
C
C  CHECK FUTURE POINTER
90    IF (IWKBUF(7).EQ.0) GO TO 120
      IF (IWKBUF(7).EQ.IWKBUF(6)) GO TO 120
C
      ITEMP7=(IWKBUF(7)-IWKBUF(6)-L)/NVPTS
      IF (ITEMP7.LE.0) GO TO 100
      NUMTS=ITEMP7/IMULF
      MODNUM=MOD(ITEMP7,IMULF)
      IF (NUMTS.EQ.0) GO TO 110
         IWKBUF(7)=NUMTS*NVPTS+IWKBUF(6)
         IF (IDTKEY.EQ.1.AND.MODNUM.NE.0) IWKBUF(7)=IWKBUF(7)+NVPTS
         GO TO 120
C
100   IWKBUF(7)=IWKBUF(6)
      GO TO 120
C
110   IWKBUF(7)=IWKBUF(6)
      IF (IDTKEY.EQ.1) IWKBUF(7)=IWKBUF(7)+NVPTS
C
C  CHANGE HEADER VALUES
120   IWKBUF(5)=NTS
      IF (IDTKEY.EQ.1) GO TO 130
         IWKBUF(14)=IWKBUF(14)+(L/NVPTS*IWKBUF(2))+IMUL*IWKBUF(2)
         IWKBUF(2)=IWKBUF(2)*IMULF
         GO TO 150
130   IWKBUF(14)=IWKBUF(14)+(L/NVPTS*IWKBUF(2))
      IWKBUF(2)=IWKBUF(2)*IMULF
      GO TO 150
C
C  NO DATA IN RECORD
140   IWKBUF(2)=IWKBUF(2)*IMULF
C
150   CALL UMEMOV (IWKBUF,BUF,LENHED)
      IF (IPRDB.GT.0) WRITE (IOGDB,180) L,NTS,IVALS,IMULF,IDTKEY,INDX
      NWDS=IWKBUF(3)*IWKBUF(5)+IWKBUF(1)
      IF (IPRDB.GT.0) WRITE (IOGDB,190) (BUF(I),I=1,NWDS)
      IF (IPRTR.GT.0) WRITE (IOGDB,160)
160   FORMAT (' *** EXIT PPIKOF')
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT (' *** ENTER PPIKOF')
180   FORMAT (' L=',I6,3X,'NTS=',I6,3X,'IVALS=',I6,3X,
     *   'IMULF=',I6,3X,'IDTKEY=',I6,3X,'INDX=',I6)
190   FORMAT (1X,7(I4,1X),2A4,2(1X,A4),2F8.2,2(1X,I7,1X,I4),1X,5A4 /
     *     (10(F8.2,1X)))
C
      END
