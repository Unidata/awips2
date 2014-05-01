C MEMBER RO33
C  (from old member FCEX33)
C  ===================================================================
C FROM MODULE RO33
C
C
C                             LAST UPDATE: 04/12/95.09:42:19 BY $WC20SV
C @PROCESS LVL(77)
      SUBROUTINE RO33(AI,RA,RO)
C
C  **********************************************************
C
C  THIS ROUTINE IS THE ANTECEDENT INDEX/RUNOFF RELATIONSHIP
C  FOR THE CINCINNATI RFC.
C
C  **********************************************************
C  ROUTINE INITIALLY WRITTEN BY
C             TIM SWEENEY  -  OHRFC               NOVEMBER 1984
C             TOM MCPHILLIPS - OHRFC  MODIFY      APRIL 1985
C  **********************************************************
C  PRINCIPLE VARIABLES...
C           AI     IANTECEDENT INDEX
C           DELC   INTERPLOATION BETWEEN COLUMN S OF TABLE
C           DELR   INTERPOLATION BETWEEN ROWS OF TABLE
C           IC     COLUMN IN TABLE
C           IRARO  RAIN/RUNOFF TABLE
C           IRH     COMPONENT OF COMPUTED RUNOFF
C           IRL     COMPONENT OF COMPUTED RUNOFF
C           KR     ROW IN TABLE
C           LOSS   INITIAL RAIN LOSS
C           RO     COMPUTED RUNOFF
C
C  ***********************************************************
C
      DIMENSION IRARO(13,9),LOSS(9)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/ro33.f,v $
     . $',                                                             '
     .$Id: ro33.f,v 1.1 1995/09/17 18:58:37 dws Exp $
     . $' /
C    ===================================================================
C
C  "LOSS", "IRARO" IN HUNDREDTHS
      DATA LOSS/10,25,40,60,90,125,180,255,310/
      DATA IRARO/0,90,190,285,385,480,580,680,780,880,975,1075,1175,
     1          0,55,135,230,320,415,510,610,705,805,905,1000,1100,
     2          0,40, 90,165,255,340,435,530,630,725,825, 925,1025,
     3          0,20, 60,115,195,280,370,465,560,655,755, 855, 950,
     4          0,13, 35, 75,135,225,315,410,505,605,705, 800, 900,
     5          0, 8, 20, 45, 90,160,255,350,445,545,640, 740, 840,
     6          0, 6, 12, 25, 50,105,190,290,385,480,580, 680, 780,
     7          0, 4,  8, 13, 30, 65,125,220,320,420,520, 620, 720,
     8          0, 3,  6,  8, 18, 40, 80,145,240,335,435, 535, 635/
C
C.....
C     RAFL  1 *    0    0    -    -    -    0
C     LESS  2 *  .90  .55                 .03
C     ILOSS - *    -    -                   -
C           - *    -    -                   -
C           - *    -    -                   -
C          12 *11.7511.00    -    -    - 6.35
C     ****************************************
C         AI  *  1.2  2.0    -    -    -  7.6
C
C.....AI IS ANTECEDENT INDEX    X.X
      JAI=AI*10.
      JAI=MAX0(JAI,12)
      JAI=MIN0(JAI,76)
C
C.....AI MUST BE BETWEEN 1.20 AND 7.60
C      CONVERT AI TO ARRAY COL NO. ... 1.2 TO 1, 2.0 TO 2 ... 7.6 TO 9
C
      IC=(JAI-4)/8
C
C.....GET THE PARTIAL DISTANCE BETWEEN COLS ... DELC.
      DELC=(FLOAT(JAI)-4.)/8. -FLOAT(IC)
C
C.....FOR AI=7.6 SET L=0 TO STAY INSIDE THE ARRAY DURING INTERPOLATION.
      IF(JAI.LT.76) THEN
        LC = 1
      ELSE
        LC = 0
      ENDIF
C
C.....COMPUTE INITIAL LOSS
      ILOSS=LOSS(IC)+(LOSS(IC+LC)-LOSS(IC))*DELC
C
      JRAIN=100.*RA
C.....IF JRAIN IS LESS THAN ILOSS ... NO RUNOFF
      IF(JRAIN.LE.ILOSS) THEN
        RO = 0.0
        GOTO 9999
      ELSE
C
C.....GET RA/RO ARRAY ROW NO.
        KR = (JRAIN-ILOSS+100)/100
      ENDIF
C
C.....GET PARTIAL DISTANCE BETWEEN ROWS ...DELR
      DELR=(FLOAT(JRAIN-ILOSS)+100.)/100.-FLOAT(KR)
C
C.....ARRAY IS GOOD UP TO RAINFALL MINUS INITIAL LOSS OF 12 IN.=13 ROWS.
C     ASSUME ALL RAINFALL EXCEEDING TABLE RUNS OFF.
      LEX=JRAIN-ILOSS-1200
      IF(LEX.GT.0) THEN
C
C.....RAIN LESS INITIAL LOSS EXCEEDS 12 INCHES.
        KL = 0
      ELSE
C
C.....RAIN LESS INITIAL LOSS IS 0 TO 12 INCHES.
        LEX = 0
        KL = 1
      ENDIF
C
      IRL=IRARO(KR,IC)+(IRARO(KR,IC+LC)-IRARO(KR,IC))*DELC
      IRH=IRARO(KR+KL,IC)+(IRARO(KR+KL,IC+LC)- IRARO(KR+KL,IC))*DELC
      IRO=IRL+(IRH-IRL)*DELR
      RO=.01*(IRO+LEX)
      IF(RO.LT.0.0) RO = 0.0
 9999 RETURN
      END
