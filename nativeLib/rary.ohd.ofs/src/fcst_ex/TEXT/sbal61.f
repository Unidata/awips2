C**********************************************

      SUBROUTINE SBAL61 (P,HO,HF,RANGE,SBALR1,SBALR2,SBALR3,SBALR4,
     +                   IDA,IHR,IDADAT,ITS,LOTS,LTS)
C
C     THIS SUBROUTINE LETS THE FORECASTER ADJUST FORECAST
C     STAGE BALANCES AND APPLY THESE BALANCES TO COMPUTED DWOPER
C     RIVER VALUES
C
C     DEBUG, UNIT NUMBER COMMONS
C        IODBUG - UNIT NUMBER TO WRITE OUT ALL DEBUG OUTPUT
C        IONUM  - SPECIFIES UNIT NUMBER
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
      COMMON /KTIME/ KKYR(720),KKMO(720),KKDAY(720),KKHR(720),
     ?               KMSAVE(30),KDSAVE(30)
      COMMON /CMPLT/ SBAL(120)
      DIMENSION SNAME(2),P(*)
      DIMENSION EST(120),ESBAL(120),IPRTY(40,12)
      DIMENSION SBALR1(*),SBALR2(*),SBALR3(*),SBALR4(*)
      DIMENSION HO(*),HF(*),RANGE(*)
      CHARACTER *1 E,EST,BLANK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/sbal61.f,v $
     . $',                                                             '
     .$Id: sbal61.f,v 1.2 1998/10/14 13:48:03 page Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME / 4hSBAL,4h61   /
      DATA E/1hE/,BLANK/1h /,((IPRTY(I,J),J=1,12),I=1,40)/
     .     5,9,13,2,6,10,3,7,11,4,8,12,
     .     6,10,14,1,3,5,7,9,11,4,8,12,
     .     7,11,15,2,4,6,8,10,12,1,5,9,
     .     8,12,16,3,7,11,2,6,10,1,15,5,
     .     1,9,13,6,2,10,7,3,11,14,4,8,
     .     2,10,14,5,7,1,3,9,11,8,4,12,
     .     3,11,15,6,8,2,4,10,12,5,1,9,
     .     4,12,16,7,3,11,6,2,10,5,1,9,
     .     5,13,1,17,10,6,14,2,11,7,15,8,
     .     6,14,2,18,9,11,5,7,13,15,3,8,
     .     7,15,3,19,10,12,6,8,14,16,9,5,
     .     8,16,4,20,11,7,15,3,10,6,14,9,
     .     9,17,5,21,14,10,18,6,15,11,19,12,
     .     10,18,6,22,13,15,9,11,17,19,2,13,
     .     11,19,7,23,14,16,10,12,18,20,3,13,
     .     12,20,8,24,15,11,19,4,7,14,10,13,
     .     13,21,9,25,18,14,22,5,10,19,15,16,
     .     14,22,10,26,17,19,13,15,21,23,6,16,
     .     15,23,11,27,18,20,14,16,22,24,7,17,
     .     16,24,12,28,19,15,23,8,11,18,10,17,
     .     17,25,13,29,22,18,26,14,23,19,27,20,
     .     18,26,14,30,21,23,17,19,25,27,10,20,
     .     19,27,15,31,22,24,18,20,26,28,11,21,
     .     20,28,16,32,23,19,27,12,15,22,18,21,
     .     21,29,17,33,26,22,30,13,18,27,24,23,
     .     22,30,18,34,25,27,21,23,29,31,14,24,
     .     23,31,19,35,26,28,22,24,30,32,15,25,
     .     24,32,20,36,27,23,31,16,19,35,26,25,
     .     25,33,21,37,30,26,34,17,31,27,35,28,
     .     26,34,22,38,29,31,25,27,33,35,18,28,
     .     27,35,23,39,30,32,26,28,34,36,19,29,
     .     28,36,24,40,31,27,35,20,23,39,30,29,
     .     29,37,25,34,30,26,21,35,31,39,27,32,
     .     30,38,26,33,35,29,31,22,37,39,27,32,
     .     31,39,27,34,36,30,32,23,38,40,26,33,
     .     32,40,28,35,31,39,24,34,30,38,27,33,
     .     33,29,25,38,34,30,39,35,31,40,36,32,
     .     34,30,26,37,39,33,35,40,36,31,32,36,
     .     35,31,27,38,40,34,36,37,30,32,33,37,
     .     36,32,28,39,35,38,31,34,37,30,33,37/
C
C     IPRTY IS AN ARRAY USED IN ESTIMATING MISSING STAGE BALANCES.
C     IT IS USED IN CONJUNCTION WITH THE ARRAY SBAL, WHICH IS A
C     ONE-DIMENSIONAL ARRAY WITH ELEMENTS NUMBERED AS FOLLOWS:
C
C     STAGE RANGE     DAY 1   DAY 2   DAY 3   DAY 4   DAY 5
C
C       RANGE 1         1       5       9      13      17
C       RANGE 2         2       6      10      14      18
C       RANGE 3         3       7      11      15      19
C       RANGE 4         4       8      12      16      20
C
C     DAYS 1 THROUGH 3 ALL HAVE OBSERVED STAGE DATA, WHILE DAYS 4 AND
C     5 HAVE NONE BECAUSE THEY ARE FUTURE DAYS.  BALANCES FOR DAY 3
C     (CURRENT DAY) ARE CARRIED FORWARD TO DAYS 4 AND 5 FOR USE IN
C     COMPUTING FORECAST STAGES.  ESTIMATED BALANCES ARE DESIRED FOR
C     ALL DAYS WITH OBSERVED DATA, THAT IS, DAYS 1 THROUGH 3, OR SBAL
C     ELEMENTS 1 THROUGH 12.  IF ONE OF THESE 12 BALANCES IS MISSING,
C     THE IPRTY ARRAY GIVES THE ORDER OR PRIORITY OF SBAL ELEMENTS TO
C     USE AS THE ESTIMATE FOR THE MISSING VALUE.  THE FIRST INDEX OF
C     THE IPRTY ARRAY IS THE SBAL ELEMENT NUMBER (1 - 12), AND THE
C     SECOND INDEX IS THE PRIORITY (1 - 11).  SO, FOR EXAMPLE, IF SBAL
C     ELEMENT NUMBER 6 IS MISSING, THE VALUE OF SBAL(2) WILL BE USED
C     FOR SBAL(6), IF SBAL(2) IS NOT ITSELF MISSING.  IF SBAL(2) IS
C     MISSING, SBAL(10) WILL BE USED, IF IT IS NOT MISSING.  THE ORDER
C     OF PRIORITY WILL THEN CONTINUE WITH SBAL(5), SBAL(7), SBAL(1),
C     ETC.
C
C

      CALL FPRBUG (SNAME,1,61,IBUG)
      ITSDLY = (IDA-IDADAT) + IHR / 24

      IBUG = 0

      IF (IBUG.EQ.1) WRITE(IODBUG,1200) ITS,ITSDLY
 1200 FORMAT(' SBAL61: ITS ITSDLY : ',2I5)

C
      WRITE(IPR,1) P(21),P(22)
    1 FORMAT(//,'-----------',/,2A,' BALANCE REVIEW:    (E=ESTIMATE)')
      NDAY = (LTS-ITS+25) / 24
      NBALBU = (LOTS-ITS+1) / 24 * 4
      KBAL = NDAY * 4
      IF (IBUG.EQ.1) WRITE(IODBUG,40) NDAY,NBALBU,KBAL
   40 FORMAT('SBAL61: NDAY NBALBU KBAL',3I8)
      IF (IBUG.EQ.1) WRITE(IODBUG,50) ITS, LTS
   50 FORMAT('SBAL61: ITS, LTS : ',2I5)
  100 DO 10 JJ=1,KBAL
        EST(JJ)=BLANK
   10 CONTINUE
C
C     ********
C     ADD PHASE SHIFT  (NEED MOD -
C                        LAG/K TO BE USED PRIOR TO OPERATION)
C     ********
C
C
      CALL ABAL61 (HO,HF,RANGE,ITS,LOTS,LTS,NBALBU)
C
C   GET SOME ESTIMATES FOR MISSING BALANCES
C
      DO 170 K=1,KBAL
         IF(SBAL(K).GT.-9.5) THEN
            ESBAL(K)=SBAL(K)
            GO TO 170
         END IF
         ESBAL(K)=0.
         IF(K.LT.(NBALBU + 1)) THEN
               DO 160 JJ=1,12
                  KK=IPRTY(K,JJ)
                  IF(SBAL(KK).GT.-9.5) THEN
                     ESBAL(K)=SBAL(KK)
                     GO TO 165
                  END IF
  160          CONTINUE
           ELSE
              ESBAL(K)=ESBAL(K-4)
         END IF
C ***********************************************
C      BIAS MATRIX  (MAXIMUM AND MINIMUM FLOW PERIOD ADJUSTMENTS)
C                                     05/92 JOAN SALERNO
C ***********************************************
C --- MAX FLOW HH STAGE EVENTS
C          IF (K.EQ.16) THEN
C              IF (NOW(1).EQ.1 .OR. NOW(1).EQ.5) THEN
C                 IF (ISTA.EQ.2) ESBAL(K)=ESBAL(K-4)-.1
C                 IF (ISTA.EQ.4) ESBAL(K)=ESBAL(K-4)-.2
C              END IF
C --- MIN FLOW HH STAGE EVENTS
C              IF (NOW(1).EQ.8 .OR. NOW(1).EQ.9) THEN
C                 IF (ISTA.GE.2 .AND. ISTA.LE.4)
C    .              ESBAL(K)=ESBAL(K-4)-.3
C                 IF (ISTA.EQ.1) ESBAL(K)=ESBAL(K-4)-.1
C                 IF (ISTA.EQ.5) ESBAL(K)=ESBAL(K-4)-.2
C              END IF
C --- MAX FLOW LL STAGE EVENTS
C           ELSE IF (K.EQ.13) THEN
C              IF (NOW(1).EQ.1 .OR. NOW(1).EQ.5) THEN
C                 IF (ISTA.EQ.1 .OR. ISTA.EQ.3 .OR. ISTA.EQ.5)
C    .              ESBAL(K)=ESBAL(K-4)+.1
C                 IF (ISTA.EQ.4) ESBAL(K)=ESBAL(K-4)+.3
C              END IF
C --- MIN FLOW LL STAGE EVENTS
C              IF (NOW(1).EQ.8 .OR. NOW(1).EQ.9) THEN
C                 IF (ISTA.EQ.1 .OR. ISTA.EQ.2) ESBAL(K)=ESBAL(K-4)+.2
C                 IF (ISTA.EQ.3) ESBAL(K)=ESBAL(K-4)+.3
C                 IF (ISTA.EQ.5) ESBAL(K)=ESBAL(K-4)+.1
C              END IF
C           ELSE
C              ESBAL(K)=ESBAL(K-4)
C        END IF
  165    EST(K)=E
  170 CONTINUE
      DO 180 K=1,KBAL
  180    SBAL(K)=ESBAL(K)
C
C     WRITE OUT BALANCE TABLE TO TERMINAL
C
      IF (NDAY.LE.10) THEN
      WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=1,NDAY)
  205 FORMAT(//,22X,'AVERAGE DAILY BALANCES',//,
     ?         ' STAGE RANGE',5X,  10(2X,I2,'/',I2))
      DO 220 K=1,4

       WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .     ((SBAL(J)*3.28),EST(J),J=K,(K+KBAL-4),4)
  210  FORMAT(F4.1,' ->',F4.1,':',6X,10(F5.1,1A1,1X))
  220 CONTINUE
        ELSEIF (NDAY.LE.20) THEN
          WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=1,10)
          DO 230 K=1,4
          WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .       ((SBAL(J)*3.28),EST(J),J=K,(K+40-4),4)
  230     CONTINUE
          WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=11,NDAY)
          DO 240 K=1,4

          WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .       ((SBAL(J)*3.28),EST(J),J=K+40,(K+KBAL-4),4)
  240     CONTINUE
        ELSE
          WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=1,10)
          DO 250 K=1,4
          WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .       ((SBAL(J)*3.28),EST(J),J=K,(K+40-4),4)
  250     CONTINUE
          WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=11,20)
          DO 260 K=1,4
          WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .       ((SBAL(J)*3.28),EST(J),J=K+40,(K+80-4),4)
  260     CONTINUE
          WRITE(IPR,205) (KMSAVE(J),KDSAVE(J),J=21,NDAY)
          DO 270 K=1,4
          WRITE(IPR,210) (RANGE(K)*3.28),((RANGE(K+1)*3.28)-.1),
     .       ((SBAL(J)*3.28),EST(J),J=K+80,(K+KBAL-4),4)
  270     CONTINUE
      END IF


C
C  --- FILL AVE DAILY RANGE BALANCES
C
      IB=ITSDLY-1
      DO 500 J=4,KBAL,4
      IB=IB+1
      SBALR4(IB)=SBAL(J)
      IF (IBUG.EQ.1) WRITE(IODBUG,501) (SBAL(J)*3.28)
  501 FORMAT('SBAL61: RANGE4 SBAL(J): ',F10.2)
  500 CONTINUE

      IB=ITSDLY-1
      DO 600 J=3,KBAL-1,4
      IB=IB+1
      SBALR3(IB)=SBAL(J)
      IF (IBUG.EQ.1) WRITE(IODBUG,601) (SBAL(J)*3.28)
  601 FORMAT('SBAL61: RANGE3 SBAL(J): ',F10.2)
  600 CONTINUE

      IB=ITSDLY-1
      DO 700 J=2,KBAL-2,4
      IB=IB+1
      SBALR2(IB)=SBAL(J)
      IF (IBUG.EQ.1) WRITE(IODBUG,701) (SBAL(J)*3.28)
  701 FORMAT('SBAL61: RANGE2 SBAL(J): ',F10.2)
  700 CONTINUE

      IB=ITSDLY-1
      DO 800 J=1,KBAL-3,4
      IB=IB+1
      SBALR1(IB)=SBAL(J)
      IF (IBUG.EQ.1) WRITE(IODBUG,801) (SBAL(J)*3.28)
  801 FORMAT('SBAL61: RANGE1 SBAL(J): ',F10.2)
  800 CONTINUE
C
  999 RETURN
      END
