C***************************************************************************
      SUBROUTINE ABAL61 (HO,HF,RANGE,ITS,LOTS,LTS,NBALBU)
C
C        DEBUG , UNIT NUMBER COMMONS
C          IODBUG - UNIT NUMBER TO WRITE OUT ALL DEBUG OUTPUT
C          IONUM  - SPECIFIES UNIT NUMBER
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
      COMMON /CMPLT/ SBAL(120)
      COMMON /KTIME/ KKYR(720),KKMO(720),KKDAY(720),KKHR(720),
     .               KMSAVE(30),KDSAVE(30)
C
      DIMENSION HO(*),HF(*),RANGE(*)
      DIMENSION BAL(4,120),KBAL(4,120),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/abal61.f,v $
     . $',                                                             '
     .$Id: abal61.f,v 1.2 1998/10/14 13:44:56 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME / 4hABAL,4h61   /
C
      CALL FPRBUG (SNAME,1,61,IBUG)
      NDAYBU = NBALBU / 4
C
C          COMPUTE TABLE OF AVERAGE DIFFERENCES BETWEEN
C          OBSERVED AND COMPUTED STAGES FOR DIFFERENT
C          STAGE RANGES AND DAYS
C
C
C               DETERMINE STAGE RANGE -- FIND MAXIMUM AND
C               MINIMUM COMPUTED STAGE FOR BACKUP PERIOD
C
         GHMAX=HF(1)
         GHMIN=HF(1)
         DO 10 J=ITS,LOTS
            IF(HF(J).GT.GHMAX) GHMAX=HF(J)
            IF(HF(J).LT.GHMIN) GHMIN=HF(J)
   10    CONTINUE
        IF(IBUG.GT.0) WRITE(IODBUG,2)
   2    FORMAT(/,' MAX AND MIN FOUND')
C
C               ROUND MAXIMUM STAGE TO NEXT HIGHER HALF FOOT
C               AND MINIMUM STAGE TO NEXT LOWER HALF FOOT
C
         CALL RNDH61 (GHMAX,1)
         CALL RNDH61 (GHMIN,-1)
        IF(IBUG.GT.0) WRITE(IODBUG,3)
   3    FORMAT( /,' ROUNDING COMPLETED')
C
C               DIVIDE RANGE INTO FOUR SUB-RANGES
C
         RINT=(GHMAX-GHMIN)/4.
         RANGE(1)=GHMIN
         DO 15 L=1,3
   15       RANGE(L+1)=RANGE(L)+RINT
         RANGE(5)=GHMAX
        IF(IBUG.GT.0) WRITE(IODBUG,4)
   4    FORMAT(/,' RANGES FOUND')
C
C               SEARCH COMPUTED STAGES AND DETERMINE WHICH ONES
C               FALL INTO WHICH STAGE RANGES FOR EACH DAY OF
C               BACKUP PERIOD
C
         IX=1
         DO 20 L=1,4
            DO 20 M=1,NBALBU
               BAL(L,M)=0.
   20          KBAL(L,M)=0
C        IF(IBUG.GT.0) WRITE(IODBUG,5)
C   5    FORMAT(/,' KBAL AND BAL ARRAYS INITIALIZED')
C
         DO 30 J=ITS,LOTS
            IF(IBUG.GT.0) WRITE(IODBUG,22) KKDAY(J),J,KDSAVE(IX),IX
   22       FORMAT(' ABAL61: KKDAY,J,KDSAVE,IX', 4I8)
            IF(KKDAY(J).NE.KDSAVE(IX)) THEN
               IF(IX.EQ.NBALBU) GO TO 21
               IX=IX+1
            END IF
   21       IF(HO(J).LE.-5.99) GO TO 30
C
C  --- CREATE BALANCES
C
            DIFF=HO(J)-HF(J)
         IF(IBUG.GT.0) WRITE(IODBUG,5) HO(J),HF(J),J,DIFF
    5    FORMAT(' ABAL61: HO,HF,J,DIFF',2F6.2,I4,F6.2)

C
C                    THROW OUT OBSERVED VALUES THAT ARE MORE
C                    THAN TEN FEET DIFFERENT FROM COMPUTED
C
            IF(ABS(DIFF).GT.10.) GO TO 30
C
            DO 25 L=1,3
             IF(HF(J).GE.RANGE(L).AND.HF(J).LT.RANGE(L+1)) THEN
                  KBAL(L,IX)=KBAL(L,IX)+1
                  BAL(L,IX)=BAL(L,IX)+DIFF
             END IF
   25       CONTINUE
            IF(HF(J).GE.RANGE(4).AND.HF(J).LE.RANGE(5)) THEN
               KBAL(4,IX)=KBAL(4,IX)+1
               BAL(4,IX)=BAL(4,IX)+DIFF
            END IF
   30    CONTINUE
C        IF(IBUG.GT.0) WRITE(IODBUG,6)
C   6    FORMAT(/,'DIFFS FOUND, READY TO COMPUTE BALANCES')
C
C  ---     COMPUTE AVERAGE BALANCES
C
         DO 40 L=1,4
            DO 40 M=1,NBALBU
               IF(KBAL(L,M).EQ.0) THEN
                     BAL(L,M)=-9.9
                  ELSE
                     BAL(L,M)=BAL(L,M)/REAL(KBAL(L,M))
               END IF
   40    CONTINUE
C
C
C  ---     WRITE OUT RESULTS TO TERMINAL
C               (NUMBER OF VALUES IN AVE TIDAL BALANCES)
C
C
        WRITE(IPR,3000) (KMSAVE(J),KDSAVE(J),J=1,IX)
 3000   FORMAT(//,22X,'NO. OF VALUES PER AVERAGE DAILY BALANCE',//,
     ?         ' STAGE RANGE',5X,10(2X,I2,'/',I2))
      DO 45 L=1,4

   45       WRITE(IPR,3010)(RANGE(L)*3.28),((RANGE(L+1)*3.28)-.1),
     ?                    (KBAL(L,J),J=1,IX)
 3010      FORMAT(F4.1,' ->',F4.1,':',9X,10(I2,5X),/)
C
C ---     TRANSFER BALANCES TO SBAL ARRAY
C
      DO 60 L=1,4
        DO 60 M=1,NBALBU
          INDEX=L+(4*(M-1))
          SBAL(INDEX)=BAL(L,M)
          IF (IBUG.EQ.1) WRITE(IODEBUG,3020) INDEX,SBAL(INDEX)
 3020     FORMAT('ABAL61: INDEX, SBAL(INDEX) ',I4,F6.2)
   60 CONTINUE
C     IF(IBUG.GT.0) WRITE(IODBUG,8)
C   8 FORMAT(/,' BALANCES TRANSFERRED TO SBAL ARRAY OKAY')
      RETURN
      END
