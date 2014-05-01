C MEMBER FQCL50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      REAL FUNCTION FQCL50( QS, QO, NDAYS, WQ, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, QAVE)
C
C    THIS ROUTINE CALCULATES THE VALUE OF THE QOBS - QSIM
C    PORTION OF THE OBJECTIVE VALUE. IT ASSUMES THAT
C    DAILY TIME SERIES ARE PASSED IN FOR ONLY ONE BASIN
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED VARIABLES
      DIMENSION QS(*), QO(*)
      INTEGER NDAYS, NDQ_PER_PRD, NQ_PRDS, ISTART
      REAL WQ, QAVE
      INCLUDE 'common/fdbug'

C    LOCAL VARIABLES
C    ARRAY_COUNT = COUNTER WHICH KEEPS TRACK OF LOCATION IN TS
C    I               = COUNTER
C    F_Q        = OBJECTIVE VALUE
C    QO_SUM        = HOLDS THE SUMMATION OF OBS TS VALUES OVER A PERIOD
C    QS_SUM        = HOLDS THE SUMMATION OF OBS TS VALUES OVER A PERIOD
      INTEGER ARRAY_COUNT, I
      REAL QO_SUM(31), QS_SUM(31), F_Q
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/fqcl50.f,v $
     . $',                                                             '
     .$Id: fqcl50.f,v 1.1 1995/09/17 18:55:36 dws Exp $
     . $' /
C    ===================================================================
C

      F_Q = 0.0
      ARRAY_COUNT = ISTART
C    FOR EACH PERIOD EXCEPT THE LAST, SUM UP BOTH THE SIMULATED VALUE
C    AND THE OBSERVED VALUES. IGNORE MISSING DAYS IN THE OBS T.S. AND
C    USE THE NEXT DAYS VALUE INSTEAD
      DO I = 1, NQ_PRDS  - 1
          QO_SUM(I) = 0.0
          QS_SUM(I) = 0.0
          J = 1
          DO WHILE (J.LE.NDQ_PER_PRD)
              ICALC = IFMSNG(QO(ARRAY_COUNT))
              IF (ICALC.EQ.1) THEN
                   J = J - 1
               ELSE
                   QO_SUM(I) = QO_SUM(I) + QO( ARRAY_COUNT )
                   QS_SUM(I) = QS_SUM(I) + QS( ARRAY_COUNT )
               END IF
               ARRAY_COUNT = ARRAY_COUNT + 1
               J = J + 1
          END DO
          F_Q = F_Q + ABS(QO_SUM(I) - QS_SUM(I))
      END DO
C    IN THE LAST PERIOD, PLACE ALL REMAINING DAYS WHICH HAVEN'T BEEN
C    USED. IT SHOULD BE NDQ_PER_PRD + MOD(NQ_DAYS, NQ_PRDS) IN SIZE
      QO_SUM(NQ_PRDS) = 0.0
      QS_SUM(NQ_PRDS) = 0.0
      DO J = ARRAY_COUNT, NDAYS + ISTART - 1
           ICALC = IFMSNG(QO(J))
           IF (ICALC.EQ.0) THEN
                QO_SUM(NQ_PRDS) = QO_SUM(NQ_PRDS) + QO( J )
                QS_SUM(NQ_PRDS) = QS_SUM(NQ_PRDS) + QS( J )
            END IF
      END DO
      F_Q = F_Q + ABS(QO_SUM(NQ_PRDS) - QS_SUM(NQ_PRDS))
      FQCL50 = F_Q * WQ / QAVE

      RETURN
      END
