C MEMBER BKPS50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE BKPS50( NBASINS, ISTART, NPR_PRDS,
     1 NDP_PER_PRD, D, IP_KP, RKPKS, IPR_EX_DAYS, RKPMIN,
     2 RKPMAX, RKSMIN, RKSMAX, NSIZE, IST_OP, IX, XN, XV,
     3 RKPOLD, ARRAY )
C
C    THIS ROUTINE PUTS THE CORRECT VALUES IN THE RKPKS
C    TIMESERIES. SETS THE UPPER AND LOWER LIMITS OF KP AND
C    KS
C
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C


C    PASSED ARGUMENTS
      INTEGER NBASINS, ISTART, NPR_PRDS, NDP_PER_PRD, IPR_EX_DAYS,
     1 NSIZE
      DIMENSION D(*), RKPKS(*), IP_KP(*), RKPMIN(*), RKPMAX(*),
     1 RKSMIN(*), RKSMAX(*), IST_OP(*), IX(*), XN(*), XV(*),
     2 RKPOLD(*)
      REAL*4 ARRAY(*)
C    EJM 01/11/95
      INCLUDE 'common/fcassm'



      INCLUDE 'common/ionum'


C    LOCAL ARGUMENTS
      INTEGER IDAY_CNT, IARRAY, I, J, K, IKS
      REAL SUM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/bkps50.f,v $
     . $',                                                             '
     .$Id: bkps50.f,v 1.1 1995/09/17 18:55:32 dws Exp $
     . $' /
C    ===================================================================
C

      IARRAY = 0
      IDAY_CNT = 0
      IF  ((ASSMPAR.EQ.1).OR.(ASSMPAR.EQ.3)) THEN
        DO I = 1, NBASINS
          IDAY_CNT = ISTART - 1
          DO J = 1, NPR_PRDS
             SUM = 0.0
             IARRAY = IARRAY + 1
             DO K = 1, NDP_PER_PRD
                IDAY_CNT = IDAY_CNT + 1
                ITEST = IFMSNG(D(IP_KP(I)+IDAY_CNT-1))
                IF ((KPTIME.EQ.1).AND.(ASSMRUN.EQ.1))
     1 ITEST = 1
                IF (ITEST.EQ.1) THEN
                    SUM = SUM + 1.0
                ELSE
                    SUM = SUM + D(IP_KP(I)+IDAY_CNT-1)
                END IF
             END DO
             IF (J.NE.NPR_PRDS) THEN
                RKPKS(IARRAY) = SUM / NDP_PER_PRD
                RKPOLD(IARRAY) = RKPKS(IARRAY)
                IF (RKPKS(IARRAY).LT.RKPMIN(I)) THEN
                   RKPKS(IARRAY)=RKPMIN(I)
                   WRITE(IPR,901) IARRAY
  901              FORMAT( 1H0, 10X, 12H**WARNING** ,
     1 28HORIGINAL KP VALUE FOR PERIOD,I2,24H IS SMALLER THAN MINIMUM,
     2 25H SETTING TO MINIMUM LIMIT )
                   CALL WARN
                ELSE IF (RKPKS(IARRAY).GT.RKPMAX(I)) THEN
                   RKPKS(IARRAY)=RKPMAX(I)
                   WRITE(IPR,902) IARRAY
  902              FORMAT( 1H0, 10X, 12H**WARNING** ,
     1 28HORIGINAL KP VALUE FOR PERIOD,I2,24H IS GREATER THAN MAXIMUM,
     2 25H SETTING TO MAXIMUM LIMIT )
                   CALL WARN
                END IF
                IX(IARRAY) = IARRAY
                XN(IARRAY) = RKPMIN(I)
                XV(IARRAY) = RKPMAX(I)
             ELSE
               DO K = 1, IPR_EX_DAYS
                   IDAY_CNT = IDAY_CNT + 1
                   ITEST = IFMSNG(D(IP_KP(I)+IDAY_CNT-1))
                   IF ((KPTIME.EQ.1).AND.(ASSMRUN.EQ.1))
     1 ITEST = 1
                   IF (ITEST.EQ.1) THEN
                      SUM = SUM + 1.0
                   ELSE
                      SUM = SUM + D(IP_KP(I)+IDAY_CNT-1)
                   END IF
               END DO
               IX(IARRAY) = IARRAY
               XN(IARRAY) = RKPMIN(I)
               XV(IARRAY) = RKPMAX(I)
               RKPKS(IARRAY) = SUM / (NDP_PER_PRD + IPR_EX_DAYS)
               RKPOLD(IARRAY) = RKPKS(IARRAY)
               IF (RKPKS(IARRAY).LT.RKPMIN(I)) THEN
                 RKPKS(IARRAY)=RKPMIN(I)
                 WRITE(IPR,901) IARRAY
                 CALL WARN
               ELSE IF (RKPKS(IARRAY).GT.RKPMAX(I)) THEN
                 RKPKS(IARRAY)=RKPMAX(I)
                 WRITE(IPR,902) IARRAY
                 CALL WARN
               END IF
             END IF
          END DO
        END DO
      END IF

      IF  ((ASSMPAR.EQ.2).OR.(ASSMPAR.EQ.3)) THEN
        IF (ASSMRUN.EQ.1) THEN   !!! KS = 1.0
          DO I = 1, NBASINS
C           SAC-SMA STATE OPTION NUMBER 1
             IF (IST_OP(I).EQ.1) THEN
                 IARRAY = IARRAY + 1
                 RKPKS(IARRAY) = 1.0
                 IX(IARRAY) = IARRAY
                 XN(IARRAY) = RKSMIN(I)
                 XV(IARRAY) = RKSMAX(I)
            END IF
          END DO
        ELSE
          IKS = 18
          DO I = 1, NBASINS
C           SAC-SMA STATE OPTION NUMBER 1
            IF (IST_OP(I).EQ.1) THEN
                IARRAY = IARRAY + 1
                RKPKS(IARRAY) = ARRAY(IKS)
                IKS = IKS + 6
            END IF
          END DO
      END IF

      END IF
      NSIZE = IARRAY

      END
