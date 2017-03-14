C MEMBER UPKP50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCUPDT50
C @PROCESS LVL(77)
      SUBROUTINE UPKP50( ISTART, NBASINS, NPR_PRDS,
     1 NDP_PER_PRD, D, IP_KP, RKPKS, IPR_EX_DAYS )
C
C    THIS ROUTINE UPDATES THE KP TIMESERIES.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C
C    PASSED ARGUMENTS
      INTEGER ISTART, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IPR_EX_DAYS

      DIMENSION D(*), IP_KP(*), RKPKS(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/upkp50.f,v $
     . $',                                                             '
     .$Id: upkp50.f,v 1.1 1995/09/17 18:55:48 dws Exp $
     . $' /
C    ===================================================================
C

C    LOCAL ARGUMENTS

      DO I = 1, NBASINS
        ICNT = ISTART - 1
        DO J = 1, NPR_PRDS
           DO K = 1, NDP_PER_PRD
              ICNT = ICNT + 1
              D(IP_KP(I) + ICNT -1) = RKPKS( J+(I-1)*NPR_PRDS )
           END DO
           IF (J.EQ.NPR_PRDS) THEN
              DO K = 1, IPR_EX_DAYS
                 ICNT = ICNT + 1
                 D(IP_KP(I) + ICNT -1) = RKPKS( J+(I-1)*NPR_PRDS )
              END DO
           END IF
        END DO
      END DO

      END
