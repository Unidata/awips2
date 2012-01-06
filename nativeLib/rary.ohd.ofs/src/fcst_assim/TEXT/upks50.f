C MEMBER UPKS50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C @PROCESS LVL(77)
      SUBROUTINE UPKS50( ASSIMID, NBASINS, ROPTY, ROPID,
     1 IST_OP, RKPKS, NPR_PRDS, ARRAY )
C
C    THIS ROUTINE UPDATES THE KS PREPROCESSOR DATA BASE.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C

C    PASSED ARGUMENTS
      DIMENSION ASSIMID(*), ROPTY(*), ROPID(*),
     1 IST_OP(*), RKPKS(*)
      REAL*4 ARRAY(100)
      INCLUDE 'common/fcassm'
      INCLUDE 'common/where'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fclfls'
      INTEGER NBASINS, NPR_PRDS



C    LOCAL ARGUMENTS
      INTEGER ICNT, LARRAY, IPTR, ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/upks50.f,v $
     . $',                                                             '
     .$Id: upks50.f,v 1.1 1995/09/17 18:55:49 dws Exp $
     . $' /
C    ===================================================================
C

      DATA ASSM /4HASSM/

      ARRAY(1)        = 1 + .01
      ARRAY(2)        = ASSIMID(1)
      ARRAY(3)        = ASSIMID(2)
      ARRAY(6)        = NOW(1) +  0.01
      ARRAY(7)        = NOW(2) +  0.01
      ARRAY(8)        = NOW(3) +  0.01
      ARRAY(9)        = NOW(4) +  0.01
      ARRAY(10)       = NOW(5) +  0.01
      ARRAY(11)       = IDARUN +  0.01
      ARRAY(12) = NBASINS + 0.01
      ICNT = 12
      DO I = 1, NBASINS
         ICNT = ICNT + 1
         ARRAY(ICNT) = ROPTY((I-1)*2 + 1)
         ICNT = ICNT + 1
         ARRAY(ICNT) = ROPTY((I-1)*2 + 2)
         ICNT = ICNT + 1
         ARRAY(ICNT) = ROPID((I-1)*2 + 1)
         ICNT = ICNT + 1
         ARRAY(ICNT) = ROPID((I-1)*2 + 2)
         IF (IST_OP(I).EQ.1) THEN       !!! SAC-SMA OPTION

C          NUMBER OF KS PER BASIN
            ICNT = ICNT + 1
            ARRAY(ICNT) = 1 + .01
C          KS VALUES
            ICNT = ICNT + 1
            IF (ASSMPAR.EQ.1) THEN
              ARRAY(ICNT) = 1.0
            ELSE IF(ASSMPAR.EQ.2) THEN
              ARRAY(ICNT)= RKPKS(I)
            ELSE
              ARRAY(ICNT) = RKPKS(NBASINS*NPR_PRDS + I)
            END IF
          END IF
      END DO
      LARRAY = ICNT
      IPTR = 0

      CALL WPPREC(ASSIMID, ASSM, LARRAY, ARRAY, IPTR, ISTAT )

      IF (ISTAT.NE.0) THEN
         WRITE(IPR,901)
  901    FORMAT(1H0, 10X, 10H**ERROR** ,
     1 'TROUBLE WRITING ASSM PARAMETER RECORD ',
     2 'NOT SAVING RUN TO ASSM RECORD' )
         CALL ERROR
         RETURN
      END IF
      IWRPPP = 1
      END
