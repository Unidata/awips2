C MEMBER SVRN50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C FROM MODULE FCSVRN50
C @PROCESS LVL(77)
      SUBROUTINE SVRN50( VALUE, NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D, RKPKS, IST_OP, ISTNF, C, IP_RRCO, QS, QO,
     2 NDAYS, WQ, NDQ_PER_PRD,  NQ_PRDS, ISTART, QAVE, RKPOLD,
     3 WP_B_PRD, WP, WS_B, WS, P, MP, MC, T, MT, TS, MTS,
     4 MD, IHZERO, IP_RRPO, IAWARN, ARRAY, ASSMID )
C
C    THIS ROUTINE CALCULATES THE SVRN50
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1

C    PASSED ARGUMENTS
      INTEGER NBASINS, NPR_PRDS, NDP_PER_PRD, NDAYS, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, MP, MC, MT, MTS, IHZERO, IAWARN
      REAL*4 ARRAY(*)
      REAL VALUE, WQ, QAVE, WP, WS
      DIMENSION IP_PR(*), IPR_IDT(*), D(MD), RKPKS(*), IST_OP(*),
     1 ISTNF(*), C(MC), IP_RRCO(*), QS(*), QO(*), RKPOLD(*),
     2 WP_B_PRD(*), WS_B(*), P(MP), TS(MTS), IP_RRPO(*), ASSMID(*)

      DATA ASSM /4HASSM/
      INTEGER T(MT)
      INCLUDE 'common/ionum'

C     EJM 01/11/95
      INCLUDE 'common/fcassm'
      INCLUDE 'common/fclfls'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/svrn50.f,v $
     . $',                                                             '
     .$Id: svrn50.f,v 1.1 1995/09/17 18:55:47 dws Exp $
     . $' /
C    ===================================================================
C


C    LOCAL VARIABLES
C    INV        - 0 MULT VALUES BY KPKS VALUES
C               - 1 MULT VALUES BY INVERSE KPKS VALUES

      INV = 0

      IF ((ASSMPAR.EQ.1).OR.(ASSMPAR.EQ.3)) THEN
           CALL MDPR50( NBASINS, NPR_PRDS, NDP_PER_PRD,
     1 IP_PR, IPR_IDT, D, RKPKS, INV )
      END IF

      IF ((ASSMPAR.EQ.2).OR.(ASSMPAR.EQ.3)) THEN
             CALL MDST50( NBASINS, IST_OP, ISTNF, C, IP_RRCO,
     1 NPR_PRDS, RKPKS, IP_RRPO, P, INV )
      END IF

      LWORKMX = 1
      CALL LDRV50( P,MP,C,MC,T,MT,TS,MTS,D,MD,IHZERO )

      VALUE = FCAL50(  QS, QO, NDAYS, WQ, NDQ_PER_PRD,
     1 NQ_PRDS, ISTART, QAVE, RKPKS, NBASINS, NPR_PRDS,
     2 RKPOLD, WP_B_PRD, WP, WS_B, WS, IST_OP, FQ, FP, FS )


C    MODIFY KS HEADER NOW

      IF (IAWARN.NE.1) THEN
         ARRAY(6) =     0 + .01
         ARRAY(7) =     0 + .01
         ARRAY(8) =     0 + .01
         ARRAY(9) =     0 + .01
         ARRAY(10) =    0 + .01
         ARRAY(11) =    0 + .01
         ARRAY(12) =    0 + .01
         ARRAY(13) =    0 + .01
         ARRAY(14) =    0 + .01
         ARRAY(15) =    0 + .01
         ARRAY(16) =    0 + .01
         ARRAY(17) =    0 + .01
         ARRAY(18) =   -999.0
         LARRAY = 18
         IPTR = 0
         CALL WPPREC(ASSMID, ASSM, LARRAY, ARRAY, IPTR, ISTAT )
         IF (IBUG.GE.1) THEN
           WRITE(IODBUG,*) 'STATUS OF WPPREC', ISTAT
         END IF
         IF (ISTAT.NE.0) THEN
           WRITE(IPR,901)
  901      FORMAT(1H0, 10X, 10H**ERROR** ,
     1 'TROUBLE WRITING ASSM PARAMETER RECORD ',
     2 'NOT SAVING RUN TO ASSM RECORD' )
           CALL ERROR
           RETURN
         END IF
         IWRPPP = 1
      END IF

      END
