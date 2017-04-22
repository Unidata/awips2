C MEMBER FRCFQ
C  (from old member FCFRCFQ)
C
      SUBROUTINE FRCFQ(FACVOL,IBUG)
C                             LAST UPDATE: 10/27/94.11:17:37 BY $WC30KH
C
C
C SUBROUTINE TO COMPUTE FLOOD DISCHARGE FOR DEFRC.
C AUTHOR - ED VANBLARGAN - HRL - JUNE, 1983
C PROCEDURE:
C 1. CHECK IF FLDSTG DEFINED
C 2. CONVERT STAGE TO DISCHARGE
C 3. ROUND TO 3 SIGNIFICANT FIGURES IN ORIGINAL UNITS
C
C ARGUMENT LIST:
C FACVOL - INPUT - DISCHARGE CONVERSION FACTOR
C
      INCLUDE 'common/fratng'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frcfq.f,v $
     . $',                                                             '
     .$Id: frcfq.f,v 1.1 1995/09/17 18:55:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C CHECK IF FLDSTG IS MISSING
      FLOODQ=-999.
      IF (IFMSNG(FLDSTG).NE.0) GO TO 999
C DETERMINE FLOODQ
      CALL FHQS1(FLDSTG,FLOODQ,1,IBUG,IDUM,JDUM,KDUM,LDUM,MDUM,NDUM,
     *    DUMMY)
C ROUND TO 3 SIGNIFICANT FIGURES USING ORIGINAL UNITS
      FLOODQ=FLOODQ*FACVOL
C  THE FOLLOWING CHANGE MADE ON 10/3/89 -- MAIN. #552
C      CALL FSIGFG(FLOODQ,3,IER)
      CALL FSIGFG(FLOODQ,4,IER)
C  END OF CHANGE OF 10/3/89
      FLOODQ=FLOODQ/FACVOL
999   RETURN
C
      END
