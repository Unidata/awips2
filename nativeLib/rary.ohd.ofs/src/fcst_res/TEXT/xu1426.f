C MEMBER XU1426
C  (from old member FCXU1426)
C
      SUBROUTINE XU1426(PO,CO,W,D,IDPT,LOCOWS)
C---------------------------------------------------------------------
C  SUBROUTINE TO EXECUTE UTILITY (S/U #14) FOR ADJUSTING THE RULE
C  CURVE USING OBSERVED ELEVATIONS.
C
C  CERTAIN VALUES NEED TO BE SET IN THE RULC26 COMMON BLOCK,
C  THE RULE CURVE 'TIME-SERIES' MUST BE CREATED, AND POINTERS TO WORK
C  ARRAY SPACE AND TIME-SERIES MUST BE DETERMINED BEFORE CALLING
C  THE ACTUAL ADJUSTING ROUTINE.
C--------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - JULY 1983
C--------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/rulc26'
      INCLUDE 'common/xqin26'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),CO(1),W(1),LOCOWS(1),D(1),IDPT(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xu1426.f,v $
     . $',                                                             '
     .$Id: xu1426.f,v 1.1 1995/09/17 19:07:10 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XU1426 ***')
C
C  FIRST THING IS TO LOCATE PARMS, TS, AND CO INFORMATION FOR THIS
C  UTILITY IN THE PO ARRAY.
C
      SUNUM = 1511.01
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
      LOCLT = (IORD-1)*3 + 2
      LOCT = W(LOCLT)
C
C  NOW FILL IN THE RULECURVE 'TIME-SERIES'
C
      CALL XFRU26(PO,LOCPM,W,LOCOWS)
C
C  NEXT FILL VARIABLES IN /RULC26/
C
      MORE = PO(LOCPM)*2 + 2
      IF (PO(LOCPM).LE.0.0) MORE = 1
C
      NTIMRL = PO(LOCPM+MORE)
      DIFMAX = PO(LOCPM+MORE+1)
      QIMAX  = PO(LOCPM+MORE+2)
C
C  GET THE NUMBER OF MISSING VALUES FROM CARRYOVER
C
      NOMSG = CO(LOCCO)
C
C  SET POINTERS TO TIME-SERIES DATA AND WORK SPACE
C
      IOEL = IDPT(LOCT) + IDOFST * NTIM24
C
      LOCRUL = LOCOWS(2)
      LOCMIS = LOCOWS(6)
      LOCDEV = LOCMIS + NDD*NTIM24
C
C  NOW CALL THE ROUTINE TO CREATE THE DEVIATIONS OF THE RULE CURVE
C
      CALL ARUL26(W(LOCMIS),W(LPTQIM),CO(LOCCO+1),D(IOEL),W(LOCRUL),
     .            W(LOCDEV))
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XU1426 ***')
C
      RETURN
      END
