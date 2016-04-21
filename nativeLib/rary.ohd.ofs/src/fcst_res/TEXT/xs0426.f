C MODULE XS0426
C
      SUBROUTINE XS0426(SUNUM,PO,W,LOCOWS)
C--------------------------------------------------------------------
C  SUBROUTINE TO EXTRACT PARMS FROM THE PO ARRAY FOR COMPUTING
C  OUTPUT FOR THE 'RULECURVE' SCHEME.
C--------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - AUGUST 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/rulc26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
C
      DIMENSION PO(*),W(*),LOCOWS(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs0426.f,v $
     . $',                                                             '
     .$Id: xs0426.f,v 1.4 1998/10/14 13:40:47 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XS0426 ***')
C
C  GET POINTERS FOR THIS SCHEME
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C
C  SET EXECUTION POINTER
C
      LOCEX = IORD*3
      W(LOCEX) = 1.01
C
C  CREATE RULE CURVE 'TIME-SERIES'
C
      CALL XFRU26(PO,LOCPM,W,LOCOWS)
C
C  NOW GET THE NO. OF BLEND PERIODS AND THE LIMITING DISCHARGE FOR
C  BRINGING THE POOL BACK TO THE RULE CURVE.
C
      IADD = 1
      IF (PO(LOCPM).GT.0.00) IADD = 2*NRUL + 2
C
      LOCNXT = LOCPM + IADD
      NTIMES = PO(LOCNXT)
      QLIMRL = PO(LOCNXT+1)
      IF(QLIMRL.LE.-999.0) QLIMRL=1.0E10
      IF(QLIMRL.LT.1.0) THEN
        ELEV2=ELEV1
        QO2=QO1
        QOM=QO1
        GO TO 1650
      ENDIF
C
      LOCRUL = LOCOWS(2)
C
C------------------------------------------------
C  DO THE COMPUTATIONS
C
      LOCPT=LOCPM
      IF(PO(LOCPM).LT.0) LOCPT=-PO(LOCPM)+IOFPRM
      NRUL=PO(LOCPT)
      CALL RULE26(W(LOCRUL),PO(LESSTO),PO(LESELV),
     & PO(LOCPT+1),PO(LOCPT+NRUL+1))
C
C------------------------------------------------
C  THAT'S ALL THERE IS!
C
 1650 IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XSO426 ***')
      RETURN
      END
