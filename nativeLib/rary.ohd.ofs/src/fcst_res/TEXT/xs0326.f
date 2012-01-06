C MEMBER XS0326
C  (from old member FCXS0326)
C
      SUBROUTINE XS0326(SUNUM,PO,W,D,LOCWS,IDPT)
C----------------------------------------------------------------------
C  SUBROUTINE TO EXTRACT ALL INFORMATION FROM PO ARRAY AND DATA FROM D
C  ARRAY AND PASS THESE TO THE ACTUAL COMPUTATION ROUTINE FOR THE SETH
C  SCHEME, PREL26.
C----------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL AUGUST 1983
C----------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/exg26'
      INCLUDE 'common/pres26'
C
      DIMENSION PO(1),W(1),D(1),LOCWS(1),IDPT(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs0326.f,v $
     . $',                                                             '
     .$Id: xs0326.f,v 1.1 1995/09/17 19:06:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XS0326 ***')
C
C------------------------------------------------------------------
C  THINGS THAT MUST BE DONE ARE:
C    1) SET THE ELEVATION VALUE
C    2) GET THE LOCATION OF CARRYOVER FOR THIS SCHEME,
C    3) MAKE THE COMPUTATIONS, AND
C    4) UPDATE THE CARRYOVER.
C
C  GET POINTER INFO FOR THIS SCHEME
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C
C  SET EXECUTION POINTER FOR THIS SCHEME
C
      LOCEX = IORD*3
      W(LOCEX) = 1.01
C
C  SET THE LOCATION OF CARRYOVER IN THE WORK ARRAY.
C
      LOCCOW = LOCWS(4)
      LOCCOT = LOCCOW + LOCCO - 1
C
C  NOW SET THE ELEVATION VALUE
C
      IWHICH = PO(LOCPM)
      IF (IWHICH.GT.0) GO TO 100
      IOPT = 0
      IF (ADJRUN .AND. .NOT.FCST) GO TO 500
C
C  ELEVATION IS SET BY USER AND IS STORED IN PO ARRAY
C
      ELEV2 = PO(LOCPM+1)
C
C  SET MISSING VALUE OPTION (NOT NEEDED WHEN USER PRESCRIBES ELEVATION)
C
      IOPT = 0
      GO TO 500
C
C---------------------------------------------
C  VALUE IS TO COME FROM TIME SERIES
C
  100 CONTINUE
      IF (ADJRUN .AND. .NOT.FCST) GO TO 300
C
C  GET DATA VALUE FROM TIME SERIES
C
      LOCTSP = W(LOCEX-1)
C
C  NEED DELTA-T OF TIME SERIES TO SEE IF WE CAN LOOK FOR A VALUE
C  IN THIS TIME PERIOD.
C
      IDTTS = PO(LOCTS+3)
      MULT = IDTTS/MINODT
      IDLOC = NS2/MULT
      LOCTSD = IDPT(LOCTSP) + IDOFST * 24/IDTTS
      ELEV2 = -999.0
      IF (MOD(NS2,MULT).EQ.0) ELEV2 = D(LOCTSD+IDLOC-1)
C
C  GET MISSING VALUE OPTION
C
  300 CONTINUE
      IOPT = PO(LOCPM+1)
C
C--------------------------------------------------------------
C  COMPUTE OUTPUT FOR THIS SCHEME
C
  500 CONTINUE
C
      CALL PREL26(W(LOCCOT),PO(LESSTO),PO(LESELV))
C
C--------------------------------------------------------------
C  UPDATE CARRYOVER IF NEEDED
C
      IF (IWHICH.EQ.0.OR.IOPT.EQ.0) GO TO 9000
C
C  NOW SHIFT CARRYOVER DOWNWARD BY ONE POSITION (I.E. - 2 TO 1, 3 TO 2)
C  IF MORE THAN ONE CARRYOVER VALUE IS SAVED.
C
      IF (IOPT.EQ.1) GO TO 650
      DO 600 I=2,IOPT
      IDOWN = I-1
      W(LOCCOT+IDOWN-1) = W(LOCCOT+I-1)
  600 CONTINUE
C
C  NOW SET THE LAST POSITION
C
  650 CONTINUE
      W(LOCCOT+IOPT-1) = ELEV2
C
C----------------------------------------------
C  ALL DONE WITH 'SETH'
C
 9000 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XS0326 ***')
      RETURN
      END
