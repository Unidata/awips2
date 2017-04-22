C MEMBER XS0726
C  (from old member FCXS0726)
C
      SUBROUTINE XS0726(SUNUM,PO,W,LOCOWS)
C---------------------------------------------------------------------
C  SUBROUTINE TO GET PARAMETERS FOR SCHEME #7 - POOL ELEVATION VS.
C  DISCHARGE.
C---------------------------------------------------------------------
C  WRITTEN - JOE OSTROWSKI - HRL - AUGUST 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),W(1),LOCOWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs0726.f,v $
     . $',                                                             '
     .$Id: xs0726.f,v 1.1 1995/09/17 19:06:57 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XS0726 ***')
C
C  GET POINTERS FOR THIS SCHEME
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,IX,IX)
C
C  SET EXECUTION FLAG
C
      LOCEX = IORD*3
      W(LOCEX) = 1.01
C
C  GET THE POOLVSQ CURVE
C
      INPQ = PO(LOCPM)
      NSO = INPQ
      IF (INPQ .LT. 0) NSO=-INPQ
      LOCEL = LOCPM + 1
      LOCQO = LOCEL + NSO
C
C  SEE IF RULE CURVE IS NEEDED
C
      LOCRUL = LOCQO + NSO
      IRTYPE = PO(LOCRUL)
      IF (IRTYPE.EQ.0) GO TO 500
C
C  GET RULE CURVE DEFINITION
C
C      LOCRUL = LOCRUL + 1
      CALL XFRU26(PO,LOCRUL,W,LOCOWS)
C
C  NOW COMPUTE STORAGES CORRESPONDING TO ELEVATIONS
C
  500 CONTINUE
C
C  IF WE'VE ALREADY PLACED THESE VALUES IN THE WORK ARRAY, NO NEED TO
C  COMPUTE THEM UNLESS WE'RE WORKING WITH RULE CURVE ELEVATIONS, IN
C  CASE WE MUST UPDATE THE RULE CURVE ELEVATION FOR THE PERIOD.
C
      LOCSTR = LOCOWS(7)
      ISU = IBASE*10 + LEVEL
      IF (MRLOC(2).EQ.ISU .AND. IRTYPE.EQ.0) GO TO 700
      MRLOC(2) = ISU
C
      DO 600 I=1,NSO
      ELT = PO(LOCEL+I-1)
      IF (IFMSNG(ELT).EQ.1) ELT = RULEL2
      CALL NTER26(ELT,W(LOCSTR+I-1),PO(LESELV),PO(LESSTO),NSE,IFLAG,
     .            NTERP,IBUG)
  600 CONTINUE
C
C  NOW HAVE ALL NECESSARY INFO, COMPUTE THE PERIOD RESULTS.
C
  700 CONTINUE
      ITEMP = NTERPQ
      NTERPQ = -1
      IF (INPQ .LT. 0) NTERPQ=0
      CALL EVSQ26(W(LOCSTR),PO(LOCEL),PO(LOCQO),PO(LESSTO),PO(LESELV))
      NTERPQ = ITEMP
C
C  THAT'S IT. NO CARRYOVER NEEDS TO BE SAVED.
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XS0726 ***')
      RETURN
      END
