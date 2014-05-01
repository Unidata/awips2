C MEMBER XU1526
C  (from old member FCXU1526)
C
      SUBROUTINE XU1526(PO,CO,W,IDPT,LOCOWS)
C--------------------------------------------------------------------
C  SUBROUTINE TO SET PARAMETERS AND DATA LOCATIONS FOR THE USE OF
C  THE SUBROUTINE FOR SUMMING INFLOWS AND DETERMINING A 'FLOOD'
C  SITUATION AS DEFINED BY THE PARAMETERS.
C----------------------------------------------------------------------
C  WRITTEN BY JOE OSTROWSKI - HRL - JULY 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/resv26'
      INCLUDE 'common/sumi26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/xqin26'
      INCLUDE 'common/fprog'
C
      DIMENSION PO(1),CO(1),W(1),IDPT(1),LOCOWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xu1526.f,v $
     . $',                                                             '
     .$Id: xu1526.f,v 1.2 1999/04/23 17:04:05 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XU1526 ***')
C
C  SET BEGINNING STORAGE BASED ON TYPE OF RUN, CARRYOVER
C  VALUE FOR SIMULATED RUN, LAST OBSERVED OR COMPUTED STORAGE FOR
C  ADJUSTED RUN
C
      BGNSTL = CO(6)
      IF (IFCST.EQ.0) GO TO 10
C
C  MUST FIND LOCATION OF LAST OBSERVED OR COMPUTED STORAGE IN THE WORK
C  ARRAY.
C
      LOCOBS = 3*NDD*NTIM24 + LOBSTO
      BGNSTL = W(LOCOBS)
C
   10 CONTINUE
C
C  NOW GET PARAMETER INFORMATION FOR THIS UTILITY
C
C
      DO 550 I=1,NSUDEF
      LOCNUM = LOCPTR + (I-1)*4 + 1
      IBASE=IFIX(PO(LOCNUM))/10
      IF (IBASE.NE.152) GO TO 550
      INUM=I
      GO TO 551
  550 CONTINUE
C
C     INUM = ISUPT(2,15)
 551  LOCP = PO(LOCPTR + (INUM-1)*4 + 2)
C
      SPCQOM = PO(LOCP)
C
C  CONVERT MAX ELEVATION IN PARMS TO MAX STORAGE.
C
      ELMAX = PO(LOCP+1)
      CALL NTER26(ELMAX,STORMX,PO(LESELV),PO(LESSTO),NSE,IFLAG,NTERP,
     . IBUG)
C
C  NOW PERFORM THE SUMMATION AND ASSOCIATED TASKS.
C
      LOCSUM = LOCOWS(3)
      CALL SUMN26(W(LPTQIM),W(LOCSUM),PO(LESSTO),PO(LESELV))
C
 1000 CONTINUE
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XU1526 ***')
C
      RETURN
      END
