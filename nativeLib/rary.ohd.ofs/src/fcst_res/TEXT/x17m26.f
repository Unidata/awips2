C MEMBER X17M26
C  (from old member FCX17M26)
C
      SUBROUTINE X17M26(W,LOCQOM,LOCQO,QOCO,IBEGIN,IEND)
C--------------------------------------------------------
C  SUBROUTINE TO COMPUTE MEAN DISCHARGES BY AVERAGING
C  INSTANTANEOUS DISCHARGES.
C-----------------------------------------------------------------------
C  ARGUMENT LIST
C
C   W      - ARRAY HOLDING BOTH INST. AND MEAN DISCHARGES
C   LOCQOM - LOCATION OFSTART OF MEAN DISCHARGES IN W ARRAY
C   LOCQO  - LOCATION OF START OF INST. DISCHARGES IN W ARRAY
C   QOCO   - INST. DISCHARGE ONE PERIOD BEFORE 'IBEGIN'
C   IBEGIN - FIRST PERIOD IN COMPUTATIONS
C   IEND   - LAST PERIOD IN COMPUTATIONS
C-----------------------------------------------------------------------
C  ORIGINALLY PROGRAMMED BY - JOE OSTROWSKI - HRL - SEPT 1984
C-----------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
C
      DIMENSION W(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/x17m26.f,v $
     . $',                                                             '
     .$Id: x17m26.f,v 1.2 1996/07/12 14:04:59 dws Exp $
     . $' /
C    ===================================================================
C
C
C  TRACE HERE IF REQUESTED
C
      IF (ITRACE .GE. 1) WRITE(IODBUG,600)
  600 FORMAT('   *** ENTER X17M26 ***')
C
C  COMPUTE MEAN VALUE FOR PERIOD IBEGIN
C
      W(LOCQOM+IBEGIN-1) = (QOCO + W(LOCQO+IBEGIN-1))/2.0
C
C  IF ONLY ONE PERIOD IS REQUESTED FOR COMPUTATION, END IT!
C
      IST = IBEGIN + 1
      IF (IST .GT. IEND) GO TO 900
C
C  AVERAGE INST. VALUES OUT TO 'IEND'
C
      DO 100 I=IST,IEND
      W(LOCQOM+I-1) = (W(LOCQO+I-2) + W(LOCQO+I-1))/2.0
  100 CONTINUE
C
C  TRACE HERE IF REQUESTED
C
  900 CONTINUE
      IF (ITRACE .GE. 1) WRITE(IODBUG,699)
  699 FORMAT('    *** EXIT X17M26 ***')
      RETURN
      END
