C MEMBER X17N26
C  (from old member FCX17N26)
C
      SUBROUTINE X17N26(ITYPE,W,PO,LOCW,LOCW2,DIFRA1,DIFRA2,IPD,NMISS,
     .                  NFILL,ISW)
C                             LAST UPDATE: 01/03/95.10:11:13 BY $WC30KH
C
C-------------------------------------------------------------------
C  SUBROUTINE TO INTERPOLATE DIFFERENCES/RATIOS BETWEEN OBSERVED
C  END POINTS. ARGUMENT LIST:
C
C   ITYPE  - WHAT TO INTERPOLATE, => 0 - RATIOS
C                                 => 1 - DIFFERENCES
C   W      - WORK ARRAY HOLDING VALUES TO BE ALTERED
C   PO     - PARAMETER ARRAY
C   LOCW   - LOCATION IN W ARRAY OF VALUES TO BE ALTERED
C   LOCW2  - LOCATION IN W ARRAY OF ELEVATIONS/STORAGES TO BE
C            COMPUTED (IF REQUESTED BY ISW VALUE)
C   DIFRA1 - DIFFERENCE/RATIO AT BEGINNING OF INTERP INTERVAL
C   DIFRA2 - DIFFERENCE/RATIO AT END OF INTERP INTERVAL
C   IPD    - PERIOD OF INTERP INTERVAL
C   NMISS  - NO. OF PERIODS TO BE FILLED
C   NFILL  - NO. OF VALUES PREVIOUSLY FILLED
C   ISW    - INDICATOR FOR ELEV/STORAGE COMPUTATION, => 0 NO CONVERSION
C                                                    => 1 STOR -> ELEV
C                                                    => 2 ELEV -> STOR
C-----------------------------------------------------------------------
C  ORIGINALLY PROGRAMMED BY - JOE OSTROWSKI - HRL - SEPT 1984
C-----------------------------------------------------------------------
C
      INCLUDE 'common/exg26'
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
C
       DIMENSION W(1),PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/x17n26.f,v $
     . $',                                                             '
     .$Id: x17n26.f,v 1.1 1995/09/17 19:06:21 dws Exp $
     . $' /
C    ===================================================================
C
C
C  PRINT TRACE IF REQUESTED
C
      IF (IBUG .GE. 1) WRITE(IODBUG,600)
  600 FORMAT('  *** ENTER X17N26 ***')
C
C-------------------------------------
C  START THE INTERPOLATION PROCESS. FIRST COMPUTE THE AVERAGE DIFFERENCE
C
      PDDIFF = (DIFRA2-DIFRA1)/(NMISS+1)
C
      NST = IPD - NMISS + NFILL
      NEND = IPD - 1
      IF (NEND .LE. 0) GO TO 900
      ADDN = DIFRA1 + NFILL*PDDIFF
C
C  SET VALUES FOR ELEV/STORAGE CONVERSION IF REQUESTED
C
      IF (ISW .EQ. 0) GO TO 10
C
C  DEFAULT IS STORAGE -> ELEVATION
C
      LOCP1 = LESSTO
      LOCP2 = LESELV
C
      IF (ISW .EQ. 1) GO TO 10
C
C  HERE IT'S ELEVATION -> STORAGE
C
      LOCP1 = LESELV
      LOCP2 = LESSTO
C
   10 CONTINUE
C
      IF (IBUG .GE. 2) WRITE(IODBUG,650) ITYPE,DIFRA1,DIFRA2,PDDIFF,
     .       IPD,NMISS,NFILL,ISW,NST,NEND
  650 FORMAT('  ITYPE    DIFRA1    DIFRA2  PD  DIFF  IPD  NMISS  NFILL',
     .       /I5,F10.2,F10.2,F10.2,I5,I6,I8,/
     .'   ISW  NST  NEND',/3I5)
      IF (IBUG .GE. 2) WRITE(IODBUG,655) (W(LOCW+K-1),K=NST,NEND)
  655 FORMAT('  BEFORE INTERPOLATION '/(6F12.3))
C
C  NOW DISTRIBUTE DIFFERENCES OR RATIOS
C
      DO 100 J=NST,NEND
      ADDN = ADDN + PDDIFF
C
C  DIFFERENCES ARE ADDED, RATIOS ARE MULTIPLIED
C
      IF (ITYPE .EQ. 0) W(LOCW+J-1) = W(LOCW+J-1) * ADDN
      IF (ITYPE .EQ. 1) W(LOCW+J-1) = W(LOCW+J-1) + ADDN
      IF (ISW.EQ.0 .AND. W(LOCW+J-1).LE.0.0) W(LOCW+J-1)=0.01
C
C  CONVERT ELEV/STORAGE IF SWITCH SAYS TO
C
      IF (ISW .GT. 0) CALL NTER26(W(LOCW+J-1),W(LOCW2+J-1),PO(LOCP1),
     .                            PO(LOCP2),NSE,IFLAG,NTERP,IBUG)
      IF (ISW.EQ.2 .AND. W(LOCW+J-1).LE.0.0) W(LOCW+J-1)=0.01
C
  100 CONTINUE
C
      IF (IBUG .GE. 2) WRITE(IODBUG,690) (W(LOCW+K-1),K=NST,NEND)
  690 FORMAT('  AFTER INTERPOLATION '/(6F12.3))
C
C  THAT'S IT!
C
  900 CONTINUE
      IF (IBUG .GE. 1) WRITE(IODBUG,699)
  699 FORMAT('    *** EXIT X17N26 ***')
      RETURN
      END
