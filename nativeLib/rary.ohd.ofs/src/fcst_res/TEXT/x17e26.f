C MEMBER X17E26
C  (from old member FCX17E26)
C
C @PROCESS LVL(77)
      SUBROUTINE X17E26(W,PO,LOCSTO,LOCEL,LOCQOM,LOCQIM,PRESTO,IBEGIN,
     .                  IEND)
C                             LAST UPDATE: 01/09/95.09:42:40 BY $WC30KH
C
C----------------------------------------------------
C  SUBROUTINE TO COMPUTE STORAGE VALUES USING THE CONTINUITY
C  EQUATION AND THEN CONVERTING THE STORAGE TO THE ASSOCIATED
C  POOL ELEVATION.
C----------------------------------------------------------
C   ARGUMENT LIST:
C
C     W      - ARRAY HOLDING STORAGES, ELEVATIONS, MEAN DISCHARGES,
C              AND INFLOWS.
C     PO     - ARRAY HOLDING STORAGE/ELEVATION CURVE
C     LOCSTO - LOCATION OF START OF STORAGES IN W ARRAY
C     LOCEL  -    "      "   "   " ELEVATIONS IN W ARRAY
C     LOCQOM -    "      "   "   " MEAN DISCHARGES IN W ARRAY
C     LOCQIM -    "      "   "   "  MEAN INFLOWS IN W ARRAY
C     PRESTO - STORAGE ONE PERIOD BEFORE 'IBEGIN'
C     IBEGIN - FIRST PERIOD TO COMPUTE STORAGE/ELEVATIONS
C     IEND   - LAST PERIOD TO COMPUTE STORAGE/ELEVATIONS
C-----------------------------------------------------------------------
C  ORIGINALLY PROGRAMMED BY - JOE OSTROWSKI - HRL - SEPT 1984
C-------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
C
      DIMENSION W(1),PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/x17e26.f,v $
     . $',                                                             '
     .$Id: x17e26.f,v 1.1 1995/09/17 19:06:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C  PRINT TRACE IF REQUESTED.
C
      IF (IBUG .GE. 1) WRITE(IODBUG,600)
  600 FORMAT('   *** ENTER X17E26 ***')
C
      DO 100 I=IBEGIN,IEND
      DELS = W(LOCQIM+I-1) - W(LOCQOM+I-1)
      PRESTO = PRESTO + DELS
      IF(PRESTO.LE.0.0) PRESTO=0.0
      W(LOCSTO+I-1) = PRESTO
      IF(PRESTO.LE.0.0) THEN
        W(LOCEL+I-1)=PO(LESELV)
        GO TO 100
      END IF
C
C  CONVERT STORAGE TO ELEVATION
C
      CALL NTER26(PRESTO,W(LOCEL+I-1),PO(LESSTO),PO(LESELV),NSE,IFLAG,
     .            NTERP,IBUG)
C
  100 CONTINUE
C
C  TRACE HERE IF REQUESTED
C
      IF (IBUG .GE. 1) WRITE(IODBUG,699)
  699 FORMAT('    *** EXIT X17E26 ***')
      RETURN
      END
