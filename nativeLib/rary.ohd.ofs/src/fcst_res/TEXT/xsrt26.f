C MEMBER XSRT26
C  (from old member FCXSRT26)
C
C DESC SORTING ROUTINE USED BY 'FLASHBDS' SCHEME.
C---------------------------------------------------------------------
      SUBROUTINE XSRT26(POSN,SIGELV,NPOS)
C----------------------------------------------------------------------
C  SUBROUTINE TO SORT POSITIONS IN THE POSN ARRAY ACCORDING TO THE
C  CORRESPONDING VALUES HELD IN THE SIGELV ARRAY. USED PRIMARILY FOR
C  THE 'FLASHBOARD' SCHEME TO SORT THE 'SIGRIS' AND 'SIGFAL' ARRAYS.
C----------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - NOV 1983
C----------------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/resv26'
C
      DIMENSION POSN(1),SIGELV(1)
      INTEGER POSN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xsrt26.f,v $
     . $',                                                             '
     .$Id: xsrt26.f,v 1.1 1995/09/17 19:07:08 dws Exp $
     . $' /
C    ===================================================================
C
C
C---------------------------------
C  TRACE OUTPUT IF REQUESTED
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XSRT26 ***')
C
C----------------------------------
C  SORT THE POSITIONS IN ASCENDING ORDER BASED ON THE CORRESPONDING
C  VALUES IN THE SIGELV ARRAY.
C
      IF (NPOS .EQ. 1) GO TO 9000
C
      M = NPOS
      L = NPOS - 1
C
      DO 10 K=1,L
C
C  SET SORTED SWITCH AND SET THE END POINT OF THE NEXT PASS
C
      IND = 0
      M = M-1
C
C  NOW REARRANGE POSITION NOS. IN ASCENDING ORDER IF OUT OF ORDER.
C
      DO 20 N = 1,M
      NN = POSN(N)
      NM = POSN(N+1)
C
      IF (SIGELV(NN) .LE. SIGELV(NM)) GO TO 20
C
      ITEMP = POSN(N)
      POSN(N) = POSN(N+1)
      POSN(N+1) = ITEMP
      IND = 1
C
   20 CONTINUE
C
C  IF IND STILL = 0, THEN ALL VALUES WERE IN ASCENDING ORDER IN THE
C  PREVIOUS PASS THRU THE VALUES AND NO FURTHER SORTING IS NEEDED.
C
      IF (IND.EQ.0) GO TO 9000
C
   10 CONTINUE
C
 9000 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XSRT26 ***')
      RETURN
      END
