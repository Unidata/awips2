C MEMBER MLTS26
C  (from old member FCMLTS26)
C
C DESC DETERMINE MULTIPLE DEFINITION OF TIME-SERIES
C------------------------------------------------------------------
      SUBROUTINE MLTS26(TSID,DTYPE,IDT,IO,IERM)
C------------------------------------------------------------------
C  ROUTINE TO SEE IF TIME SERIES SPECIFIED HAS BEEN MULTIPLY DEFINED
C------------------------------------------------------------------
C
C  AN ERROR IS CALLED IF:
C   1) A MATCH IS FOUND AND EITHER I/O FLAG IS OUTPUT.
C
C  IF NO MATCH IS FOUND, STORE IDENTIFYING INFO IN /TS26/.
C
C----------------------------------------------------------------------
C
      INCLUDE 'common/ts26'
C
      DIMENSION TSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/mlts26.f,v $
     . $',                                                             '
     .$Id: mlts26.f,v 1.1 1995/09/17 18:51:50 dws Exp $
     . $' /
C    ===================================================================
C
      IERM = 0
C
C  IF NO TIME-SERIES INPUT, JUST ADD THIS ONE TO LIST.
C
      IF (N26TS.EQ.0) GO TO 900
C
      DO 500 I=1,N26TS
C
C  SEE IF ID'S MATCH
C
      IF (TSID(1).NE.T26ID(1,I).OR.TSID(2).NE.T26ID(2,I)) GO TO 500
C
C  SEE IF DATA TYPES MATCH
C
      IF (DTYPE.NE.D26TY(I)) GO TO 500
C
C  SEE IF TIME INTERVAL MATCHES
C
      IF (IDT.NE.I26DT(I)) GO TO 500
C
C  WE NOW HAVE A MATCH OF TS IDENTIFYING INFO
C  IF EITHER OF THE TIME-SERIES ARE OUTPUT, CALL IT AN ERROR
C
      IF (IO26(I).EQ.0.AND.IO.EQ.0) GO TO 999
C
      CALL STER26(69,1)
      GO TO 999
C
  500 CONTINUE
C
C  ADD THE IDENTIFYING INFO TO THE LIST
C
  900 CONTINUE
      N26TS = N26TS + 1
      T26ID(1,N26TS) = TSID(1)
      T26ID(2,N26TS) = TSID(2)
      D26TY(N26TS) = DTYPE
      I26DT(N26TS) = IDT
      IO26(N26TS) = IO
C
  999 CONTINUE
      RETURN
      END
