C MODULE FSERCH
C-----------------------------------------------------------------------
C
      SUBROUTINE FSERCH (NUMOP,OPNAME,LOCP,P,MP)
C.
C  THIS ROUTINE SEARCHES THE P ARRAY TO FIND AN OPERATION.
C
C  IF LOCP.EQ.0 THE OPERATION IS LOCATED BY BOTH NUMBER AND NAME.
C  IF LOCP.GT.0 THE OPERATION IS LOCATED BY NUMBER ONLY.
C  SEARCH BEGINS AT LOCATION 1 UNLESS LCOP.GT.1 THEN SEARCH BEGINS AT
C   THE LOCATION OF THE NEXT OPERATION IN THE P ARRAY.
C  LOCP RETURNED IS THE LOCATION OF THE FIRST ENTRY IN THE SECOND PART
C   OF THE P ARRAY FOR THE OPERATION IF FOUND. LOCP=0 IF OPERATION NOT 
C   FOUND.
C
C   ROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL 6/1979
C
      DIMENSION OPNAME(2)
      DIMENSION P(MP)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fserch.f,v $
     . $',                                                             '
     .$Id: fserch.f,v 1.3 2002/10/10 15:01:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'ENTER FSERCH'
C
      IBUG=IFBUG('SEGI')
C
      IF (IBUG.EQ.1) WRITE (IODBUG,10) NUMOP,OPNAME,LOCP
10    FORMAT (' IN FSERCH : NUMOP=',I3,' NAME=',2A4,' LOCP=',I5)
C
C  DETERMINE WHERE THE SEARCH SHOULD START AND IF THE OPERATION NAME
C  SHOULD BE USED TO LOCATE THE OPERATION
      IF (LOCP.EQ.0) THEN
C     START AT BEGINNING AND USE NAME
         LOCP=1
         NAME=1
         GO TO 20
         ENDIF
C
C  USE NUMBER ONLY
      NAME=0
      IF (LOCP.EQ.1) GO TO 20
      IF (LOCP.GT.MP) GO TO 50
      IF (LOCP.LT.8) GO TO 50
      LOCP=P(LOCP-6)
C
C  SEARCH THE P ARRAY FOR THE OPERATION
20    NUM=P(LOCP)
CMGM  If P array is empty then operation not found. Previously there
C     was an infinite loop for an incomplete segment.
      IF (NUM.EQ.0) GO TO 50
CMGMend  
      IF (NUM.EQ.-1) GO TO 50
      IF (NUM.NE.NUMOP) GO TO 40
      IF (NAME.EQ.0) GO TO 30
      IF (P(LOCP+2).NE.OPNAME(1).OR.P(LOCP+3).NE.OPNAME(2)) GO TO 40
C
C  OPERATION FOUND
30    LOCP=LOCP+7
      IF (NAME.EQ.1) GO TO 60
      OPNAME(1)=P(LOCP-5)
      OPNAME(2)=P(LOCP-4)
      GO TO 60
C
C  INCREMENT TO THE NEXT OPERATION
40    LOCP=P(LOCP+1)
      IF (LOCP.LE.MP) GO TO 20
C
C  OPERATION NOT FOUND
50    LOCP=0
C
60    IF (IBUG.EQ.1) WRITE (IODBUG,10) NUMOP,OPNAME,LOCP
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'EXIT FSERCH'
C
      RETURN
C
      END
