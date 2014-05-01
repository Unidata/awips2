C MEMBER FRCTP
C  (from old member FCFRCTP)
C  DESC -- FINDS RATING CURVES USED BY SEGMENT BY SCANNING T & P ARRAYS
C
C...........................................
C
      SUBROUTINE FRCTP(RCOPY,NCOPY,T,MT,P,MP,IER)
C
C...........................................
C
C  FRCTP LOOKS IN THE T ARRAY FOR A SEGMENT FOR ANY OPERATIONS THAT
C  MAY USE A RATING CURVE (CURRENTLY ONLY OPS 12,23 & 25). IF ANY OF
C  THOSE OPS FOUND, FIND ID, IF ANY, IN THE P ARRAY.
C
C  THE LIST CREATED BY FRCTP MAY CONTAIN REDUNDANT ID'S.
C
C  ARGUMENT LIST:
C    RCOPY  (OUT) - ARRAY HOLDING LIST OF RC'S.
C    NCOPY  (OUT) - NUMBER OF RC'S FOUND.
C        T  (IN)  - T ARRAY FOR SEGMENT
C       MT  (IN)  - DIMENSION OF T ARRAY
C        P  (IN)  - P ARRAY FOR SEGMENT
C       MP  (IN)  - DIMENSION OF P ARRAY
C      IER  (OUT) - ERROR RETURN CODE;
C                   =0, NO ERRORS OCCURRED,
C                   =1, MORE THAN 500 RC'S FOUND IN SEGMENT.
C
C----------------------------------------------------------
C  MODIFICATIONS -
C       FEB 85 - SCAN TO FIND OPERATIONS 21 (DWOPER) AND 26 (RES-SNGL)
C                ALSO, OP 26 CAN HAVE MULTIPLE RC'S, SO CHANGED
C                STRATEGY FOR LOOKING FOR RC'S.
C       FEB 92 - ADD IN ABILITY TO WORK WITH DIFFERENT VERSION
C                NUMBERS OF DWOPER OPERATION (OPER # 21) - JTO
C
C................................................................
C
C  ORIGINALLY PROGRAMMED BY -- JOE OSTROWSKI -- HRL --810513
C
C   ADDITION OF SCAN FOR OPERATION 25 ADDED 820616 -- JTOSTROWSKI -- HRL
C
C...............................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      INTEGER T
      DIMENSION T(MT),P(MP),RCOPY(2,50),LOCP(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/frctp.f,v $
     . $',                                                             '
     .$Id: frctp.f,v 1.1 1995/09/17 19:23:56 dws Exp $
     . $' /
C    ===================================================================
C
      DATA N12,N21,N23,N25,N26,RTCV/12,21,23,25,26,4HRTCV/
C
      IBUG = 0
      IER = 0
      NCOPY = 0
C
      IF (IFBUG(RTCV).EQ.1) IBUG = 1
      IF (ITRACE.GE.1) WRITE(IODBUG,600)
  600 FORMAT(21H  *** ENTER FRCTP ***)
C
      LOC = 1
   10 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,601) LOC,T(LOC)
  601 FORMAT(17H  LOCATION IN T =,I4,14H  OPER. NO. = ,I4)
C
      IF (T(LOC).EQ.-1) GO TO 9999
      NLOC = T(LOC+1)
      IF (IBUG.GE.1) WRITE(IODBUG,602) NLOC
  602 FORMAT(28H  LOCATION OF NEXT OP NO. = ,I4)
C
      IF (T(LOC).EQ.N12) GO TO 120
      IF (T(LOC).EQ.N21) GO TO 210
      IF (T(LOC).EQ.N23) GO TO 230
      IF (T(LOC).EQ.N25) GO TO 250
      IF (T(LOC).EQ.N26) GO TO 260
      GO TO 1000
C
C.......................
C  OPERATION 12 FOUND
C
  120 CONTINUE
      NLOOK = 1
      LOCP(1) = T(LOC+5)
      GO TO 500
C
C.......................
C  OPERATION 21 FOUND
C
  210 CONTINUE
C
C  DETERMINE VERSION NO OF DWOPER OPERATION
C
      IF (P(T(LOC+2)) .GT. 1) GO TO 215
C
C  VERSION 1.0 OF DWOPER
C
      NLOOK = 1
      LOCP(1) = T(LOC+4)
      GO TO 500
C
C  VERSION 2.0 OF DWOPER
C
  215 CONTINUE
      NLOOK = T(LOC+31)
      IF (NLOOK .EQ. 0) GO TO 1000
C
C  FOR OP 21, LOCATION OF FIRST RC ID IN P ARRAY IS HELD IN POS'N 5
C   OF OP 21'S PORTION OF THE T ARRAY.  ALL OTHER RC ID'S ARE HELD
C   IN SUCCESSIVE POSITIONS OF THE P ARRAY
C
      LRCINP = T(LOC+4)
      DO 218 I=1,NLOOK
      LOCP(I) = LRCINP+(I-1)*2
  218 CONTINUE
      GO TO 500
C
C........................
C  OPERATION 23 FOUND
C
  230 CONTINUE
      NLOOK = 1
      LOCP(1) = T(LOC+6)
      GO TO 500
C
C...........................
C  OPERATION 25 FOUND
C
  250 CONTINUE
      NLOOK = 1
      LOCP(1) = T(LOC+3)
      GO TO 500
C
C...........................
C  OPERATION 26 FOUND
C
  260 CONTINUE
      NLOOK = T(LOC+5)
      IF (NLOOK .EQ. 0) GO TO 1000
C
C  FOR OP 26, LOCATIONS OF ALL RATING CURVE ID'S ARE HELD IN
C   OP 26'S PORTION OF THE T ARRAY, POS'NS 7 THRU 6+NUM. OF RC'S
C
      DO 265 I=1,NLOOK
      LOCP(I) = T(LOC+5+I)
  265 CONTINUE
      GO TO 500
C
C.....................................................................
C  NOW STORE ALL RC ID'S IN THE RC HOLDING LIST ARRAY
C
  500 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,603) (LOCP(I),I=1,NLOOK)
  603 FORMAT(29H  LOC. IN P ARRAY OF RC IDS =,(T29,5I4))
      DO 550 I=1,NLOOK
      IF (LOCP(I).EQ.0) GO TO 1000
      NCOPY = NCOPY+1
      IF (NCOPY.GT.50) GO TO 8000
      RCOPY(1,NCOPY) = P(LOCP(I))
      RCOPY(2,NCOPY) = P(LOCP(I)+1)
      IF (IBUG.GE.1) WRITE(IODBUG,604) P(LOCP(I)),P(LOCP(I)+1)
  604 FORMAT(18H   RAT CURVE ID = ,2A4)
C
  550 CONTINUE
 1000 CONTINUE
      IF (NLOC.GT.MT) GO TO 9999
      LOC = NLOC
      GO TO 10
C
 8000 CONTINUE
      WRITE(IPR,1601)
 1601 FORMAT(10X,41HMORE THAN FIFTY(50) RATING CURVES IN THIS,
     .  09H SEGMENT./22X,39H CALL HSD OR HRL FOR PROGRAM EXPANSION.)
      CALL ERROR
      IER = 1
C
 9999 CONTINUE
      IF (ITRACE.GE.1) WRITE(IODBUG,699)
  699 FORMAT(15H  ** EXIT FRCTP)
      RETURN
      END
