C MEMBER ITRP26
C  (from old member FCITRP26)
C ......................................................................
      SUBROUTINE ITRP26(XX,IPT,X,NPT)
C ......................................................................
C THIS SUBROUTINE DETERMINES THE LOCATION OF QUANTITY XX
C IN THE CURVE (X(I), I=1,NPT)
C ......................................................................
C THIS SUBROUTINE WAS ORIGINALLY PROGRAMMED BY
C     KUANG S. HSU
C     DECEMBER, 1988
C ......................................................................
C SUBROUTINE ITRP26 IS IN
C ......................................................................
C VARIABLES PASSED TO OR FROM THIS SUBROUTINE ARE DEFINED AS FOLLOWS:
C     XX -- INPUT QUANTITY TO BE LOCATED IN CURVE X(I)
C     IPT -- OUTPUT LOWER NUMBER OF THE SEGMENT WHERE XX IS LOCATED
C     X -- INPUT DISCRETE CURVE TO BE SEARCHED THROUGH
C     NPT -- INPUT TOTAL NUMBER OF DATA POINTS IN DISCRETE CURVE X
C
      DIMENSION X(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/itrp26.f,v $
     . $',                                                             '
     .$Id: itrp26.f,v 1.1 1995/09/17 19:05:46 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (XX .GT. X(1)) GO TO 100
      IPT = 0
      GO TO 900
  100 IF (XX .LT. X(NPT)) GO TO 110
      IPT = NPT+1
      GO TO 900
  110 CONTINUE
      DO 120 I=1,NPT
      IPT = I
      IPT1 = I+1
      IF (XX .LT. X(IPT1)) GO TO 900
  120 CONTINUE
  900 CONTINUE
      RETURN
      END
