C MEMBER LCAT21
C  (from old member FCLCAT21)
C
C DESC -- THE FUNCTION OF THIS SUBPROGRAM IS TO FIND THE LOCATION OF A
C DESC    GIVEN VARIABLE IN THE ARRAY.
C                             LAST UPDATE: 03/03/94.13:52:28 BY $WC30JL
C
C @PROCESS LVL(77)
C
      FUNCTION LCAT21(KA,JA,NUM)
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDBUG(20)
C
      DIMENSION SNAME(2),NUM(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_dwoper/RCS/lcat21.f,v $
     . $',                                                             '
     .$Id: lcat21.f,v 1.2 1996/01/17 19:56:02 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HLCAT,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
C
C
      IF(JA.GT.1) GO TO 10
      LCAT21=KA
      GO TO 100
   10 NSUM=0
      JM1=JA-1
      DO 20 I=1,JM1
   20 NSUM=NSUM+NUM(I)
      LCAT21=NSUM+KA
C
  100 IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
