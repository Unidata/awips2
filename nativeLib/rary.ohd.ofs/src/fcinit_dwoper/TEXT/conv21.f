C MEMBER CONV21
C  (from old member FCPIN21)
C
C DES -- THIS SUBROUTINE REAL ARRAY VALUES INTO INTEGER ARRAY VALUES.
C
      SUBROUTINE CONV21(I,J,N)
C
C           THE FUNCTION OF THIS SUBROUTINE IS TO CONVERT THE REAL
C           ARRAY TO INTEGERS
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION J(N),I(N),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_dwoper/RCS/conv21.f,v $
     . $',                                                             '
     .$Id: conv21.f,v 1.1 1995/09/17 18:47:43 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HCONV,4H21  /
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
C
      DO 10 K=1,N
   10 J(K)=I(K)
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,9H  EXITED.)
      RETURN
      END
