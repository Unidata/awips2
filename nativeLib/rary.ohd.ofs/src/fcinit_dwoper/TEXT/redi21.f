C MEMBER REDI21
C  (from old member FCPIN21)
C
      SUBROUTINE REDI21(INT,N,ICHK,IVAR)
C
C           THE FUNCTION OF THIS SUBROUTINE IS TO READ THE INTEGER
C           INPUT DATA.
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
      DIMENSION SNAME(2),INT(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_dwoper/RCS/redi21.f,v $
     . $',                                                             '
     .$Id: redi21.f,v 1.1 1995/09/17 18:47:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HREDI,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      IF(ICHK.EQ.1) GO TO 200
C
C        READ INTEGER ARRAY
C
      READ(IN,100) INT
  100 FORMAT(7I10)
      IF(IBUG.EQ.1) WRITE(IODBUG,110) INT
  110 FORMAT(1H ,10X,7I10)
      GO TO 500
C
C        READ INTEGER VARIABLE
C
  200 READ(IN,100) IVAR
      IF(IBUG.EQ.1) WRITE(IODBUG,110) IVAR
C
  500 IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
