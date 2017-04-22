C MEMBER MTRX21
C  (from old member FCMTRX21)
C
      SUBROUTINE MTRX21(C,D,XX,NN2,J)
C
C      THIS SUBROUTINE COMPUTES THE GAUSSIAN ELIMINATION MATRIX SOLUTION
C      OF COEFFICIENT MATRIX ASSOCIATED WITH NEWTON-RAPHSON
C      ITERATIVE SOLUTION OF UNSTEADY FLOW EQUATIONS
C
C           THIS SUBROUTINE WAS WRITTEN ORIGINALLY BY:
C           DR. DANNY FREAD   HRL   APRIL 1978
C
C           THIS SUBROUTINE WAS MODIFIED TO MEET VER. NO. 5 STANDARDS
C           OF THE NWSRFS BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/XCED21/NBDXCD,NCMXCD,NFRXCD,NICXCD,NINXCD,NONXCD,MTXDV
C
      DIMENSION C(1),D(4,1),XX(1),NN2(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/mtrx21.f,v $
     . $',                                                             '
     .$Id: mtrx21.f,v 1.1 1995/09/17 18:56:10 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HMTRX,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      N2=NN2(J)
      MM=N2-2
      K=0
      DO 10 I=2,MM,2
      IM=I-1
      IP=I+1
      KX=K+1
      KY=K+2
      BTM=D(KX,IM)
      IF(BTM.EQ.0) MTXDV=MTXDV+1
      IF(BTM.EQ.0) BTM=1.00
      B=-D(1,I)/BTM
      D(2,I)=B*D(KY,IM)+D(2,I)
      C(I)=B*C(IM)+C(I)
      B=-D(1,IP)/BTM
      D(2,IP)=B*D(KY,IM)+D(2,IP)
      C(IP)=B*C(IM)+C(IP)
      BTM2=D(2,I)
      IF(BTM2.EQ.0) MTXDV=MTXDV+1
      IF(BTM2.EQ.0) BTM2=1.00
      B=-D(2,IP)/BTM2
      D(3,IP)=B*D(3,I)+D(3,IP)
      D(4,IP)=B*D(4,I)+D(4,IP)
      C(IP)=B*C(I)+C(IP)
      K=2
   10 CONTINUE
      I=N2-1
      BTM3=D(3,I)
      IF(BTM3.EQ.0) MTXDV=MTXDV+1
      IF(BTM3.EQ.0) BTM3=1.00
      B=-D(3,N2)/BTM3
      D(4,N2)=B*D(4,I)+D(4,N2)
      C(N2)=B*C(I)+C(N2)
      BTM4=D(4,N2)
      IF(BTM4.EQ.0) MTXDV=MTXDV+1
      IF(BTM4.EQ.0) BTM4=1.00
      XX(N2)=C(N2)/BTM4
   20 BTM5=D(1+K,I)
      IF(BTM5.EQ.0) MTXDV=MTXDV+1
      IF(BTM5.EQ.0) BTM5=1.00
      XX(I)=(C(I)-D(2+K,I)*XX(I+1))/BTM5
      IF(I-1) 50,50,30
   30 II=I-1
      BTM6=D(2,II)
      IF(BTM6.EQ.0) MTXDV=MTXDV+1
      IF(BTM6.EQ.0) BTM6=1.00
      XX(II)=(C(II)-D(4,II)*XX(II+2)-D(3,II)*XX(II+1))/BTM6
      I=I-2
      IF(I-1) 40,40,20
   40 K=0
      GO TO 20
   50 CONTINUE
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
