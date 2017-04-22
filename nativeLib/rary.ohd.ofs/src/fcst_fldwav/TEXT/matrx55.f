      SUBROUTINE MATRX55(C,D,XX,N2,K15)
C
C      THIS SUBROUTINE COMPUTES THE GAUSSIAN ELIMINATION MATRIX SOLUTION
C          OF COEFFICIENT MATRIX ASSOCIATED WITH NEWTON-RAPHSON
C          ITERATIVE SOLUTION OF UNSTEADY FLOW EQUATIONS
C
      DIMENSION C(K15),D(4,K15),XX(K15)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/matrx55.f,v $
     . $',                                                             '
     .$Id: matrx55.f,v 1.1 1999/04/23 18:08:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      MM=N2-2
      K=0
      DO 10 I=2,MM,2
      IM=I-1
      IP=I+1
      KX=K+1
      KY=K+2
      B=-D(1,I)/D(KX,IM)
      D(2,I)=B*D(KY,IM)+D(2,I)
      C(I)=B*C(IM)+C(I)
      B=-D(1,IP)/D(KX,IM)
      D(2,IP)=B*D(KY,IM)+D(2,IP)
      C(IP)=B*C(IM)+C(IP)
      B=-D(2,IP)/D(2,I)
      D(3,IP)=B*D(3,I)+D(3,IP)
      D(4,IP)=B*D(4,I)+D(4,IP)
      C(IP)=B*C(I)+C(IP)
      K=2
   10 CONTINUE
      I=N2-1
      B=-D(3,N2)/D(3,I)
      D(4,N2)=B*D(4,I)+D(4,N2)
      C(N2)=B*C(I)+C(N2)
      XX(N2)=C(N2)/D(4,N2)
   20 XX(I)=(C(I)-D(2+K,I)*XX(I+1))/D(1+K,I)
      IF(I-1) 50,50,30
   30 II=I-1
      XX(II)=(C(II)-D(4,II)*XX(II+2)-D(3,II)*XX(II+1))/D(2,II)
      I=I-2
      IF(I-1) 40,40,20
   40 K=0
      GO TO 20
   50 CONTINUE
      RETURN
      END
