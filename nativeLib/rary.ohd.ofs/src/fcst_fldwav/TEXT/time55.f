C-----------------------------------------------------------------------
C      THIS IS ONE OF THE SUBPROGRAMS FOR KALMAN FILTER
C      IT DEALS WITH MATRIXS TIME OPERATION   C=A*B
C-----------------------------------------------------------------------
         SUBROUTINE TIME55(A,B,C,N,L,M)
         DIMENSION A(N,L),B(L,M),C(N,M)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/time55.f,v $
     . $',                                                             '
     .$Id: time55.f,v 1.1 1999/04/23 18:08:56 dws Exp $
     . $' /
C    ===================================================================
C
         DO 200 I=1,N
         DO 200 J=1,M
         C(I,J)=0.0
         DO 200 K=1,L
         C(I,J)=C(I,J)+A(I,K)*B(K,J)
200      CONTINUE         
C-----------------------------------------------------------------------
         RETURN
         END
