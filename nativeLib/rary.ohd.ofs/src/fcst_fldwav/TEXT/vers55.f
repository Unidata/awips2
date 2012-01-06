C-----------------------------------------------------------------------
C       THIS IS ONE OF THE SUBPROGRAMS FOR KALMAN FILTER
C       IT GETS THE INVERSE MATRIX FOR A BOUND MATRIX
C       B=A(-1)
C-----------------------------------------------------------------------
         SUBROUTINE VERS55(A,B,NS)
         DIMENSION A(2*NS,2*NS),B(2*NS,2*NS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/vers55.f,v $
     . $',                                                             '
     .$Id: vers55.f,v 1.1 1999/04/23 18:08:58 dws Exp $
     . $' /
C    ===================================================================
C

         DO 100 I=1,2*NS
         DO 100 J=1,2*NS
100      B(I,J)=0.0
         DO 110 L=1,2*NS
110      B(L,L)=1.0
C-----------------------------------------------------------------------
         DO 500 L=1,NS-1
         IF (ABS(A(2*L-1,2*L-1)) .LE. 10.0**(-10)) THEN
         XY1=A(2*L+1,2*L-1)
         DO 120 K=1,2*L+2
         A(2*L+1,K)=A(2*L+1,K)*A(2*L,2*L-1)-A(2*L,K)*XY1
120      B(2*L+1,K)=B(2*L+1,K)*A(2*L,2*L-1)-B(2*L,K)*XY1
         XY1=A(2*L+1,2*L)/A(2*L-1,2*L)
         DO 140 K=1,2*L+2
         A(2*L+1,K)=A(2*L+1,K)-A(2*L-1,K)*XY1
140      B(2*L+1,K)=B(2*L+1,K)-B(2*L-1,K)*XY1
         ELSE IF (ABS(A(2*L-1,2*L)) .LE. 10.0**(-10)) THEN
         XY1=A(2*L+1,2*L)/A(2*L,2*L)
         DO 160 K=1,2*L+2
         A(2*L+1,K)=A(2*L+1,K)-A(2*L,K)*XY1
160      B(2*L+1,K)=B(2*L+1,K)-B(2*L,K)*XY1
         XY1=A(2*L+1,2*L-1)
         DO 180 K=1,2*L+2
         A(2*L+1,K)=A(2*L+1,K)*A(2*L-1,2*L-1)-A(2*L-1,K)*XY1
180      B(2*L+1,K)=B(2*L+1,K)*A(2*L-1,2*L-1)-B(2*L-1,K)*XY1
         ELSE
         XY1=A(2*L,2*L-1)/A(2*L-1,2*L-1)
         XY2=A(2*L+1,2*L-1)/A(2*L-1,2*L-1)
         DO 200 K=1,2*L+2
         A(2*L,K)=A(2*L,K)-A(2*L-1,K)*XY1
         B(2*L,K)=B(2*L,K)-B(2*L-1,K)*XY1
         A(2*L+1,K)=A(2*L+1,K)-A(2*L-1,K)*XY2
200      B(2*L+1,K)=B(2*L+1,K)-B(2*L-1,K)*XY2
         XY1=A(2*L+1,2*L)/A(2*L,2*L)
         DO 220 K=1,2*L+2
         A(2*L+1,K)=A(2*L+1,K)-A(2*L,K)*XY1
220      B(2*L+1,K)=B(2*L+1,K)-B(2*L,K)*XY1
         END IF
500      CONTINUE
C ---------------------------  BACK ----------------------------------
         DO 1000 L=NS,2,-1
         XY1=A(2*L-1,2*L)/A(2*L,2*L)
         DO 510 K=1,2*NS
         A(2*L-1,K)=A(2*L-1,K)-A(2*L,K)*XY1
510      B(2*L-1,K)=B(2*L-1,K)-B(2*L,K)*XY1
         XY1=A(2*L,2*L-1)/A(2*L-1,2*L-1)
         DO 520 K=1,2*NS
         A(2*L,K)=A(2*L,K)-A(2*L-1,K)*XY1
520      B(2*L,K)=B(2*L,K)-B(2*L-1,K)*XY1
         XY1=A(2*L-2,2*L)/A(2*L,2*L)
         DO 600 K=1,2*NS
         A(2*L-2,K)=A(2*L-2,K)-A(2*L,K)*XY1
600      B(2*L-2,K)=B(2*L-2,K)-B(2*L,K)*XY1
         XY2=A(2*L-2,2*L-1)/A(2*L-1,2*L-1)
         DO 620 K=1,2*NS
         A(2*L-2,K)=A(2*L-2,K)-A(2*L-1,K)*XY2
620      B(2*L-2,K)=B(2*L-2,K)-B(2*L-1,K)*XY2
1000     CONTINUE
         XY1=A(1,2)
         DO 1100 K=1,2*NS
         A(1,K)=A(1,K)*A(2,2)-A(2,K)*XY1
1100     B(1,K)=B(1,K)*A(2,2)-B(2,K)*XY1
         XY2=A(2,1)
         DO 1120 K=1,2*NS
         A(2,K)=A(2,K)*A(1,1)-A(1,K)*XY2
1120     B(2,K)=B(2,K)*A(1,1)-B(1,K)*XY2
         DO 1200 L=1,2*NS
         XY1=A(L,L)
         DO 1200 J=1,2*NS
         A(L,J)=A(L,J)/XY1
1200     B(L,J)=B(L,J)/XY1
         RETURN
         END
