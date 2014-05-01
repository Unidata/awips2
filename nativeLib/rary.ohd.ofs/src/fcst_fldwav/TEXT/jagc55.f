C-----------------------------------------------------------------------
C   ONE OF THE PROGRAMS FOR KALMAN FILTER
C   IT DETERMINES JACOBIAN MATRIX C=[DF/DY(j)]
C   C IS PASED BY A MATRIX FROM SUBPROGRAM INTER
C   V  --- STATE Y(j) RELATED PROPERTIES AT EVERY CROSS SECTIONS
C-----------------------------------------------------------------------
         SUBROUTINE JAGC55(NS,C,T,KU,KD,V,Y)
         DIMENSION C(2*NS,2*NS),T(4,1),V(10,NS),Y(2*NS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/jagc55.f,v $
     . $',                                                             '
     .$Id: jagc55.f,v 1.1 1999/04/23 18:08:39 dws Exp $
     . $' /
C    ===================================================================
C
         DO 100 I=1,2*NS
         DO 100 J=1,2*NS
100      C(I,J)=0.0 
         DO 140 L=1,NS-1
         L1=2*L
         L2=2*L+1
         I1=2*L-1
         I2=2*L
         I3=2*L+1
         I4=2*L+2
         C(L1,I1)=T(1,L1)
         C(L1,I2)=T(2,L1)
         C(L1,I3)=T(3,L1)
         C(L1,I4)=T(4,L1)
         C(L2,I1)=T(1,L2)
         C(L2,I2)=T(2,L2)
         C(L2,I3)=T(3,L2)
         C(L2,I4)=T(4,L2)
140     CONTINUE
C------------------ BOUNDARY CONDITION RELATED PART -----------------------
         IF (KU .EQ. 1) C(1,2)=1.0
         IF (KU .EQ. 2) C(1,1)=1.0
         IF (KD .LE. 1) THEN
         C(2*NS,2*NS)=1.0
         ELSE IF (KD .EQ.2) THEN
         C(2*NS,2*NS-1)=1.0
         ELSE
         C(2*NS,2*NS)=-V(1,NS)*Y(2*NS-1)/V(2,NS)
         C(2*NS,2*NS-1)=1.0
         ENDIF
C--------------------------------------------------------------------------
         RETURN
         END 
