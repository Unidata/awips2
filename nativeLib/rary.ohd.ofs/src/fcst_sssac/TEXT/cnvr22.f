C MEMBER CNVR22
C*********************************************************
       SUBROUTINE CNVR22 (IND,N,Y,X,CX)
C
C  THIS SUBROUTINE IS PART OF SS-SAC
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL-NWS, FALL-1985
C
C  IT CONVERTS A 1-D VECTOR TO A N-VECTOR AND A NXN-SYMMETRIC MATRIX (IN
C  AND VICE-VERSA(IND=1)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
         DIMENSION Y(91),X(12),CX(12,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/cnvr22.f,v $
     . $',                                                             '
     .$Id: cnvr22.f,v 1.2 2002/05/15 13:56:43 hank Exp $
     . $' /
C    ===================================================================
C
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990      FORMAT(/10X,'** CNVR22 ENTERED.')
C
         DO 10 I=1,N
         IF(IND.EQ.0)X(I)=Y(I)
         IF(IND.GT.0)Y(I)=X(I)
 10      CONTINUE
C
         NC=N-1
         NT=N+N*(N+1)/2
         DO 30 I=1,NC
         II1=I+1
         K2=(I-1)*N+1+N
         IF(I.GT.2)K2=K2-(I-1)*(I-2)/2
         IF(IND.EQ.0)CX(I,I)=Y(K2)
         IF(IND.GT.0)Y(K2)=CX(I,I)
         DO 20 J=II1,N
         K1=N+(I-1)*N+1+J-I
         IF(I.GT.2)K1=K1-(I-1)*(I-2)/2
         IF(IND.EQ.0)CX(I,J)=Y(K1)
         IF(IND.GT.0)Y(K1)=CX(I,J)
         IF(IND.EQ.0)CX(J,I)=CX(I,J)
 20       CONTINUE
 30       CONTINUE
C
         IF(IND.EQ.0)CX(N,N)=Y(NT)
         IF(IND.GT.0)Y(NT)=CX(N,N)
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991      FORMAT(/10X,'** EXIT CNVR22.')
C
         RETURN
         END
