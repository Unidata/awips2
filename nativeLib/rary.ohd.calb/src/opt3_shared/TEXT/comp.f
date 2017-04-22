C MEMBER COMP
C  (from old member OSCEUA)
C
C====================================================================
C @PROCESS LVL(77)
      SUBROUTINE COMP(N,NPT,NGS1,NGS2,NPG,A,AF,B,BF)
C
C  THIS SUBROUTINE REDUCE INPUT MATRIX A(N,NGS2*NPG) TO MATRIX
C  B(N,NGS1*NPG) AND VECTOR AF(NGS2*NPG) TO VECTOR BF(NGS1*NPG)
C      DIMENSION A(1980,16),AF(1980),B(1980,16),BF(1980)
      integer, parameter::MaxNopt=100
	  integer, parameter::MaxNpg=2*MaxNopt+1,MaxNpt=MaxNopt*MaxNpg
      DIMENSION A(MaxNpt,MaxNopt),AF(MaxNpt),B(MaxNpt,MaxNopt),
     &          BF(MaxNpt)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/comp.f,v $
     . $',                                                             '
     .$Id: comp.f,v 1.3 2003/08/26 17:41:54 wkwock Exp $
     . $' /
C    ===================================================================
C
      DO 20 IGS=1, NGS1
        DO 10 IPG=1, NPG
          K1=(IPG-1)*NGS2 + IGS
          K2=(IPG-1)*NGS1 + IGS
          DO 5 I=1, N
            B(K2,I) = A(K1,I)
    5     CONTINUE
          BF(K2) = AF(K1)
   10   CONTINUE
   20 CONTINUE
C
      DO 40 J=1, NPT
        DO 30 I=1, N
          A(J,I) = B(J,I)
   30   CONTINUE
        AF(J) = BF(J)
   40 CONTINUE
      RETURN
      END
