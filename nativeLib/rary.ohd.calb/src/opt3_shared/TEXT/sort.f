C MEMBER SORT
C  (from old member OSCEUA)
C
C===================================================================
C @PROCESS LVL(77)
      SUBROUTINE SORT(M,N,RB,RA)
C
C
C  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
C  BY W.H. PRESS ET AL., PP. 233-234
C
C  LIST OF VARIABLES
C     RA(.) = ARRAY TO BE SORTED
C     RB(.,.) = ARRAYS ORDERED CORRESPONDING TO REARRANGEMENT OF RA(.)
C     WK(.,.), IWK(.) = LOCAL VARIBLES
C
C
      integer, parameter::MaxNopt=100
	  integer, parameter::MaxNpg=2*MaxNopt+1,MaxNpt=MaxNopt*MaxNpg
C      DIMENSION RA(1980),RB(1980,16),WK(1980,16),IWK(1980)
      DIMENSION RA(MaxNpt),RB(MaxNpt,MaxNopt),WK(MaxNpt,MaxNopt),
     &          IWK(MaxNpt)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/sort.f,v $
     . $',                                                             '
     .$Id: sort.f,v 1.3 2003/08/26 17:47:03 wkwock Exp $
     . $' /
C    ===================================================================
C
C
      CALL INDEXX(N, RA, IWK)
      DO 11 I = 1, N
      WK(I,1) = RA(I)
   11 CONTINUE
      DO 12 I = 1, N
      RA(I) = WK(IWK(I),1)
   12 CONTINUE
      DO 14 J = 1, M
      DO 13 I = 1, N
      WK(I,J) = RB(I,J)
   13 CONTINUE
   14 CONTINUE
      DO 16 J = 1, M
      DO 15 I = 1, N
      RB(I,J) = WK(IWK(I),J)
   15 CONTINUE
   16 CONTINUE
C
C  END OF SUBROUTINE SORT
      RETURN
      END
