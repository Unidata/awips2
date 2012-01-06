      SUBROUTINE INTPOS(VDIF,HT,P,T,NLVLS)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This subroutine interpolates data between the level of free convection
C and the equilibrium level to a finer resolution for the computation
C of positive energy.
C
C History.
C --------                    
C Don Baker      15 Jun 85    Original version.
C
C Description of input and output.
C --------------------------------
C On input
C --------
C VDIF        Real          Minimum thickness between output levels.
C
C On input/output
C ---------------
C HT          Real Array    Parcel level heights between LFC and EL (m asl).
C P           Real Array    Parcel level pressures between LFC and EL (mb).
C T           Real Array    Parcel level sounding temperatures between
C                           LFC and EL (C).
C NLVLS       Integer       Number of parcel levels between the LFC and EL.
C
C User notes:
C -----------
C 1) This routine is called internally by SUBROUTINE POSAREA.  It's
C    intent is to give more data to work with when computing the positive
C    area.  The resolution (VDIF) is set to be 1/25th of the thickness
C    between the LFC and EL.
C
C
C Input arguments.
C
      REAL VDIF
C
C Input/output arguments.
C
      REAL HT(1),P(1),T(1)
      INTEGER NLVLS
C
C Internal variables.
C
      INTEGER I,J,K
      REAL ZDIF
C
C External functions.
C
      REAL INTERP1
C
C Perform interpolation in height to specified minumum height resolution.
C
      J=2
 50   DO 100 I=J,NLVLS
       ZDIF=HT(I)-HT(I-1)
       IF (ZDIF.GT.VDIF) THEN
        NLVLS=NLVLS+1
        DO 200 K=NLVLS,I+1,-1
         HT(K)=HT(K-1)
         T(K)=T(K-1)
         P(K)=P(K-1)
 200    CONTINUE
        HT(I)=0.5*(HT(I+1)+HT(I-1))
        P(I)=INTERP1(P(I+1),P(I-1),HT(I+1),HT(I),HT(I-1))
        T(I)=INTERP1(T(I+1),T(I-1),HT(I+1),HT(I),HT(I-1))
        J=I
        GO TO 50
       ENDIF
 100  CONTINUE
C
C Exit to calling program.
C
      RETURN
      END

C      IF (MAXZ.GT.CHECK) THEN
C         J=2
C 50      DO 100 I=J,NLVLS
C            ZDIF=HT(I)-HT(I-1)
C            IF (ZDIF.GT.MAXZ) THEN
C               NLVLS=NLVLS+1
C               DO 200 K=NLVLS,I+1,-1
C                  HT(K)=HT(K-1)
C                  T(K)=T(K-1)
C                  TD(K)=TD(K-1)
C                  P(K)=P(K-1)
C 200           CONTINUE
C               HT(I)=0.5*(HT(I+1)+HT(I-1))
C               P(I)=INTERP1(P(I+1),P(I-1),HT(I+1),HT(I),HT(I-1))
C               T(I)=INTERP1(T(I+1),T(I-1),HT(I+1),HT(I),HT(I-1))
C               TD(I)=INTERP1(TD(I+1),TD(I-1),HT(I+1),HT(I),HT(I-1))
C               J=I
C               GO TO 50
C            ENDIF
C 100     CONTINUE
C      ENDIF
