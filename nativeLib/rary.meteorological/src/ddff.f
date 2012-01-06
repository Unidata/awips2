      SUBROUTINE DDFF(U,V,DIR,SPD,NLVLS)
      IMPLICIT NONE

C Statement of purpose.
C ---------------------
C This routine computes wind direction (deg) and speed given rectangular
C wind components.
C
C History.
C --------
C D. Baker       01 Jul 84    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C U           Real Array    U wind components.
C V           Real Array    V wind components.
C NLVLS       Integer       Number of wind levels passed.
C
C On output:
C ----------
C DIR         Real Array    Wind directions (deg).
C SPD         Real Array    Wind speeds (same units as input components).
C
C User notes:
C -----------
C 1) User may use this routine to convert only one set of components by
C    passing a value of 1 for NLVLS.

C---- Input arguments.

      REAL U(1),V(1)
      INTEGER NLVLS

C---- Output arguments.

      REAL DIR(1),SPD(1)

C---- Internal variables.

      INTEGER I

C---- Subroutine constants.

      REAL FLAG,DPR
      PARAMETER (FLAG=99999.,DPR=57.29578)

C---- Calculate direction and speed.  Assign flags if components are bad.

      DO I=1,NLVLS
       IF (U(I).GT.150. .OR. U(I).LT.-150. .OR.
     $  V(I).GT.150. .OR. V(I).LT.-150.) THEN
        DIR(I)=FLAG
        SPD(I)=FLAG
       ELSE if (abs(u(i)).lt.0.05 .and. abs(v(i)).lt.0.05) then
        dir(i)=0.0
        spd(i)=0.0
       else 
        DIR(I)=DPR*(ATAN2(U(I),V(I)))+180.
        SPD(I)=SQRT(U(I)*U(I)+V(I)*V(I))
       ENDIF
      END DO

C---- Exit.

      RETURN
      END
