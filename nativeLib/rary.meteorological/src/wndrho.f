      SUBROUTINE WNDRHO(RHO,HT,NLVLS,HW,NW,RHOW)
      IMPLICIT NONE
C PURPOSE:
C --------
C INTERPOLATE TO DETERMINE DENSITY AT WIND LEVELS GIVEN DENSITY AT
C PRESSURE LEVELS IN A SOUNDING.  INTERPOLATION IS LINEAR BY HEIGHT.

C	T. Schlatter	late 82		Probable original author.
C	D. Baker	17 Dec 85	Added doc and indentation (?)
C	D. Baker (?)	after Dec 85	Replaced 100 loop with 300 loop.  It
C					appears that the interpolation is out.
C	J. Wakefield	17 Nov 92	Added parameter list documentation.
C       D. Perry           Sep 96       Adapted code to work with WFO.

C Argument	I/O	Type			Description
C --------	---	----	-----------------------------------------------
C Rho		 I	R*4 A	Density (kg m-3) at sounding levels.
C Ht		 I	R*4 A	Heights (m) at sounding levels.
C NLvls		 I	I*4	Number of sounding levels.
C HW		 I	R*4 A	Heights (m) of wind obs.
C NW		 I	I*4	Number of wind obs.
C RhoW		 O	R*4 A	Density interpolated to wind obs heights.

C VARIABLE/ARRAY DECLARATIONS.
      REAL RHO(1),HT(1),HW(1),RHOW(1)
      REAL INTERP1
      INTEGER NW,NLVLS,I,J,K1,K2

C INTERPOLATE TO DERIVE DENSITY AT WIND HEIGHTS.
c      J=1
c      DO 100 I=1,NW
c         K=J
c         DO 200 J=K,NLVLS-1
c         IF (HW(I).GE.HT(J) .AND. HW(I).LE.HT(J+1)) THEN
c            RHOW(I)=INTERP1(RHO(J),RHO(J+1),HT(J),HW(I),HT(J+1))
c            GO TO 100
c         ENDIF
c 200  CONTINUE
c 100  CONTINUE

       RHOW(1)=RHO(1)
       K1=1
       K2=2
       Do 300 I=2,NW
299    IF (HT(K2).LT.HW(I)) THEN
           K1=K2
           K2=K2+1
           IF (K2.GT.NLVLS) THEN
               DO 298 J=I,NW
298            RHOW(J)=RHO(K1)
               RETURN
           END IF
           GOTO 299
       END IF
       RHOW(I)=INTERP1(RHO(K1),RHO(K2),HT(K1),HW(I),HT(K2))
300    CONTINUE

C EXIT.
      RETURN
      END
