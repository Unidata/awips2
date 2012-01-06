      SUBROUTINE TOTALS(P,T,TD,NLVLS,TOTIDX,CRSTOT,VERTOT)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes the totals index, cross totals index, and the
C vertical totals index from a sounding.
C
C History.
C --------                    
C Don Baker      01 Jul 84    Original version.
C Don Baker      12 May 85    Added cross and vertical totals.
C Don Baker      15 Jun 85    Adapted code for CWP.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C P           Real Array    Sounding pressures (mb).
C T           Real Array    Sounding temperatures (C).
C TD          Real Array    Sounding dew points (C).
C NLVLS       Integer       Number of sounding levels passed.
C
C On output:
C ----------               
C TOTIDX      Real          Total totals index (C).
C CRSTOT      Real          Cross totals index (C).
C VERTOT      Real          Vertical totals index (C).
C
C
C Input arguments.
C
      REAL P(1),T(1),TD(1)
      INTEGER NLVLS
C
C Output arguments.
C
      REAL TOTIDX,CRSTOT,VERTOT
C
C Internal variables.
C
      REAL P1,P2,P3,T850,TD850,T500,PRES
      INTEGER I, j
C
C External functions.
C
      REAL INTERP1
C
C Subroutine constants.
C
      REAL FLAG
      PARAMETER (FLAG=99999.)
C
C Initialize totals index to flag.  If the surface pressure is too low,
C exit from routine.
C
      TOTIDX=FLAG
      CRSTOT=FLAG
      VERTOT=FLAG
      IF (P(1).LT.820.) GO TO 999
C
C Determine 850 mb temperature and dew point from sounding.
C
      T850 =T(1)
      TD850=TD(1)
      IF (P(1).LT.900.) THEN
         PRES=800.
      ELSE
         PRES=850.
      ENDIF
      DO 1 I=1,NLVLS
C
C Case where the 850 level is at the surface.
C
         IF ((I.EQ.1).AND.(P(I).EQ.PRES)) THEN
            T850=T(1)
            TD850=TD(1)
            GOTO 2
         ELSE IF (P(I).LE.PRES) THEN
            P1=ALOG(P(I-1))
            P2=ALOG(PRES)
            P3=ALOG(P(I))
            T850=INTERP1(T(I-1),T(I),P1,P2,P3)
            TD850=INTERP1(TD(I-1),TD(I),P1,P2,P3)
            GO TO 2
         ENDIF
 1    CONTINUE
c  if we have gotten to here, the data doesn't support the algoithm
      goto 999

 2    CONTINUE
C
C Determine 500 mb temperature from the sounding.
C
      PRES=500.
      T500=T(i)
C
C Start above the level we left off for the 850 data
C
      DO 3 j=i+1,NLVLS
         IF (P(j).LE.PRES) THEN
            P1=ALOG(P(j-1))
            P2=ALOG(PRES)
            P3=ALOG(P(j))
            T500=INTERP1(T(j-1),T(j),P1,P2,P3)
            GO TO 4
         ENDIF
 3    CONTINUE
c  if we have gotten to here, the data doesn't support the algoithm
      goto 999

 4    CONTINUE
C
C Compute the total totals, cross totals, and, vertical totals index.
C
      CRSTOT=TD850-T500
      VERTOT=T850-T500
      TOTIDX=CRSTOT+VERTOT
C
C Exit.
C
 999  CONTINUE
      RETURN
      END
