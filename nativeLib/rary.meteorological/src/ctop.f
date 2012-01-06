      SUBROUTINE CTOP(P,HT,VV,PEQLEV,NPAR,CLDTOP)
      IMPLICIT NONE
                   
C Statement of purpose.
C ---------------------
C This routine estimates the cloud top based on the undiluted parcel
C vertical velocity profile.
C
C History.
C --------
C D. Baker       10 May 85    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C P           Real Array    Parcel level pressures (mb).
C HT          Real Array    Parcel level heights (m asl).
C VV          Real Array    Parcel vertical velocity profile (m/s).
C PEQLEV      Real          Equilibrium level pressure (mb).
C NPAR        Integer       Number of parcel levels passed.
C
C On output:
C ----------
C CLDTOP      Real          Estimated cloud top (m asl).
C
C User notes:
C -----------
C 1) The estimated cloud top is the level where the vertical velocity
C    drops to zero above the equilibrium level.
C 2) If the parcel vertical velocity does not drop to zero, a value
C    of 99999 is returned for the cloud top...meaning that the top is
C    above the top of the sounding.
                                   
C---- Input arguments.
                  
      INTEGER NPAR
      REAL HT(NPAR),P(NPAR),VV(NPAR),PEQLEV
                  
C---- Output arguments.
                   
      REAL CLDTOP
                 
C---- Internal variables.
                     
      INTEGER I,J
      REAL FLAG,FLG
      PARAMETER (FLAG=99999.,FLG=99998.)
                                      
C---- Initialize cloud top flag to value for the case of no equilibrium level.
C---- Search for equilibrium level.  If not found, exit.
                                                    
      CLDTOP=FLAG
      IF (PEQLEV.LT.-FLG) GO TO 999
      DO I=1,NPAR
       IF (P(I).LE.PEQLEV) GO TO 101
      END DO
101   IF (I.EQ.NPAR) GO TO 999
                              
C---- Iterate above the equilibrium level until vertical velocity drops to
C---- zero.  If this does not happen by the time the top of the sounding is
C---- reached, return flag value corresponding to this case.
                                                        
      DO J=I,NPAR
       IF (VV(J).LT.0.01) THEN
        CLDTOP=HT(J)
        GO TO 999
       END IF
      END DO
              
C---- Exit.
       
999   CONTINUE
      RETURN
      END
