      SUBROUTINE SWEAT(P,T,TD,NLVLS,PW,UW,VW,NW,SWIDX)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes the severe weather threat (SWEAT) index from
C sounding data.
C
C History.
C --------                    
C Don Baker      01 Jul 84    Original version.
C Don Baker      15 May 85    Updated for CWP.
C Dale Perry        Dec 96    Updated for WFO.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C P           Real Array    Sounding pressures (mb).
C T           Real Array    Sounding temperatures (K).
C TD          Real Array    Sounding dew points (K).
C NLVLS       Integer       Number of sounding levels passed.
C PW          Real Array    Wind level pressures (mb).
C UW          Real Array    Wind level u-components (m/s).
C VW          Real Array    Wind level v-components (m/s).
C NW          Integer       Number of wind levels passed.
C
C On output:
C ----------               
C SWIDX       Real          Severe weather threat index.
C
C
C Input arguments.
C
      INTEGER NLVLS,NW
      REAL P(NLVLS),T(NLVLS),TD(NLVLS),UW(NW),VW(NW),PW(NW)
C
C Output arguments.
C
      REAL SWIDX
C
C Internal variables.
C
      REAL P1,P2,P3,TD850,T500,DD500,FF500,DD850,FF850
      REAL U500,V500,U850,V850,SINDIR,TOTAL,DP,PRES,X1,X2
      INTEGER I,K
C
C External functions.
C
      REAL INTERP1
C
C Subroutine constants.
C
      REAL RPD,MS2KT,FLAG
      PARAMETER (RPD=0.017,MS2KT=1.94,FLAG=99999.)
C
C Initialize value of SWEAT index.  If the surface pressure is too low,
C exit the routine.  Also, if the top of the data is not high enough,
C exit the routine.
C
      SWIDX=FLAG
      IF (P(1).LT.820. .OR. PW(1).LT.820. .OR. P(NLVLS).GT.500. .OR.
     +    PW(NW).GT.500.) GO TO 999
C
C Determine 850 mb temperature and dew point from sounding.
C
      IF (P(1).LT.900.) THEN
         PRES=800.
      ELSE
         PRES=850.
      ENDIF

      DO 1 I=1,NLVLS
         IF (P(I).LE.PRES) THEN
            IF (I.EQ.1) THEN
               TD850=TD(I)
               GO TO 2
            END IF
            P1=ALOG(P(I-1))
            P2=ALOG(PRES)
            P3=ALOG(P(I))
            TD850=INTERP1(TD(I-1),TD(I),P1,P2,P3)
            GO TO 2
         ENDIF
 1    CONTINUE
c if we got here then the data doesn't support the computation
      GO TO 999

 2    CONTINUE
C
C Determine 850 mb wind direction and speed.
C
      DO 3 I=1,NW
         IF (PW(I).LE.PRES) THEN
            IF (I.EQ.1) THEN
               U850=UW(I)
               V850=VW(I)
               GO TO 4
            END IF
            P1=ALOG(PW(I-1))
            P2=ALOG(PRES)
            P3=ALOG(PW(I))
            U850=INTERP1(UW(I-1),UW(I),P1,P2,P3)
            V850=INTERP1(VW(I-1),VW(I),P1,P2,P3)
            CALL DDFF(U850,V850,DD850,FF850,1)
            GO TO 4
         ENDIF
 3    CONTINUE
c if we got here then the data doesn't support the computation
      GO TO 999

 4    CONTINUE
C
C Determine 500 mb temperature from the sounding.
C
      PRES=500.
      DO 8 I=1,NLVLS
         IF (P(I).LE.PRES) THEN
            IF (I.EQ.1) THEN
               T500=T(I)
               GO TO 9
            END IF
            P1=ALOG(P(I-1))
            P2=ALOG(PRES)
            P3=ALOG(P(I))
            T500=INTERP1(T(I-1),T(I),P1,P2,P3)
            GO TO 9
         ENDIF
 8    CONTINUE
c if we got here then the data doesn't support the computation
      GO TO 999

 9    CONTINUE
C
C Determine 500 mb wind direction and speed.
C
      DO 13 I=1,NW
         IF (PW(I).LE.PRES) THEN
            IF (I.EQ.1) THEN
               U500=UW(I)
               V500=VW(I)
               GO TO 14
            END IF
            P1=ALOG(PW(I-1))
            P2=ALOG(PRES)
            P3=ALOG(PW(I))
            U500=INTERP1(UW(I-1),UW(I),P1,P2,P3)
            V500=INTERP1(VW(I-1),VW(I),P1,P2,P3)
            CALL DDFF(U500,V500,DD500,FF500,1)
            K=I
            GO TO 14
         ENDIF
 13   CONTINUE
c if we got here then the data doesn't support the computation
      GO TO 999

 14   CONTINUE
C
C Compute the totals index.
C
      CALL TOTALS(P,T,TD,NLVLS,TOTAL,X1,X2)
C
C Compute the SWEAT index.  Wind speeds must be in knots.
C
      FF850=FF850*MS2KT
      FF500=FF500*MS2KT
      IF (DD850.LT.130. .OR. DD850.GT.250. .OR.
     +    DD500.LT.210. .OR. DD500.GT.310. .OR.
     +   (DD500-DD850).LT.0. .OR.
     +    FF500.LT.15. .OR. FF850.LT.15.) THEN
         SINDIR=-0.2
      ELSE
         SINDIR=SIN((DD500-DD850)*RPD)
      ENDIF
      IF (TOTAL.LT.49.) TOTAL=49.
      IF (TD850.LT.273.15) THEN
         DP=0.
      ELSE
         DP=TD850-273.15
      ENDIF
      SWIDX=12*DP+20*(TOTAL-49.)+2*FF850+FF500+125*(SINDIR+0.2)
C
C EXIT.
C
 999  CONTINUE
      RETURN
      END
