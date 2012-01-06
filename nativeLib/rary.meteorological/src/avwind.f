 2    SUBROUTINE AVWIND(ELEV,TOP,BOT,HW,PW,TW,UW,VW,NW,
     +                  UAVG,VAVG,AVDIR,AVSPD)
      IMPLICIT NONE

C Statement of purpose.
C ---------------------
C This subroutine calculate a layer mean wind (direction and speed) given
C given 'TOP' and 'BOT' of the layer defined in kilometers above
C ground level.
C
C History.
C --------
C D. Baker       01 Jul 84    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C ELEV        Real          Station elevation (m agl).
C TOP         Real          Top of desired mean wind layer (km agl).
C BOT         Real          Bottom of desired mean wind layer (km agl).
C HW          Real Array    Wind level heights (m asl).
C TW          Real Array    Wind level temperatures (kelvin).
C UW          Real Array    Wind level u-components (m/s).
C VW          Real Array    Wind level v-components (m/s).
C NW          Integer       Number of wind levels passed.
C
C On output:
C ----------
C UAVG        Real          Layer mean u-component wind
C VAVG        Real          Layer mean v-component wind
C AVDIR       Real          Layer mean wind direction (deg).
C AVSPD       Real          Layer mean wind speed (m/s).

C---- Input arguments.

      INTEGER NW
      REAL ELEV,TOP,BOT,HW(NW),TW(NW),PW(NW),UW(NW),VW(NW)

C---- Output arguments.

      REAL AVDIR,AVSPD,UAVG,VAVG

C---- Internal variables.
	
      REAL TOP1,BOT1,SU,SV,UTOP,VTOP,UBOT,VBOT,DENSITY,WGT
      INTEGER I,ITOP,IBOT

C---- External functions.

      REAL INTERP1

C---- Subroutine constants.

      REAL KM2M,FLAG,RCONST
      PARAMETER (KM2M=1000.,FLAG=99999.,RCONST=287.)

C---- Initialize output mean wind value to FLAG.

      AVDIR=FLAG
      AVSPD=FLAG

C---- Initialize sum parameters.

      SU=0.
      SV=0.
      WGT=0.

C---- Calculate the top and bottom of the desired layer in meters
C---- above ground level.  Exit with 'flag' values assigned to mean wind
C---- if unexpected conditions occur, or if sounding not deep enough to
C---- perform calculation.
	
      TOP1=(TOP*KM2M)+ELEV
      BOT1=(BOT*KM2M)+ELEV
      
      IF (TOP1.LE.BOT1) GO TO 9999
      IF (UW(1).GT.99998. .OR. VW(1) .GT.99998.) GO TO 9999
      IF (HW(1).GT.BOT1   .OR. HW(NW).LT.TOP1)   GO TO 9999

C---- Loop 100 interpolates u and v components to the level that is
C---- to be the bottom of the layer for which the mean wind is
C---- desired.  The subscript in the height array that is at or just
C---- above the bottom is saved.
      IBOT=1
      UBOT=UW(IBOT)
      VBOT=VW(IBOT)

      DO I=2,NW
       IF (HW(I).GE.BOT1) THEN
        UBOT=INTERP1(UW(I),UW(I-1),HW(I),BOT1,HW(I-1))
        VBOT=INTERP1(VW(I),VW(I-1),HW(I),BOT1,HW(I-1))
        IBOT=I
        GO TO 150
       ENDIF
      END DO
150   CONTINUE

C---- Loop 200 interpolates u and v components to the level that is
C---- to be the top of the layer for which the mean wind is
C---- desired.  The subscript in the height array that is at or just below
C---- the top is saved.
      ITOP=NW
      UTOP=UW(NW)
      VTOP=VW(NW)

      DO I=NW-1,1,-1
       IF (HW(I).LE.TOP1) THEN
        UTOP=INTERP1(UW(I),UW(I+1),HW(I),TOP1,HW(I+1))
        VTOP=INTERP1(VW(I),VW(I+1),HW(I),TOP1,HW(I+1))
        ITOP=I
        GO TO 250
       ENDIF
      END DO
250   CONTINUE

C---- Check to see if we are only dealing with wind-only data
C---- i.e., profiler, VWP. etc.  If so skip the density-weighted
C---- process and just sum the u- and v-components

      IF (TW(IBOT).GT.350.0 .OR. TW(IBOT).LE.150.0) GO TO 300

C---- Begin the density-weighted sum process by first considering the 
C---- component derived from the interpolated data at the 'bot' and 
C---- the level immediately above.

      DENSITY=(PW(IBOT)*100.)/(TW(IBOT)*RCONST)
      SU=DENSITY*UBOT
      SV=DENSITY*VBOT
      WGT=DENSITY
C---- Now, loop through all levels (until within one level of the
C---- interpolated top of the layer).

      DO I=IBOT+1,ITOP
       DENSITY=(PW(I)*100.)/(TW(I)*RCONST)
       SU=SU+(DENSITY*UW(I))
       SV=SV+(DENSITY*VW(I))
       WGT=WGT+DENSITY
      END DO

C---- Similarly to above, finish the process by incorporating the last
C---- sublayer, from one level below the interpolated top to the
C---- top.

      DENSITY=(PW(ITOP)*100.)/(TW(ITOP)*RCONST)
      SU=SU+(DENSITY*UTOP)
      SV=SV+(DENSITY*VTOP)
      WGT=WGT+DENSITY

      GO TO 350

C---- Just sum up the u- and v-components if we have only wind
C---- data w/o temp profiles (no density-weighted process possible).

300   SU=DENSITY*UBOT
      SV=DENSITY*VBOT
      WGT=1

      DO I=IBOT+1,ITOP
       SU=SU+UW(I)
       SV=SV+DENSITY*VW(I)
       WGT=WGT+1
      END DO
      
      SU=SU+UTOP
      SV=SV+VTOP
      WGT=WGT+1

C---- Finally, calculate the layer mean wind speed and direction.

350   UAVG=SU/WGT
      VAVG=SV/WGT
     
      CALL DDFF(UAVG,VAVG,AVDIR,AVSPD,1)

C---- Exit.

9999  CONTINUE
      RETURN
      END



