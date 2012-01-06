	SUBROUTINE RHBAR (  ENDLVL,    !Array of pressure bounds
     .			    MRH,       !Array of mean relative humidities
     .			    NCLYR,     !Number of layers to be computed for
     .			    SFCP,      !Surface pressure (mb)
     .			    P,         !Interpolated pressure array (mb)
     .			    TL,	       !Interpolated temperature array (mb)
     .			    TDL)       !Interpolated dew point array (mb)


C========================================================================
C   This routine computes the mean relative humidity in the 150 mb layer
C   just above the surface, the 150 mb layer above that, and the 200 mb 
C   layer above the second layer.
C========================================================================
      Implicit None
c declare the formal arguments
      real     ENDLVL(*)
      integer  MRH(*), NCLYR
      real     SFCP, P(*), TL(*), TDL(*) 

C=========================
C   Variable declarations
C=========================

        REAL      E,ES,W,WS  
c ----------------
      real       rh, sumrh
      integer*2  i, idiv, istart, lyr
      external   esat
      real       esat
c ----------------

C================================================
C   Initialize running humidity sum and counters
C================================================

	SUMRH= 0.
	I= 1
	ISTART= I
	LYR= 1

C=========================================================
C   Compute mean relative humidity for each of the layers
C=========================================================

 50	CONTINUE
C	RH= HUM(TL(I),TDL(I))  !Calculate relative humidity at each level
        E= ESAT(TL(I))
        W= (0.622*E)/(P(I)-E)
        ES= ESAT(TDL(I))
        WS= (0.622*ES)/(P(I)-ES)
        RH= 100*(WS/W)
	SUMRH= SUMRH+RH	       !Compute running sum

	IF (P(I).GT.ENDLVL(LYR)) THEN    !Keep summing as long as still in 
	   I= I+1			 !given layer
           GO TO 50
	ENDIF

	IDIV= (I-ISTART)+1      !Calcultae mean RH for layer by dividing by
	MRH(LYR)= (SUMRH/IDIV)	!total times RH was computed within layer.

	IF (MRH(LYR).LT.0) MRH(LYR)= 0      !Make sure mean RH value is
	IF (MRH(LYR).GT.100) MRH(LYR)= 100  !physically reasonable.

	IF (LYR.LT.NCLYR) THEN  !If number of layers is still less than the
	   LYR= LYR+1           !total number of layers desired (NCLYR), 
	   SUMRH= 0.	        !reset counters and start over with new layer.
	   ISTART= I
	   GO TO 50
	ENDIF

C=================
C   Normal return
C=================

	RETURN
	END
