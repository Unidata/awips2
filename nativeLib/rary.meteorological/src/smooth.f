
	SUBROUTINE SMOOTH (INPUT,OUTPUT,MNX,IX,IY,SMTH)

C	Reference: Shapiro, 1970: "Smoothing, Filtering, and
C	  Boundary Effects", Rev. Geophys. Sp. Phys., 359-387.

C	This filter is of the type 
C	  z(i) = (1-s)z(i) + s(z(i+1)+z(i-1))/2
C	For a filter which is supposed to damp 2dx waves completely
C	  but leave 4dx and longer with little damping,
C	  it should be run with 2 passes using SMTH (or s) of 0.5
C	  and -0.5.

C	HISTORY
C	S. Benjamin	1985		Original version
C	J. Ramer   	1990		Missing value handling.
c
      Implicit None
c declare formal arguments
      integer mnx, ix, iy
      REAL    INPUT(MNX,IY), OUTPUT (MNX,IY)
      real    smth
c
	REAL 	FLG
	INTEGER IP,I,IM,JP,J,JM
	DATA	FLG/99998./
c ----------------
      real   SMTH1, SMTH2, SMTH3, SMTH4, SMTH5, sum1, sum2
c ----------------

        SMTH1 = 0.25 * SMTH * SMTH
        SMTH2 = 0.5  * SMTH * (1.-SMTH)
      	SMTH3 = (1.-SMTH) * (1.-SMTH)
     	SMTH4 = (1.-SMTH)
     	SMTH5 = 0.5 * SMTH

	JM=1
	J=2
	DO JP=3,IY
	  IM=1
	  I=2
	  DO IP = 3,IX
            IF (INPUT(I,J).GT.FLG .OR.
     &          INPUT(IP,JP).GT.FLG .OR. INPUT(IM,JM).GT.FLG .OR.
     &          INPUT(IP,JM).GT.FLG .OR. INPUT(IM,JP).GT.FLG .OR.
     &          INPUT(I,JP).GT.FLG .OR. INPUT(I,JM).GT.FLG .OR.
     &          INPUT(IP,J).GT.FLG .OR. INPUT(IM,J).GT.FLG) THEN
                OUTPUT(I,J)=INPUT(I,J)
	    ELSE 
        	SUM1 = INPUT (IM,JP) + INPUT (IM,JM)
     &	     + INPUT (IP,JP) + INPUT (IP,JM)
		SUM2 = INPUT (I ,JP) + INPUT (IP,J )
     &	     + INPUT (I ,JM) + INPUT (IM,J )
	 	OUTPUT(I,J) = SMTH1*SUM1 + SMTH2*SUM2 +
     &		      SMTH3*INPUT(I,J)
	    END IF
            IM=I
            I=IP
	  END DO
	  JM=J
          J=JP
	END DO


	DO J=1,IY,IY-1
	  IM=1
	  I=2
	  DO IP = 3,IX
            IF (INPUT(I,J).GT.FLG .OR.
     &       INPUT(IP,J).GT.FLG .OR. INPUT(IM,J).GT.FLG) THEN
                OUTPUT(I,J)=INPUT(I,J)
	    ELSE 
		OUTPUT(I,J) = SMTH4* INPUT(I,J) 
     &		    + SMTH5 * (INPUT(IM,J) + INPUT(IP,J))
	    END IF
            IM=I
            I=IP
	  END DO
	END DO

	DO I=1,IX,IX-1
	  JM=1
	  J=2
	  DO JP = 3,IY
            IF (INPUT(I,J).GT.FLG .OR.
     &       INPUT(I,JM).GT.FLG .OR. INPUT(I,JP).GT.FLG) THEN
                OUTPUT(I,J)=INPUT(I,J)
	    ELSE 
		OUTPUT(I,J) = SMTH4* INPUT(I,J) 
     &		    + SMTH5 * (INPUT(I,JM) + INPUT(I,JP))
	    END IF
            JM=J
            J=JP
	  END DO
	END DO

	OUTPUT(1,1)=INPUT(1,1)
	OUTPUT(IX,1)=INPUT(IX,1)
	OUTPUT(1,IY)=INPUT(1,IY)
	OUTPUT(IX,IY)=INPUT(IX,IY)

        RETURN
	END
