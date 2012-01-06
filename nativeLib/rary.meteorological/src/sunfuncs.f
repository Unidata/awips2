C	J. Wakefield	28 Jan 82	Original version

C***These formulas are from Paltridge and Platt, 1976.  They reference Spencer,
C***1971 for the solar declination and equation of time.
C------------------------------------------------------------------------------
	real FUNCTION RADNORM(JD)
C***Normalized earth-sun distance factor (R0/R)**2
C***JD is input Julian day number
      Implicit None
c declare formal arguments
      integer jd
c ----------------
      real    DAYANG1, DAYANG2
c ----------------
c
	DAYANG1=2.*3.14159265*(JD-1)/365.
	DAYANG2=2.*DAYANG1

	RADNORM= 1.000110
     1		+0.034221*COS(DAYANG1)+0.001280*SIN(DAYANG1)
     2		+0.000719*COS(DAYANG2)+0.000077*SIN(DAYANG2)

	RETURN
	END
C------------------------------------------------------------------------------
	real FUNCTION SOLDEC(JD)
C***Solar declination angle (radians)
C***JD is input Julian day number
      Implicit None
c declare formal arguments
      integer jd
c ----------------
      real    DAYANG1, DAYANG2, DAYANG3
c ----------------
c
	DAYANG1=2.*3.14159265*(JD-1)/365.
	DAYANG2=2.*DAYANG1
	DAYANG3=3.*DAYANG1

	SOLDEC=  0.006918
     1		-0.399912*COS(DAYANG1)+0.070257*SIN(DAYANG1)
     2		-0.006758*COS(DAYANG2)+0.000907*SIN(DAYANG2)
     3		-0.002697*COS(DAYANG3)+0.001480*SIN(DAYANG3)

	RETURN
	END
C------------------------------------------------------------------------------
	real FUNCTION TIMEQ(JD)
C***Equation of time (radians)
C***JD is input Julian day number
      Implicit None
c declare formal arguments
      integer jd
c ----------------
      real    DAYANG1, DAYANG2
c ----------------
c
	DAYANG1=2.*3.14159265*(JD-1)/365.
	DAYANG2=2.*DAYANG1

	TIMEQ=   0.000075
     1		+0.001868*COS(DAYANG1)-0.032077*SIN(DAYANG1)
     2		-0.014615*COS(DAYANG2)-0.040849*SIN(DAYANG2)

	RETURN
	END
