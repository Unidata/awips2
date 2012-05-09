	SUBROUTINE GD_RDATW  ( iacss, gdattm1, gdattm2, level1, level2,
     +                     ivcord, parm, grid, igx, igy, ighdr, iret )
C************************************************************************
C* GD_RDATW                                                             *
C*                                                                      *
C* This subroutine reads the requested grid from a grid file.           *
C*                                                                      *
C* GD_RDATW ( IACSS, GDATTM1, GDATTM2, LEVEL1, LEVEL2, IVCORD, PARM,	*
C*            GRID, IGX, IGY, IGHDR,  IRET )				* 
C*                                                                      *
C* Input parameters:                                                    *
C*      IACSS           INTEGER         Grid access number              *
C*      GDATTM1		CHAR*20         GEMPAK times                    *
C*      GDATTM2		CHAR*20         GEMPAK times                    *
C*      LEVEL1		INTEGER         Vertical levels                 *
C*      LEVEL2		INTEGER         Vertical levels                 *
C*      IVCORD          INTEGER         Vertical coordinate             *
C*                                        0 = NONE                      *
C*                                        1 = PRES                      *
C*                                        2 = THTA                      *
C*                                        3 = HGHT                      *
C*      PARM            CHAR*12         Parameter name                  *
C*                                                                      *
C* Output parameters:                                                   *
C*      GRID (IGX,IGY)  REAL            Grid data                       *
C*      IGX             INTEGER         Number of horizontal points     *
C*      IGY             INTEGER         Number of vertical points       *
C*      IGHDR (IHDRSZ)  INTEGER         Grid header                     *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -4 = file not open             *
C*                                       -6 = read/write error          *
C*                                      -12 = grid does not exist       *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC          2/06						*
C************************************************************************
	CHARACTER*(*)	gdattm1, gdattm2, parm
	INTEGER         ighdr (*)
	REAL            grid (*)
C*
        CHARACTER       time  (2)*20
	INTEGER         level (2)
C-----------------------------------------------------------------------
        iret = 0
C
C*	Call GD_RDAT	
C
	time (1) = gdattm1
	time (2) = gdattm2
	level (1) = level1
	level (2) = level2
	CALL GD_RDAT ( iacss, time, level, ivcord, parm, grid, igx,
     +                 igy, ighdr, iret )
C*
	RETURN
	END
