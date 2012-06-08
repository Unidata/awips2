	SUBROUTINE GDPGTS  ( iflno, time, ivcord, rgx, rgy,  
     +			     vclsfc, havsfc, parm, havgfs, iret )
C************************************************************************
C* GDPGTS								*
C*									*
C* This subroutine gets the surface data for a cross section.		*
C*									*
C* GDPGTS  ( IFLNO, TIME, IVCORD, RGX, RGY,  VCLSFC, HAVSFC, 		*
C*	     PARM, HAVGFS, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		  INTEGER	Grid file number		*
C*	TIME  (2)	  CHAR*		Time to search for levels	*
C*	IVCORD		  INTEGER	Vertical coordinate for search	*
C*	RGX  		  REAL		X grid coordinate		*
C*	RGY  		  REAL		Y grid coordinate		*
C*									*
C* Output parameters:							*
C*      VCLSFC            REAL 		Vert coord location of sfc	*
C*      HAVSFC            LOGICAL       Flag for existence of sfc	*
C*	PARM		  CHAR*		Parameter name			*
C*      HAVGFS            LOGICAL       Flag for existence of sfc data	*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -6 = GVCORD is invalid		*
C*					 +2 = no sfc value found	*
C**									*
C* Log:									*
C* D. McCann/NSSFC	12/94		Created from gdxgts.f		*
C* R. Tian/SAIC		 5/06		Changed to call new DG subs	*
C************************************************************************
	INCLUDE		'DGCMN.CMN'
C*
	CHARACTER*(*)	time ( 2 ), parm
        REAL		rgx , rgy , vclsfc 
	LOGICAL		havsfc, havgfs, ermiss
C*
	REAL		grid ( LLMXGD )
	INTEGER		level ( 2 ), igrhdr ( LLGDHD )
C------------------------------------------------------------------------
	iret = 0
	havsfc = .false.
	havgfs = .false.
       ermiss = .false.
C
C*	Set the level and parameter for which to search.
C
	level (1) =  0
	level (2) = -1
	CALL LV_CCRD  ( ivcord, parm, ier )
C
C*	Try to read the surface data on IVCORD.
C
	CALL DG_NRDT  ( iflno, time, level, ivcord, parm,
     +			grid, igx, igy, igrhdr, ier )
	IF  ( ier .eq. 0 )  THEN
	    havsfc = .true.
	    havgfs = .true.
	ELSE
C
C*	    Try to read the surface data on JVCORD = 0.
C
	    jvcord = 0
	    CALL DG_NRDT  ( iflno, time, level, jvcord, parm,
     +			    grid, igx, igy, igrhdr, ier )
	    IF  ( ier .eq. 0 )  THEN
		havsfc = .true.
		ELSE
C
C*	    	Try to read the surface data on JVCORD = 4.
C
	    	jvcord = 4
	    	level (1) = 10000
	    	CALL DG_NRDT  ( iflno, time, level, jvcord, parm,
     +			    grid, igx, igy, igrhdr, ier )
		IF  ( ier .eq. 0 )  THEN
			havsfc = .true.
	    	END IF
	    END IF
	END IF
C
	if (havsfc) THEN
	    CALL GR_INTP ( 1, rgx, rgy, 1, igx, igy, grid, vclsfc, ierr )
	    IF  ( ierr .ne. 0 ) THEN
		vclsfc = RMISSD
		havsfc = .false.
	    END IF
	ELSE
            vclsfc = RMISSD
	END IF
C*
	RETURN
	END
