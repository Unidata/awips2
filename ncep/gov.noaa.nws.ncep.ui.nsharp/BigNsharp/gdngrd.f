	SUBROUTINE GD_NGRD  ( iacss, numgrd, firstm, lasttm, iret )
C************************************************************************
C* GD_NGRD								*
C*									*
C* This subroutine returns the number of grids in a grid file along	*
C* with the first and last time.					*
C*									*
C* GD_NGRD  ( IACSS, NUMGRD, FIRSTM, LASTTM, IRET )			*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*									*
C* Output parameters:							*
C*	NUMGRD		INTEGER		Number of grids			*
C*	FIRSTM		CHAR*20		Earliest time1 in file		*
C*	LASTTM		CHAR*20		Latest time1 in file		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 8/88	Changed calling sequence		*
C* M. desJardins/GSFC	 4/89	Add first time; change sorting		*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	lasttm, firstm
C-----------------------------------------------------------------------
        iret = 0

	print *, 'gdngrd.f: entering'
	print *, 'gdngrd.f: iacss', iacss
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
	print *, 'gdngrd.f: GD_FCHK iacss = ', iacss
	print *, 'gdngrd.f: GD_FCHK igdfln = ', igdfln
	print *, 'gdngrd.f: GD_FCHK iret = ', iret
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Return the number of grids.
C
	print *, 'gdngrd.f: before kgrid numgrd = ', numgrd
	numgrd = kgrid ( igdfln )
	print *, 'gdngrd.f: after kgrid numgrd = ', numgrd
	IF  ( numgrd .le. 0 )  RETURN
C
C*	Get the first time from the sorted time array.  Convert to a
C*	character time.
C
	CALL TG_ITOC  ( igdatm (1,1,igdfln), firstm, ier )
C
C*	Do the same for the last time.
C
	CALL TG_ITOC  ( igdatm ( 1, ktgrid (igdfln), igdfln ), lasttm, 
     +			ier )
C*
	RETURN
	END
