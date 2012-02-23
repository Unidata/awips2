	SUBROUTINE GD_SRTT  ( iacss, iret )
C************************************************************************
C* GD_SRTT								*
C*									*
C* This subroutine sorts the times from all the grid identifiers.	*
C*									*
C* GD_SRTT  ( IACSS, IRET )						*
C*									*
C* Input parameters:							*
C*	IACSS		INTEGER		Grid file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/89	Replaced GD_SORT			*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C
	INTEGER		ihdarr (10)
	INTEGER		intdtf (3)
C------------------------------------------------------------------------
	iret = 0
C
C*      Convert access number to DM number.
C
c        print *, "inside GD_SRTT calling GD_FCHK"
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Reset the pointers in the DM file.
C
c        print *, "inside GD_SRTT calling DM_BEGS"
	CALL DM_BEGS ( igdfln, ier )
C
C*	Loop through all the grids in the file.
C
	ierr = 0
	DO WHILE  ( ierr .eq. 0 )
C
C*	    Get next column.
C
c            print *, "inside GD_SRTT calling DM_NEXT"
	    CALL DM_NEXT  ( igdfln, irow, icol, ierr )
c            print *, "inside GD_SRTT after DM_NEXT irow=",irow
c            print *, "inside GD_SRTT after DM_NEXT icol=",icol
c            print *, "inside GD_SRTT after DM_NEXT ierr=",ierr
C
	    IF  ( ierr .eq. 0 )  THEN
C
C*		Read in column header.
C
		CALL DM_RCLH  ( igdfln, icol, ihdarr, ier )
c                print *, "inside GD_SRTT after DM_RCLH ihdarr=",ihdarr
C
C*		Translate the first time into 3 integers.
C
		CALL TG_FTOI  ( ihdarr, intdtf, ier )
c                print *, "inside GD_SRTT after TG_FTOI:", intdtf
C
C*		Add to list of grids.
C
c            print *, "inside GD_SRTT calling GD_ADDT"
		CALL GD_ADDT  ( iacss, intdtf, ier )
	    END IF
	END DO
C*
	RETURN
	END
