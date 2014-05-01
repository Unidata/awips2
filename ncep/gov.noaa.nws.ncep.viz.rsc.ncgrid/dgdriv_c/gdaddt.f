	SUBROUTINE GD_ADDT  ( iacss, intdtf, iret )
C************************************************************************
C* GD_ADDT								*
C*									*
C* This subroutine adds a grid time to the sorted grid time list.	*
C*									*
C* GD_ADDT  ( IACSS, INTDTF, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	INTDTF (3)	INTEGER		Date, time, forecast time	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* D. Kidwell/NCEP	 3/99	Added function call TG_YYMD for Y2K     *
C* R. Tian/SAIC		 1/04	Added GD_FCHK call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	INTEGER		intdtf (*), TG_YYMD
C*
	LOGICAL		equal, found
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert access number to DM number.
C
	CALL GD_FCHK ( iacss, igdfln, iret )
	IF ( iret .ne. 0 ) THEN
	    RETURN
	END IF
c        print *, "GD_ADDT:", intdtf (1), intdtf (2), intdtf (3)
C
C*	Loop through all the times checking for this time.
C
	equal = .false.
	found = .false.
	igrid = ktgrid ( igdfln )
	intd1 = TG_YYMD ( intdtf (1) )
	DO WHILE  ( ( igrid .ge. 1 ) .and. ( .not. found ) )
C
C*	    See if the times are the same.
C
	    IF  ( ( intdtf (1) .eq. igdatm (1,igrid,igdfln) ) .and.
     +		  ( intdtf (2) .eq. igdatm (2,igrid,igdfln) ) .and.
     +		  ( intdtf (3) .eq. igdatm (3,igrid,igdfln) ) )  THEN
		equal = .true.
		found = .true.
C
C*		Check to see if time is after this time.
C
	      ELSE IF  ( ( intd1 .gt. TG_YYMD ( igdatm(1,igrid,igdfln)))
     +					.or.
     +			(( intdtf (1) .eq. igdatm (1,igrid,igdfln) )
     +					.and.
     +			 ( intdtf (2) .gt. igdatm (2,igrid,igdfln) ))
     +					.or.
     +			(( intdtf (1) .eq. igdatm (1,igrid,igdfln) )
     +					.and.
     +			 ( intdtf (2) .eq. igdatm (2,igrid,igdfln) )
     +					.and.
     +			 ( intdtf (3) .gt. igdatm (3,igrid,igdfln) )))
     +								THEN
		found = .true.
C
C*		Otherwise, decrement counter and continue.
C
	      ELSE
		igrid = igrid - 1
	    END IF
	END DO
C
C*	Update counter if this time is already in the file.
C
	IF  ( equal )  THEN
	    ndattm (igrid,igdfln) = ndattm (igrid,igdfln) + 1
C
C*	    Otherwise insert in the list.
C
	  ELSE
	    igrid = igrid + 1
	    DO  i = ktgrid ( igdfln ), igrid, -1
		igdatm (1,i+1,igdfln) = igdatm (1,i,igdfln)
		igdatm (2,i+1,igdfln) = igdatm (2,i,igdfln)
		igdatm (3,i+1,igdfln) = igdatm (3,i,igdfln)
		ndattm (i+1,igdfln)   = ndattm (i,igdfln)
	    END DO
	    igdatm (1,igrid,igdfln) = intdtf (1)
	    igdatm (2,igrid,igdfln) = intdtf (2)
	    igdatm (3,igrid,igdfln) = intdtf (3)
	    ndattm (igrid,igdfln)   = 1
	    ktgrid  (igdfln) = ktgrid (igdfln) + 1
	END IF
C
C*	Make sure the grid list appears unsorted.
C
	ksrtl (1,1,igdfln) = 0
C
C*	Increment total number of grids.
C
	kgrid (igdfln) = kgrid (igdfln) + 1
C*
	RETURN
	END
