	SUBROUTINE DM_RLBL  ( iflno, iret )
C************************************************************************
C* DM_RLBL								*
C*									*
C* This subroutine reads a label from a DM file.			*
C*									*
C* DM_RLBL  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = read error		*
C*					-20 = invalid DM file		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 3/87	Changed label format			*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C* K. Brill/NMC		 3/91	Add calls to MV_ library		*
C* M. desJardins/NMC	 5/91	Add logical vars for machine type	*
C* K. Brill/NMC		 8/91	Set KIEEET to FALSE after ELSE		*
C* S. Jacobs/EAI	 8/92	Added check for ULTRIX machine		*
C* K. Brill/NMC		 5/93	Added check for HP machine		*
C* S. Jacobs/EAI	10/93	Added check for ALPHA machine		*
C* S. Jacobs/NCEP	 2/01	Made MTLNUX a separate machine type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C*
	INTEGER		label (30)
	CHARACTER	dmlbl*28, lbl*28
	CHARACTER	dblbl*28, datab*128
	DATA		dmlbl / 'GEMPAK DATA MANAGEMENT FILE ' /
	DATA		dblbl / 'GEMPAK A2 DATABASE INTERFACE' /
        LOGICAL         dmsrc, dbsrc
C------------------------------------------------------------------------
        dmsrc = .false.
        dbsrc = .false.
C
C*	Check that the first seven words in the label contain the
C*	string:  "GEMPAK DATA MANAGEMENT FILE " or
C*      string:  "GEMPAK A2 DATABASE INTERFACE"
C
	CALL DM_RSTR  ( iflno, 1, 28, lbl, iret )
        IF  ( lbl .ne. dmlbl .and. lbl .ne. dblbl ) THEN
           iret = -20
           RETURN
	END IF
        IF ( lbl .eq. dmlbl ) dmsrc = .true.
        IF ( lbl .eq. dblbl ) dbsrc = .true.
        IF ( dbsrc ) THEN
           dbread = .true.
           datab = ''
	   CALL DM_RSTR  ( iflno, 1, 128, datab, iret )
           CALL ST_LSTR (datab, ilstr, iret )
	   CALL DB_INFO  ( datab(:ilstr), iret )
           IF ( iret .ne. 0 ) THEN 
              iret = -7
              RETURN
           END IF
           RETURN
	END IF
        IF ( dmsrc ) THEN
C*	   The first piece of information must be the machine type.
C*	   The file machine type is set to the current machine so
C*	   that no translation will be performed.
C
	   kmachn ( iflno ) = MTMACH
           CALL DM_RINT  ( iflno, 26, 1, mmmm, iret )
	   IF  ( iret .ne. 0 )  RETURN
	   IF  ( mmmm .gt. 100 )  THEN
	    ier = MV_SWP4 ( 1, mmmm, mmmm )
	   END IF
	   kmachn ( iflno ) = mmmm
C
C*	   Set flags for data type.
C
	   IF  ( ( kmachn (iflno) .eq. MTVAX  ) .or.
     +	         ( kmachn (iflno) .eq. MTULTX ) .or.
     +	         ( kmachn (iflno) .eq. MTALPH ) .or.
     +	         ( kmachn (iflno) .eq. MTLNUX ) .or.
     +	         ( kmachn (iflno) .eq. MTIGPH ) )  THEN
	       kvmst ( iflno ) = .true.
	     ELSE
	       kvmst ( iflno ) = .false.
	   END IF
	   IF  ( ( kmachn (iflno) .eq. MTSUN  ) .or.
     +	         ( kmachn (iflno) .eq. MTIRIS ) .or.
     +	         ( kmachn (iflno) .eq. MTAPOL ) .or.
     +	         ( kmachn (iflno) .eq. MTHP   ) .or.
     +	         ( kmachn (iflno) .eq. MTIBM  ) )  THEN
	       kieeet ( iflno ) = .true.
	     ELSE
	       kieeet ( iflno ) = .false.
	   END IF
	   IF  ( ( MTMACH .eq. MTVAX  ) .or.
     +	         ( MTMACH .eq. MTULTX ) .or.
     +	         ( MTMACH .eq. MTALPH ) .or.
     +	         ( MTMACH .eq. MTLNUX ) .or.
     +	         ( MTMACH .eq. MTIGPH ) )  THEN
	       mvmst = .true.
	     ELSE
	       mvmst = .false.
	   END IF
	   IF  ( ( MTMACH .eq. MTSUN  ) .or. ( MTMACH .eq. MTIRIS ) .or.
     +	         ( MTMACH .eq. MTAPOL ) .or. ( MTMACH .eq. MTIBM  ) .or.
     +           ( MTMACH .eq. MTHP ) )  THEN
	       mieeet = .true.
	     ELSE
	       mieeet = .false.
	   END IF
C
C*	   Set the file values of the missing data values to the current
C*	   system values so that random values will not be converted.
C
	   kmissd ( iflno ) = IMISSD
	   smissd ( iflno ) = RMISSD
C
C*	   Read the integer part of the label.
C
	   CALL DM_RINT ( iflno, 8, 23, label (8), iret )
	   IF ( iret .ne. 0 ) RETURN
C
C*	   Put the label values into the common variables.
C
	   kversn ( iflno ) = label (  8 )
	   kfhdrs ( iflno ) = label (  9 )
	   kpfile ( iflno ) = label ( 10 )
	   krow   ( iflno ) = label ( 11 )
	   krkeys ( iflno ) = label ( 12 )
	   kprkey ( iflno ) = label ( 13 )
	   kprowh ( iflno ) = label ( 14 )
	   kcol   ( iflno ) = label ( 15 )
	   kckeys ( iflno ) = label ( 16 )
	   kpckey ( iflno ) = label ( 17 )
	   kpcolh ( iflno ) = label ( 18 )
	   kprt   ( iflno ) = label ( 19 )
	   kppart ( iflno ) = label ( 20 )
	   kpdmgt ( iflno ) = label ( 21 )
	   kldmgt ( iflno ) = label ( 22 )
	   kpdata ( iflno ) = label ( 23 )
	   kftype ( iflno ) = label ( 24 )
	   kfsrce ( iflno ) = label ( 25 )
C
C*	   The following has been commented because the machine type
C*	   has already been determined at the beginning of the routine.
C--	   kmachn ( iflno ) = label ( 26 )
C
	   kmissd ( iflno ) = label ( 27 )
C
C*	   Read in the real values.
C
	   CALL DM_RFLT ( iflno, 31, 1, sval, iret )
	   smissd ( iflno ) = sval
C*
	   RETURN
	END IF
	END
