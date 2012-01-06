	SUBROUTINE GET_GEM_STNS ( snfile, counin, time_dat, stn_list, 
     +				   nstns, sta_lat, sta_lon )
C************************************************************************
C* GET_GEM_STNS								*
C*									*
C* This program lists data from a sounding dataset.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* M. desJardins/GSFC	 4/89	Modify to list unmerged data		*
C* S. Schotz/GSC	 8/90	Corrected bogus error message for 	*
C*				unmerged listing			*
C* J. Whistler/SSAI	 5/91	Changed output*20 to output*48		*
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNLUPD			*
C* S. Jacobs/NMC	 3/95	Changed call to SNLLEV to pass file num	*
C* T. Piper/SAIC	 1/02	Initialized isnfln			*
C* T. Lee/SAIC		10/02	Used * in stn_list dimension		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, counin, time_dat
	CHARACTER	stn_list(*)*18, arecur*48, stncnt*48
	CHARACTER	snfcur*72, pmdset (MMPARM)*4, voutc*4, snparm*72
	CHARACTER	prmlst (MMPARM)*4, stnprm (MMPARM)*4, stid*8
	CHARACTER	stat*4, coun*4, cstnm*6, area*16
	REAL		data (LLMXDT), sta_lat(*), sta_lon(*)
	LOGICAL		newfil, mrgflg
C------------------------------------------------------------------------
C*	Initialize user interface.
C
	nstns = 0
	isnfln = 0

c	CALL IN_BDTA  ( ier ) 11/19
C
C*	Open the input file.
C
C	Initialize snfcur, since file is always closed at exit (chiz)
	snfcur = ' '
        CALL SNLFIL  ( snfile, snfcur, isnfln, newfil, iflsrc, pmdset,
     +		       npmdst, ivert, mrgflg, iret )

C
C*              Get parameter information.
C
	snparm = 'PRES;TMPC;DWPC;DRCT;SPED;HGHT'
	voutc = 'PRES'
	CALL SNLPRM  ( snparm, ' ', voutc, pmdset,
     +		       npmdst, nparms, prmlst, nstnp,
     +		       stnprm, iret )

C
C*	Set the area to DSET.
C       area must be passed as variable rather than string since value is changed in ST_LCUC
	arecur = ' '
	area = 'GAREA'
c	CALL LC_UARE ( 'DSET', snfile, isnfln, arecur, stncnt, ier )
	CALL LC_UARE ( area, newfil, isnfln, arecur, stncnt, ier )
C
C*	Get input times and pointers.
C
	CALL SN_STIM  ( isnfln, time_dat, ier )
C
	iout = 0
	DO WHILE ( iout .eq. 0 )
	    CALL SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv,
     +			    iout )
	    IF ( iout .eq. 0 ) THEN
		CALL SN_RDAT  ( isnfln, ndlev, data, ihhmm, ier )
		IF ( ier .eq. 0 ) THEN
		    CALL SN_QSTN  ( isnfln, stid, istnm, slat, slon, 
     +				    selv, stat, coun, iret )
C		    IF ( coun .eq. counin ) THEN 
			nstns = nstns + 1
			CALL ST_INCH ( istnm, cstnm, ier )
			stn_list(nstns) = stid // '  ' // cstnm
			sta_lat(nstns)  = slat
			sta_lon(nstns)  = slon
C		    END IF
		END IF
	    END IF
	END DO

c	PRINT *, nstns, " Stations Found."
c	write(*,*) 'Stations Found ',nstns
	CALL SN_CLOS  ( isnfln, iret )

C*
	END
