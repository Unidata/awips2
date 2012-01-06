	SUBROUTINE GET_GEM_STNS(snfile, counin, time_dat, stn_list, 
     +				nstns, sta_lat, sta_lon)

C-----------------------------------------------------------------------
C
C	Routine to read list of stations in sounding file at
C	given date/time.
C
C	John Hart, SPC Norman, OK   4/99
C       10/99 got rid of calling IN_BDTA here. -mkay
C-----------------------------------------------------------------------
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'ERMISS.FNC'

	CHARACTER*(*)	snfile, counin, time_dat
	CHARACTER	stn_list(2000)*18, arecur*48, stncnt*48
	CHARACTER	prmlst (MMPARM)*4, stid*8, cstnm*6
	CHARACTER	area*16
	REAL		sta_lat(*), sta_lon(*)
	LOGICAL		newfil, mrgflg, wrtflg, datflg
	INTEGER		isnfln, iflsrc, nparm, ivert, iret
C------------------------------------------------------------------------
C	Open sounding file
	wrtflg=.false.
	isnfln = 0
	CALL SN_OPNF(snfile, wrtflg, isnfln, iflsrc, nparm, prmlst,
     + 		     ivert, mrgflg, iret)
	IF (iret .ne. 0) THEN
                CALL ER_WMSG( 'SN_OPNF', iret, ' ', ier)
        END IF


C	Set Time/date
	CALL SN_STIM(isnfln, time_dat, iret)
	IF (iret .ne. 0) THEN
                CALL ER_WMSG( 'SN_STIM', iret, ' ', ier)
        END IF


C	Set search area
	area = 'DSET'
	arecur = ' '
	newfil=.false.
C	CALL LC_UARE('DSET', newfil, isnfln, arecur, stncnt, ier)
	CALL LC_UARE(area, newfil, isnfln, arecur, stncnt, ier)


C	Loop through each station, adding to list
	iout = 0
	nstns = 0
	DO WHILE ( iout .eq. 0 )
	    CALL SN_SNXT(isnfln, stid, istnm, slat, slon, selv,
     +			 iout)
	    CALL SN_QDAT(isnfln, datflg, iret)
	    if (datflg .eqv. .true.) then
	    	nstns = nstns + 1
 	    	CALL ST_INCH(istnm, cstnm, ier)
	    	stn_list(nstns) = stid // '  ' // cstnm
	    	sta_lat(nstns)  = slat
	    	sta_lon(nstns)  = slon
	    end if
	END DO


C	Close file and end
	CALL SN_CLOS(isnfln, iret)
	END
