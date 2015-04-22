	subroutine get_mdl_stns(maxstn, stn_list, sta_lat, sta_lon, 
     +                          nstn)
	parameter (MAXSTATIONS=2000)
	integer lunstn, iret, ier
	character stn_list(*)*18
	real sta_lat(*), sta_lon(*)
	character*128 tbfile
	data tbfile     /'sfstns.tbl'/

        CHARACTER       coun(MAXSTATIONS)*2, stid(MAXSTATIONS)*8
        CHARACTER       stat(MAXSTATIONS)*2
        CHARACTER       stnnam(MAXSTATIONS)*32, tbchrs(MAXSTATIONS)*14
        REAL            selv(MAXSTATIONS)
        INTEGER         isnm(MAXSTATIONS)
        INTEGER         ispri(MAXSTATIONS)

C
C* Add the station information
C
        call FL_TBOP(tbfile, 'stns', lunstn, iret)

        if (iret .ne. 0) then
            CALL ER_WMSG('FL', iret, ' ', ier)
            return
        endif

	nstn = 0

	if (maxstn .gt. MAXSTATIONS) then
C Don't overflow any space we have
	  maxstn = MAXSTATIONS
	endif

C
C*      Read in the stations.
C
        CALL TB_ASTN(lunstn, maxstn, nstn, stid, stnnam, isnm, 
     +               stat, coun, sta_lat, sta_lon, selv, ispri, tbchrs, 
     +               iret)

        if (iret .ne. 0) then
            CALL ER_WMSG('TB', iret, ' ', ier)
            return
        endif

	do i = 1, nstn
	  stn_list(i) = stid(i)
	enddo

C
C*      Close station table.
C
        CALL FL_CLOS(lunstn, iret)

	return
	end
