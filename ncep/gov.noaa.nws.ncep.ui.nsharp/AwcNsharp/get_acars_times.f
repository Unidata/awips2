	SUBROUTINE GET_ACARS_TIMES ( sffile, time_list, ntimf, iret )
C************************************************************************
C* SNLIST								*
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
C* L. Hinson/AWC         3/03   Defined/Init lenaux                     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'ERMISS.FNC'
C*
	CHARACTER*(*)	sffile
	CHARACTER	time_list(500)*20
	CHARACTER	sffcur*72, pmdset (MMPARM)*4
	LOGICAL		newfil
C------------------------------------------------------------------------
C*	Initialize user interface.
C
	iret = 0
	CALL IN_BDTA  ( ier )
C
C*	Open the input file.
C
	sffcur = ' '
        CALL SFLFIL  ( sffile, sffcur, isffln, newfil, iflsrc,
     +                 pmdset, npmdst, iret )
	IF ( iret .eq. 0 ) THEN
C
C*	   Get input times and pointers.
C
	   CALL SF_GTIM  ( isffln, LLMXTM, ntimf, time_list, ier )
C
	   CALL SF_CLOS  ( isffln, iret )
	END IF
C*
	END

	SUBROUTINE	GET_ACARS_POINTS ( sffile, ntime, tlist, iret )
	INCLUDE         'GEMPRM.PRM'
	CHARACTER*(*)   sffile
	INTEGER		ntime
	CHARACTER	tlist(500)*20
	CHARACTER	times(LLMXTM)*15,stn*8,sffcur*72
        CHARACTER	pmdset (MMPARM)*4
	REAL		slat(MMHDRS), slon(MMHDRS), selv(MMHDRS)
	LOGICAL		newfil,pltval
	REAL		breaks(24)
	INTEGER		icolors(24), istns

c       do i=1,ntime
c          write(*,*) 'look dattim ',i,tlist(i)
c       end do

        ncolor    = 23
        mrktyp    = 1
        sizmrk    = .4
        mrkwid    = 1
        pltval    = .false.
        iposn     =  0
        do i=1,ncolor
c          breaks(i) = float(i * 10000)/float(ncolor)
           breaks(i) = float(i * 450)
           icolors(i) = 31 - i
        end do
        icolors(24) = 7
        breaks(24) = 10800

	CALL GG_CBAR('31/V/CR/.02;.5/.85;.01/',ncolor,breaks,
     +                icolors,ier)


	iret = 0
	sffcur = ' '
        isffln = 0
        CALL IN_BDTA  ( ier )
	CALL SFLFIL  ( sffile, sffcur, isffln, newfil, iflsrc,
     +	  pmdset, npmdst, iret )
        IF ( iret .eq. 0 ) THEN
           if(ntime .eq. 0) then
	      CALL SF_GTIM  ( isffln, LLMXTM, ntimf, times, ier )
c             write(*,*) 'get time ',ntimf,ier
           else
              ntimf = ntime
              do i=1,ntime
                 times(i) = tlist(i)(1:15)
              end do
           end if
           itime = 0
           iext = ier
           istns = 0
           do while ((itime .lt. ntimf).and.(iext.eq.0))
              itime = itime + 1
              CALL SF_STIM  ( isffln, times (itime), ier )
	      CALL SF_BEGS  ( isffln, ier )
C
C*              Loop through stations.
C
                iout = 0
                DO WHILE  ( iout .eq. 0 )
C
C*                  Get next station.
C
                    istns = istns + 1
                    CALL SF_SNXT  ( isffln, stn, istnm,
     +                              slat(istns), slon(istns), 
     +                              selv(istns), ispri, iout )
                END DO
                istns = istns - 1
           end do
            IF (istns .gt. 0) THEN
            CALL MAP_MARK (istns, slat, slon, selv,
     +          ncolor, breaks, icolors, mrktyp,
     +          sizmrk, mrkwid, pltval, ipos, iret)
            END IF
	   CALL SF_CLOS  ( isffln, iret )
	END IF
	CALL GEPLOT ( ier )

	RETURN
	END

        SUBROUTINE FLTPATH (lat1, lon1, lat2, lon2)
	REAL lat1,lat2,lon1,lon2
        REAL x(2), y(2)
        INTEGER iret

        CALL GSCOLR(1, iret)
        CALL GSLINE(10,2,1,2,iret)
        x(1) = lat1
        x(2) = lat2
        y(1) = lon1
        y(2) = lon2

        CALL GLINE ('M',2,x,y,iret)
        RETURN
        END

	SUBROUTINE MKNAM (ival,istr,iret)
	INTEGER	ival
	CHARACTER*(*) istr
	CHARACTER*4 ichnam
	INTEGER	nampos, icnt, iret

        nampos = 1
	ichnam = ' '
        iret = 0
        do while(ival .gt. 0)
	   if(nampos.gt.8) then
	      iret = -1
              return
           endif
           if(mod(ival,36) .lt. 10) then
              ichnam(nampos:nampos) = char(mod(ival,36) + 48)
           else
              ichnam(nampos:nampos) = char(mod(ival,36) + 65 - 10)
           endif
 
           ival = ival / 36
           nampos = nampos + 1
        end do
        icnt = 1
        do while(nampos .gt. 1)
           nampos = nampos - 1
           istr(icnt:icnt) = ichnam(nampos:nampos)
           icnt = icnt + 1
        end do
	RETURN
	END

	SUBROUTINE GET_APTS (isffln, data, ip1, ip2, ip3, apts)
	INTEGER	isffln
	REAL	data(*)
	INTEGER	ip1, ip2, ip3
	CHARACTER*(*)	apts(3)

        apts(1) = ' '
        apts(2) = ' '
        apts(3) = ' '
        if(ip1.gt.0) then
           ival = data(ip1)
	   call mknam(ival,apts(1),ier)
        endif
        if(ip2.gt.0) then
           ival = data(ip2)
	   call mknam(ival,apts(2),ier)
        endif
        if(ip3.gt.0) then
           ival = data(ip3)
	   call mknam(ival,apts(3),ier)
        endif

	RETURN
	END

        SUBROUTINE      GET_NEAREST ( sffile, ntime, tlist,
     +			    lat, lon, cstn, auxinfo)
                INCLUDE         'GEMPRM.PRM'
        CHARACTER*(*)   sffile
	CHARACTER*(*)	cstn, auxinfo
        CHARACTER       tlist(500)*20
        INTEGER         ntime
        REAL            lat,lon

        CHARACTER       times(LLMXTM)*15,sffcur*72
        CHARACTER       pmdset (MMPARM)*4
        REAL            slat, slon, selv
        LOGICAL         newfil
        CHARACTER       alat1*9,alon1*9,alat2*9,alon2*9
        CHARACTER       tarea*48,area*48,arecur*48
        CHARACTER       stn*8,astn*8,apts(3)*4,atime*15
        REAL            data (LLMXDT)
	REAL		dist,mindist
	INTEGER         lenaux
	
	lenaux=0

	nlev = 0
	mindist = 999999.

        rlatsz = .3
        rlonsz = .3
        CALL IN_BDTA  ( ier )

        CALL ST_RLCH (lat-rlatsz,3,alat1,ier)
        CALL ST_RLCH (lon-rlonsz,3,alon1,ier)
        CALL ST_RLCH (lat+rlatsz,3,alat2,ier)
        CALL ST_RLCH (lon+rlonsz,3,alon2,ier)
        tarea = alat1//';'//alon1//';'//alat2//';'//alon2

        CALL ST_RMBL(tarea,area,lenarea,ier)

	sffcur = ' '
        newfil = .true.
        isffln = 0
        iret = 0
        CALL SFLFIL  ( sffile, sffcur, isffln, newfil, iflsrc,
     +    pmdset, npmdst, iret )
	if(iret.ne.0) return
        CALL ST_FIND ( 'PRES', pmdset, npmdst, ip_pres, ier)
        CALL ST_FIND ( 'HGHT', pmdset, npmdst, ip_hght, ier)
        CALL ST_FIND ( 'TMPC', pmdset, npmdst, ip_tmpc, ier)
        CALL ST_FIND ( 'DWPC', pmdset, npmdst, ip_dwpc, ier)
        CALL ST_FIND ( 'SPED', pmdset, npmdst, ip_sped, ier)
        CALL ST_FIND ( 'DRCT', pmdset, npmdst, ip_drct, ier)
        CALL ST_FIND ( 'ORIG', pmdset, npmdst, ip_orig, ier)
        CALL ST_FIND ( 'DEST', pmdset, npmdst, ip_dest, ier)
        CALL ST_FIND ( 'RPTS', pmdset, npmdst, ip_rpts, ier)

        CALL LC_UARE ( area, newfil, isffln, arecur, astn, ier )

	IF ( iret .eq. 0 ) THEN
           if(ntime .eq. 0) then
              CALL SF_GTIM  ( isffln, LLMXTM, ntimf, times, ier )
           else
              ntimf = ntime
              do i=1,ntime
                 times(i) = tlist(i)(1:15)
              end do
           end if

        itime = 0
        iext = ier
        cstn = ' '
        do while ((itime .lt. ntimf).and.(iext.eq.0))
           itime = itime + 1
           CALL SF_STIM  ( isffln, times (itime), ier )
           CALL SF_BEGS  ( isffln, ier )
C
C*              Loop through stations.
C
           iout = 0
           DO WHILE  ( iout .eq. 0 )
C
C*                  Get next station.
C
               CALL SF_SNXT  ( isffln, stn, istnm, slat, slon,
     +                         selv, ispri, iret )
	       if(iret.eq.0) then
                  dist = (slat - lat)*(slat - lat) +
     +                   (slon - lon)*(slon - lon)
                  if(dist.lt.mindist) then
                     cstn = stn
	             atime = times (itime)
                     mindist = dist
	             CALL SF_RDAT (isffln, data, ihhmm, iret )
                     CALL GET_APTS (isffln, data, ip_orig, ip_dest,
     +                              ip_rpts, apts)
                  end if
	       else
                  iout = iret
               endif
           END DO
        end do
        CALL SF_CLOS  ( isffln, iret )
        end if
        if(mindist.lt.999999) then 
c	   write(auxinfo,*) 'Station ',cstn,' Time: ',atime,
c     +                apts(1),'->',apts(2),' [',apts(3),']'
	   write(auxinfo,*) cstn,' ',atime,
     +                apts(1),'->',apts(2),' [',apts(3),']'
           do i=1,len(auxinfo)
              if(ichar( auxinfo(i:i) ) .lt. 32) 
     +           auxinfo(i:i) = ' '
           end do
c          write(*,*) auxinfo
	   lenaux=len(auxinfo)
           call st_lstr(auxinfo,lenaux,iret)
           write(auxinfo(lenaux+1:),10) data(ip_pres),data(ip_hght),
     +        data(ip_tmpc), data(ip_dwpc), data(ip_sped), data(ip_drct)
10	   FORMAT(6 (1x,F8.2) )
c          if(ip_pres.gt.0) write(*,*) 'PRES = ',data(ip_pres)
c          if(ip_hght.gt.0) write(*,*) 'HGHT = ',data(ip_hght)
c          if(ip_tmpc.gt.0) write(*,*) 'TMPC = ',data(ip_tmpc)
c          if(ip_dwpc.gt.0) write(*,*) 'DWPC = ',data(ip_dwpc)
c          if(ip_sped.gt.0) write(*,*) 'SPED = ',data(ip_sped)
c          if(ip_drct.gt.0) write(*,*) 'DRCT = ',data(ip_drct)
        endif
	RETURN
	END





	SUBROUTINE	GET_ACARS_SND ( sffile, ntime, tlist,
     +                    mode, lat, lon, cstn, rdata, nlev )
	INCLUDE         'GEMPRM.PRM'
	CHARACTER*(*)   sffile,cstn
        CHARACTER       tlist(500)*20
	REAL		rdata(*)
	INTEGER		nlev, ntime
	REAL		lat,lon
	INTEGER		mode

	CHARACTER	times(LLMXTM)*15,sffcur*72
        CHARACTER	pmdset (MMPARM)*4
	REAL		slat, slon, selv, plat, plon
	LOGICAL		newfil
	CHARACTER	alat1*9,alon1*9,alat2*9,alon2*9
	CHARACTER	tarea*48,area*48,arecur*48
	CHARACTER	stn*8,astn*8,apts(3)*4
	REAL		data (LLMXDT)

        nlev = 0

	plat = -9999.
        plon = -9999.

        rlatsz = 0.7
        rlonsz = 0.7
        CALL IN_BDTA  ( ier )

        if(mode.eq.1) then
        CALL ST_RLCH (lat-rlatsz,3,alat1,ier)
        CALL ST_RLCH (lon-rlonsz,3,alon1,ier)
        CALL ST_RLCH (lat+rlatsz,3,alat2,ier)
        CALL ST_RLCH (lon+rlonsz,3,alon2,ier)
	tarea = alat1//';'//alon1//';'//alat2//';'//alon2
        else
           tarea = '@'//cstn
        endif

	CALL ST_RMBL(tarea,area,lenarea,ier)

	sffcur = ' '
	newfil = .true.
        isffln = 0
	iret = 0
	CALL SFLFIL  ( sffile, sffcur, isffln, newfil, iflsrc,
     +	  pmdset, npmdst, iret )
        
        if(iret.ne.0) return
        CALL ST_FIND ( 'PRES', pmdset, npmdst, ip_pres, ier)
        CALL ST_FIND ( 'HGHT', pmdset, npmdst, ip_hght, ier)
        CALL ST_FIND ( 'TMPC', pmdset, npmdst, ip_tmpc, ier)
        CALL ST_FIND ( 'DWPC', pmdset, npmdst, ip_dwpc, ier)
        CALL ST_FIND ( 'SPED', pmdset, npmdst, ip_sped, ier)
        CALL ST_FIND ( 'DRCT', pmdset, npmdst, ip_drct, ier)
        CALL ST_FIND ( 'ORIG', pmdset, npmdst, ip_orig, ier)
        CALL ST_FIND ( 'DEST', pmdset, npmdst, ip_dest, ier)
        CALL ST_FIND ( 'RPTS', pmdset, npmdst, ip_rpts, ier)

        CALL LC_UARE ( area, newfil, isffln, arecur, astn, ier )


        IF ( iret .eq. 0 ) THEN
           if(ntime .eq. 0) then
	      CALL SF_GTIM  ( isffln, LLMXTM, ntimf, times, ier )
           else
              ntimf = ntime
              do i=1,ntime
                 times(i) = tlist(i)(1:15)
              end do
           end if

           itime = 0
           iext = ier
           do while ((itime .lt. ntimf).and.(iext.eq.0))
              itime = itime + 1
              CALL SF_STIM  ( isffln, times (itime), ier )
	      CALL SF_BEGS  ( isffln, ier )
C
C*              Loop through stations.
C
                iout = 0
                if(nlev.ge.200) then
                   write(*,*) 'Found more than 200 observations'
                   write(*,*) 'Decrease time range'
                   iout = -1
                   iext = -1
                endif
                DO WHILE  ( iout .eq. 0 )
C
C*                  Get next station.
C
                    CALL SF_SNXT  ( isffln, stn, istnm, slat, slon,
     +                              selv, ispri, iret )
                    IF  ( iret .ne. 0 )  THEN
                        iout = iret
                      ELSE
                       nlev = nlev + 1
	               CALL SF_RDAT (isffln, data, ihhmm, iret )
                       do icnt=1,7
                          rdata((nlev-1)*7+icnt) = -999.
                          if(data(icnt).lt.-9998) data(icnt) =-999.
                       end do
                       if(ip_pres.gt.0) rdata((nlev-1)*7+2) =
     +                    data(ip_pres)
                       if(ip_hght.gt.0) rdata((nlev-1)*7+3) =
     +                    data(ip_hght)
                       if(ip_tmpc.gt.0) rdata((nlev-1)*7+4) =
     +                    data(ip_tmpc)
                       if(ip_dwpc.gt.0) rdata((nlev-1)*7+5) =
     +                    data(ip_dwpc)
                       if(ip_drct.gt.0) rdata((nlev-1)*7+6) =
     +                    data(ip_drct)
C       LJH modified this line to get wind speed in knots...SKNT does not work
                       if(ip_sped.gt.0) rdata((nlev-1)*7+7) =
     +                    data(ip_sped) * 1.94254
                       CALL GET_APTS (isffln, data, ip_orig, ip_dest,
     +                               ip_rpts, apts)
c 		       write(*,*) nlev,' ',times(itime),' ',stn,slat,slon,
c     +                 data(1),data(2),
c     +                 apts(1),' ',apts(2),' ',apts(3)
                       if(mode.eq.3) then
                          if(plat.lt.-9998) then
                             plat = slat
                             plon = slon
                          else
                             call fltpath (plat,plon,slat,slon)
                             plat = slat
                             plon = slon
                          endif
                       endif
                    END IF
                    if(nlev.ge.200) then
                       write(*,*) 'Found more than 200 observations'
                       write(*,*) 'Decrease time range'
                       iout = -1
                       iext = -1
                    endif
                END DO
           end do
	   CALL SF_CLOS  ( isffln, iret )
	END IF

C       ioff = 2 (pressure)
	ioff = 2
        isrt = 1
        do while(isrt.ne.0)
           isrt = 0
           do i=1,nlev-1
               j = (i - 1)*7
               if(rdata(j+ioff).lt.rdata(j+7+ioff)) then
                  isrt = isrt + 1
                  do k=1,7
                     rtemp = rdata(j+k)
                     rdata(j+k) = rdata(j+7+k)
                     rdata(j+7+k) = rtemp
                  end do
               endif
            end do
         end do
	RETURN
	END
