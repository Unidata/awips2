C Common blocks should be initialized before this routine is called

	subroutine getsndg(filnam, parmstr, dattim, stn, stype, data, 
     +                     nlvl, psfc, sfct, sfctd, sfch, sdir, sspd,
     +                     slat, slon, ier)

	character*(*) filnam, parmstr, dattim, stn
	real          data(*), psfc, slat, slon, sfct, sfctd, sfch
	integer       stype, nlvl, ier

C
C* Local variables
C

	parameter (nparms=40, maxlev=500)
c	real rdata(nparms, maxlev)
	real          rdata(8192)
	integer       num
	character     myparms(nparms)*12, cdata(nparms)*80
	character*12  prmdst(60)
	character*8   stid
	logical       cmpflg(nparms), chrflg(nparms)

	nlvl = 0
	ier  = 0

C Check to see if this is a model sounding. If so, call that routine
C and return. Point forecast soundings are stored just like normal 
C GEMPAK upper air data so they should be handled by this subroutine
C Should handle ACARS data the same way.
C
C Sounding type 2 is model, 1 observed, 3, PFC
C check globals.h for the #defines

        sfct =  -9999
        sfctd = -9999
        sfch  = -9999
        sdir  = -9999
        sspd  = -9999

CJL	print *, 'getsndg.f: filnam = ', filnam
CJL	print *, 'getsndg.f: parmstr = ', parmstr
CJL	print *, 'getsndg.f: dattim = ', dattim
CJL	print *, 'getsndg.f: stn = ', stn
CJL	print *, 'getsndg.f: stype = ', stype

	IF (stype .eq. 2) THEN
CJL	  print *, 'getsndg.f: filnam = ', filnam
          call GET_MDL_SND( filnam, parmstr, dattim, stn, data, 
     +                      numlev, psfc, sfct, sfctd, sfch, 
     +                      sdir, sspd, ier )
CJL       print *, 'getsndg.f: GET_MDL_SND ier = ', ier
	  if (ier .eq. 0) then
	    nlvl = numlev
	  else
	    nlvl = 0
	  endif
	  return
	ENDIF

C
C*      Open file.
C
        CALL SN_OPNF( filnam, .false., isnfln, isrc, npmdst, prmdst,
     +                ivert, mrgflg, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from SN_OPNF was ', ier
	  return
	ENDIF

	CALL PC_INIT( ivert, npmdst, prmdst, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from PC_INIT was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	ENDIF

C
C*          Define the level parms we want to compute
C 
	call ST_CLST( parmstr, ';', 'UNKNOWN', nparms, myparms, num, 
     +                ier )
	IF (ier .lt. 0 .or. ier .gt. 1) THEN
	  print*,'ier from ST_CLST was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	ENDIF

	CALL PC_DFLV( num, myparms, chrflg, cmpflg, np, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from PC_DFLV was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	ENDIF

C
C*          Set conditions.
C
        IF ( num .gt. 0 ) THEN
	  CALL PC_SLCD( num, myparms, ier )
	  IF (ier .ne. 0) THEN
	    print*,'ier from PC_SLCD was ', ier
	    CALL SN_CLOS( isnfln, ier )
	    return
	  ENDIF
	ENDIF

C HEY. I DON'T NEED TO COMPUTE ANY STATION PARAMETERS
C I'LL LEAVE THE CODE IN HERE THOUGH (COMMENTED OUT)

C
C*          Determine list of calculable station parameters.
C
C        CALL PC_DFST( np, tmprm, chrflg, cmpflg, nn, ier )
C
C*          Set conditions on station parameters.
C
C        IF ( nstnp .gt. 0 )  CALL PC_SSCD( nstnp, tmcnd, ier )

C
C*          Set the time.
C
	print *, "Trying to set the time to ", dattim
        CALL SN_STIM( isnfln, dattim, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from SN_STIM was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	ENDIF

C 
C* Set the station
C

	CALL SN_SSTN( isnfln, stn, stid, istnm, slat, slon, 
     +                selv, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from SN_SSTN was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	ENDIF

        CALL SN_RDATJH( isnfln, ndlev, rdata, ihhmm, ier )
	IF (ier .ne. 0) THEN
	  print*,'ier from SN_RDAT was ', ier
	  CALL SN_CLOS( isnfln, ier )
	  return
	endif

        IF  ( ier .eq. 0 )  THEN
          ispri = 0
          CALL PC_SSTN( stid, istnm, slat, slon, selv,
     +                  ispri, ihhmm, ndlev, ier )
	  if (ier .ne. 0) then
	    print*,'ier from PC_SSTN was ', ier
	    CALL SN_CLOS( isnfln, ier )
	    return
	  endif
        END IF
	print *, slat, slon
C
C Do not compute any station parms
C        CALL PC_CMST( rdata, data, cdata, ier )
C	print*,'ier from PC_CMST was ', ier

	knt = 1

        DO  i = 1, ndlev
	  iloc = (i-1) * num + 1
	  DO j = iloc, iloc + num
	    data(j) = -9999.0
	  ENDDO
          CALL PC_CMLV( i, rdata, data(iloc), cdata, 
     +                  ier )
        END DO

C           Close the file.
C
	CALL SN_CLOS( isnfln, ier )
	if (ier .ne. 0) then
	  print*,'ier from SN_CLOS was ', ier
	  return
	endif

	nlvl = ndlev

	return
	end
