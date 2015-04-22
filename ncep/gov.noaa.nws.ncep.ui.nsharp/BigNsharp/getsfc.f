
C*************************************************************
C*                                                           *
C* getsfcdata                                                *
C*                                                           *
C* subroutine to read surface obs from a METAR file          *
C* in_bdta() should be called outside of this routine        *
C*                                                           *
C* mkay                                                      *
C* 9/22/98                                                   *
C*************************************************************

	subroutine getsfcdata(filnam, stnid, curtim, timestr,
     +                        temp, dwpt, drct, sknt)

c Passed variables
	character*(*) filnam
	character*(*) curtim
	character*(*) stnid
	character*(*) timestr
	real          temp, dwpt, drct, sknt

c Local variables
	integer ihhmm, np
	integer nparms, isffln, ispri, iret, ntime, maxtim
	data maxtim /200/
	character timlst(200)*12, dattim*12, coun*2, parms(40)*4
	character cparms(4)*4, stat*2
	data cparms /'TMPF', 'DWPF', 'DRCT', 'SKNT'/
	logical cmpflg(4)
	data cmpflg /.TRUE., .TRUE., .TRUE., .TRUE./
	logical chrflg(4)
	data chrflg /.FALSE., .FALSE., .FALSE., .FALSE./
	logical levflg(4)
	data levflg /.TRUE., .TRUE., .TRUE., .TRUE./
	real data(40), rdata(4), rlat, rlon, elev
	character cdata(4)*80

	drct = -9999.0
	sknt = -9999.0
	temp = -9999.0
	dwpt = -9999.0

	timestr = '          '

	CALL SF_OPNF(filnam, .false., isffln, iflsrc, nparms, parms,
     +               iret)
	if (iret .ne. 0) then
	  print*,'GETSFC: SF_OPNF: ',iret
	  print*,'GETSFC: Could not open surface file.'
	  return
	endif

c Make sure we don't exceed array bounds

	if (nparms .gt. 40) then
	  print*,'GETSFC: ERROR: nparms > array bounds'
	  CALL SF_CLOS(isffln, iret)
	  return
	endif

	CALL SF_GTIM(isffln, maxtim, ntime, timlst, iret)
	if (iret .ne. 0) then
	  print*,'GETSFC: SF_GTIM: ',iret
	  CALL SF_CLOS(isffln, iret)
	  return
	endif

c Here we should pass a flag that says either use the requested time or
c the latest time

	if (curtim(1:6) .eq. 'latest' .or. 
     +      curtim(1:6) .eq. 'LATEST') then
	  dattim = timlst(ntime)
	else
c Passing ntime to TI_FIND causes an error that I can't explain
c simple fix gets us past it. can't explain why!
c mkay 8/10/99
	  it = ntime
	  CALL TI_FIND(curtim, it, timlst, dattim, ntime, 
     +                 timlst, iret)
	  if (iret .eq. 0) then
	    dattim = timlst(1)
	  else
	    CALL SF_CLOS(isffln, iret)
	    return
	  endif
	endif

	timestr = dattim

c Strip off the @ symbol

	CALL SF_TSTN(isffln, stnid(2:), iret)
	if (iret. ne. 0) then
	  print*,'GETSFC: SF_TSTN: ',iret
	  print*,'Error finding data for station ',stnid
	  CALL SF_CLOS(isffln, iret)
	  return
	endif

	CALL SF_TTIM(isffln, dattim, iret)
        IF  ( iret .ne. 0 )  THEN
	  print*,'GETSFC: SF_TTIM: ',iret
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

C
C*      Set PC package.
C
        CALL PC_INIT ( 0, nparms, parms, ier )
        IF  ( ier .ne. 0 )  THEN
	  print*,'PC_INIT: ',ier
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

C
C Define the parameters that we want
C
        CALL PC_DFLS ( 4, cparms, chrflg, cmpflg, 
     +                 levflg, np, iret )
        IF  ( iret .ne. 0 )  THEN
	  print*,'PC_DFLS: ',iret
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

c	do i=1,4
c	  cdata(i) = ''
c	enddo

c	CALL PC_SLCD(4, cdata, iret)
c	print*,'PC_SLCD: ', iret
c        IF  ( iret .ne. 0 )  THEN
c	  print*,'DAMNIT2 ',iret
c	  RETURN
c	ENDIF

C
C*      Get requested station info.
C
	CALL SF_QSTN ( isffln, stnid(2:), istnm, rlat, rlon, elev,
     +                 ispri, stat, coun, iret )
        IF  ( iret .ne. 0 )  THEN
	  print*,'SF_QSTN: ', iret
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

        CALL PC_SSTN ( stnid(2:), istnm, rlat, rlon, elev, ispri,
     +                 ihhmm, 1, ier )
        IF  ( ier .ne. 0 )  THEN
	  print*,'PC_SSTN: ', ier
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

C
C*      Read station data, check for errors.
C
        CALL SF_RDAT ( isffln, data, ihhmm, iret )
        IF  ( iret .ne. 0 )  THEN
	  print*,'SF_RDAT returned ', iret
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

C
C*      Compute parameters when there is no error.
C
        CALL PC_STIM ( ihhmm, ier )
        IF  ( ier .ne. 0 )  THEN
	  print*,'PC_STIM: ', ier
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF

        CALL PC_CMVS ( 0., 0, data, rdata, cdata, ier )
        IF  ( ier .ne. 0 )  then
	  print*,'PC_CMVS: ', ier
	  CALL SF_CLOS(isffln, iret)
	  RETURN
	ENDIF 

	CALL SF_CLOS(isffln, iret)

	IF (ier .eq. 0) THEN
	  temp = rdata(1)
	  dwpt = rdata(2)
	  drct = rdata(3)
	  sknt = rdata(4)
	  print*,'TEMP: ', temp
	  print*,'DWPT: ', dwpt
	  print*,'DRCT: ', drct
	  print*,'SKNT: ', sknt
	ELSE
	  temp = -9999.0
	  dwpt = -9999.0
	  drct = -9999.0
	  sknt = -9999.0
	ENDIF

	return
	end
