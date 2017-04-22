c  module ofs_carryover_dates.f
c
c  desc 'return the number of carryover dates,
c        the dates themselves, and
c        the status of the dates
c        for a given carryover group
c        or segment'
c
      Subroutine ofs_carryover_dates(co_group,
     1                               f_group,
     2                               num_dates,
     3                               co_dates,
     4                               co_status)
c...................................................................
c  argument list:
c
c    co_group:  input   8-character carryover group id or 'Special'
c                        for special forecast groups
c                       for special fgroups use the fgroup id
c                        to find the first segment in the
c                        group and find carryover dates saved
c                        for that segment
c    f_group:   input   8-character forecast group id
c    num_dates: output  number of carryover dates
c    co_dates:  output  carryover dates as Julian hours
c                        since 1/1/1900/12Z
c    co_status: output  status of each co_date
c...................................................................
c
c  routine originally written by George Smith, HRL, November 14, 1992
c  modified to allow segment id input as well as carryover group
c   G. Smith - HRL - April 6, 1993
c...................................................................
c
      INCLUDE 'common/fccgd'                                            RTi     
      INCLUDE 'common/fccgd1'                                           RTi     
      INCLUDE 'common/fcrunc'                                           RTi     
      INCLUDE 'common/fcsegn'                                           RTi     
      INCLUDE 'common/fcunit'                                           RTi     
      INCLUDE 'common/ionum'                                            RTi     
c
c  Make id real so it can be compared with CGIDC or SEGID
c
      Real*4  co_group
      Real*4  f_group
      Real*4  co_group_temp
      Real*4  special
      Integer num_dates
      Integer co_dates
      Integer co_status
      Integer type_of_run
c
      Integer icg, idates
c
      Dimension co_group(2)
      Dimension f_group(2)
      Dimension co_group_temp(2)
      Dimension id(2)
      Dimension special(2)
      Dimension co_dates(20)
      Dimension co_status(20)
c
      Data special /4hSpec, 4hial /
c
c  Create minimal space for P, T, and TS arrays as place
c   holders for FGETSG - we will not actually fill these arrays
c
      Dimension P(1), T(1), TS(1)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/ofs_carryover_dates.f,v $
     . $',                                                             '
     .$Id: ofs_carryover_dates.f,v 1.2 1999/01/20 21:15:44 page Exp $
     . $' /
C  =====================================================================
C
c
c  If co_group ne 'Special' (i.e., a carryover group id was input)
c   copy id into co_group_temp and load dates and statuses.
c
c  If co_group eq 'Special (i.e., a special fg was selected)
c   find the first segment in the fgroup and to which
c    carryover group this segment belongs,
c   then copy id into co_group_temp and load dates and statuses.
c
      If(co_group(1) .ne. special(1) .or.
     1   co_group(2) .ne. special(2)) then
c
c  umemov moves n words (4 characters long each) from src to dest
c   arguments with syntax -- call umemov(src, dest, n)
c
	Call umemov(co_group, co_group_temp, 2)
c
      else
c
	type_of_run = 2
c
C  FILL CB FCRUNC - with list of segments in this fgroup
c
	CALL FCORDR(type_of_run, f_group, IER, D, MD)
c  fgetsg fills common block fcsegn with info for segment
c
	if(ier .ne. 0) Write(ipr, 950) f_group, ier
 950        FORMAT(" *** RETURN FROM FCORDR, fgroup = ", 2a4,
     1       "status = ", i3)
c
c  Now get load fcsegn common for the first segment in f_group
c
	MP = 1
	MT = 1
	MTS = 1
c  iopt = 1 says search by record number
	IOPT = 1
c  noparm = 1 suppresses reading P, T, and TS arrays
	NOPARM = 1
	IER = 0
C-----------------------------------------------------------------------
	CALL FGETSG(id, IRSGEX(1),
     1              MP,P,MT,T,MTS,TS,IOPT,NOPARM,IER)
C-----------------------------------------------------------------------
	if(IER .gt. 0) then
	  Write(ipr, 600) ier, IRSGEX(1)
 600      Format(" In ofs_carryover_dates, status of", i3,
     1           " returned from FGETSG for record ", i8)
c
	end if
c
c  Copy icgid from common fcsegn into co_group_temp
c   and load dates and statuses.
c
	Call umemov(ICGID, co_group_temp, 2)
c
      end if
C
C  FCCGD
C      COPY 1ST RECORD OF FILE FCCOGDEF INTO CB FCCGD
C-------
      CALL UREADT(KFCGD,1,NSLOTS,ISTAT)
C
      DO 100 ICG = 1, NCG
c
      num_dates = 0
c
C  FCCGD1
C      Fill common FCCGD1 for each carryover group
C--------
      CALL UREADT(KFCGD,ICOREC(ICG),CGIDC,ISTAT)
c
c  See if the carryover group in common FCCGD1 matches co_group_temp
c
      if(CGIDC(1) .eq. co_group_temp(1) .and.
     1   CGIDC(2) .eq. co_group_temp(2)) then
	  do 10 idate = 1, NSLOTS
	    if(ICODAY(idate) .gt. 1 .and.
     1         (IPC(idate) .eq. 1 .or. IPC(idate) .eq. 3))
     2      then
		num_dates = num_dates + 1
		co_dates(num_dates) = (ICODAY(idate) - 1) * 24 +
     1                                 ICOTIM(idate)
		co_status(num_dates) = IPC(idate)
	    end if
 10       Continue
	  Return
      end if
c
 100  Continue
c
      Return
      End
