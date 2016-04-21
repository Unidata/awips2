C$PRAGMA C (GET_APPS_DEFAULTS)
c  ==================================================================
c  pgm:  wrxmrg (idev,iupr,xor,yor,maxx,maxy,
c                user,sdatim,proces,vdatim,mxval,
c                ihfld,istat)
c
c   in: idev    .... xmrg file device number
c  out: iupr    .... debug and error output device
c   in: xor     .... most west HRAP column
c   in: yor     .... most south HRAP row
c   in: maxx    .... number of columns
c   in: maxy    .... number of rows
c   in: user    .... office generating file
c   in: sdatim  .... date and time data saved
c   in: proces  .... process flag
c   in: vdatim  .... valid date and time based on LSTCMPDY
c   in: mxval   .... maximum value of data in unita units
c   in: ihfld   .... data array, first element is western most
c                    column in southern most row
c  i/o: istat   .... completion code
c  ==================================================================
      subroutine wrxmrg (idev,iupr,xor,yor,maxx,maxy,
     1                   user,sdatim,proces,vdatim,mxval,
     2                   ihfld,istat)
c....................................................................
c  Routine writes gridded values to a file using xmrg format (same
c  format as estimated rainfall from radar)
c
c....................................................................
c  Initially written by
c        Tim Sweeney, HRL                                 Nov 1997
c
c  Added three more fields to second record
c        Tim Sweeney, HRL                                 Oct 1998
c....................................................................
c
      integer xor,yor
      integer*2 ihfld(*)
c
      character OSStr*2,OSToken*8,user*8,sdatim*20,proces*8,vdatim*20
	  integer OSTknLen,OSStrLen
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/wrxmrg.f,v $
     . $',                                                             '
     .$Id: wrxmrg.f,v 1.1 2006/05/03 13:44:00 gsood Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
      istat = 0
      irec = 0
      irow = 0
      xver = 2.1
	  OSStr = ' '
	  
	  OSToken='HOSTACRO'
	  OSTknLen=lenstr(OSToken)
	  call get_apps_defaults (OSToken,OSTknLen,OSStr,OSStrLen)
c
c  format version 2.1
      irec=irec+1
      write (idev,err=20) xor,yor,maxx,maxy
      if (ibug.gt.0) write (iupr,*) 'in wrxmrg - xor=',xor,' yor=',yor,
     +   ' maxx=',maxx,' maxy=',maxy
      irec=irec+1
      write (idev,err=20) OSStr,user,sdatim,proces,vdatim,mxval,xver
c
c  data records
      do 10 irow=1,maxy
         ib = (irow-1)*maxx + 1
         ie = ib + maxx - 1
         irec=irec+1
         write (idev,err=20) (ihfld(i),i=ib,ie)
10       continue
      go to 50
c
20    istat = 1
      if (irow.eq.0) then
         write (iupr,30) irec
30    format (' ERROR: writing grid file header record ',i2,'.')
         else
           write (iupr,40) irow,irec
40    format (' ERROR: writing gridded data to row ',i4,
     +   'to record ',i4,'.')
         endif
c
50    return
c
      end

