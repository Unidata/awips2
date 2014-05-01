c  ==================================================================
c  pgm:  rdxmrg (pthnam,iunit,ipr,xver,ixor,iyor,maxx,maxy,
c                user,sdatim,proces,vdatim,mxval,
c                ihfld,istat)
c
c   in: pthnam  .... xmrg file path name
c   in: iunit   .... xmrg file unit number
c   in: ipr     .... print device number
c  out: xver    .... xmrg file format version number
c  i/o: ixor    .... most west HRAP column
c  i/o: iyor    .... most south HRAP row
c  i/o: maxx    .... number of columns
c  i/o: maxy    .... number of rows
c  out: user    .... office generating the file
c  out: sdatim  .... date and time (Informix format) data saved
c  out: process  ... process generating the data
c  out: vdatim  .... valid date and time (Informix format)
c  out: mxval   .... maximum value of data in hundredths of millimeters
c  out: ihfld   .... array of HRAP data (southwest corner is ihfld(1))
c  out: istat   .... status code
c  ==================================================================
c
      subroutine rdxmrg (pthnam,iunit,ipr,xver,ixor,iyor,maxx,maxy,
     *                   user,sdatim,proces,vdatim,mxval,
     *                   ihfld,istat)
c
c....................................................................
c
c  Routine reads gridded values from a file in xmrg format
c
c....................................................................
c  Initially written by
c        Tim Sweeney, HRL                                 Nov 1997
c
c  Added three more fields to second record.  If extra fields not
c  found (error), then rewind and read as old unformat.
c        Tim Sweeney, HRL                                 Oct 1998
c....................................................................
c
      integer*2 ihfld(*)
c
      character*128 pthnam
ckwz.10/13/04.user should not include os.
ckwz      character user*10,sdatim*20,proces*8,vdatim*20
      character os*2,user*8,sdatim*20,proces*8,vdatim*20
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/rdxmrg.f,v $
     . $',                                                             '
     .$Id: rdxmrg.f,v 1.5 2005/03/18 14:38:43 wkwock Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('rdxmrg',1,1,ibug)
ccc      ibug = 1
c
      istat = 0
c
      os = ' '
      user = ' '
      sdatim = ' '
      proces = ' '
      vdatim = ' '
      mxval = -999
c
      iform = 2
      if (ibug.eq.1) write (ipr,*) 'iform=',iform
      irec = 0
      irow = 0
c
c  read header records
10    if (iform.ne.2) then
         rewind (iunit)
         irec=0
         endif
      if (iform.eq.2) then
         iform = 3
         if (ibug.eq.1) write (ipr,*) 'iform=',iform
c     version 2.1 format
         irec=irec+1
         read (iunit,err=10,end=90) ixor,iyor,maxx,maxy
         if (ibug.eq.1) write (ipr,*) 'ixor=',ixor,' iyor=',iyor,
     +      ' maxx=',maxx,' maxy=',maxy
         irec=irec+1
         read (iunit,err=15,end=90) os,user,sdatim,proces,vdatim,mxval,
     +      xver
         if (ibug.eq.1) write (ipr,*) 'os=',os,'user=',user,' sdatim=',
     +      sdatim,' proces=',proces,' vdatim=',vdatim,' mxval=',mxval,
     +      ' xver=',xver
         go to 17
15       iform = 1
         if (ibug.eq.1) write (ipr,*) 'iform=',iform
         go to 10
17       if (xver.le.0.0) xver = 2.1
         else if (iform.eq.1) then
c        version 2.0 format
            iform = 4
            if (ibug.eq.1) write (ipr,*) 'iform=',iform
            irec=irec+1
            read (iunit,err=60,end=90) ixor,iyor,maxx,maxy
            if (ibug.eq.1) write (ipr,*) 'ixor=',ixor,' iyor=',iyor,
     +         ' maxx=',maxx,' maxy=',maxy
            irec=irec+1
            read (iunit,err=60,end=90) os,user,vdatim(1:19),proces
            if (ibug.eq.1) write (ipr,*) 'os=',os,'user=',user,
     +         ' vdatim=',vdatim,' proces=',proces
            xver = 2.0
            write(ipr,20)
20    format (' NOTE: using old xmrg file format.')
         else
            write (ipr,30)
30    format (' ERROR: cannot read xmrg file headers.')
            istat=1
            go to 100
         endif
c
      do 35 i=1,len(proces)
         if (proces(i:i).eq.char(0)) then
            if (ibug.eq.1) write (ipr,*) 'i=',i,
     +         'proces(i:i)=',proces(i:i)
            proces(i:i)=' '
            endif
35       continue
c
      if (maxx.eq.0) then
         write (ipr,40) 'maxx'
40    format (' ERROR: value of variable ',a,' read from xmrg file ',
     +   'header is zero.')
         istat=1
         endif
      if (maxy.eq.0) then
         write (ipr,40) 'maxy'
         istat=1
         endif
      if (istat.ne.0) go to 100
c
c  read data records
      maxval=0
      do 50 irow=1,maxy
         ib = (irow-1)*maxx + 1
         ie = ib + maxx - 1
         irec=irec+1
         read (iunit,err=60,end=90) (ihfld(i),i=ib,ie)
         if (ibug.eq.1) write (ipr,*) 'ib=',ib,' ie=',ie,
     +      ' (ihfld(i),i=ib,ie)=',(ihfld(i),i=ib,ie)
         do 45 i=ib,ie
            if (ihfld(i).gt.maxval) maxval=ihfld(i)
45          continue
50       continue
      if (ibug.eq.1) write (ipr,*) 'maxval=',maxval
      go to 100
c
c  error reading record
60    if (irow.eq.0) then
         write (ipr,70) 'i/o error encountered',irec,
     +      pthnam(1:lenstr(pthnam))
70    format (' ERROR: ',a,' reading xmrg file header record ',i1,
     +   ' from file ',a,'.')
         istat=1
         else
            write (ipr,80) 'i/o error encountered',irow,irec
80    format (' ERROR: ',a,' reading xmrg data for row ',i4,
     +   ' from record ',i4,'.')
         istat=1
         endif
      go to 100
c
c  end-of-file encountered
90    if (irow.eq.0) then
         write (ipr,70) 'end-of-file encountered',irec,
     +      pthnam(1:lenstr(pthnam))
         istat=1
         else
            write (ipr,80) 'end-of-file encountered',irow,irec
         endif
c
100   return
c
      end

