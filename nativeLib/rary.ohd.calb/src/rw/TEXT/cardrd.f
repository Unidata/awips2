C$PRAGMA C (GET_APPS_DEFAULTS)
cfan add for pgf90 port 7/3/01
C$PRAGMA C (UGZRD,UGZRWD,UGZNAM)
C MODULE CARDRD
C-----------------------------------------------------------------------
C  Routine to read a month of data from a DATACARD file.
C     ------------------------------------------------------------------
C     notes: (1) This routine reads a month of data from an open file.
C            (2  If the specified month does not fall in the
C                period of record for the TS file and "mflag" is 1,
C                then the data array will be filled with missing data.
C                If "mflag" is 0, then an error message will be printed.
C     ------------------------------------------------------------------
C     variables:
C
C     ijmon     .... Julian month as read from file
C     icount    .... count of the total number of data read for month
C     ierror    .... error code
C     mcard     .... counter for cards within month
C     mflag     .... indicates whether missing data is allowed (1 if it
C                    is, 0 if not)
C     month     .... month read from data file
C     ncards    .... number of data cards for a month
C     ndata     .... number of data values for month
C     nday      .... number of days in "month"
C     ndpl      .... number of data values for current line
C     ndatapl   .... number of data values for full line
C     nts       .... number of TS to read (this must be one)
C     oformat   .... format for data read from file
C     oformat2  .... format for entire line read from file
C     irjmon    .... Julian month requested
C     irmonth   .... requested month (1-12)
C     iryear    .... requested year (4-digit)
C     iryear0   .... original value of "iryear"
C     staid     .... location identifier
C     tsdata    .... array to hold data values for month
C     iunit     .... unit number for TS file
C     iyear     .... year read from data file
C     ------------------------------------------------------------------

      subroutine cardrd ( nts, iunit, oformat, ijmon1, ijmon2, convf1,
     +                    convf2, itime, irmonth, iryear0, tsdata,
     +                    mflag, ierror )

      include 'common/fdbug'
      include 'common/ionum'
      include 'common/unitno'

      dimension tsdata(*)
      character staid*12
      character oformat*12,oformat2*32
      character filnam*128
      character*20 envvar/' '/,usegzio/' '/
      character*50 origin/'?'/
      character*150 string
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cardrd.f,v $
     . $',                                                             '
     .$Id: cardrd.f,v 1.11 2002/02/11 16:54:38 dws Exp $
     . $' /
C    ===================================================================
C

      iopnum=-1
      call fstwhr ('CARDRD  ',iopnum,oldopn,iopold)

      ierror=0
      numrewind=0
      string=' '
      lstring=len(string)
      valmsng=-999.0

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CARDRD'
         endif

C  Check if more than one time series to be read
      if (nts.gt.1) then
         write (ipr,10)
10    format ('0**ERROR** Only one TS can be read at a time.')
         call error
         ierror=1
         go to 200
         endif

C  Make sure month and year are in correct range
      if ((irmonth.lt.1) .or. (irmonth.gt.12)) then
         write (ipr,20) irmonth, iryear0
20    format ('0**ERROR**  Requested month date is invalid: ',
     +   i2,'/',i4)
         call error
         ierror=1
         go to 200
         endif
      iryear=iryear0
      call ddycdl (iryear,irmonth,1)

C  Determine whether requested month falls within TS period of record
      irjmon=iryear*12+irmonth
      if ((irjmon.lt.ijmon1).or.(irjmon.gt.ijmon2)) then
         if (mflag.eq.1) then
            call ddgcdm (iryear,irmonth,nday)
            ndata=nday*24/itime
            do 30 i=1, ndata
               tsdata(i)=valmsng
30             continue
            go to 200
            else
               write (ipr,40) irmonth, iryear
40    format ('0**ERROR** Missing data not allowed: ',i2,'/',i4)
               call error
               ierror=1
               go to 200
               endif
         endif
 
C  Get the number of data per line from the output format
C  (xFx.x) or (xxFx.x), and set the complete output format.
      if ((oformat(3:3).eq.'f').or.(oformat(3:3).eq.'F')) then
         read (oformat(2:2),'(i1)') ndatapl
         else if ((oformat(4:4).eq.'f').or.(oformat(4:4).eq.'F')) then
            read (oformat(2:3),'(i2)') ndatapl
         endif
      oformat2='(a,i2,i2,4x,'//oformat(2:12)

C  Check if to use gzip input/output
      igzio=0
      if (igziord.eq.-1) then
         igziord=0
         envvar='calb_gzio_read'
         call get_apps_defaults (envvar,lenstr(envvar),
     *      usegzio,lusegzio)
         if (lusegzio.gt.0) then
            musegzio=len(usegzio)
            if (lusegzio.gt.musegzio) then
               write (ipr,55) envvar(1:lenstr(envvar)),
     +            lusegzio,musegzio
55    format ('0**ERROR** in CARDRD - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
               call error
               ierror=1
               go to 200
               endif
            if (usegzio(1:lusegzio).eq.'on') then
               igziord=1
               igzio=1
               endif
            endif
         else
            igzio=igziord
         endif

50    mcard=1
      ncards=0
      icount=0
      ndpl=ndatapl

CCC      write (ipr,*) ' iunit=',iunit

60    if (igzio.eq.0) then
         if (iascii.eq.1) read (iunit,oformat2,end=170,iostat=iostat,
     +      err=130)
     +      staid,month,iyear,(tsdata(icount+i),i=1,ndpl)
         if (iascii.eq.0) read (iunit,end=170,iostat=iostat,
     +      err=130)
     +      staid,month,iyear,(tsdata(icount+i),i=1,ndpl)
         if (iostat.ne.0) then
            write (ipr,65) 'read',iunit,iostat
            call error
            ierror=1
            go to 200
            endif
65     format ('0**ERROR** in CARDRD - Doing ',a,' for ',
     +   'DATACARD TS file on unit ',i2,'. iostat=',i3)
         endif

      if (igzio.eq.1) then
C     Read entire line in as string and use the "internal"
C     reads of FORTRAN to parse the string.  Store current string
C     for use in error condition output.
         call ugzrd (iunit, string, lstring, ierror)
         origin='error occurred in ugzrd'
         if (ierror.eq.1) go to 130
         if (ierror.eq.2) go to 170
         origin='error occurred in ''read (string,oformat2,'''
         read (string,oformat2,iostat=iostat,err=130)
     +      staid, month, iyear, (tsdata(icount+i),i=1,ndpl)
         if (iostat.ne.0) then
            write (ipr,67) 'internal read',oformat(1:lenstr(oformat)),
     +        iostat
67     format ('0**ERROR** in CARDRD - Doing ',a,' using ',
     +   'format ',a,'. iostat=',i3)
            call error
            ierror=1
            go to 200
            endif
         endif

      icount=icount+ndpl
      if (ncards.eq.0) then
         call ddycdl (iyear,month,1)
         call ddgcdm (iyear,month,nday)
         ndata =nday*24/itime
         ncards=ndata/ndpl
         if (((ndata/ndpl)*ndpl).ne.ndata ) ncards=ncards+1
         ijmon=iyear*12+month
         endif

C  Check for run period greater than a year and skip header info
      if (ijmon.gt.irjmon) then
         numrewind=numrewind+1
         if (numrewind.gt.1) then
            write (ipr,70) irmonth, iryear, staid, iunit
70    format ('0**ERROR** cannot find ',I2.2,'/',I4.4,
     +   ' in file for station ',a,' read from unit number ',I2,'.')
             go to 130
             endif
         write (ipr,*) 'In CARDRD - Rewinding unit number ',iunit,
     +      '. Rewinding ',numrewind,' times.'
         if (igzio.eq.0) then
            rewind (iunit)
80          read (iunit,'(a)',end=170,iostat=iostat,err=130) string
            if (iostat.ne.0) then
               write (ipr,65) 'read',iunit,iostat
               call error
               ierror=1
               go to 200
               endif
            if (string(1:1).eq.'$' ) go to 80
            read (iunit,'(a)',end=170,iostat=iostat,err=130) string
            if (iostat.ne.0) then
               write (ipr,65) 'read',iunit,iostat
               call error
               ierror=1
               go to 200
               endif
            go to 50
            endif
         if (igzio.eq.1) then
C        Use the gzip rewind routine
            call ugzrwd (iunit, ierror)
100         call ugzrd (iunit, string, lstring, ierror)
            if (ierror.ne.0) go to 130
            if (string(1:1).eq.'$' ) go to 100
            call ugzrd (iunit, string, lstring, ierror)
            if (ierror.ne.0) go to 130
            go to 50
            endif
         endif

C  Check if all cards have been read
      if (mcard.lt.ncards) then
         mcard=mcard+1
         if (mcard.eq.ncards) then
            ndpl=ndata-ndpl*(ncards-1)
            endif
         go to 60
         endif

C  Check if this is the month requested
      if (ijmon.ne.irjmon) go to 50

C  Convert data
      do 110 i=1, ndata
C     Check if missing data            
         if (ifmsng(tsdata(i)).eq.0) then
            tsdata(i)=tsdata(i)*convf1+convf2
            endif
110      continue
      go to 200

C  Error reading file
130   filnam='?'
CCC      write (ipr,*) origin
      if (igzio.eq.0) then
         backspace iunit
         read (iunit,'(a)',end=170,iostat=iostat,err=155) string
         if (iostat.ne.0) then
            write (ipr,65) 'read',iunit,iostat
            call error
            ierror=1
            go to 200
            endif
         inquire (unit=iunit,iostat=iostat,name=filnam)
         if (iostat.ne.0) then
            write (ipr,65) 'inquire',iunit,iostat
            call error
            ierror=1
            go to 200
            endif
         endif
      if (igzio.eq.1) then
         call ugznam (iunit, filnam, lfilnam, ierr)
         endif
      write (ipr,140) iunit,string(1:lenstr(string))
140   format ('0**ERROR** Reading the following record from ',
     +   'DATACARD TS file on unit ',i2,':' /
     +   ' ',a)
      call error
      ierror=1
      if (filnam.eq.' ') then
         write (ipr,160)
160   format ('0**NOTE** DATACARD file name cannot be obtained.')
         else
            write (ipr,150) filnam(1:lenstr(filnam))
150   format ('0**NOTE** DATACARD file name is: ',a)
         endif
      go to 200
155   write (ipr,157) iunit
157   format ('0**ERROR** Reading record from ',
     +   'DATACARD TS file on unit ',i2,'.')
      call error
      ierror=1
      go to 200

C  End of file encountered
170   write (ipr,180) iunit
180   format ('0**ERROR** End of file encountered while reading ',
     +   'DATACARD file on unit ',i2,'.')
      if (igzio.eq.0) then
         inquire (unit=iunit,iostat=iostat,name=filnam)
         if (iostat.ne.0) then
            write (ipr,65) 'inquire',iunit,iostat
            call error
            ierror=1
            go to 200
            endif
         endif
      if (igzio.eq.1) then
C     Get file name from gzip I/O package.
         call ugznam (iunit, filnam, lfilnam, ierr)
         endif
      if (filnam.eq.' ') then
         write (ipr,160)
         else
            write (ipr,150) filnam(1:lfilnam)
         endif

200   call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CARDRD'
         endif

      return

      end
