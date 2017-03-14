C$PRAGMA C (GET_APPS_DEFAULTS)
cc AV added pgf90 7/73/01
C$PRAGMA C (UGZRD)
C MODULE CARDLO
C-----------------------------------------------------------------------
C  Routine to locate and open a DATACARD file.
C     ------------------------------------------------------------------
C     notes: (1) This routine will attempt to locate the TS file
C                in the current directory.
C            (2) If located, the TS file is opened and the header
C                information is read.  Checks are performed to
C                insure that the TS is of the correct type.
C                Information which needs to be used elsewhere
C                are returned to the calling program.
C            (4) If the requested period is all zeros, then the
C                full period from the TS file will be returned.
C            (5) Comment cards at the top of input TS are
C                allowed.  These cards begin with the character '$'.
C     ------------------------------------------------------------------
C
C     variables:
C
C     convf1    .... multiplication factor in units conversion
C     convf2    .... addition factor in units conversion
C     desc      .... description
C     dimn      .... data dimension code (declared in pre_common)
C     dtype     .... data type code (declared in pre_common)
C     ierror    .... error code (0=okay)
C     ipos      .... position of TS file in TS file array
C     itime     .... data interval (hours)
C     iunit     .... unit number for input file
C     itmo1     .... first month from TS header
C     itmo2     .... last month from TS header
C     irmo1     .... requested first month
C     irmo2     .... requested last month
C     ndatapl   .... number of data per line
C     oformat   .... format used to read the data records
C     rdtype    .... requested data type
C     runits    .... requested data units (if 'SAME' no conversion
C                    is done)
C     sformat   .... "short format", e.g. F10.2 in (6F10.2)
C     sname     .... name of this routine
C     staid     .... location identifier
C     filnam    .... DATACARD file name
C     iunit     .... unit number to use for file
C     units     .... data units from TS
C     ityr1     .... first year from TS header
C     ityr2     .... last year from TS header
C     iryr1     .... requested first year
C     iryr2     .... requested last year
C     ------------------------------------------------------------------

      subroutine cardlo ( irmo1, iryr1, irmo2, iryr2, 
     +                    itmo1, ityr1, itmo2, ityr2,
     +                    runits, units, filnam, rdtype, itime, staid,
     +                    desc, iunit, oformat, convf1, convf2,
     +                    ijmon1, ijmon2, ierror )

      include 'common/ionum'
      include 'common/fdbug'
      include 'common/unitno'
      include 'pre_common'
  
      character oldopn*8
      character runits*4,units*4,rdtype*4,desc*20,staid*12,oformat*12
      character*112 filnam
      character fileformat*15
      character sformat*12,kh*1
      character*20 envvar/' '/,usegzio/' '/
      character*150 string

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cardlo.f,v $
     . $',                                                             '
     .$Id: cardlo.f,v 1.10 2002/02/11 16:54:20 dws Exp $
     . $' /
C    ===================================================================
C

      iopnum=-1
      call fstwhr ('CARDLO  ',iopnum,oldopn,iopold)

      ierror=0
      string=' '
      lstring=len(string)
      staid=' '
      desc=' '
      oformat=' '
      convf1=0.0
      convf2=0.0

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CARDLO'
         endif

CCC      write (iodbug,*) 'filnam=',filnam

C  Open the file for the input TS
      fileiotype='input'
      if (iascii.eq.1) fileformat='FORMATTED'
      if (iascii.eq.0) fileformat='UNFORMATTED'
      lrecl=0
      call opfile ( filnam, 'DATACARD-TS ', 'SEQUENTIAL',
     +              'OLD', fileformat, lrecl, iunit, ierror )
      if (ierror.gt.0) then
         write (ipr,10) filnam(1:lenstr(filnam))
10       format ('0**ERROR** Unable to open DATACARD-TS file: ',a)
         call error
         ierror=1
         go to 180
         endif
      fileiotypes(iunit)=fileiotype
CCC      write (ipr,*) ' iunit=',iunit,' filnam=',filnam

C  Check if to use gzip input/output
      igzio=0
      envvar='calb_gzio_read'
      call get_apps_defaults (envvar,lenstr(envvar),
     *   usegzio,lusegzio)
      if (lusegzio.gt.0) then
         musegzio=len(usegzio)
         if (lusegzio.gt.musegzio) then
            write (ipr,15) envvar(1:lenstr(envvar)),
     +         lusegzio,musegzio
15    format ('0**ERROR** in CARDLO - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
            call error
            ierror=1
            go to 180
            endif
         if (usegzio(1:lusegzio).eq.'on') igzio=1
         endif

C  Read header information
      if (igzio.eq.0) then
20       if (iascii.eq.1) then
            read (iunit,'(a)',end=155,err=160) string
C        Check for comment
            if (string(1:1).eq.'$') go to 20
            read (string,40,end=155,err=160) dtype,dimn,units,itime,
     +         staid,desc
            endif
         if (iascii.eq.0) then
            read (iunit,end=155,err=160) dtype,dimn,units,itime,
     +         staid,desc
            endif
40    format (14x,a,1x,a,1x,a,1x,i2,3x,a,3x,a)
         if (iascii.eq.1) read (iunit,50,end=155,err=160)  
     +      itmo1,ityr1,itmo2,ityr2,ndatapl,sformat
         if (iascii.eq.0) read (iunit,end=155,err=160)  
     +      itmo1,ityr1,itmo2,ityr2,ndatapl,sformat
50    format (i2,2x,i4,1x,i2,3x,i4,1x,i2,3x,a)
         endif

      if (igzio.eq.1) then
C     Get the file line from the "gzipped" file and place into
C     char variable and use internal I/O for the formatted read
60       call ugzrd (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 160
C     Check for comment
         if (string(1:1).eq.'$') go to 60
C     Get last character (not linefeed)
         ie=len(string)
         do 64 ii=1,len(string)
            if (string(ii:ii).eq.char(10)) ie=ii-1
64          continue
         if (ie.le.0) go to 160
         read (string(1:ie),40,end=155,err=160) dtype, dimn, units, 
     +      itime, staid, desc
         call ugzrd (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 160
         read (string,50) itmo1, ityr1, itmo2, ityr2, ndatapl, sformat
         endif

C  Check to make sure that the dates are okay
      if ((itmo1.lt.1).or.(itmo1.gt.12)) then
         write (ipr,70) 'Starting',itmo1
70    format ('0**ERROR** in CARDLO - ',a,' month is invalid: ',i2)
         call error
         ierror=1
         go to 180
         endif
      if ((itmo2.lt.1).or.(itmo2.gt.12)) then
         write (ipr,70) 'Ending',itmo2
         call error
         ierror=1
         go to 180
         endif
      if (ityr1.lt.100) then
          write (ipr,90) 'Starting',ityr1
90    format ('0**ERROR** in CARDLO - ',a,' year is not 4-digit: ',
     +      i4)
         call error 
         ierror=1
         go to 180
         endif
      if (ityr2.lt.100) then
         write (ipr,90) 'Ending',ityr2
         call error
         ierror=1
         go to 180
         endif

C  Save Julian month numbers
      ijmon1=ityr1*12+itmo1
      ijmon2=ityr2*12+itmo2

      if (ijmon2.lt.ijmon1) then
         write (ipr,*) '0**ERROR** in CARDLO - Ending date is earlier ',
     +      'than beginning date.'
         call error
         ierror=1
         go to 180
         endif

C  Check to see if returned period needs to be set from period read
C  from file.
      if ((irmo1.eq.0).or.(irmo2.eq.0) .or.
     +    (iryr1.eq.0).or.(iryr2.eq.0)) then
         irmo1=itmo1
         irmo2=itmo2
         iryr1=ityr1
         iryr2=ityr2
         endif

C  Check to make sure that requested data type matches file
      if (rdtype.ne.dtype) then
C     this is a warning to accomodate the situation where
C     the requested ts is an rqim etc and the data ts is an qme
         write (ipr,110) dtype, rdtype
110   format ('0**WARNING** in CARDLO - DATACARD data type (',a,
     +   ') does not match requested type (',a,').')
         write (ipr,159) filnam(1:lenstr(filnam))
         call warn
         endif

C  Find the last non-blank character
      ib=0
      ie=0
      do 130 ii=1,len(sformat)
         kh=sformat(ii:ii)
         if (ib.eq.0.and.kh.ne.' ') ib=ii
         if (ib.ne.0.and.kh.ne.' '
     +              .and.kh.ne.char(0)
     +              .and.kh.ne.char(10)) ie=ii
  130    continue

C  Create the format for the data
      oformat='*'
      if (ie.gt.0) then
         if (ndatapl.lt.10) then
            write (oformat,140) ndatapl,sformat(ib:ie)
140         format ('(',i1,a,')')
            else
               if (ndatapl.lt.100) then
                  write (oformat,150) ndatapl,sformat(ib:ie)
150               format ('(',i2,a,')')
                  endif
            endif
         endif

C  Check to see if data need to be converted
      if (runits.eq.'SAME') then
          convf1=1.0
          convf2=0.0
          runits=units
          else
             convf1=1.0
             convf2=0.0
             if (runits.ne.units) then
                call uducnv (units,runits,2,1,convf1,convf2,ierror)
                if (ierror.ne.0) go to 180
                endif
         endif

      go to 180

C  Error reading file
155   if (igzio.eq.0) then
         backspace iunit
         read (iunit,'(a)',end=160,err=180) string
         endif
      write (ipr,157) iunit,string(1:lenstr(string))
157   format ('0**ERROR** in CARDLO - Reading the following record ',
     +   'from DATACARD file on unit ',i3,':' /
     +   ' ',a)
      write (ipr,159) filnam(1:lenstr(filnam))
159   format ('0**NOTE** DATACARD file name is: ',a)
      ierror=1
      call error
      go to 180

C  End of file
160   write (ipr,170) iunit
170   format ('0**ERROR** in CARDLO - End of file encountered while ',
     +   'reading DATACARD file on unit ',i3,'.')
      write (ipr,159) filnam(1:lenstr(filnam))
      call error
      ierror=1
         
180   call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CARDLO'
         endif

      return

      end
