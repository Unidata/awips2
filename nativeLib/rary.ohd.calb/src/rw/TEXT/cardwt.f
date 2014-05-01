C$PRAGMA C (GET_APPS_DEFAULTS)
cc AV added for pgf90 port 7/3/01
C$PRAGMA C (UGZWT)
C MODULE CARDWT
C-----------------------------------------------------------------------
C  Routine to write a month of data to a DATACARD file.
C     ------------------------------------------------------------------
C     variables:
C
C     icard     .... card number in DATACARD deck
C     icount    .... count of data within month
C     ipos      .... position in TS data arrays
C     mcard     .... counter of data cards written for month
C     ncards    .... number of data cards to be written for month
C     ndata     .... number of data values for month
C     nday      .... number of days in "month"
C     ndpl      .... number of data values for current line
C     ndatapl   .... number of data values for a full line of output
C     nts       .... number of TS to write (this must be one)
C     oformat   .... output format
C     irmonth   .... requested month (1-12)
C     iryear    .... requested year (2-digit)
C     iryear0   .... original value of "iryear"
C     staid     .... location identifier
C     tsdata    .... array to hold data values for month
C     iunit     .... unit number for TS file
C     year      .... year for output calculations
C     ------------------------------------------------------------------

      subroutine cardwt ( dtype, itime, irmonth, iryear0, nts,
     +                    tsdata, iunit, oformat, ierror )

      include 'ufiles'
      include 'common/fdbug'
      include 'common/ionum'
      include 'common/unitno'

      dimension tsdata(*)
      character oldopn*8
      character dtype*4,staid*12,oformat*12,kh*1
      character sformat*12,oformatt*32,oformatc*32
      character*20 envvar/' '/,usegzio/' '/
      character*150 string

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cardwt.f,v $
     . $',                                                             '
     .$Id: cardwt.f,v 1.8 2002/02/11 18:33:56 dws Exp $
     . $' /
C    ===================================================================
C

      iopnum=-1
      call fstwhr ('CARDWT  ',iopnum,oldopn,iopold)

      ierror = 0
      string=' '
      staid=' '

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CARDWT'
         endif

C  Check number of time series to be written
      if (nts.gt.1) then
          write (ipr,10)
10    format ('0**ERROR** Only one TS can be written at a time')
          ierror = 1
          go to 60
          endif

C  Check if month and year are in correct range
      if ((irmonth.lt.1) .or. (irmonth.gt.12)) then
         write (ipr,15) irmonth, iryear0
15    format ('0**ERROR**  Requested month date is invalid: ',
     +   i2,'/',i4)
         ierror=1
         go to 60
         endif
      iryear = iryear0
      call ddycdl (iryear,irmonth,1)
      iryear2 = iryear - (iryear/100)*100

C  Check for parentheses in output format
      oformatt=oformat
      call ulenth (oformatt,len(oformatt),loformatt)
      if (oformatt(1:1).eq.'(') then
         oformatt=oformatt(2:loformatt)
         loformatt=loformatt-1
         endif
      if (oformatt(loformatt:loformatt).eq.')') then
         oformatt=oformatt(1:loformatt-1)
         loformatt=loformatt-1
         endif

C  Get the number of data per line from the output format
C  (xFx.x) or (xxFx.x)
      sformat=' '
      ipos=2
      if (oformatt(ipos:ipos).eq.'f'.or.
     +    oformatt(ipos:ipos).eq.'F') then
         read (oformatt(ipos-1:ipos-1),'(i1)') ndatapl
         sformat = oformatt(ipos+1:loformatt)
         else
            ipos=3
            if (oformatt(ipos:ipos).eq.'f'.or.
     +          oformatt(ipos:ipos).eq.'F') then
               read (oformatt(ipos-2:ipos-1),'(i2)') ndatapl
               sformat = oformatt(ipos+1:loformatt)
               endif
         endif
      if (sformat.eq.' ' ) then
         write (ipr,20) oformat(1:lenstr(oformat))
20    format ('0**ERROR** in CARDHD - Unable get values from the ',
     +      'specified output format: ',a)
         call error
         ierror = 1
         go to 60
         endif

C  Get last valid character in oformatt
      ie = loformatt
      ii = 1
22    if (ii .ge. ie) goto 24
         kh = oformatt(ii:ii)
         if (kh.eq.' ' .or. kh.eq.char(0) .or. kh.eq.char(10)) ie=ii-1
         ii = ii+1
         goto 22
24    continue

      if (ie .le. 0) then
         write (ipr,26)
26       format ('0**ERROR** in CARDHD - output format blank')
         call error
         ierror = 1
         go to 60
         endif

C  Set the complete output format
      oformatc = '(a,2i2,i4,'//oformatt(1:ie)//')'

      ndpl = ndatapl
      mcard = 1
      ncards = 0
      icount = 0
      icard  = 1

C  Check if to use gzip input/output
      igzio=0
      if (igziowt.eq.-1) then
         igziowt=0
         envvar='calb_gzio_write'
         call get_apps_defaults (envvar,lenstr(envvar),
     *      usegzio,lusegzio)
         if (lusegzio.gt.0) then
            musegzio=len(usegzio)
            if (lusegzio.gt.musegzio) then
               write (ipr,25) envvar(1:lenstr(envvar)),
     +            lusegzio,musegzio
25    format ('0**ERROR** in CARDWT - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
               call error
               ierror = 1
               go to 40
               endif
            if (usegzio(1:lusegzio).eq.'on') then
               igziowt=1
               igzio=1
               endif
            endif
         else
            igzio=igziowt
         endif

30    if (igzio.eq.0) then
         if (iascii.eq.1)  write (iunit,oformatc,err=40)
     +      staid, irmonth, iryear2, icard,
     +      (tsdata(icount+i),i=1,ndpl)
         if (iascii.eq.0)  write (iunit,err=40)
     +      staid, irmonth, iryear2, icard,
     +      (tsdata(icount+i),i=1,ndpl)
         endif

      if (igzio.eq.1) then
         write (string,oformatc)
     +      staid, irmonth, iryear2, icard,
     +      (tsdata(icount+i),i=1,ndpl)
C     To avoid having each written line be filled out to the full
C     length of "string", use ULENTH to determine the last non-blank.
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 40
         endif

      icount = icount + ndpl
      icard  = icard + 1
      if (ncards.eq.0) then
         call ddgcdm (iryear,irmonth,nday)
         ndata  = nday*24/itime
         ncards = ndata/ndpl
         if (((ndata/ndpl)*ndpl).ne.ndata ) ncards=ncards+1
         endif
      if (mcard.lt.ncards) then
         mcard = mcard + 1
         if (mcard.eq.ncards) then
            ndpl = ndata - ndpl*(ncards - 1)
            endif
         go to 30
         endif

      go to 60

40    write (ipr,50)
50    format ('0**ERROR** Error encountered while writing TS.')
      ierror = 1
         
60    call fstwhr (oldopn,iopold,oldopn,iopold)
      
      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CARDWT'
         endif

      return
 
      end
