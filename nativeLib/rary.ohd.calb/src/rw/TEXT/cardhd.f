!$PRAGMA C (GET_APPS_DEFAULTS)
cc AV added for pgf90 port 7/3/01
!$PRAGMA C (UGZWT)
C MODULE CARDHD
C-----------------------------------------------------------------------
C  Routine to write header records to a DATACARD file.
C     ------------------------------------------------------------------
C     notes: (1) This routine writes a header into a TS file.
C            (2  TS files are ALWAYS written into the current directory.
C     ------------------------------------------------------------------
C     variables:
C
C     ijmo1     .... Julian month of first data value
C     ijmo2     .... julian month of last data value
C     desc      .... TS description
C     dimn      .... data dimension code
C     ierror    .... error code
C     ipr       .... unit number for global output file (common/ionum)
C     itime     .... data interval for TS
C     iwmo1     .... first month in TS
C     iwmo2     .... last month in TS
C     ncatapl   .... number of data values per line
C     oformat   .... format for output in the form #F#.#
C     oformatc  .... format for entire output record
C     iunit     .... unit number for output file
C     sformat   .... "short format", e.g. F10.2 if "oformat" is (6F10.2)
C     staid     .... location identifier
C     filnam    .... name of output TS
C     type      .... data type code
C     units     .... data units code
C     iwyr1     .... first year in TS
C     iwyr2     .... last year in TS
C     ------------------------------------------------------------------

      subroutine cardhd ( filnam, dtype, units, dimn, itime,
     +                    iwmo1, iwyr1, iwmo2, iwyr2, staid,
     +                    desc, iunit, oformat, ijmo1, ijmo2, ierror )

      include 'common/ionum'
      include 'common/fdbug'
      include 'common/unitno'
  
      character oldopn*8
      character filnam*(*),dtype*4,units*4,dimn*4,desc*20,staid*12,
     +          oformat*12
      character fileformat*15
      character oformatt*32,oformatc*32,sformat*12
      character*20 envvar/' '/,usegzio/' '/
      character*150 string
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cardhd.f,v $
     . $',                                                             '
     .$Id: cardhd.f,v 1.10 2002/02/11 16:54:02 dws Exp $
     . $' /
C    ===================================================================
C

      iopnum=-1
      call fstwhr ('CARDHD  ',iopnum,oldopn,iopold)

      ierror = 0
      string=' '

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CARDHD'
         endif

C  Make sure that the dates are full 4-digit years
      iyr1 = iwyr1
      iyr2 = iwyr2
      call ddycdl (iyr1,iwmo1,1)
      call ddycdl (iyr2,iwmo2,1)

C  Open the output file
      fileiotype='output'
      if (iascii.eq.1) fileformat='FORMATTED'
      if (iascii.eq.0) fileformat='UNFORMATTED'
      lrecl=0
      call opfile ( filnam, 'DATACARD-TS ', 'SEQUENTIAL',
     +              'UNKNOWN', fileformat, lrecl, iunit, ierror )
      if (ierror.gt.0) then
         write (ipr,10) filnam(1:lenstr(filnam))
10    format ('0**ERROR** in CARDHD - Unable to open DATACARD file: ',a)
         call error
         ierror = 1
         go to 140
         endif

      fileiotypes(iunit)=fileiotype

      if (oformat.eq.' ') then
         oformat='6F10.2'
         write (ipr,15) oformat(1:lenstr(oformat))
15    format ('0**WARNING** in CARDHD - Output format is blank. ',a,
     +      ' will be used.')
         call warn
         endif

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
         sformat = oformatt(ipos:loformatt)
         else
            ipos=3
            if (oformatt(ipos:ipos).eq.'f'.or.
     +          oformatt(ipos:ipos).eq.'F') then
               read (oformatt(ipos-2:ipos-1),'(i2)') ndatapl
               sformat = oformatt(ipos:loformatt)
               endif
         endif
      if (sformat.eq.' ' ) then
         write (ipr,20) oformat(1:lenstr(oformat))
20    format ('0**ERROR** in CARDHD - Unable get values from the ',
     +      'specified output format: ',a)
         call error
         ierror = 1
         go to 140
         endif

C  Set the complete output format
      oformatc = '(3A4,2I2,I4,'//oformatt(1:loformatt)//')'

C  Check if to use gzip input/output
      igzio=0
      envvar='calb_gzio_write'
      call get_apps_defaults (envvar,lenstr(envvar),
     *   usegzio,lusegzio)
      if (lusegzio.gt.0) then
         musegzio=len(usegzio)
         if (lusegzio.gt.musegzio) then
            write (ipr,25) envvar(1:lenstr(envvar)),
     +         lusegzio,musegzio
25    format ('0**ERROR** in CARDHD - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
            call error
            ierror = 1
            go to 140
            endif
         if (usegzio(1:lusegzio).eq.'on') igzio=1
         endif

C  Write the header

      if (igzio.eq.0) then
         if (iascii.eq.1) write (iunit,30,err=110) staid, desc,
     +      iwmo1, iyr1, iwmo2, iyr2,
     +      dtype, units, dimn, itime,
     +      oformatc,
     +      dtype, dimn, units, itime,
     +      staid, desc,
     +      iwmo1, iyr1, iwmo2, iyr2, ndatapl,
     +      sformat
         if (iascii.eq.0) then
            write (iunit,err=110)
     +         dtype, dimn, units, itime,
     +         staid, desc
            write (iunit,err=110)
     +         iwmo1, iyr1, iwmo2, iyr2, ndatapl,
     +         sformat
            endif
30    format (
     + '$  IDENTIFIER=',a,'   DESCRIPTION=',a,/,
     + '$  PERIOD OF RECORD=',i2.2,'/',i4,' THRU ',i2.2,'/',i4 / 
     + '$  SYMBOL FOR MISSING DATA=-999.00',
     + '   SYMBOL FOR ACCUMULATED DATA=-998.00' / 
     + '$  TYPE=',a,'   UNITS=',a,'   DIMENSIONS=',a,
     + '   DATA TIME INTERVAL=',i2,' HOURS' / 
     + '$  OUTPUT FORMAT=',a / 
     + 'DATACARD      ',a4,' ',a4,' ',a4,' ',i2,'   ',a,'   ',a / 
     + i2,'  ',i4,' ',i2,'   ',i4,' ',i2,'   ',a)
         endif

      if (igzio.eq.1) then
         write (string,40) staid, desc
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,50) iwmo1, iyr1, iwmo2, iyr2
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,60)
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,70) dtype, units, dimn, itime
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,80) oformatc
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,90) dtype, dimn, units, itime, staid, desc
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
         write (string,100) iwmo1, iyr1, iwmo2, iyr2, ndatapl, sformat
         call ulenth (string, len(string), lstring)
         call ugzwt (iunit, string, lstring, ierror)
         if (ierror.ne.0) go to 110
40    format ('$  IDENTIFIER=',a,'   DESCRIPTION=',a)
50    format ('$  PERIOD OF RECORD=',i2,'/',i4,' THRU ',i2,'/',i4)
60    format ('$  SYMBOL FOR MISSING DATA=-999.00',
     + '   SYMBOL FOR ACCUMULATED DATA=-998.00')
70    format ('$  TYPE=',a,'   UNITS=',a,'   DIMENSIONS=',a,
     + '   DATA TIME INTERVAL=',i2,' HOURS')
80    format ('$  OUTPUT FORMAT=',a)
90    format ('DATACARD      ',a4,' ',a4,' ',a4,' ',i2,'   ',a,'   ',
     + a)
100   format (i2,'  ',i4,' ',i2,'   ',i4,' ',i2,'   ',9a)
         endif

C  Set Julian months
      ijmo1 = iyr1*12 + iwmo1
      ijmo2 = iyr2*12 + iwmo2

      go to 140

C  Error writing file
110   write (ipr,120) iunit
120   format ('0**ERROR** Writing file headers ',
     + 'for DATACARD file on unit ',i2,'.')
      write (ipr,130) filnam(1:lenstr(filnam))
130   format ('0**NOTE** DATACARD file name is: ',a)
      call error
      ierror = 1
         
140   call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CARDHD'
         endif

      return

      end
