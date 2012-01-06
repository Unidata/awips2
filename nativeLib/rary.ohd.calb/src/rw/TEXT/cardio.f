C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE CARDIO
C-----------------------------------------------------------------------
C  Routine to initialize input/output TS for DATACARD format file.
C     ------------------------------------------------------------------
C     notes: (1) This routine initializes an input or output TS by
C                opening a file.
C            (2) The values stored in EXTLOC are the same for
C                both input and output TS to avoid confusion.
C     ------------------------------------------------------------------
C
C     variables:
C
C     convf1    .... multiplication factor in units conversion
C     convf2    .... addition factor in units conversion
C     ijmo1     .... Julian month of first day of data
C     ijmo2     .... Julian month of last day of data
C     descrp    .... time series description
C     dtype     .... TS data type requested by calling routine
C     dunits    .... data units in the output file
C     extloc    .... external location information array
C     ierror    .... error flag (0 if no error)
C     itime     .... data interval (hours)
C     irmo1     .... requested first month of data
C     irmo2     .... requested last month of data
C     iryr1     .... requested first year of data (4 digit)
C     iryr2     .... requested last year of data (4 digit)
C     itstyp    .... time series type:
c                       1=input
C                       3=output
C     npdt      .... number of values per interval for TS
C     numext    .... number of "extloc" values assigned here
C     oformat   .... format to be used for TS data
C     sname     .... subroutine name
C     staid     .... station ID
C     tsname    .... time series name in datacard format (this is read
C                    as the next line in the input deck)
C     iunit     .... unit number for file to open
C     units     .... TS data units requested by calling routine (input)
C     ------------------------------------------------------------------

      subroutine cardio ( itstyp,numext,extloc,dtype,units,dimn,
     +                    itime,npdt,tsname,ierror )

      include 'common/ionum'
      include 'common/fdbug'
      include 'common/calbrt'

      dimension extloc(*)
      character*8 oldopn
      character*12 staid
      character*20 descrp
      character*32 tsname
      character*12 oformat
      character*80 dirnam
      character*112 filnam
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cardio.f,v $
     . $',                                                            '
     .$Id: cardio.f,v 1.9 2002/02/11 13:06:02 michaelo Exp $
     . $' /
C    ===================================================================
C
      data blank /4h    /


      iopnum=-1
      call fstwhr ('CARDIO  ',iopnum,oldopn,iopold)

      ierror=0

      npdt=1
      filnam=' '
      dirnam=' '
      tsname=' '
      descrp=' '

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CARDIO'
         endif

C  Get the default directory for calibration card data
      call get_apps_defaults ('calb_area_ts_dir',16,dirnam,ldirname)

C  Check length of directory name
      mdirnam=len(dirnam)
      if (ldirname.gt.mdirnam) then
         write (ipr,5) mdirnam
5     FORMAT ('0**ERROR** in CARDIO - Directory name is ',
     *   'longer than ',I3,' characters.')
         call error
         ierror=1
         go to 130
         endif

C  Check if an input time series
      if (itstyp.eq.1) then
         read (in,10) tsname
10    format (a)
C     Set path name
         call ulenth (tsname,len(tsname),ltsname)
         if (ldirname.gt.0) then
            filnam=dirnam(1:ldirname)//'/'//tsname(1:ltsname)
            else
               filnam=tsname(1:ltsname)
            endif
         irmo1=0
         iryr1=0
         irmo2=0
         iryr2=0
         call cardlo ( irmo1,iryr1,irmo2,iryr2,
     +                 itmo1,ityr1,itmo2,ityr2,
     +                 units,dunits,filnam,dtype,itime,
     +                 staid,descrp,iunit,oformat,
     +                 convf1,convf2,
     +                 ijmo1,ijmo2,ierr )
         if (ierr.ne.0) then
            write (ipr,20) filnam(1:lenstr(filnam))
20    format ('0**ERROR** Error reading input DATACARD file: ',a)
            call error
            ierror=1
            go to 130
            endif
         extloc(1)=iunit+.01
         extloc(2)=ijmo1+.01
         extloc(3)=ijmo2+.01
         extloc(4)=convf1
         extloc(5)=convf2
         extloc(6)=itime+.01
         extloc(7)=npdt+.01
         extloc(8)=ldirname
C     Save path name in the extloc array
         do 30 i=9,36
            extloc(i)=blank
30          continue
         read (dirnam(1:ldirname),'(20a4)') (extloc(i),i=9,28)
         read (tsname,'(8a4)') (extloc(i),i=29,36)
         read (oformat,'(3a4)') (extloc(i),i=37,39)
         extloc(40)=dtype
         extloc(41)=itime+.01
         call umemov ('CARD',extloc(42),1)
         numext=42
         call tsprt_card ('CARD',staid,descrp,itmo1,ityr1,itmo2,ityr2,
     *      filnam)
         go to 130
         endif

C  Check if an output time series
      if (itstyp.eq.3) then
         read (in,90) tsname,staid,descrp,oformat
90    format (a,7x,a,a,a)
C     Set path name
         call ulenth (tsname,len(tsname),ltsname)
         if (ldirname.gt.0) then
            filnam=dirnam(1:ldirname)//'/'//tsname(1:ltsname)
            else
               filnam=tsname(1:ltsname)
            endif
         call cardhd ( filnam,dtype,units,dimn,itime,
     +                 imo,iyr,lmo,lyr,staid,descrp,iunit,
     +                 oformat,ijmo1,ijmo2,ierr )
         if (ierr.ne.0) then
            write (ipr,100) filnam(1:lenstr(filnam))
100   format ('0**ERROR** Error writing output DATACARD file: ',a)
            call error
            ierror=1
            go to 130
            endif
         extloc(1)=iunit+.01
         extloc(2)=ijmo1+.01
         extloc(3)=ijmo2+.01
         extloc(4)=0.0
         extloc(5)=0.0
         extloc(6)=itime+.01
         extloc(7)=npdt+.01
         extloc(8)=ldirname
C     Save path name in the extloc array
         do 110 i=9,36
            extloc(i)=blank
110         continue
         read (dirnam(1:ldirname),'(20a4)') (extloc(i),i=9,28)
         read (tsname,'(8a4)') (extloc(i),i=29,36)
         read (oformat,'(3a4)') (extloc(i),i=37,39)
C     Set numext to 42 to be same as input
         numext=42
         call tsprt_card ('CARD',staid,descrp,imo,iyr,lmo,lyr,
     *      filnam)
         go to 130
         endif

C  Not a valid TS type
      write (ipr,120)
120   format ('0**ERROR** Only INPUT and OUTPUT time series can ',
     +  'have a file type of ''CARD''.')
      write (ipr,125)  tsname(1:lenstr(tsname))
125   format ('0**NOTE** DATACARD file name is: ',a)
      call error
      ierror=1

130   if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CARDIO'
         endif

      call fstwhr (oldopn,iopold,oldopn,iopold)

      return

      end
