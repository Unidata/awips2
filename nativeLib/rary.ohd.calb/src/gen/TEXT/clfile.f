C$PRAGMA C (GET_APPS_DEFAULTS)
cc AV added for pgf90 port 7/3/01
C$PRAGMA C (UGZNAM)
C$PRAGMA C (UGZCLS)
C$PRAGMA C (SETNOT)
cc AV end for pgf90 port 7/3/01
C MODULE CLFILE
C-----------------------------------------------------------------------
C  Routine to close a file.
C     ------------------------------------------------------------------
C     notes: (1) This routine closes an NWSRFS file that has been
C                opened.
C            (2) Unit numbers are set in the FILEUNIT file under the
C                NWSRFS system files.
C            (3) The NWSRFS array IFILES is used to indicate the
C                 status of a file.  Originally, the flags for this
C                 variable were:
C                      0 = file unused
C                      1 = file is DAIO (direct access)
C                      2 = file is sequential
C                 In order to add the capability to reserve files, the
C                 array IFILES is treated as a bit mask, where the
C                 above values apply, and, additionally:
C                      4 = file is reserved
C            (4)  The file open/close sequence currently has one flaw,
C                 and that is that files that are categorically the
C                 same, e.g., time series files read from historical
C                 files, are not differentiated in the program.  Because
C                 a normal execution stop will close these files
C                 normally, this is currently not a problem, but should
C                 be addressed in the future.
C            (5)  When closing a file, the key is checked first, and if
C                 not specified, the unit number is checked.
C-----------------------------------------------------------------------
C variables:
C
C ierror        .... error status variable (1 if error, 0 if not)
C ifiles        .... array used to indicate file status (taken from
C                    "ufiles" common block)
C itrace        .... trace level for program execution
C key           .... keyword that is used to look up unit number for
C                    file
C mfiles        .... size of array IFILES (taken from "ufiles" common
C                    block)
C iunit         .... unit number to be closed (only used if "key" has
C                    a space in the first character)
C-----------------------------------------------------------------------

      subroutine clfile ( key, iunit, ierror )

      include 'ufiles'
      include 'upvrsx'
      include 'common/fdbug'
      include 'common/ionum'
      include 'common/sysbug'
      include 'common/unitno'

      INCLUDE 'updaio'

      character*(*) key
      character oldopn*8
      character*20 envvar/' '/,usegzio/' '/
      character*128 filnam/' '/
      logical*4 opened
C
C  ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/clfile.f,v $
     . $',                                                             '
     .$Id: clfile.f,v 1.10 2002/02/11 18:34:14 dws Exp $
     . $' /
C  ===================================================================
C

      iopnum=-1
      call fstwhr ('CLFILE  ',iopnum,oldopn,iopold)

      ierror=0
      ibug=0

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER CLFILE'
         endif

      if (ibug.eq.1) then
         write (iodbug,*) ' Closing unit # ', iunit, ' ', key
         endif

      if (key(1:1).eq.' '.or.iunit.gt.0) then
         if (iunit.ne.UU .and. iunit.ne.UE) then
C        Check if to use gzip input/output
            igzio=0
            envvar='calb_gzio_read'
            call get_apps_defaults (envvar,lenstr(envvar),
     +         usegzio,lusegzio)
            if (lusegzio.gt.0) then
               musegzio=len(usegzio)
               if (lusegzio.gt.musegzio) then
                  write (ipr,30) envvar(1:lenstr(envvar)),
     +               lusegzio,musegzio
                  call error
                  ierror = 1
                  go to 20
                  endif
               if (usegzio(1:lusegzio).ne.'on'.and.
     *             usegzio(1:lusegzio).ne.'off') then
                  write (ipr,40) envvar(1:lenstr(envvar))
                  call error
                  ierror=1
                  go to 20
                  endif
               if (usegzio(1:lusegzio).eq.'on'.and.
     +            fileiotypes(iunit).eq.'input') igzio=1
               endif
            envvar='calb_gzio_write'
            call get_apps_defaults (envvar,lenstr(envvar),
     +         usegzio,lusegzio)
            if (lusegzio.gt.0) then
               musegzio=len(usegzio)
               if (lusegzio.gt.musegzio) then
                  write (ipr,30) envvar(1:lenstr(envvar)),
     +               lusegzio,musegzio
                  call error
                  ierror = 1
                  go to 20
                  endif
               if (usegzio(1:lusegzio).ne.'on'.and.
     *             usegzio(1:lusegzio).ne.'off') then
                  write (ipr,40) envvar(1:lenstr(envvar))
                  call error
                  ierror=1
                  go to 20
                  endif
               if (usegzio(1:lusegzio).eq.'on'.and.
     +            fileiotypes(iunit).eq.'output') igzio=1
               endif
            if (igzio.eq.1.and.
     +         key(1:8).eq.'DATACARD') then
CCC        WRITE (6,*) 'IN CLFILE - CALLING UGZCLS TO CLOSE UNIT ',iunit
               filnam=' '
               call ugznam (iunit, filnam, lfilnam, ierr)
               call ugzcls (iunit)
               else
CCC            WRITE (6,*) 'IN CLFILE - CLOSING UNIT ',iunit
                  inquire (unit=iunit,iostat=iostat,name=filnam,
     +               opened=opened)
                  if (iostat.ne.0) then
                     write (ipr,50) iostat,'an inquire',iunit
                     call error
                     ierror=1
                     go to 20
                     endif
                  if (.not.opened) then
                     write (ipr,60) iunit
                     call error
                     ierror=1
                     go to 20
                     endif
                  close (unit=iunit,iostat=iostat)
                  if (iostat.ne.0) then
                     write (ipr,50) iostat,'a close',iunit
                     call error
                     ierror=1
                     go to 20
                     endif
               endif
            ifiles(iunit)=0
            uprecl(iunit)=-1
            filekeys(iunit)=' '
C        write message to log file if option is on
            if (UU.ne.-1) then
               write (UU,80) iunit,filnam(1:lenstr(filnam))
               endif
            endif
         else
            if (mfiles.le.0) then
               write (ipr,70) mfiles
               call warn
               endif
            do 10 i=1,mfiles
               if (key.eq.filekeys(i)) then
C              Check if to use gzip input/output
                  igzio=0
                  envvar='calb_gzio_read'
                  call get_apps_defaults (envvar,lenstr(envvar),
     +               usegzio,lusegzio)
                  if (lusegzio.gt.0) then
                     musegzio=len(usegzio)
                     if (lusegzio.gt.musegzio) then
                        write (ipr,30) envvar(1:lenstr(envvar)),
     +                     lusegzio,musegzio
                        call error
                        ierror = 1
                        go to 20
                        endif
                     if (usegzio(1:lusegzio).ne.'on'.and.
     *                   usegzio(1:lusegzio).ne.'off') then
                        write (ipr,40) envvar(1:lenstr(envvar))
                        call error
                        ierror=1
                        go to 20
                        endif
                     if (usegzio(1:lusegzio).eq.'on'.and.
     +                  fileiotypes(i).eq.'input') igzio=1
                     endif
                  envvar='calb_gzio_write'
                  call get_apps_defaults (envvar,lenstr(envvar),
     +               usegzio,lusegzio)
                  if (lusegzio.gt.0) then
                     musegzio=len(usegzio)
                     if (lusegzio.gt.musegzio) then
                        write (ipr,30) envvar(1:lenstr(envvar)),
     +                     lusegzio,musegzio
                        call error
                        ierror = 1
                        go to 20
                        endif
                     if (usegzio(1:lusegzio).ne.'on'.and.
     *                   usegzio(1:lusegzio).ne.'off') then
                        write (ipr,40) envvar(1:lenstr(envvar))
                        call error
                        ierror=1
                        go to 20
                        endif
                     if (usegzio(1:lusegzio).eq.'on'.and.
     +                  fileiotypes(i).eq.'output') igzio=1
                     endif
                  if (igzio.eq.1.and.
     +               key(1:8).eq.'DATACARD') then
CCC           WRITE (6,*) 'IN CLFILE - CALLING UGZCLS TO CLOSE UNIT ',i
                        filnam=' '
                        call ugznam (i, filnam, lfilnam, ierr)
                        call ugzcls (i)
                        else
CCC                         WRITE (6,*) 'IN CLFILE - CLOSING UNIT ',i
                            inquire (unit=i,iostat=iostat,name=filnam)
                            if (iostat.ne.0) then
                               write (ipr,50) iostat,'an inquire',iunit
                               call error
                               ierror=1
                               go to 20
                               endif
                            close (unit=i,iostat=iostat)
                            if (iostat.ne.0) then
                               write (ipr,50) iostat,'a close',iunit
                               call error
                               ierror=1
                               go to 20
                               endif
                        endif
                     ifiles(i)=0
                     call setnot (ifiles(i))
                     uprecl(i)=-1
                     filekeys(i)=' '
C                 write message to log file if option is on
                     if (UU.ne.-1) then
                        write (UU,80) i,filnam(1:lenstr(filnam))
                        endif
                  endif
10             continue
         endif

20    call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT CLFILE'
         endif

      return

30    format ('0**ERROR** in CLFILE - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
40    format ('0**ERROR** in CLFILE - Value for apps_default ',
     *   a,' is not ''on'' or ''off''.')
50    format ('0**ERROR** in CLFILE - Status code ',i3,' ',
     +   'encountered doing ',a,' for unit ',i3,'.')
60    format ('0**ERROR** in CLFILE - Unit ',i3,' ',
     +   'is not open.')
70    format ('0**WARNING** in CLFILE - Value for number of ',
     *   'files (',i3,') is less than or equal to zero.')
80    format (' clfile ',I3,'     closed            ',a)

      end
