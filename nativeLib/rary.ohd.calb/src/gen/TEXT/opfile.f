C$PRAGMA C (GETUNO)
C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (ISUSED,SETDA,UGZOPN,SETSEQ)
C MODULE OPFILE
C-----------------------------------------------------------------------
C  Routine to open a file.
C     ------------------------------------------------------------------
C    notes:  (1)  This routine opens any file.  It is used as an
C                 interface to low-level file I/O routines.
C            (2)  Once a file has been opened, it cannot be opened in
C                 any other way until it is closed by calling the
C                 routine CLFILE.
C            (3)  Unit numbers are set in the FILEUNIT file under the
C                 NWSRFS system files.
C            (4)  The array IFILES is used to indicate the
C                 status of a file. Originally, the flags for this
C                 variable were:
C                    0=file unused
C                    1=file is DAIO (direct access)
C                    2=file is sequential
C                 In order to add the capability to reserve files,
C                 array IFILES is treated as a bit mask, where the
C                 above values apply, and, additionally:
C                    4=file is reserved
C            (5)  One-word FORTRAN strings that are used must end in a
C                 space.  The space will be replaced with a NULL by any
C                 C routines that are called.
C            (6)  The record length for direct access files on some
C                 machines may be different depending on whether the
C                 file is formatted or unformatted. To account for this,
C                 use the "platform" common block.  Also, the record
C                 length could be looked up in the NWSRFS system file
C                 FS5FLDCB, but need to coordinate this with continuing
C                 NWS development efforts.
C            (7)  Currently, not much information about a file is saved,
C                 which means that the reserve flag for a file can be
C                 used only to reserve a file, but not to open it later
C                 on.
C-----------------------------------------------------------------------
C variables:
C
C access0       .... file access ('DIRECT' or 'SEQUENTIAL', or any
C                    abbreviation, as long as first letter is unique)
C access        .... access in standard form (not abbreviated)
C filekeys      .... array of "key" values saved for use in other s
C                    routines (especially "clfile")
C filnam        .... actual filnam to open
C form0         .... file format ('FORMATTED' or 'UNFORMATTED', or any
C                    abbreviation, as long as first letter is unique)
C form          .... file format in standard form (not abbreviated)
C i             .... loop counter
C ierror        .... error status variable (1 if error, 0 if not)
C ifiles        .... array used to indicate file status (taken from
C                    "ufiles" common block)
C istat         .... return status from function calls
C ipr           .... unit number for standard error (taken from "ionum"
C                    common block)
C isused        .... function to return flag indicating whether a file
C                    is used
C key           .... keyword that is used to look up unit number for
C                    file (the last letter as passed in must be a space)
C mfiles        .... size of "ifiles" array (taken from "ufiles" common
C                    block)
C pgmnam        .... name of the program that is opening the file (taken
C                    from the "upvrsx" common block)
C lrecl0        .... record length for direct access file (in bytes)
C resuno        .... function to reserve unit number
C setda         .... function to set the direct access bit in IFILES
C                    array
C status0       .... file status ('NEW','OLD','UNKNOWN','SCRATCH', or
C                    any abbreviation, as long as first letter is
C                    unique)
C status        .... status in standard form, not abbreviated
C iunit         .... unit number that is opened
C iunitd        .... dummy unit number of -1 for rtn upstat
C-----------------------------------------------------------------------

      subroutine opfile ( filnam, key, access0, status0, form0, lrecl0,
     *                    iunit, ierror )

      include 'ufiles'
      include 'upvrsx'
      include 'common/fdbug'
      include 'common/ionum'
      include 'common/errdat'
      include 'common/unitno'
      include 'common/platform'

      INCLUDE 'updaio'

      character*(*) access0, filnam, form0, key, status0
      character*8   oldopn
      character*20  access, status
      character*1   form
      integer       setda
      character*20  envvar, usegzio
      character*150 filnam2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/opfile.f,v $
     . $',                                                             '
     .$Id: opfile.f,v 1.12 2002/02/11 16:52:09 dws Exp $
     . $' /
C    ===================================================================
C
      data    envvar  / ' ' /
      data    usegzio / ' ' /
      data    filnam2 / ' ' /


      iopnum=-1
      call fstwhr ('OPFILE  ',iopnum,oldopn,iopold)

      ibug=0
      iunitd=-1

      if (itrace.ge.1) then
         write (iodbug,*) 'ENTER OPFILE'
         endif

C  Get the unit number to use for the file
      call getuno ( mfiles, ifiles, pgmnam, key, iunit, ierror )
      if (ierror.ne.0) then
         write (ipr,10) key(1:lenstr(key))
10    format ('0**ERROR** Unable to get unit number for key ',a,'.')
         call error
         ierror=1
         go to 90
         endif

C  Check if file already opened
      istat=isused(ifiles(iunit))
      if (istat.ne.0) then
         write (ipr,20) iunit, key(1:lenstr(key))
20    format ('0**ERROR** Unit number ',i3,' for key ',a,
     *   ' is already used.')
         call error
         ierror=1
         go to 90
         endif

C  Default access is SEQUENTIAL
      if (access0(1:1).eq.'D') then
         access='DIRECT'
         else
            access='SEQUENTIAL'
         endif

C  Default status is UNKNOWN
      if (status0(1:1).eq.'N') then
         status='NEW'
         else if (status0(1:1).eq.'O') then
            status='OLD'
         else if (status0(1:1).eq.'S') then
            status='SCRATCH'
         else
            status='UNKNOWN'
         endif

C  Default form is FORMATTED for SEQUENTIAL files and UNFORMATTED for
C  DIRECT access files
      if (form0(1:1).eq.'U') then
         form='U'
         else if (form0(1:1).eq.'F') then
            form='F'
         else
            if (access(1:1).eq.'D') then
               form='U'
               else
                  form='F'
               endif
         endif

C  Adjust the record length so that it is in terms of long words
C  so that upopen can deal with the record length - upopen uses
C  words and opfile uses bytes
        lrecl=lrecl0
        lrecl=(lrecl+3)/4

C  Open the file according to the calling routines specifications.
C  Note that it should be possible to use existing low-level NWSRFS
C  routines in some places, but open file here for now)

      if (ibug.gt.0) then
          if (filnam(1:1).eq.' ') then
             write (iodbug,30)
30    format (' ** Debug : Opening scratch file.' )
             else
                write (iodbug,40) filnam
40    format (' ** Debug :  Opening file:  ',a112 )
             endif
          write (iodbug,50) iunit, access(1:3), lrecl,
     *       form0(1:3), status(1:3)
50    format (' ** Debug :  iunit=',i3,' access=',a,
     *   ' lrecl=',i6, ' form=',a,' status=',a)
         endif

C  Do not include the filnam if one was not specified
      if (access(1:1).eq.'D') then
         if (filnam(1:1).eq.' ') then
            filnam2=' '
            call upstat (iunit,filnam2,status,icond)
            if (icond.eq.0) then
               call upopen (iunit,filnam2,lrecl,form,icond)
               else
                  call upstae (ipr,iunit,filnam2,status,icond)
               endif
            else
               call upstat (iunitd,filnam,status,icond)
               if (icond.eq.0) then
                  call upopen (iunit,filnam,lrecl,form,icond)
                  else
                     call upstae (ipr,iunitd,filnam,status,icond)
                  endif
             endif
         if (icond.ne.0) go to 70
         istat=setda(ifiles(iunit))
         else if (access(1:1).eq.'S') then
            if (filnam(1:1).eq.' ') then
               lrecl=0
               filnam2=' '
               call upstat (iunit,filnam2,status,icond)
               if (icond.eq.0) then
                  call upopen (iunit,filnam2,lrecl,form,icond)
                  else
                     call upstae (ipr,iunit,filnam2,status,icond)
                     endif
               else
                  lrecl=0
C              Check if to use gzip input/output
                  igzio=0
                  inderr=0
                  envvar='calb_gzio_read'
                  call get_apps_defaults (envvar,lenstr(envvar),
     *               usegzio,lusegzio)
                  if (lusegzio.gt.0) then
                     musegzio=len(usegzio)
                     if (lusegzio.gt.musegzio) then
                        write (ipr,55) envvar(1:lenstr(envvar)),
     *                     lusegzio,musegzio
55    format ('0**ERROR** in OPFILE - Length of apps_default ',
     *   'environment variable ',a,' (',i2,') ',
     *   'exceeds maximum (',i2,').')
                        call error
                        ierror=1
                        endif
                     if (usegzio(1:lusegzio).ne.'on'.and.
     *                   usegzio(1:lusegzio).ne.'off') then
                        write (ipr,60) envvar(1:lenstr(envvar))
60       format ('0**ERROR** Value for apps_default ',
     *      a,' is not ''on'' or ''off''.')
                        call error
                        inderr=1
                        endif
                     endif
                  igziord=0
                  if (usegzio(1:lusegzio).eq.'on') then
                     igziord=1
                     if (fileiotype.eq.'input') igzio=1
                     endif
                  if (ibug.gt.0) write (ipr,*)
     *               ' envvar=',envvar(1:lenstr(envvar)),
     *               ' usegzio=',usegzio(1:lenstr(usegzio)),
     *               ' igzio=',igzio
                  envvar='calb_gzio_write'
                  call get_apps_defaults (envvar,lenstr(envvar),
     *               usegzio,lusegzio)
                  if (lusegzio.gt.0) then
                     musegzio=len(usegzio)
                     if (lusegzio.gt.musegzio) then
                        write (ipr,55) envvar(1:lenstr(envvar)),
     *                     lusegzio,musegzio
                        call error
                        ierror=1
                        endif
                     if (usegzio(1:lusegzio).ne.'on'.and.
     *                   usegzio(1:lusegzio).ne.'off') then
                        write (ipr,60) envvar(1:lenstr(envvar))
                        inderr=1
                        endif
                     endif
                  if (inderr.eq.1) then
                     ierror=1
                     go to 90
                     endif
                  igziowt=0
                  if (usegzio(1:lusegzio).eq.'on') then
                     igziowt=1
                     if (fileiotype.eq.'output') igzio=1
                     endif
                  if (ibug.gt.0) write (ipr,*)
     *               ' envvar=',envvar(1:lenstr(envvar)),
     *               ' usegzio=',usegzio(1:lenstr(usegzio)),
     *               ' igzio=',igzio
                  if (ibug.gt.0) write (ipr,*)
     *               ' igziord=',igziord,
     *               ' igziowt=',igziowt
                  if (igzio.eq.1.and.
     *               key(1:8).eq.'DATACARD') then
                     lfn=index(filnam,' ')-1
                     filnam2=' '
                     call ugzopn (iunit,filnam,filnam2,lfn,status,icond)
                     filnam2(lfn+1:lfn+1)=' '
                     if (icond.eq.0) then
C                    write message to log file if option is on
                        if (UU.ne.-1) then
                           write (UU,65) iunit,filnam2(1:lfn)
65      format (' opfile ',I3,'     opened            ',a)
                           endif
                        else
                           write (ipr,67) filnam2(1:lenstr(filnam2))
67    format ('0**ERROR** Error encountered in routine ugzopn ',
     * 'opening file ',a,'.')
                           call error
                           ierror=1
                        endif
                  else
                     call upstat (iunitd,filnam,status,icond)
                     if (icond.eq.0) then
                        call upopen (iunit,filnam,lrecl,form,icond)
                        else
                           call upstae (ipr,iunitd,filnam,status,icond)
                        endif
                  endif
               endif
            if (icond.ne.0) go to 70
            istat=setseq(ifiles(iunit))
         endif

C  Save key for later use
      filekeys(iunit)=key
      go to 90

70    write (ipr,80) iunit, key(1:lenstr(key))
80    format ('0**ERROR** Unable to open file for unit ',i3,
     * ' and key ',a,'.')
      call error
      ierror=1

90    call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write (iodbug,*) 'EXIT OPFILE'
         endif

      return

      end
