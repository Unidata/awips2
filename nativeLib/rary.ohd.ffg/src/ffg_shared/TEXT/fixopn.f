C$PRAGMA C (CHECK_ACCESS_MODE)
c  =====================================================================
c  pgm:  fixopn (filnam,filtyp,pthnam,accmode,kod,iunit,istat)
c
c   in: filnam  .... file name
c   in: filtyp  .... file type
c   in: pthnam  .... path name
c   in: accmode .... file access mode
c   in: kod     .... code for handling non-existing files:
c                      1 = error message
c                      2 = prompt to create
c                      3 = create file without prompt
c  out: iunit   .... unit number
c  out: istat   .... status code
c
c  =====================================================================
c
      subroutine fixopn (filnam,filtyp,pthnam,accmode,kod,iunit,istat)
c
c  .....................................................................
c
c  This routine opens a file using file attributes from routine fixfil.
c
c  .....................................................................
c  Initially written by
c       Tim Sweeney, HRL - Feb 1994
c  .....................................................................
c
      character*(*) filnam,pthnam
      character*1 filfmt,resp
      character*4 filtyp
      character*4 accmode
      character*5 accmodez
      character*128 filnamz,pthnamz
      character*150 unixcmd
c
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iuws'
      include 'ffg_inc/count'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/fixopn.f,v $
     . $',                                                             '
     .$Id: fixopn.f,v 1.6 2004/01/30 17:48:26 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug=0
c
      istat=0
c
      filnamz=filnam
c
      if (ibug.eq.1) write (iud,*) 'in fixopn -',
     +   ' filnam=',filnam(1:lenstr(filnam)),' filtyp=',filtyp
c
c  set path name
      call fixfil (filnam,filtyp,iunit,pthnam,irecl,filfmt,ierr)
      lpthnam=lenstr(pthnam)
      if (ibug.eq.1) write (iud,*) 'in fixopn -',
     *   ' pthnam=',pthnam(1:lpthnam),' ierr=',ierr
      if (ierr.ne.0) then
         istat = -1
         go to 80
         endif
c
c  check if exists
      call upexis (iunit,pthnam(1:lpthnam),ierr)
      if (ierr.eq.-1) then
c     file does not exist
         if (kod.eq.0) then
ccc            istat = 7
         else if (kod.eq.1) then
            istat = 1
            go to 80
         else if (kod.eq.2) then
            istat = -1
            write (iutw,10) pthnam(1:lpthnam)
10    format (' WARNING: file ',a,' not found.')
            write (iutw,20)
20    format (' Create file ([y] or n)? ',$)
            read (iutr,'(a)') resp
            if (resp.eq.'N'.or.resp.eq.'n') then
               istat = 7
               go to 80
               endif
            else if (kod.eq.3) then
               istat = 7
            endif
         else
            pthnamz = pthnam(1:lpthnam)//char(0)
            lpthnamz = lenstr(pthnamz)
            laccmode = lenstr(accmode)
            accmodez = accmode(1:laccmode)//char(0)
            iprint = 0
            call check_access_mode (pthnamz,accmodez,iaccmode,iprint)
            if (iaccmode.ne.0) then
               istat = 7
               if (iprint.eq.0) then
                  if (iaccmode.eq.1) then
                     write (iutw,30) pthnamz(1:lpthnamz)
30    format (' ERROR in check_access_mode: pathname ',a,' not found.')
                     endif
                  if (iaccmode.eq.2) then
                     write (iutw,40) pthnamz(1:lpthnamz),
     +                  accmode(1:laccmode)
40    format (' ERROR in check_access_mode: pathname ',a,
     +   ' cannot be checked for mode for access mode ''',a,'''.')
                     endif
                  if (iaccmode.eq.3) then
                     write (iutw,50) pthnamz(1:lpthnamz),
     +                  accmode(1:laccmode)
50    format (' ERROR in check_access_mode: pathname ',a,
     +   ' cannot be opened for access mode ''',a,'''.')
                     unixcmd='ls -l '//pthnamz(1:lpthnamz)
                     write (iutw,60) unixcmd(1:lenstr(unixcmd))
60    format (' NOTE: output from UNIX command ''',a,''':')
                     call system (unixcmd)
                     endif
                  endif
               endif
         endif
      if (ibug.eq.1) write (iud,*) 'in fixopn - kod=',kod,
     +   ' istat=',istat
c
c  open file
      if (ibug.eq.1) write (iud,*) 'in fixopn - iunit=',iunit,
     +   ' pthnam=',pthnam(1:lpthnam),' irecl=',irecl,
     +   ' filfmt=',filfmt
      call upopen (iunit,pthnam(1:lpthnam),irecl,filfmt,ierr)
      if (ierr.ne.0) then
         write (iutw,70) ierr,pthnam(1:lpthnam)
70    format (' ERROR: in fixopn - upopen status code ',i3,
     +   ' encountered opening file ',a,'.')
         istat = 1
         nerr = nerr + 1
         endif
c
80    if (ibug.eq.1) write (iud,*) 'in fixopn - kod=',kod,
     +   ' istat=',istat
c
      return
c
      end
