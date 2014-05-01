C$PRAGMA C (CHECK_EXIST)
c  =====================================================================
c  pgm:  mkfnam (dirnam,ldirnam,filnam,pthnam,istat)
c
c   in: dirnam  .... directory name
c   in: ldirnam .... length of directory name in characters
c   in: filnam  .... file name
c  out: pthnam  .... path name
c  out: istat   .... status code
c  =====================================================================
c
      subroutine mkfnam (dirnam,ldirnam,filnam,pthnam,istat)
c
c.......................................................................
c
c  Create path name from directory and file name.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Mar 1992
c.......................................................................
c
      character*1 resp
      character*150 unixcmd
      character dirnam*(*),filnam*(*),pthnam*(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/mkfnam.f,v $
     . $',                                                             '
     .$Id: mkfnam.f,v 1.4 2004/01/30 17:50:20 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('mkfnam',1,4,ibug)
c
      istat=0
c
      if (ibug.eq.1) write (iud,*) 'in mkfnam -',
     +   ' dirnam=',dirnam(1:lenstr(dirnam)),
     +   ' ldirnam=',ldirnam,
     +   ' filnam=',filnam(1:lenstr(filnam))
c
c  determine non-blank characters in dirnam
      if (ldirnam.le.0) then
         ldirnam = index(dirnam,'\0') - 1
         endif
c
      lfilnam=lenstr(filnam)
c
c  check if directory exists
      iprint=0
      call check_exist (dirnam,'directory',iexist,iprint)
      if (iexist.eq.0) then
         write (iutw,10) dirnam(1:ldirnam)
10    format (' WARNING: directory ',a,' not found.')
         istat=1
         icreat=1
         if (ibug.eq.1) write (iud,*) 'in mkfnam - icreat=',icreat
         if (icreat.eq.1) then
            write (iutw,20) dirnam(1:ldirnam)
20    format (' Okay to create directory ',a,' ([y] or n)? ',$)
            read (iutr,'(a)',end=30) resp
            go to 50
30          write (iutw,40)
40    format (' ERROR: in mkfnam - unexpected end of file encountered.')
            stop 8
50          if (resp.eq.'y'.or.resp.eq.'Y'.or.resp.eq.' ') then
               unixcmd='mkdir -p '//dirnam(1:ldirnam)
               write (iutw,'(a,a)') ' NOTE: running unix command: ',
     *            unixcmd(1:lenstr(unixcmd))
               call system (unixcmd)
               istat=0
               endif
            endif
         endif
c
      if (ibug.eq.1) write (iud,*) ' in mkfnam - ',
     +   'filnam=',filnam(1:lfilnam),' dirnam=',dirnam(1:ldirnam)
c
      pthnam = dirnam(1:ldirnam)//'/'//filnam(1:lfilnam)
c
      if (ibug.eq.1) write (iud,*) 'in mkfnam - ',
     *   'pthnam=',pthnam(1:lenstr(pthnam))
c
      return
c
      end
