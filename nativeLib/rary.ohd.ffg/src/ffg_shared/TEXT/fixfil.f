c  =====================================================================
c  pgm:  subroutine fixfil (filnam,filtyp,iunit,pthnam,irecl,filfmt,
c                           istat)
c
c   in: filnam .... file name
c   in: filtyp .... file type
c  out: iunit  .... unit number
c  out: pthnam .... path name
c  out: irecl  .... record length
c  out: filfmt .... file format
c  out: istat  .... status code:
c                     0 = no errors
c                     1 = errors
c  =====================================================================
c
      subroutine fixfil (filnam,filtyp,iunit,pthnam,irecl,filfmt,istat)
c
c.......................................................................
c
c  Set path name and unit number based on file type (and id if an index 
c  file).
c
c.......................................................................
c      Initially written by
c            Tim Sweeney, HRL                            Mar 1992
c.......................................................................
c
      character*1 filfmt
      character*4 filtyp
      character*128 dirnam
      character*(*) pthnam
      character*(*) filnam
c
      include 'ffg_inc/paths'
      include 'ffg_inc/fpath'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'ffg_inc/iodno'
      include 'ffg_inc/count'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/fixfil.f,v $
     . $',                                                             '
     .$Id: fixfil.f,v 1.6 2004/09/13 14:23:40 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
      istat = 0
c
      filfmt = 'u'
      irecl = -1
c
      if (ibug.eq.1) write (iud,*) 'in fixfil - filtyp=',filtyp,
     +   ' filnam=',filnam(1:lenstr(filnam))
c
c  check file type
      if (filtyp.eq.'affg') then
         dirnam  = arpa
         ldirnam = lar
         iunit   = iupm
      else if (filtyp.eq.'basn') then
         dirnam  = basnpa
         ldirnam = lbasn
         iunit   = iubb
      else if (filtyp.eq.'ffg') then
         dirnam  = ffoppa
         ldirnam = lffop
         iunit   = iuop
      else if (filtyp.eq.'grff') then
         dirnam  = gffpa
         ldirnam = lgff
         iunit   = iuff
      else if (filtyp.eq.'grro') then
         dirnam  = gropa
         ldirnam = lgro
         iunit   = iuro
      else if (filtyp.eq.'hffg') then
         dirnam  = hdwpa
         ldirnam = lhdw
         iunit   = iupm
      else if (filtyp.eq.'gdpm') then
         dirnam  = igpmpa
         ldirnam = lgpm
         iunit   = iuiov
      else if (filtyp.eq.'prod') then
         dirnam  = prodpa
         ldirnam = lprod
         iunit   = iupm
      else if (filtyp.eq.'grpp') then
         dirnam  = grpppa
         ldirnam = lgrpp
         iunit   = iupm
      else if (filtyp.eq.'rtcv') then
         dirnam  = rcpa
         ldirnam = lrc
         iunit   = iurc
      else if (filtyp.eq.'text') then
         dirnam  = txtpa
         ldirnam = ltxt
         iunit   = iupm
      else if (filtyp.eq.'tser') then
         dirnam  = tspa
         ldirnam = lts
         iunit   = iuts
      else if (filtyp.eq.'user') then
         dirnam  = usrpa
         ldirnam = lusr
         iunit   = iupm
      else if (filtyp.eq.'log') then
         dirnam  = outpa
         ldirnam = lout
         iunit  = iul
         irecl = 0
         filfmt   = 'f'
      else if (filtyp.eq.'out') then
         dirnam  = outpa
         ldirnam = lout
         iunit  = iuf
         irecl = 0
         filfmt   = 'f'
      else if (filtyp.eq.'outu') then
         dirnam = outpa
         ldirnam = lout
         iunit  = iuu
         irecl = -1
         filfmt   = 'u'
      else if (filtyp.eq.'cary') then
         dirnam = carypa
         ldirnam = lcary
         iunit  = iupm
      else if (filtyp.eq.'lang') then
         dirnam = langpa
         ldirnam = llang
         iunit  = iupm
      else if (filtyp.eq.'wsup') then
         dirnam = wsuppa
         ldirnam = lwsup
         iunit  = iupm
      else if (filtyp.eq.'fcst') then
         dirnam = fcstpa
         ldirnam = lfcst
         iunit  = iuff
      else if (filtyp.eq.'tabl') then
         irecl = 0
         filfmt   = 'f'
         dirnam = tablpa
         ldirnam = ltabl
         iunit  = iupm
      else if (filtyp.eq.'anam') then
         irecl = 0
         filfmt   = 'f'
         go to 30
      else
         write (iutw,10) filtyp(1:lenstr(filtyp))
10    format (' ERROR: in fixfil - file type ',a,' is invalid.')
         istat = 1
         nerr = nerr + 1
         go to 30
      endif
c
c  check if an index file
      if (filnam(1:4).eq.'inde') iunit = iudx
c
      if (ldirnam.eq.0) then
         write (iutw,20) filtyp(1:lenstr(filtyp))
20    format (' ERROR: in fixfil - length of file name for file type ',
     +   a,' is zero.')
         istat = 1
         nerr = nerr + 1
         go to 30
         endif
c
c  set path name
      call mkfnam (dirnam,ldirnam,filnam,pthnam,istat)
c
30    return
c
      end
