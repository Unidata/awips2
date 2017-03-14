C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  devnum
c
c  all variables are passed through common blocks
c  =====================================================================
      subroutine devnum
c.......................................................................
c  program assigns unit numbers
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character*1 filfmt
      character*20 appsvar
      character*128 pthnam
c
      include 'udebug'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/count'
      include 'common/ionum'
c
      integer ftn_std_err
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/devnum.f,v $
     . $',                                                             '
     .$Id: devnum.f,v 1.4 2004/09/13 14:23:19 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
c  standard error device
      iser = ftn_std_err()
c
c  input/print unit numbers
      iutr  = 5
      iutw  = 6
      appsvar='ffg_print_filename'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,pthnam,lpthnam)
      if (lpthnam.gt.0) then
c     check if file exists
         iunit = 0
         call upexis (iunit,pthnam(1:lpthnam),istat)
         if (istat.eq.0) then
            call updele (iunit,pthnam(1:lpthnam),istat)
            if (ibug.eq.1) write (6,*) 'in devnum - updele',
     +         ' istat=',istat
            endif
         iutw  = 98
         irecl = 0
         filfmt = 'F'
         call upopen (iutw,pthnam(1:lpthnam),irecl,filfmt,ierr)
         if (ierr.ne.0) then
            write (6,10) ierr,pthnam(1:lpthnam)
10    format (' ERROR: in devnum - status code ',i3,
     +   ' encountered opening file ',a,'.')
            nerr = nerr + 1
            endif
         endif
      iupr  = iutw
      iue   = iutw
      iul   = 9
      iud   = iutw
      iogdb = iutw
      in    = iutr
      ipr   = iutw
      ipu   = 8
c
c  file unit numbers
      iuf   = 20
      iuu   = 21
      iubb  = 22
      iudx  = 24
      iuff  = 26
      iuiov = 28
      iupm  = 30
      iuop  = 32
      iurc  = 34
      iuro  = 36
      iuts  = 38
c
      return
c
      end
