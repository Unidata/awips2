c  =====================================================================
c  pgm:  opnlog (ilogopen,filnam,pthnam,istat)
c
c   in: ilogopen .... log file opened indicator:
c                      0 = not opened
c                      1 = opened
c   in: filnam   .... file name
c   in: pthnam   .... path name
c  out: istat    .... status code
c
c  =====================================================================
c
      subroutine opnlog (ilogopen,filnam,pthnam,istat)
c
c.......................................................................
c
c  routine to open the log file
c
c.......................................................................
c
      character*4 filtyp
      character*4 accmode
      character*(*) filnam,pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/opnlog.f,v $
     . $',                                                             '
     .$Id: opnlog.f,v 1.3 2004/01/30 17:50:34 scv Exp $
     . $' /
C    ===================================================================
C
c
      istat = 0
c
      if (ilogopen.eq.1) then
         write (iutw,5) filnam(1:lenstr(filnam))
5     format (/ ' NOTE: logfile ',a,' is already open.')
         istat = 1
         go to 20
         endif
c
ccc      if (iupr.ne.iul) go to 20
c
c  temporarily change debug and error output to screen until logfile
c  is opened
      iudh = iud
      iueh = iue
      iud = iutw
      iue = iutw
c
      filtyp='log'
      accmode='rw'
      kod = 0
      call fixopn (filnam,filtyp,pthnam,accmode,kod,iul,istat)
      if (istat.eq.0) then
         rewind (iul)
         ilogopen = 1
         endif
c
c  reset error and debug devices
      iud = iudh
      iue = iueh
c
20    return
c
      end
