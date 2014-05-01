c  =====================================================================
c  pgm:  getinf (kod,mpo,po,iunit,istat)
c
c   in: kod    .... code for handling non-existing files:
c                     0 = error message
c                     1 = create file
c   in: mpo    .... maximum size of array po
c  out: po     .... parameter array
c  out: iunit  .... unit number
c  out: istat  .... status code
c  =====================================================================
c
      subroutine getinf (kod,mpo,po,iunit,istat)
c
c.......................................................................
c
c  routine to read user controls from file
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Sept 1992
c.......................................................................
c
      character*8 filnam
      character*128 geopath/' '/
c
      dimension po(*),drnfl(6)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
c
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getinf.f,v $
     . $',                                                             '
     .$Id: getinf.f,v 1.6 2004/01/30 17:49:16 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getinf',1,1,ibug)
ccc      ibug = 1
c
      istat = 0
c
c  set allowable durations
      idurt(1) = 1
      idurt(2) = 3
      idurt(3) = 6
      idurt(4) = 12
      idurt(5) = 24
c
c  set default water supply rainfall
      drnfl(1) = 1.0
      drnfl(2) = 2.0
      drnfl(3) = 3.0
      drnfl(4) = 4.0
      drnfl(5) = 5.0
      drnfl(6) = 8.0 
c
c  read file
      filnam='usrinfo'
      call umemov (filnam,usrid,2)
      usrtyp = 'user'
      call rppfil (filnam,usrtyp,kod,iunit,mpo,po,npo,istat)
      if (ibug.eq.1) write (iutw,*) 'in getinf after rppfil -',
     *   ' kod=',kod,' istat=',istat
      if (istat.ne.0) go to 100
      rewind (iunit)
      if (kod.eq.0) then
         filnam = ' '
         call upclos (iunit,filnam,istat)
         endif
c
c  file version
      uvers = po(1)
c
c  identifier
      call umemov (po(2),usrid,2)
c
c  data type
      call umemov (po(4),usrtyp,1)
c
c  number of words
      iuseu = po(5)
c
c  location of extrema data
      loed = po(6)
c
c  location of options
      loop = po(7)
c
c  file conversion
      if (loop.eq.40) then
         k = 1
         else
            loop = 40
          k = 0
         endif
c
c  location of HRAP data
      loch = po(7+k)
c
c  location of water supply control
      lows = po(8+k)
c
c  location of land slide information
      lols = po(9+k)
c
c  location of product generation options
      lopg = po(12+k)
c
c  warning and error output flag
      iweout = po(13+k)
c
c  alternate OFS files
      iofs = po(14+k)
c
c  new feature control
      nfeat = po(15+k)
c
c  extrema
      k = loed - 1
      do 10 i=1,20
         ext(i) = po(i+k)
10       continue
c
c  extrema for grids
      do 20 i=1,5
         j = (i-1)*2 + 1
         pmaxg(i) = ext(j)
         pming(i) = ext(j+1)
20       continue
c
c  determine max duration for grids
      mxdurg = -1
      do 30 i=1,5
         if (pmaxg(i).ge.0.0) mxdurg = i
30       continue
      if (ibug.eq.1) write (iud,*) 'in getinf - mxdurg=',mxdurg
c
c  extrema for headwaters
      do 40 i=1,5
         j = (i-1)*2 + 11
         pmaxh(i) = ext(j)
         pminh(i) = ext(j+1)
40       continue
c
c  determine max duration for headwaters
      mxdurh = -1
      do 50 i=1,5
         if (pmaxh(i).gt.0.0) mxdurh = i
50       continue
      if (ibug.eq.1) write (iud,*) 'in getinf - mxdurh=',mxdurh
c
c  runoff adjustment control for grids
      irctlg = po(loop)
c
c  runoff adjustment control for headwaters
      irctlh = po(loop+1)
c
c  high flow adjustment control for grids
      iqctlg = po(loop+2)
c
c  high flow adjustment control for headwaters
      iqctlh = po(loop+3)
c
c  area computation method
      iameth = po(loop+4)
c
c  check version number
      if (uvers.ge.2.40) then
c     bankfull factor
         gbank = po(loop+5)
c     computer system time zone - OFS variable clkzon
         call umemov (po(loop+6),cpzone,1)
c     hour offset to local time - OFS variable local
         loclo = po(loop+7)
c     time zone number of local standard time - OFS variable nlstz
         nlstzo = po(loop+8)
c     user name - OFS variable hnamrf
         call umemov (po(loop+9),usrnam,2)
         else
            call umemov (po(loop+5),cpzone,1)
            loclo = po(loop+6)
            nlstzo = po(loop+7)
            call umemov (po(loop+8),usrnam,2)
            gbank = 1.10
         endif
      if (usrnam.eq.'none') then
         write (iutw,55) usrnam
         if (iupr.ne.iutw) write (iupr,55) usrnam
55    format (/ ' WARNING in getinf: user name is ''',a,'''.')
         nwarn = nwarn + 1
         endif
c
c  check version number - moved ngfil, added ickval
      if (uvers.ge.2.43.and.loch.gt.51) then
c     extend number of col and rows
         ngfil = po(loop+11)
c     check decreasing ffg values
         ickval = po(loop+12)
         else
            ngfil = 3
            ickval = 0
         endif
c
c  HRAP grid
      mwcol = po(loch)
      ncol  = po(loch+1)
      msrow = po(loch+2)
      nrow  = po(loch+3)
c
c  get HRAP subset from geo data file
      iugeo=99
      call get_geo_coord (iutw,iugeo,geopath,nhwcol,nhcol,nhsrow,nhrow,
     +   istat2)
      if (istat2.eq.0) then
c     check subset values
         inderr=0
         if (mwcol.ne.nhwcol) then
            if (inderr.eq.0) then
               write (iutw,*)
               if (iupr.ne.iutw) write (iupr,*)
               endif
            write (iutw,60) 'western HRAP column',mwcol,nhwcol
            if (iupr.ne.iutw) then
               write (iupr,60) 'western HRAP column',mwcol,nhwcol
               endif
60    format (' WARNING: in getinf - FFG ',a,' (',i4,') ',
     *   'does not equal the value in the geo data file (',i4,').')
            inderr=1
            endif
         if (ncol.ne.nhcol) then
            if (inderr.eq.0) then
               write (iutw,*)
               if (iupr.ne.iutw) write (iupr,*)
               endif
            write (iutw,60) 'number of HRAP columns',ncol,nhcol
            if (iupr.ne.iutw) then
               write (iupr,60) 'number of HRAP columns',ncol,nhcol
               endif
            inderr=1
            endif
         if (msrow.ne.nhsrow) then
            if (inderr.eq.0) then
               write (iutw,*)
               if (iupr.ne.iutw) write (iupr,*)
               endif
            write (iutw,60) 'southern HRAP row',msrow,nhsrow
            if (iupr.ne.iutw) then
               write (iupr,60) 'southern HRAP row',msrow,nhsrow
               endif
            inderr=1
            endif
         if (nrow.ne.nhrow) then
            if (inderr.eq.0) then
               write (iutw,*)
               if (iupr.ne.iutw) write (iupr,*)
               endif
            write (iutw,60) 'number of HRAP rows',nrow,nhrow
            if (iupr.ne.iutw) then
               write (iupr,60) 'number of HRAP rows',nrow,nhrow
               endif
            inderr=1
            endif
         if (inderr.eq.1) then
            write (iutw,70) geopath(1:lenstr(geopath))
            if (iupr.ne.iutw) write (iupr,70) geopath(1:lenstr(geopath))
            endif
70    format (' WARNING: in getinf - one or more of the FFG HRAP ',
     *      'subset values does not match the value in the geo data ',
     *      'file' /
     *   t10,' ',a,'.')
         else
            write (iutw,75) geopath(1:lenstr(geopath))
            if (iupr.ne.iutw) write (iupr,75) geopath(1:lenstr(geopath))
75    format (' WARNING: in getinf - file ',a,' not found. ',
     +   'HRAP subset cannot be checked.')    
         endif
c
c  water supply control
      iwats = po(lows)
c
c  water supply rainfall
      do 90 i=1,6
         rnfl(i) = po(lows+i)
90       continue
c
c  land slide control
      lslide = po(lols)
c
c  single file for SHEF products
      ising = po(lopg)
c
c  control for comms system
      icom = po(lopg+1)
c
c  control for FFG physical elements
      iffpe = po(lopg+2)
c
c  control for year format
      icent = po(lopg+3)
c
c  P array conversion control - remove later
      if (uvers.lt.2.301) then
         if (ising.lt.2) then
            iffpe = 1
            else
               ising = ising - 2
               iffpe = 2
             endif
          icent = 4
          lopg = lopg - 5
          else
             if (iffpe.ne.1.and.iffpe.ne.2) iffpe = 1
             if (icent.ne.2.and.icent.ne.4) icent = 4
         endif
c
c  control to append duty forecasters' names
      igduty = po(lopg+7)
c
c  number of forecasters
      mdf = po(lopg+8)
c
c  forecasters
      if (mdf.gt.0) then
         n = mdf*16
         call umemov (po(lopg+9),fcstr,n)
         endif
c
      go to 150
c
c.......................................................................
c
c  directory or file not found
c
100   if (kod.eq.1) go to 150
c
      if (kod.eq.0) then
         write (iutw,110) filnam(1:lenstr(filnam))
         if (iupr.ne.iutw) write (iupr,110) filnam(1:lenstr(filnam))
110   format (' ERROR: file ',a,' not found.')
         nerr = nerr + 1
         istat = 1
         go to 150
         else
            if (istat.eq.-1) then
               write (iutw,113) filnam(1:lenstr(filnam))
               if (iupr.ne.iutw) then
                  write (iupr,113) filnam(1:lenstr(filnam))
                  endif
113   format (' ERROR: directory for file ',a,' not found.')
               nerr = nerr + 1
               istat = 2
               go to 150
               else
                  write (iutw,115) filnam(1:lenstr(filnam))
                  if (iupr.ne.iutw) then
                     write (iupr,115) filnam(1:lenstr(filnam))
                     endif
115   format (' NOTE: file ',a,' not found. ',
     *   'Parameter values will be set to defaults.')
                  endif
         endif
c
c  set defaults
      uvers = -1.0
      iweout = 0
      iofs = 0
      nfeat = 0
      do 120 i=1,20,2
         ext(i)   = 20.0
         ext(i+1) = 0.1
120      continue
      do 130 i=7,10,2
         ext(i)   = -1.0
         ext(i+1) = -1.0
         ext(i+10) = -1.0
         ext(i+11) = -1.0
130      continue
      irctlg = 0
      irctlh = 0
      iqctlg = 0
      iqctlh = 0
      iameth = 2
      gbank = 1.10
      usrnam = 'none'
      cpzone = 'Z'
      loclo = 0
      nlstzo = 0
      ngfil = 3
      ickval = 0
      mwcol  = -900
      ncol   = -900
      msrow  = -900
      nrow   = -900
      ising  = 0
      icom   = 1
      iffpe  = 1
      icent  = 4
      igduty = 0
      mdf    = 0
      iwats  = 0
      do 140 i=1,6
         rnfl(i) = drnfl(i)
140      continue
      lslide = 0
c
c  write to file
      call strinf (iunit,po)
c
      istat = 0
c
150   return
c
      end
