c  =====================================================================
c  pgm:  pinhed (ident,istat)
c
c  out: ident  .... headwater identifier
c  out: istat  .... completion code
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pinhed (ident,istat)
c
c.......................................................................
c
c  input parameters for type code hffg for headwaters
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL                          March 1992
c
c      changed to free format
c           Tim Sweeney, HRL                             Feb 1997
c.......................................................................
c
      character*4 cend(2),wid(2)
      character*4 lcend(2),type
      character*8 ident
      character*20 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/hwparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/pinhed.f,v $
     . $',                                                             '
     .$Id: pinhed.f,v 1.5 2004/01/06 12:46:26 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   ' /
      data lcend/ 'endi','d   '/
c
c
      call prbug ('pinhed',1,1,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iutw,*) 'enter pinhed'
c
      istat = 0
c
      hdwtyp = 'hffg'
c
      inp = iuu
c
      lhcpd = -1
      do 10 i=1,5
         hffg(i) = -99.0
10       continue
c
c  read first input record
c
      read (inp,'(a)',end=30,err=30) line
      go to 40
c
c  end of file for input
30    istat = 1
      go to 190
c
c  data type
40    iptr = 1
      iwidth = 4
      call uffch (line,iptr,iwidth,type,nxt,ierr)
      if (ierr.gt.0) go to 185
      if (type.ne.hdwtyp.and.type.ne.'HFFG') then
         istat = 2
         go to 190
         endif
c
c  headwater id
      iptr = nxt
      iwidth = 8
      call uffch (line,iptr,iwidth,work,nxt,ierr)
      if (ierr.gt.0) go to 185
      call umemov (work,hdid,2)
      call umemov (hdid,ident,2)
c
c  description
      iptr = nxt
      iwidth = 20
      call uffch (line,iptr,iwidth,work,nxt,ierr)
      if (ierr.gt.0) go to 185
      call umemov (work,desc,5)
c
c  stream name
      iptr = nxt
      iwidth = 20
      call uffch (line,iptr,iwidth,work,nxt,ierr)
      if (ierr.gt.0) go to 185
      call umemov (work,strnam,5)
c
c  latitude
      iptr = nxt
      iwidth = 7
      call uffir (line,iptr,iwidth,lat,real,nxt,itype)
      if (lat.gt.9999) then
c     specified as mmddss
         latd = lat/10000
         lats = lat - latd*10000
         latm = lats/100
         lats = lats - latm*100
c     specified as mmdd
         else
            latd = lat/100
            latm = lat - latd*100
         endif
c
c  longitude
      iptr = nxt
      iwidth = 7
      call uffir (line,iptr,iwidth,lon,real,nxt,itype)
      if (lon.gt.9999) then
c     specified as mmddss
         lond = lon/10000
         lons = lon - lond*10000
         lonm = lons/100
         lons = lons - lonm*100
c     specified as mmdd
         else
            lond = lon/100
            lonm = lon - lond*100
         endif
c
      if (ibug.eq.1) write (iud,*) 'type=',type,
     +   ' hid=',(hdid(i),i=1,2),
     +   ' desc=',(desc(i),i=1,5),
     +   ' strnam=',(strnam(i),i=1,5),
     +   ' lat=',lat,' lon=',lon,
     +   ' latd=',latd,' latm=',latm,
     +   ' lond=',lond,' lonm=',lonm 
c
      if (iud.ne.iupr) write (iupr,50) type,hdid,desc,strnam,lat,lon
50    format (' ',a4,1x,2a4,1x,5a4,1x,5a4,1x,i6,1x,i7)
c
c  read second input record
c
      read (inp,'(a)') line
c
c  high flow adjust control
      iptr = 1
      iwidth = 4
      call uffir (line,iptr,iwidth,iqopth,real,nxt,itype)
c
c  runoff adjust control
      iptr = nxt
      iwidth = 4
      call uffir (line,iptr,iwidth,iropth,real,nxt,itype)
c
c  percent impervious
      iptr = nxt
      iwidth = 4
      call uffir (line,iptr,iwidth,i,pcimpv,nxt,itype)
c
c  rating curve identifier
      iptr = nxt
      iwidth = 8
      call uffch (line,iptr,iwidth,work,nxt,itype)
      if (ierr.gt.0) go to 185
      call umemov (work,rcid,2)
c
c  flow at flood stage
      iptr = nxt
      iwidth = 7
      call uffir (line,iptr,iwidth,i,fsflow,nxt,itype)
      if (ierr.gt.0) go to 190
c
c  unitgraph peak flows (or threshold runoffs when fsflow = 0)
      iwidth = 7
      do 70 i=1,5
         iptr = nxt
         call uffir (line,iptr,iwidth,j,upk(i),nxt,itype)
70       continue
c
c   height of basin in minutes of latitude
      iptr = nxt
      iwidth = 7
      call uffir (line,iptr,iwidth,lath,real,nxt,itype)
c
c  width of basin in minutes of longitude
      iptr = nxt
      iwidth = 7
      call uffir (line,iptr,iwidth,lonh,real,nxt,itype)
c
      if (ibug.gt.0) write (iud,80) iqopth,iropth,pcimpv,rcid,
     +   fsflow,upk,lath,lonh
80    format (4x, 2(1x,i1), f4.2,1x,2a4,1x,6f8.0,2i4 )
c
c  convert centroid of box to integer deg min
      lat = latd*100 + latm
      lon = lond*100 + lonm
c
c  convert integer deg min to decimal degrees
      icv = 1
      call degcon (icv,lat,hlat)
      call degcon (icv,lon,hlon)
      hlath = lath/60.
      hlonh = lonh/60.
c
      if (iqopth.lt.1) go to 110
c
c  read optional third record - high base flow adjustment
c
      read (inp,'(a)') line
c
c  time associated with high flow adjustment (interpretation
c  depends on option selected, i.e. iqopth)
      nxt = 1
      iwidth = 4
      do 90 i=1,5
         iptr = nxt
         call uffir (line,iptr,iwidth,j,taq(i),nxt,itype)
90       continue
c
c  forecast flow time series id
      iptr = nxt
      iwidth = 8
      call uffch (line,iptr,iwidth,work,nxt,ierr)
      if (ierr.gt.0) go to 185
      call umemov (work,qtsid,2)
c
c  data type code for fcst flow time series
      iptr = nxt
      iwidth = 4
      call uffch (line,iptr,iwidth,dtcq,nxt,ierr)
      if (ierr.gt.0) go to 185
c
c  time interval of time series
      iptr = nxt
      iwidth = 4
      call uffir (line,iptr,iwidth,intq,real,nxt,itype)
c
      if (ibug.gt.0) write (iud,100) taq,qtsid,dtcq,intq
100   format (4x,5(2x,f4.0),1x,2a4,1x,a4,2x,i2)
c
110   if (iropth.lt.1) go to 140
c
c  read optional fourth record - runoff adjust parameters
c
      read (inp,'(a)') line
      nxt = 1
      iwidth = 6
      do 120 i=1,5
         iptr = nxt
         call uffir (line,iptr,iwidth,r,hinten(i),nxt,itype)
120      continue
      if (ibug.gt.0) write (iud,130) hinten
130   format (4x,5f6.2 )
c
c  component areas (maps)
140   na = 0
      do 180 lr=1,4
         ib = (lr-1)*5 + 1
         ie = lr*5
         read (inp,'(a)') line
         nxt = 1
         do 150 i=ib,ie
c        basin weight
            iptr = nxt
            iwidth = 3
            call uffir (line,iptr,iwidth,j,wt(i),nxt,itype)
            if (wt(i).gt.1.00) wt(i) = wt(i)/100.0
c        area id
            iptr = nxt
            iwidth = 8
            call uffch (line,iptr,iwidth,work,nxt,ierr)
            if (ierr.gt.0) go to 185
            call umemov (work,wid,2)
            arid(1,i) = wid(1)
            arid(2,i) = wid(2)
c        check for 'endid'
            if (arid(1,i).eq.cend(1).or.arid(1,i).eq.lcend(1)) then
               if (arid(2,i).eq.cend(2).or.arid(2,i).eq.lcend(2)) then
                  ie = i
                  na = 1
                  go to 160
                  endif
               endif
150         continue
160      if (ibug.gt.0) write (iud,170)
     +      (wt(i),(arid(j,i),j=1,2),i=ib,ie)
170   format (' ',5(f5.2,1x,2a4))
         if (na.gt.0) go to 190
180      continue
c
      go to 190
c
185   if (ierr.eq.2) then
         write (iutw,187)
         if (iupr.ne.iutw) write (iupr,187)
187   format (' ERROR in pinhed: null field encountered.')
         else
            write (iutw,189) ierr
            if (iupr.ne.iutw) write (iupr,189) ierr
189   format (' ERROR in pinhed: status code = ',i2)
         endif
c
190   if (ibug.eq.1) write (iutw,*) 'exit pinhed - istat=',istat
c
      return
c
      end
