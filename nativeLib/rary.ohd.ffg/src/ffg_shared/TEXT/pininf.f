c  =====================================================================
c  pgm:  pininf (icaller,pthnam,istat)
c
c  in:  icaller .... caller indicator:
c                      1 = program ffguid
c                      2 = program prodgen
c  in:  pthnam  .... path name of input file
c  out: istat   .... completion code:
c                      0 = successful
c                      1 = end of file or error
c                      2 = invalid data type
c  =====================================================================
c
      subroutine pininf (icaller,pthnam,istat)
c
c.......................................................................
c
c  read user control parameters from file
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL                          March 1992
c
c      changed to free format
c           Tim Sweeney, HRL                             Feb 1997
c.......................................................................
c
      character*1 dlim
      character*10 type
      character*20 strng
      character*80 line
      character*128 pthnam
      integer labl(25)
     +         / 1, 1, 2,11,12,
     +           3, 3, 4,13,14,
     +           6, 5, 6,15,16,
     +          12, 7, 8,17,18,
     +          24, 9,10,19,20/
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/pininf.f,v $
     . $',                                                             '
     .$Id: pininf.f,v 1.3 2004/01/30 17:51:05 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('pininf',1,1,ibug)
ccc     ibug = 1
c
      istat = 0
c
      inp = iuu
      rewind inp
c
      dlim = ' '
      nxt = 1
c
c  read first line
10    read (inp,'(a)',end=20,err=40) line
      go to 50
c
c  end of file
20    write (iutw,30) 'unexpected end of file',
     +   pthnam(1:lenstr(pthnam))
      if (iupr.ne.iutw) write (iupr,30) 'unexpected end of file',
     +   pthnam(1:lenstr(pthnam))
30    format (' ERROR: in pininf - ',a,' encountered reading file ',a,
     +   '.')
      nerr = nerr + 1
      istat = 1
      go to 130
c
c  read error
40    write (iutw,30) 'read error',
     +   pthnam(1:lenstr(pthnam))
      if (iupr.ne.iutw) write (iupr,30) 'read error',
     +   pthnam(1:lenstr(pthnam))
      nerr = nerr + 1
      istat = 1
      go to 130
c
c  data type
50    iptr = 1
      iwidth = len(type)
      call uffch (line,iptr,iwidth,type,nxt,istat)
      if (istat.gt.0) go to 130
      if (icaller.eq.1.and.type.eq.'uinf_guid') go to 60
      if (icaller.eq.2.and.type.eq.'uinf_prod') go to 100
      go to 10
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  maximum and minimum extremum for Grids and Headwaters for each
c  duration
60    do 70 i=1,5
         read (inp,'(a)',end=20,err=40) line
         nxt = 1
         ii = (i-1)*5 + 1
         ij = ii + 1
         ik = labl(ij)
         il = ii + 2
         im = labl(il)
         in = ii + 3
         ji = labl(in)
         jj = ii + 4
         jk = labl(jj)
         iptr = 1
         iwidth = 5
         call uffir (line,iptr,iwidth,integr,real,nxt,itype)
         ext(ik) = real
         iptr = nxt
         iwidth = 5
         call uffir (line,iptr,iwidth,integr,real,nxt,itype)
         ext(im) = real
         iptr = nxt
         iwidth = 5
         call uffir (line,iptr,iwidth,integr,real,nxt,itype)
         ext(ji) = real
         iptr = nxt
         iwidth = 5
         call uffir (line,iptr,iwidth,integr,real,nxt,itype)
         ext(jk) = real
70       continue
c
c  bankfull factor, runoff adjust, high flow adjust and area method
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 5
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      gbank = real
      iptr = nxt
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      irctlg = integr
      iptr = nxt
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      irctlh = integr
      iptr = nxt
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      iqctlg = integr
      iptr = nxt
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      iqctlh = integr
      iptr = nxt
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      iameth = integr
c
c  computer system time zone, user name, hour offset to local time,
c  time zone number of local standard time
      read (inp,'(a)',end=20,err=40) line
      if (ibug.eq.1) write (iud,*) 'in pininf - line=',
     +   line(1:lenstr(line))
      nscan = 1
      call uscan2 (line,' ',nscan,strng,lstrng,ierr)
      cpzone = strng
      if (ibug.eq.1) write (iud,*) 'in pininf - cpzone=',cpzone
      nscan = nscan + 1
      call uscan2 (line,' ',nscan,strng,lstrng,ierr)
      if (strng.ne.' ') then
         usrnam = strng
         if (ibug.eq.1) write (iud,*) 'in pininf - usrnam=',usrnam
         else
            write (iutw,80) 'usrnam'
            if (iupr.ne.iutw) write (iupr,80) 'usrnam'
80    format (' ERROR: in pininf - field for variable ',a,' is blank.')
            nerr = nerr + 1
         endif
      nscan = nscan + 1
      call uscan2 (line,' ',nscan,strng,lstrng,ierr)
      if (strng.ne.' ') then
         nxt = 1
         iptr = 1
         iwidth = lstrng
         call uffir (strng,iptr,iwidth,integr,real,nxt,itype)
         loclo = integr
         if (ibug.eq.1) write (iud,*) 'in pininf - loclo=',loclo
         else
            write (iutw,80) 'loclo'
            if (iupr.ne.iutw) write (iupr,80) 'loclo'
            nerr = nerr + 1
         endif
      nscan = nscan + 1
      call uscan2 (line,' ',nscan,strng,lstrng,ierr)
      if (strng.ne.' ') then
         nxt = 1
         iptr = 1
         iwidth = lstrng
         call uffir (strng,iptr,iwidth,integr,real,nxt,itype)
         nlstzo = integr
         if (ibug.eq.1) write (iud,*) 'in pininf - nlstzo=',nlstzo
         else
            write (iutw,80) 'nlstzo'
            if (iupr.ne.iutw) write (iupr,80) 'nlstzo'
            nerr = nerr + 1
         endif
c
c  HRAP grid subset
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 5
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      mwcol = integr
      iptr = nxt
      iwidth = 5
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      ncol = integr
      iptr = nxt
      iwidth = 5
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      msrow = integr
      iptr = nxt
      iwidth = 5
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      nrow = integr
c
c  grid fill control
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      ngfil = integr
c
c  check decreasing FFG
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      ickval = integr
c
c  water supply runoff
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      iwats = integr
      if (iwats.gt.0) then
c     rainfall
         read (inp,'(a)',end=20,err=40) line
         nxt = 1
         do 90 i=1,6
            iptr = nxt
            iwidth = 4
            call uffir (line,iptr,iwidth,integr,real,nxt,itype)
            rnfl(i) = real
90          continue
         endif
c
      istat = 1
c
      go to 130
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  FFG product files, comms system, FFG physical elements, year format,
c  append name and duty forecaster
100   read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      ising = integr
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      icom = integr
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      iffpe = integr
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      icent = integr
      read (inp,'(a)',end=20,err=40) line
      nxt = 1
      iptr = 1
      iwidth = 2
      call uffir (line,iptr,iwidth,integr,real,nxt,itype)
      igduty = integr
c
      if (igduty.gt.0) then
c     duty forecasters
         if (mdf.gt.0) then
            nperg=16
            ix1=1
            ix2=nperg
            nxt = 1
            do 120 i=1,mdf
               read (inp,'(a)',end=20,err=40) line
               nxt = 1
               iptr = nxt
               iwidth = len(strng)
               call uffch (line,iptr,iwidth,strng,nxt,istat)
               call umemov (strng,fcstr(ix1),nperg)
               if (ibug.eq.1) write (iud,110) (fcstr(j),j=ix1,ix2)
110   format (' in pininf - (fcstr(i),i=ix1,ix2)=',16a4)
               ix1=ix1+nperg
               ix2=ix2+nperg
120            continue
            endif
         endif
c
      istat = 1
c
130   return
c
      end
