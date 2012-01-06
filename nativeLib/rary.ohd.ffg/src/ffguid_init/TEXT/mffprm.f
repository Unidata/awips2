c  =====================================================================
c  pgm:  mffprm (stype,istat)
c
c   in: stype  .... code for data type
c  out: istat  .... completion code:
c                     -1 = selected data type not found
c                      0 = sucessful
c                     >1 = error
c  =====================================================================
c
      subroutine mffprm (stype,istat)
c
c.......................................................................
c
c  This routine defines and redefines guidance parameters.
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab            Jan 1992
c.......................................................................
c
      character cresp,resp,stype,tent
      character*2 bname
      character*4 dtype,namtyp
      character*4 accmode
      character*8 ident
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gpo'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/mffprm.f,v $
     . $',                                                             '
     .$Id: mffprm.f,v 1.6 2004/01/30 17:50:00 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('mffprm',1,1,ibug)
c
      istat = 0
c
      nper = 0
      istridx = 0
      bname = ' '
c
c  check for valid data type
      if (stype.eq.'A'.or.stype.eq.'a') then
         dtype = 'affg'
         else if (stype.eq.'H'.or.stype.eq.'h') then
            dtype = 'hffg'
         else if (stype.eq.'R'.or.stype.eq.'r') then
            dtype = 'gdpm'
         else if (stype.eq.'L'.or.stype.eq.'l') then
ccc            dtype = 'lang'
            write (iutw,5)
5     format (' ERROR: cannot setup Landslide Guidance parameters.')
            istat = -1
            go to 290
         else if (stype.eq.'W'.or.stype.eq.'w') then
            dtype = 'wsup'
         else if (stype.eq.'U'.or.stype.eq.'u') then
            dtype = 'uinf'
            tent = 'f'
            go to 90
         else
            write (iutw,7) stype
7     format (' ERROR: ',a,' is an invalid option.')
            istat = -1
            go to 290
         endif
c
c  get index
      call getidx (dtype,iunitx,mcidx,po,ncidx,cidx,istat)
c
10    if (dtype.eq.'affg') then
         write (iutw,20)
20    format (/ 15x,'AREAS - ZONES/COUNTIES' /)
         else if (dtype.eq.'hffg') then
            write (iutw,30)
30    format (/ 15x,'HEADWATERS' /)
         else if (dtype.eq.'gdpm') then
            write (iutw,40)
40    format (/ 15x,'GRID PARAMETERS' /)
         else if (dtype.eq.'lang') then
            write (iutw,50)
50    format (/ 15x,'LANDSLIDE GUIDANCE' /)
         else if (dtype.eq.'wsup') then
            write (iutw,60)
60    format (/ 15x,'WATER SUPPLY LOCATIONS' /)
         else
            go to 290
         endif
c
c  display identifiers
      ifirst = -1
ccc      nper = ncidx
      if (nper.eq.0) nper = 99
      nwds = 2
63    call dispid (resp,ifirst,nper,nwds,ncidx,cidx)
      if (resp.eq.' ') go to 280
      nper = 99
c
c.......................................................................
c
c  add or change parameters
c
      if (resp.eq.'A'.or.resp.eq.'a') then
65       write (iutw,70)
70    format (' Add at number: ',$)
         read (iutr,'(i4)',err=65) num
         i = ncidx + 1
         if (num.eq.0) go to 10
         if (num.lt.0) go to 65
         if (num.gt.i) then
            write (iutw,75) num,i
75    format (' ERROR: specified number (',i2,') is greater than ',i2,
     +   '.')
            go to 65
            endif
         else if (resp.eq.'C'.or.resp.eq.'c') then
            num = 0
         else
            go to 150
         endif
c
      call typent (tent)
c
      if (tent.eq.'f'.or.tent.eq.'m') go to 90
c
c  use editor
      is = 2
      if (dtype.eq.'affg') then
         call edarea (ident,ipdv,is,mpo,po,istat)
         else if (dtype.eq.'gdpm') then
            call edgdpm (ident,ipdv,is,mpo,po,istat)
         else if (dtype.eq.'hffg') then
            call edhed (ident,ipdv,is,mpo,po,istat)
ccc         else if (dtype.eq.'lang') then
ccc            call edlang (ident,ipdv,is,mpo,po,istat)
         else if (dtype.eq.'wsup') then
            call edsup (ident,ipdv,is,mpo,po,istat)
         endif
      if (istat.ne.0) go to 10
      ieditor = 1
      go to 130
c
90    if (tent.ne.'f') go to 280

c  get name of file
95    write (iutw,100)
100   format (' Enter pathname or filename: ',$)
      read (iutr,'(a)',end=280,err=95) pthnam
      if (ibug.eq.1) write (iutw,*) 'in mffprm - pthnam=',pthnam
      if (pthnam.eq.' ') go to 280
c
      ieditor = 0
      inp = iuu
c
      ident=' '
      namtyp='anam'
      accmode = 'r'
      kod = 1
      call fixopn (ident,namtyp,pthnam,accmode,kod,inp,istat)
      if (istat.ne.0) then
         write (iutw,110) pthnam(1:lenstr(pthnam))
110   format (' ERROR: file ',a,' not found or cannot be opened.')
         go to 95
         endif
      write (iutw,115) pthnam(1:lenstr(pthnam))
      if (iupr.ne.iutw) write (iupr,115) pthnam(1:lenstr(pthnam))
115   format (/ ' NOTE: reading input from file ',a,'.')
c
c  read parameters from ASCII file
120   if (dtype.eq.'affg') then
         call pinare (ident,istat)
         else if (dtype.eq.'gdpm') then
            call pingpm (ident,istat)
         else if (dtype.eq.'hffg') then
            call pinhed (ident,istat)
ccc         else if (dtype.eq.'lang') then
ccc            call pinlan (ident,istat)
         else if (dtype.eq.'wsup') then
            call pinsup (ident,istat)
         else if (dtype.eq.'uinf') then
            icaller = 1
            call pininf (icaller,pthnam,istat)
         else
         endif
      if (istat.ne.0) then
         call upclos (inp,bname,istat)
         if (dtype.eq.'uinf') then
            istat = 0
            go to 280
            endif
         go to 95
         endif
c
      if (dtype.eq.'uinf') go to 280
c
c  open parameter file
      accmode = 'rw'
      kod = 0
      call fixopn (ident,dtype,pthnam,accmode,kod,ipdv,istat)
      if (istat.ne.0) go to 280
c
c  put parameters in po array
130   if (dtype.eq.'affg') then
         call putare (iusep,po,istat)
         else if (dtype.eq.'gdpm') then
            call putgpm (iusep,po,istat)
         else if (dtype.eq.'hffg') then
            call puthed (ieditor,iusep,po,istat)
ccc         else if (dtype.eq.'lang') then
ccc            call putlan (iusep,po,istat)
         else if (dtype.eq.'wsup') then
            call putsup (ieditor,iusep,po,istat)
         endif
c
c  write to file
      call wppfil (ipdv,iusep,po,istat)
      if (istat.ne.0) call pstcod (istat,ident,dtype,ipdv)
      call upclos (ipdv,bname,istat)
c
c  add identifier to index
      call addidx (num,ident,cidx,mcidx,ncidx,istat)
      if (istat.eq.0) then
         write (iutw,140) dtype,ident(1:lenstr(ident))
         if (iupr.ne.iutw) write (iupr,140) dtype,
     +      ident(1:lenstr(ident))
140   format (' NOTE: ',a,' parameters written for identifier ',a,'.')
         istridx = 1
         endif
      if (ibug.eq.1) write (iud,*) 'in mffprm: addidx istat=',istat,
     +   ' istridx=',istridx
c
      if (ieditor.eq.0) then
         if (num.gt.0) num = num + 1
         go to 120
         endif
c
      go to 10
c
c.......................................................................
c
150   if (resp.ne.'D'.and.resp.ne.'d') go to 270
c
c  delete parameters
c
160   write (iutw,170)
170   format (' Delete from number: ',$)
      read (iutr,180,err=160) nb
180   format (i4)
      if (nb.eq.0) go to 10
      if (nb.lt.1.or.nb.gt.ncidx) then
         write (iutw,190) nb
190   format (' ERROR: ',i2,' is an invalid number.')
         go to 160
         endif
200   write (iutw,210)
210   format (9x,' to number: ',$)
      read (iutr,180,err=200) ne
      if (ne.le.0) ne = nb
      if (ne.lt.nb.or.ne.gt.ncidx.or.ne.lt.nb) then
         write (iutw,190) ne
         go to 200
         endif
215   write (iutw,220)
220   format (' Are you sure (y or n): ',$)
      read (iutr,'(a1)',err=215) cresp
      if (cresp.eq.'Y'.or.cresp.eq.'y') then
c     delete file of specified data type dtype
         do 260 num=nb,ne
            call umemov (cidx(1,num),ident,2)
            accmode = 'rw'
            kod = 0
            call fixopn (ident,dtype,pthnam,accmode,kod,kpdv,istat)
            close (kpdv,status='delete',iostat=iostat,err=240)
            write (iutw,230) dtype,ident(1:lenstr(ident))
230   format (' NOTE: ',a,' parameters deleted for identifier ',a,'.')
            go to 260
240         write (iutw,250) dtype,ident(1:lenstr(ident)),iostat
250   format (' ERROR: ',a,' parameters not deleted for identifier ',a,
     +   '. iostat=',i2)
260         continue
c     compress index file
         call subidx (nb,ne,mcidx,ncidx,cidx)
         istridx = 1
         if (ne.gt.nper) nper = ncidx
         endif
c
      go to 10
c
c.......................................................................
c
270   if (resp.eq.'L'.or.resp.eq.'l') then
c     list parameters
         call duffpm (dtype,ncidx,cidx,mpo,po)
         go to 10
         endif
c
c.......................................................................
c
      write (iutw,275) resp
275   format (/ ' ERROR: ',a,' is an invalid option.')
      nwds = -1
      go to 63
c
280   if (istridx.eq.1) call stridx (iunitx,mcidx,po,ncidx,cidx,dtype)
c
290   return
c
      end
