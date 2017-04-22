c  =====================================================================
c  pgm:  mpgprm (stype,mcidx,cidx,mpo,po,istat)
c
c   in: stype  .... code for selected data type
c   in: mcidx  .... maximum number of identifiers in array cidx
c  i/o: cidx   .... array of identifiers
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... completion status:
c                     -1 = selected data type not found
c                      0 = sucessful
c                     >1 = error
c  =====================================================================
c
      subroutine mpgprm (stype,mcidx,cidx,mpo,po,istat)
c
c.......................................................................
c
c  This routine defines and redefines product parameters.
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab            Jan 1992
c.......................................................................
c
      character cresp,resp,stype,tent
      character*2 bname
      character*4 dtype,cidx(2,1)
      character*4 namtyp
      character*4 accmode
      character*8 ident
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/mpgprm.f,v $
     . $',                                                             '
     .$Id: mpgprm.f,v 1.5 2004/01/30 17:57:31 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('mpgprm',1,1,ibug)
c
      istat = 0
c
      nper = 0
      istridx = 0
      bname = ' '
      mcidx2 = mcidx
c
c  check for valid data type
      if (stype.eq.'P'.or.stype.eq.'p') then
         mcidx2 = 200
         dtype = 'prod'
         else if (stype.eq.'T'.or.stype.eq.'t') then
            mcidx2 = 200
            dtype = 'text'
         else if (stype.eq.'G'.or.stype.eq.'g') then
            mcidx2 = 100
            dtype = 'grpp'
         else if (stype.eq.'U'.or.stype.eq.'u') then
            dtype = 'uinf'
            tent = 'f'
            go to 100
         else
            write (iutw,10) stype
10    format (' ERROR: ',a,' is an invalid option.')
            istat = -1
            go to 330
         endif
c
c  get index
      call getidx (dtype,iunitx,mcidx2,po,ncidx,cidx,istat)
c
20    if (dtype.eq.'prod') then
         write (iutw,30)
30    format (/ 26x,'Product Identifiers' /)
         else if (dtype.eq.'text') then
            write (iutw,40)
40    format (/ 37x,'Text Identifiers' /)
         else if (dtype.eq.'grpp') then
            write (iutw,50)
50    format (/ 27x,'Product Group Identifiers' /)
         else
            go to 320
         endif
c
c  display identifiers
      ifirst = -1
ccc      nper = ncidx
      if (nper.eq.0) nper = 99
      nwds = 2
60    call dispid (resp,ifirst,nper,nwds,ncidx,cidx)
      if (resp.eq.' ') go to 320
c
c.......................................................................
c
c  add or change parameters
c
      if (resp.eq.'A'.or.resp.eq.'a') then
70       write (iutw,80)
80    format (' Add at (number): ',$)
         read (iutr,90,end=320,err=70) num
90       format (i4)
         i = ncidx + 1
         if (num.lt.0.or.num.gt.i) go to 20
         else if (resp.eq.'C'.or.resp.eq.'c') then
            num = 0
         else
            go to 180
         endif
c
      call typent (tent)
c
      if (tent.eq.'f'.or.tent.eq.'m') go to 100
c
c  use editor
      is = 2
      if (dtype.eq.'prod') then
         call edprod (ident,ipdv,is,mpo,po,istat)
         else if (dtype.eq.'text') then
            call edtext (ident,ipdv,is,mpo,po,istat)
         else if (dtype.eq.'grpp') then
            call edgrpp (ident,ipdv,mpo,po,istat)
         endif
      if (istat.ne.0) go to 20
      jed = 1
      go to 160
c
100   if (tent.ne.'f') go to 320
c
c  read parameters from ASCII file
110   write (iutw,120)
120   format (' Enter pathname or filename: ',$)
      read (iutr,'(a)',end=320,err=110) pthnam
      if (pthnam.eq.' ') go to 320
      jed = 0
      inp = iuu
      namtyp='anam'
      accmode = 'r'
      kod = 1
      call fixopn (ident,namtyp,pthnam,accmode,kod,inp,istat)
      if (istat.ne.0) then
         write (iutw,130) pthnam(1:lenstr(pthnam))
130      format (' ERROR: file ',a,' not found.')
         go to 110
         endif
      write (iutw,140) pthnam(1:lenstr(pthnam))
      if (iupr.ne.iutw) write (iupr,140) pthnam(1:lenstr(pthnam))
140   format (/ ' NOTE: reading input from file ',a,'.')
150   if (dtype.eq.'prod') then
         call pinprd (ident,istat)
         else if (dtype.eq.'grpp') then
            call pingrp (ident,istat)
         else if (dtype.eq.'text') then
            call pintxt (ident,istat)
         else if (dtype.eq.'uinf') then
            icaller = 2
            call pininf (icaller,pthnam,istat)
         endif
      if (istat.ne.0) then
         call upclos (inp,bname,istat)
         if (dtype.eq.'uinf') then
            istat = 0
            go to 320
            endif
         go to 110
         endif
c
      if (dtype.eq.'uinf') go to 320
c
c  open file
      accmode = 'r'
      kod = 0
      call fixopn (ident,dtype,pthnam,accmode,kod,ipdv,istat)
      if (istat.ne.0) go to 320
c
c  put parameters in po array
160   if (dtype.eq.'prod') then
         call putprd (jed,iusep,po,istat)
         else if (dtype.eq.'text') then
            call puttxt (iusep,po,istat)
         else if (dtype.eq.'grpp') then
            call putgrp (jed,iusep,po,istat)
         endif
c
c  write to file
      call wppfil (ipdv,iusep,po,istat)
      if (istat.ne.0) call pstcod (istat,ident,dtype,ipdv)
      call upclos (ipdv,bname,istat)
c
c  add identifier to index
      call addidx (num,ident,cidx,mcidx2,ncidx,istat)
      if (istat.eq.0) then
         write (iutw,170) dtype,ident(1:lenstr(ident))
         if (iupr.ne.iutw) write (iupr,170) dtype,
     +      ident(1:lenstr(ident))
170   format (' NOTE: ',a,' parameters written for identifier ',a,'.')
         istridx = 1
         endif
c
      if (jed.eq.0) then
         if (num.gt.0) num = num + 1
         go to 150
         endif
      go to 20
c
c.......................................................................
c
180   if (resp.ne.'D'.and.resp.ne.'d') go to 300
c
c  delete parameters
c
190   write (iutw,200)
200   format (' Delete from number: ',$)
      read (iutr,210,end=320,err=190) nb
210   format (i4)
      if (nb.eq.0) go to 20
      if (nb.lt.1.or.nb.gt.ncidx) then
         write (iutw,220) nb
220   format (' ERROR: ',i2,' is an invalid number.')
         go to 190
         endif
230   write (iutw,240)
240   format (9x,' to number: ',$)
      read (iutr,210,end=320,err=230) ne
      if (ne.le.0) ne = nb
      if (ne.lt.nb.or.ne.gt.ncidx) then
         write (iutw,220) ne
         go to 230
         endif
250   write (iutw,260)
260   format (' Are you sure (y or n): ',$)
      read (iutr,'(a1)',end=320,err=250) cresp
      if (cresp.eq.'Y'.or.cresp.eq.'y') then
c     delete file of specified data type dtype
         do 290 num=nb,ne
            call umemov (cidx(1,num),ident,2)
            accmode = 'rw'
            kod = 0
            call fixopn (ident,dtype,pthnam,accmode,kod,kpdv,istat)
            call updele (kpdv,' ',iostat)
            if (iostat.eq.0) then
               write (iutw,270) dtype,ident(1:lenstr(ident))
270   format (' NOTE: ',a,' parameters deleted for identifier ',a,'.')
               go to 290
               else
                  write (iutw,280) dtype,ident(1:lenstr(ident)),iostat
280   format (' ERROR: ',a,' parameters not deleted for identifier ',a,
     +   '. iostat=',i2)
               endif
290         continue
c     compress index file
         call subidx (nb,ne,mcidx2,ncidx,cidx)
         istridx = 1
         if (ne.gt.nper) nper = ncidx
         endif
c
      go to 20
c
c.......................................................................
c
300   if (resp.eq.'L'.or.resp.eq.'l') then
c     list parameters
         call dupgpm (dtype,ncidx,cidx,mpo,po)
         go to 20
         endif
c
c.......................................................................
c
      write (iutw,310) resp
310   format (/ ' ERROR: ',a,' is an invalid option.')
      nwds = -1
      go to 60
c
320   if (istridx.gt.0) call stridx (iunitx,mcidx2,po,ncidx,cidx,dtype)
c
330   return
c
      end
