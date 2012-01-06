c  =====================================================================
c  pgm:  dupgpm (dtype,nids,sdx,mpo,po)
c
c   in: dtype  .... data type code
c   in: nids   .... number of 8-character identifiers in array sdx
c   in: sdx    .... aarray of 8-character identifiers
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  =====================================================================
c
      subroutine dupgpm (dtype,nids,sdx,mpo,po)
c
c.......................................................................
c
c  This routine dumps parameters to a file in the same format that
c  can be used to define the parameters.
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                            Sept 1995
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
c
      character*2 bname
      character*4 dtype,sdx(2,1)
      character*4 namtyp
      character*4 accmode
      character*8 ident
      character*128 pthnam
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/dupgpm.f,v $
     . $',                                                             '
     .$Id: dupgpm.f,v 1.4 2004/01/30 17:55:49 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dupgpm',1,1,ibug)
c
      bname = ' '
c
10    write (iutw,20)
20    format (' Enter output file name (<return>-exit): ',$)
      read (iutr,'(a)',err=10) pthnam
      if (pthnam.eq.' ') go to 120
      lpthnam=lenstr(pthnam)
c
c  open output file
      iout = iuu
      namtyp='anam'
      accmode = 'rw'
      kod = 0
      call fixopn (ident,namtyp,pthnam,accmode,kod,iout,istat)
      if (istat.gt.1) go to 10
c
      if (dtype.eq.'uinf') then
         call duuinf (po,iout)
         go to 105
         endif
c
c  get range of numbers to list
      ib = 1
      ie = nids
30    write (iutw,40) ib
40    format (' List from (',i3,'): ',$)
      read (iutr,50,err=30) nb
50    format (i4)
      if (nb.eq.0) nb=ib
      if (nb.lt.1.or.nb.gt.ie) then
         write (iutw,60) nb
60    format (' ERROR: ',i2,' is an invalid number.')
         go to 30
         endif
70    write (iutw,80) ie
80    format (8x,'to (',i3,'): ',$)
      read (iutr,50,err=70) ne
      if (ne.eq.0) ne=ie
      if (ne.lt.1.or.ne.gt.ie.or.ne.lt.nb) then
         write (iutw,60) ne
         go to 70
         endif
c
c  process each number
      do 100 ia=nb,ne
         if (sdx(1,ia).eq.' '.or.sdx(1,ia).eq.'none') go to 100
c     open typ file
         call umemov (sdx(1,ia),ident,2)
         if (ibug.eq.1) write (iud,*) 'ident=',ident
c     open dtype file
         kod = 1
         call rppfil (ident,dtype,kod,ipdv,mpo,po,npo,istat)
         if (istat.eq.0) then
            call upclos (ipdv,bname,ic)
            else
               go to 100
            endif
         if (dtype.eq.'grpp') then
            call dugrpp (po,iout)
            else if (dtype.eq.'prod') then
               call duprod (po,iout)
            else if (dtype.eq.'text') then
               call dutext (po,iout)
            endif
         write (iutw,90) ia,ident
90       format (' NOTE: identifier ',i3,' (',a,') processed.')
100      continue
c
105   call upclos (iout,bname,ic)
      write (iutw,110) pthnam(1:lpthnam)
110   format (' NOTE: output written to file ',a,'.')
      go to 10
c
120   return
c
      end
