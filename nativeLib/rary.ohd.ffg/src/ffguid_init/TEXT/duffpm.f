c  =====================================================================
c  pgm:  duffpm (dtype,nids,sdx,mpo,po)
c
c   in: dtype  .... data type code
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c   in: nids   .... number of 8-character identifiers in array sdx
c   in: sdx    .... array of 8-character identifiers
c  =====================================================================
c
      subroutine duffpm (dtype,nids,sdx,mpo,po)
c
c.......................................................................
c
c  this routine dumps parameters to a file in the same format that
c  can be used to define the parameters.
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                            Sept 1995
c.......................................................................
c
      character*2 bname
      character*4 dtype,namtyp,sdx(2,1)
      character*4 accmode
      character*8 ident
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/duffpm.f,v $
     . $',                                                             '
     .$Id: duffpm.f,v 1.7 2004/01/30 17:46:50 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('duffpm',1,1,ibug)
c
      bname = '  '
c
10    write (iutw,20)
20    format (' Enter output file name (<return>-exit): ',$)
      read (iutr,'(a)',err=10) pthnam
      if (pthnam.eq.' ') go to 120
      lpthnam=lenstr(pthnam)
c
c  open outfile for parameters
      iout = iuu
      namtyp = 'anam'
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
      if (nids.eq.1.and.sdx(1,1).eq.' '.or.sdx(1,1).eq.'none') then
         write (iutw,85) dtype
         if (iupr.ne.iutw) write (iupr,85) dtype
85    format (' NOTE: no ',a,' parameters are defined.')
         call updele (iout,pthnam(1:lpthnam),istat)
         go to 10
         endif
c
c  process each number
      do 100 ia=nb,ne
         if (sdx(1,ia).eq.' '.or.sdx(1,ia).eq.'none') go to 100
         call umemov (sdx(1,ia),ident,2)
         if (ibug.gt.0) write (iud,*) 'ident=',ident
c     open file
         kod = 1
         call rppfil (ident,dtype,kod,ipdv,mpo,po,npo,istat)
         if (istat.eq.0) then
            call upclos (ipdv,bname,ic)
            else
               go to 100
            endif
         if (dtype.eq.'affg') then
            call duarea (po,iout)
         else if (dtype.eq.'gdpm') then
            call dugdpm (po,iout)
         else if (dtype.eq.'hffg') then
            call duhed (po,iout)
         else if (dtype.eq.'wsup') then
            call dusup (po,iout)
         endif
         write (iutw,90) ia,ident
90    format (' NOTE: identifier ',i3,' (',a,') processed.')
100      continue
c
105   call upclos (iout,bname,ic)
      write (iutw,110) pthnam(1:lenstr(pthnam))
110   format (' NOTE: output written to file ',a,'.')
ccc      go to 10
c
120   return
c
      end
