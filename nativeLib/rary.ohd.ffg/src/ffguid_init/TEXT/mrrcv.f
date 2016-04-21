c  =====================================================================
c  pgm:  mrrcv (rdx,mpo,po)
c
c   in: rdx    .... array of 8-character identifiers
c  out: mpo    .... maximum words in array po
c   in: po     .... rainfall-runoff curve parameters
c  =====================================================================
c
      subroutine mrrcv (rdx,mpo,po)
c
c.......................................................................
c
c  define parameters for rainfall-runoff curves
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character cresp,resp,tent
      character*2 bname
      character*4 ddesc(5),bbid(2),desc(5)
      character*4 dtype,rdx(2,1)
      character*4 accmode
      character*8 ffgid
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      dimension ldur(5),rr(40)
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/mrrcv.f,v $
     . $',                                                             '
     .$Id: mrrcv.f,v 1.6 2004/09/13 14:23:54 scv Exp $
     . $' /
C    ===================================================================
C
      data ddesc/'ente','r de','scri','ptio','n   '/
      data ldur/1,3,6,12,24/
      data bname/ '  ' /
c
c
      call prbug ('mrrcv',1,1,ibug)
c
      npo = 57
c
c  maximum number of ffg rainfall-runoff files for a
c  new index file (times 2), if needed
      mxdx = 4000
c
c  get index file for ffg rainfall-runoff files
      dtype = 'ffg'
      call getidx (dtype,idxdv,mxdx,po,mr,rdx,istat)
      if (istat.ne.0) then
         write (iutw,10) dtype(1:lenstr(dtype))
         if (iupr.ne.iutw) write (iupr,10) dtype(1:lenstr(dtype))
10    format (' ERROR: cannot open file type ',a,'.')
         go to 330
         endif
      ifirst = -1
c
20    write (iutw,30)
30    format (29x,'Rainfall Runoff Curves' /)
c
c  display ffg area ids
      call dispid (resp,ifirst,mr,rdx)
      if (resp.eq.' ') then
         go to 320
      else if (resp.eq.'A'.or.resp.eq.'a') then
         write (iutw,40)
40    format (' Add at number: ',$)
         read (iutr,50,err=20) num
50    format (i4)
         i = mr + 1
         if (num.lt.0.or.num.gt.i) go to 20
      else if (resp.eq.'C'.or.resp.eq.'c') then
         num = 0
         go to 60
      else
         go to 230
      endif
c
60    call typent (tent)
      jfun = 0
      if (tent.eq.'f') go to 190
      if (tent.eq.'m') go to 320
c
c  editor
      write (iutw,80)
80    format (' Enter FFG identifier: ',$)
      read (iutr,'(a)') ffgid
c
c  read the file
      kod = 2
      call rppfil (ffgid,dtype,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         call umemov (po(4),desc,5)
         call umemov (po(9),bbid,2)
         ndur = po(13) + 0.01
         do 100 i=1,40
            rr(i) = po(i+17)
100         continue
      else if (istat.eq.1) then
c     create new file - set default values
         istat = 0
         do 110 i=1,5
            desc(i) = ddesc(i)
110         continue
         kdur = 0
         do 120 i=1,40
            rr(i) = -99.0
120         continue
         else
            go to 20
         endif
c
130   write (iutw,140) desc,ndur
140   format (5x,'1 - Description:   ',5a4 /
     +        5x,'2 - Duration flag: ',i2 )
      call umemov (ffgid,bbid,2)
      ndur = kdur + 3
      do 150 i=1,ndur
         itmx = i + 2
         k = (i-1)*8
         write (iutw,160) itmx,ldur(i),(rr(k+l),l=1,8)
150      continue
160   format (5x,i1,' - ',i2,'-hour rainfall-runoff curve:' /
     +   12x,4(2x,2f6.2))
      write (iutw,170)
170   format (' Select: ',$)
c
      read (iutr,180) it
180   format (i4)
      isl = 2
      nperg = 2
      if (it.eq.1) then
         call edvca (isl,iutw,5,desc)
      else if (it.eq.2) then
         call edvi (isl,iutw,ndur)
      else if (it.gt.2.and.it.le.itmx) then
         locrr = (it-3)*8 + 1
         call edvrag (isl,iutr,iutw,rr(locrr),8,nperg,npr)
      else
         jfun = 1
         go to 210
      endif
      go to 130
c
c  ascii file input for add and change
190   write (iutw,200)
200   format (' ERROR: ASCII file input not found for rainfall ',
     +   'runoff curves.')
      go to 20

c  fill po array
210   rvers = 1.0
      po(1) = rvers
      call umemov (ffgid,po(2),2)
      call umemov (desc,po(4),5)
      call umemov (bbid,po(9),2)
      po(13) = kdur + 0.01
      do 220 i=1,40
         po(i+17) = rr(i)
220      continue
c
c  write to file
      call wppfil (ipdv,npo,po,istat)
      call pstcod (istat,ffgid,dtype,ipdv)
      call upclos (ipdv,bname,ic)
c
c  add identifier ffgid to index for ffg files
      call addidx (num,ffgid,rdx,mxdx,mr,istat)
c
      if (jfun.eq.0) then
         if (num.gt.0) num = num + 1
         go to 190
         endif
      go to 20
c
c  delete
230   if (resp.ne.'D'.and.resp.ne.'d') go to 300
      write (iutw,240)
240   format (' Delete rainfall runoff number: ',$)
      read (iutr,250) num
250   format (i4)
      if (num.lt.1.or.num.gt.mr) go to 20
      write (iutw,260)
260   format (' Identifier: ',$)
      read (iutr,'(a)') ffgid
      write (iutw,280)
280   format (' Are you sure (y or n): ',$)
      read (iutr,290) cresp
290   format (a1)
      if (cresp.eq.'Y'.or.cresp.eq.'y') then
c     delete r-r curve
         call umemov (rdx(1,num),ffgid,2)
         accmode = 'rw'
         kod = 0
         call fixopn (ffgid,dtype,bname,accmode,kod,ipdv,istat)
         close (ipdv,status='delete',iostat=iostat,err=292)
         write (iutw,291) ffgid(1:lenstr(ffgid))
291   format (' NOTE: rainfall-runoff curve deleted for ',
     +   'identifier ',a,'.')
c     compress index file
         call subidx (num,mxdx,mr,rdx)
         go to 20
292      write (iutw,293) ffgid(1:lenstr(ffgid)),iostat
293   format (' ERROR: rainfall-runoff curve not deleted for ',
     +   'identifier ',a,'. iostat=',i2)
         go to 20
         endif
c
c  list rainfall runoff curves
300   if (resp.ne.'L'.and.resp.ne.'l') go to 20
      write (iutw,310)
310   format (' Refer to NWSRFS to list rainfall runoff curves.' /
     +   ' Enter <return> to continue.')
      read (iutr,290) cresp
      go to 20
c
c  store index array for rainfall runoff curves
320   call stridx (idxdv,mxdx,po,mr,rdx,dtype)
c
330   return
c
      end
