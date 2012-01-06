c  =====================================================================
c  pgm:  edsup (ident,ipdv,is,mpo,po,istat)
c
c  out: ident  .... identifier
c   in: ipdv   .... device number of parameter file
c   in: is     .... edit mode
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine edsup (ident,ipdv,is,mpo,po,istat)
c
c.......................................................................
c
c  editor routine for water supply definition
c
c.......................................................................
c    Initially written by
c       Tim Sweeney, HRL                               Sept 1995
c.......................................................................
c
      character resp
      character*4 blnk,cnone
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/wsparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/edsup.f,v $
     . $',                                                             '
     .$Id: edsup.f,v 1.5 2004/09/13 14:23:33 scv Exp $
     . $' /
C    ===================================================================
C
      data blnk/'    '/
      data cnone/'none'/
c
c
      call prbug ('edsup',1,1,ibug)
c
      mx = 16
      wstyp = 'wsup'
c
10    write (iutw,20)
20    format (' Enter water supply identifier: ',$)
      read (iutr,'(a)',err=10) ident
c
c  open an wsup file
      kod = 2
      call rppfil (ident,wstyp,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         call getsup (po)
         call umemov (ident,wsid,2)
      else if (istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,wsid,2)
         do 40 i=1,5
            sdesc(i) = cnone
40          continue
         sarea = 0.
         wsbid(1) = blnk
         wsbid(2) = blnk
         do 50 i=1,5
            wsup(i) = -999.
50          continue
         lwcpd = -1
         nsars = 1
         swt(1) = 0.
         sarid(1,1) = cnone
         sarid(2,1) = blnk
      else
         go to 120
      endif
c
60    write (iutw,70) wsid
70    format (24x,'Edit Water Supply Parameters - ',2a4 /)
      write (iutw,80) sdesc,sarea,wsbid
c
80    format (5x,'( 1) Description:  ',5a4 /
     +        5x,'( 2) Area (sq mi): ',f6.0 /
     +        5x,'( 3) Boundary ID:  ',2a4 /)
c
      num = 4
      n = nsars
      if (n.gt.3) n = 3
      write (iutw,90) num,nsars,(swt(i),sarid(1,i),sarid(2,i),i=1,n)
90    format (5x,'(',i2,') Number of weighted areas: ',i2 /
     +        8x,   3( 2x, f4.2,2x,2a4) )
      write (iutw,100)
100   format (/' Select (number or <return>-exit): ',$)
c
      read (iutr,110,err=60) resp
110   format (a1)
c
      if (resp.eq.' ') then
         go to 120
c
      else if (resp.eq.'1') then
         call edvca (is,iutr,iutw,5,sdesc)
      else if (resp.eq.'2') then
         call edvra (is,iutr,iutw,sarea,1)
      else if (resp.eq.'3') then
         call edvca (is,iutr,iutw,2,wsbid)
      else if (resp.eq.'4') then
         call edwtid (is,iutr,iutw,swt,sarid,mx,nsars)
      else
         go to 120
      endif
      go to 60
c
120   return
c
      end
