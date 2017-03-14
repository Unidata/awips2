c=======================================================================
c
      subroutine edgrpp (ident,kpdv,mpo,po,istat)
c
c.......................................................................
c
c  Editor routine for group of products definitions
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character resp
      character*4 blnk,cnone
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/grppar'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/edgrpp.f,v $
     . $',                                                             '
     .$Id: edgrpp.f,v 1.4 2004/09/13 15:03:20 scv Exp $
     . $' /
C    ===================================================================
C
      data blnk/ '    ' /
      data cnone/ 'none' /
c
c
      call prbug ('edgrpp',1,1,ibug)
c
10    write (iutw,20)
20    format (' Enter Product Group identifier: ',$)
      read (iutr,'(a)',err=10) ident
c
c  Open a Product Group (GRPP) file, get parameters from existing
c  group definition
      kod = 2
      call rppfil (ident,grptyp,kod,kpdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (kpdv)
         call getgrp (po)
         call umemov (ident,grpid,2)
      else if (istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,grpid,2)
         nopid = 1
         apid(1,1) = cnone
         apid(2,1) = blnk
      else
         go to 50
      endif
      ib = 1
      idel = 99
      igrp = 2
c
30    write (iutw,40) grpid
40    format (17x,'Edit Product Identifiers for Group - ',2a4 /)
c
      call dispid (resp,ib,idel,igrp,nopid,apid)
c
      if (resp.eq.' ') go to 50
c
      call edvid (iutr,iutw,resp,ib,igrp,nopid,apid)
      go to 30
c
50    return
c
      end
