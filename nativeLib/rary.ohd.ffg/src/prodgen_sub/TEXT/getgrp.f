c=======================================================================
      subroutine getgrp (po)
c.......................................................................
c  This routine fills grppar with parameters from PO array
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 1992
c.......................................................................
c
      character*4 tempid(2)
      character*8 sname
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
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/getgrp.f,v $
     . $',                                                             '
     .$Id: getgrp.f,v 1.2 2004/09/13 14:23:43 scv Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'getgrp  ' /
c
      call prbug (sname,1,1,ibug)
c
c  File version
      gvers = po(1)
c
c  ID
      call umemov (po(2),grpid,2)
c
c  Type
      call umemov (po(4),grptyp,1)
c
c  Number of words
      iuseg = po(5) + 0.01
c
c  Location in PO array of Number of PROD identifiers
      locno = po(6) + 0.01
c
c  Number of PROD identifers
      nopid = po(locno) + 0.01
c
c  Location in PO array of Product identifiers
      locid = locno + 1
c
c  Put product identifiers in their working array
      do 1030 i=1,nopid
      k = (I-1)*2 + locid
      call umemov (po(k),tempid,2)
      apid(1,i) = tempid(1)
 1030 apid(2,i) = tempid(2)
c
      return
      end
