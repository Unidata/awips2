c=======================================================================
c
      subroutine dugrpp (po,iout)
c
c.......................................................................
c
c  This routine dumps parameters that define Groups of Products
c  (data type GRPP) to a file in the same format that can be used
c  to define Groups of Products.
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL - Aug 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/grppar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/dugrpp.f,v $
     . $',                                                             '
     .$Id: dugrpp.f,v 1.2 2004/01/30 17:55:43 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dugrpp',1,1,ibug)
c
      call getgrp (po)
c
      write (iout,10) grptyp,grpid
10    format (a4,1x,2a4)
c
c  add 'ENDID' as last identifier
      k = nopid + 1
      apid(1,k) = 'ENDI'
      apid(2,k) = 'D   '
c
      write (iout,20) ((apid(j,i),j=1,2),i=1,k)
20    format ((4x,7(1x,2a4)))
c
      return
c
      end
