c=======================================================================
      subroutine gpgrpp(po)
c***********************************************************************
c  Generate several products as a group.  Identifiers of individual
c  products are defined by parameter code type GRPP
c
c***********************************************************************
c       Initially written by
c             Tim Sweeney, HRL - Apr 1992
c***********************************************************************
      character*8 sname
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'prodgen_inc/grppar'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpgrpp.f,v $
     . $',                                                             '
     .$Id: gpgrpp.f,v 1.1 2001/08/16 17:43:04 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/'gpgrpp  '/
c
      call prbug (sname,1,1,ibug)
c
c  Fill 'common\grppar'
      call getgrp (po)
c
      if(ibug.gt.0) then
         write(iud,7060) nopid,((apid(j,i),j=1,2),i=1,nopid)
      end if
c
      return
c
c  Debug formats
 7060 format(5x,'nopid=',i3,  10 (/,6x,7(2a4,2x) ) )
      end
