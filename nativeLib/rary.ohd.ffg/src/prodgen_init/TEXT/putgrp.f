c  =====================================================================
c  pgm:  putgrp (jed,iusep,po,ic)
c
c   in: jed      .... editor control
c  out: iusep    .... number of words used to store parameters in po
c  out: po       .... parameter array
c  i/o: istat    .... status code
c  =====================================================================
c
      subroutine putgrp (jed,iusep,po,istat)
c
c.......................................................................
c
c  puts data type 'grpp' in parameter array po
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab             Jan 1992
c.......................................................................
c
      character*4 cend(2),lcend(2)
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
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/putgrp.f,v $
     . $',                                                             '
     .$Id: putgrp.f,v 1.2 2004/01/30 17:58:18 scv Exp $
     . $' /
C    ===================================================================
C
      data cend/ 'ENDI','D   '/
      data lcend/ 'endi','d   '/
c
c
      call prbug ('putgrp',1,1,ibug)
c
      istat = 0
c
      grptyp = 'grpp'
c
c  version number
      gvers = 1.0
      po(1) = gvers
c
c  identifier
      call umemov (grpid,po(2),2)
c
c  parameter type code
      call umemov (grptyp,po(4),1)
c
c  location in PO array of number of PROD identifers (NOID)
      locno = 12
      po(6) = locno
c
c  unused
      do 20 i=1,5
         po(i+6) = -999.0
20       continue
c
c  product contents identifiers
      locid = locno + 1
      if (jed.eq.0) then
         k = 60
         else
            k = nopid
         endif
      nopid = 0
c
c  identifiers
      do 40 i=1,k
         if (apid(1,i).eq.' ') go to 40
         if (jed.eq.1) go to 30
         if (apid(1,I).ne.cend(1).and.apid(1,I).ne.lcend(1)) go to 30
         if (apid(2,I).eq.cend(2).or.apid(2,I).eq.lcend(2)) go to 50
30       nopid = nopid + 1
         k = (nopid-1)*2 + locid
         call umemov (apid(1,i),po(k),2)
40       continue
c
c  number of location identifiers
50    po(locno) = float(nopid)
c
c  number of words for parameters
      iusep = locid + nopid*2 - 1
      po(5) = iusep + 0.01
      if (ibug.gt.0) write (iud,*) 'iusep=',iusep
      iuseg = iusep
c
      return
c
      end
