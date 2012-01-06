c=======================================================================
c
      subroutine gettxt (po)
c
c.......................................................................
c
c  This routine fills txtpar with parameters from PO array
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL - Oct 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/txtpar'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gettxt.f,v $
     . $',                                                             '
     .$Id: gettxt.f,v 1.3 2004/09/13 15:03:32 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gettxt',1,1,ibug)
c
c  version number
      tvers = po(1)
c
c  identifier
      call umemov (po(2),txtid,2)
c
c  type
      call umemov (po(4),txtyp,1)
c
c  number of words
      iuset = po(5) + 0.01
c
c  location in PO array of number of lines of text
      locno = po(6) + 0.01
c
c  number of lines of text
      nlines = po(locno) + 0.01
      if (nlines.lt.1) go to 10
c
c  location in PO array of text
      loctxt = locno + 1
c
c  text
      nw = nlines*18
      call umemov (po(loctxt),atext,nw)
c
10    return
c
      end
