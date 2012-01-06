c  =====================================================================
c  pgm: puttxt (iusep,po,istat)
c
c  out: iusep    .... number of words used to define parameters in po
c  out: po       .... parameter array
c  i/o: istat    .... completion status
c  =====================================================================
c
      subroutine puttxt (iusep,po,istat)
c
c.......................................................................
c
c  puts data type 'TEXT' parameters in array po.
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab             Jan 1992
c.......................................................................
c
      character*4 etxt(2),letxt(2)
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
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/puttxt.f,v $
     . $',                                                             '
     .$Id: puttxt.f,v 1.2 2004/01/30 17:58:33 scv Exp $
     . $' /
C    ===================================================================
C
      data etxt / 'ENDT','EXT '/
      data letxt/ 'endt','ext '/
c
c
      call prbug ('puttxt',1,1,ibug)
c
      txtyp = 'text'
c
      if (istat.gt.0) go to 30
c
c  version number
      tvers = 1.0
      po(1) = tvers
c
c  identifier
      call umemov (txtid,po(2),2)
c
c  parameter type code
      call umemov (txtyp,po(4),1)
c
c  location in PO array of number of lines of text
      locno = 10
      po(6) = locno
c
c  unused
      do 20 i=1,3
         po(i+6) = -999.0
20       continue
c
c  number of lines of text
      po(locno) = nlines
c
c  text lines
      loctxt = locno + 1
      nw = nlines*18
      call umemov (atext,po(loctxt),nw)
c
c  number of words for parameters
      iusep = loctxt + nw - 1
      po(5) = iusep + 0.01
      if (ibug.gt.0) write (iud,*) 'iusep=',iusep
      iuset = iusep
c
30    return
c
      end
