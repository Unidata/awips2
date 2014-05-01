c  =====================================================================
c  pgm:  putprd (jed,iusepz,po,istat)
c
c   in: jed    .... editor control
c  out: iusepz .... number of words to store parameters in po
c  out: po     .... parameter array
c  i/o: istat  .... completion status
c  =====================================================================
c
      subroutine putprd (jed,iusepz,po,istat)
c
c.......................................................................
c
c  This routine puts data type PROD parameters in array po.
c
c.......................................................................
c  Initially written by
c     Timothy L. Swbeeney - Hydrologic Research Lab             Jan 1992
c.......................................................................
c
      character*4 cend(2),lcend(2)
c
      dimension po(*),ldur(3)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/putprd.f,v $
     . $',                                                             '
     .$Id: putprd.f,v 1.2 2004/01/30 17:58:26 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   '/
      data lcend/ 'endi','d   '/
      data ldur/6,12,24/
c
c
      call prbug ('putprd',1,1,ibug)
c
      protyp = 'prod'
c
      if (istat.gt.0) go to 120
c
c  version number
      pvers = 1.1
      po(1) = pvers
c
c  identifier
       call umemov (prodid,po(2),2)
c
c  parameter type code
      call umemov (protyp,po(4),1)
c
c  product type code
       call umemov (pform,po(6),1)
c
c  product id
      call umemov (pid,po(7),3)
c
c  circuit designators
      call umemov (cct,po(10),1)
c
c  time zone designator
      call umemov (tzone,po(11),1)
c
c  WMO header
      call umemov (wmo,po(12),2)
c
c  sending office
      call umemov (senof,po(14),1)
c
c  default type code for identifiers
      call umemov (dftyp,po(15),1)
c
c  duration flag
      do 20 i=1,3
         if (jdur.eq.ldur(i)) go to 30
20       continue
      jdur = 6
30    po(16) = jdur
c
c  parameters for grid product
      if (dftyp.ne.'GFFG'.and.dftyp.ne.'gffg'.and.
     +    dftyp.ne.'SIII'.and.dftyp.ne.'siii') go to 50
c
c  pack bulletins for all durations into single product
      po(17) = jdes
      po(18) = float(lwcol)
      po(19) = float(lncol)
      po(20) = float(lsrow)
      po(21) = float(lnrow)
      jstr = 0
c
c  unused
      do 40 i=1,11
         po(21+i) = -999.0
40       continue
c
      iusepz = 32
      go to 100
c
c  description flag
50    po(17) = jdes
c
c  stream name flag
      po(18) = jstr
c
c  location in PO array of number of identifiers
      locno = 23
      po(19) = locno
c
c  unused
      do 60 i=1,3
         po(i+19) = -999.0
60       continue
c
c  identifiers
      locid = locno + 1
      if (jed.eq.0) then
         k = 400
         else
            k = nolid
         endif
      nolid = 0
      DO 80 I=1,k
         if (alidty(1,i).eq.' ') go to 80
         if (jed.eq.1) go to 70
         if (alidty(1,I).ne.cend(1).and.
     +       alidty(1,I).ne.lcend(1)) go to 70
         if (alidty(2,I).eq.cend(2).or.alidty(2,I).eq.lcend(2)) go to 90
70       nolid = nolid + 1
         k = (nolid-1)*3 + locid
         call umemov (alidty(1,i),po(k),3)
80       continue
c
c  number of location identifiers
90    po(23) = float(nolid)
c
      iusepz = locid + nolid*3 - 1
c
c  set number of words for parameters
100   po(5) = iusepz + 0.01
      if (ibug.gt.0) write (iud,*) 'iusepz=',iusepz
      iusep = iusepz
c
120   return
c
      end
