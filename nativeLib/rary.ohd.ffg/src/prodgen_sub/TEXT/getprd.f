c  =====================================================================
c  pgm:  getprd (po)
c
c   in: po     .... array containing parameters for product
c  =====================================================================
      subroutine getprd (po)
c.......................................................................
c  This routine fills common block propar with type prod parameters
c  from the po array.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL - Oct 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/getprd.f,v $
     . $',                                                             '
     .$Id: getprd.f,v 1.2 2004/01/30 17:56:08 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getprd',1,1,ibug)
c
c  version number
      pvers = po(1)
c
c  identifier
      call umemov (po(2),prodid,2)
c
c  type
      call umemov (po(4),protyp,1)
c
c  number of words used
      iusep = po(5) + 0.01
c
c  product format
      call umemov (po(6),pform,1)
c
c  product ID on comms circuit
      call umemov (po(7),pid,3)
c
c  comms circuit distribution
      call umemov (po(10),cct,1)
c
c  time zone
      call umemov (po(11),tzone,1)
c
c  WMO identifier
      call umemov (po(12),wmo,2)
c
c  sending office
      call umemov (po(14),senof,1)
c
c  default location ID type code
      call umemov (po(15),dftyp,1)
c
c  duration flag
      jdur = po(16)
c
c  parameters for local grid defintion
      if (dftyp.ne.'GFFG'.and.dftyp.ne.'gffg'.and.
     +    dftyp.ne.'SIII'.and.dftyp.ne.'siii') go to 10
c
c  pack bulletins for all durations
      jdes = po(17)
c
c  version adjust - remove later
      k = 0
      if (pvers.lt.1.1) then
         k = 1
         jdes = 0
         endif

c  local western most HRAP column
      lwcol = po(18-k)
c  local number of columns
      lncol = po(19-k)
c  local southern most HRAP row
      lsrow = po(20-k)
c  local number of rows
      lnrow = po(21-k)
c
      jstr = 0
c
c  generate a fictitious single location ID to satisfy program logic
c  for grid output
      nolid = 1
      alidty(1,1) = dftyp
      alidty(2,1) = dftyp
      alidty(3,1) = dftyp
      go to 30
c
c.......................................................................
c
c  parameters for all except grids using 'GFFG' or 'SIII'
c
c  description flag
10    jdes = po(17)
c
c  stream name flag
      jstr = po(18)
c
c  location in PO array of number of location identifiers
      locno = po(19)
c
c  number of location identifiers
      nolid = po(locno)
c
c  location of identifiers
      locid = locno + 1
c
c  put identifiers and types in work array
      do 20 i=1,nolid
         k = (i-1)*3 + locid
         call umemov (po(k),alidty(1,i),3)
20       continue
c
c.......................................................................
c
30    return
c
      end
