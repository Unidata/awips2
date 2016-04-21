c  =====================================================================
c  pgm:  gplang (po)
c
c   in: po     .... parameter array
c  =====================================================================
c
      subroutine gplang (po)
c
c.......................................................................
c
c  Generate single line of land slide guidance defined by parameter
c  data type code lang
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL 
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/wsparm'
      include 'ffg_inc/count'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/pfparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gplang.f,v $
     . $',                                                             '
     .$Id: gplang.f,v 1.2 2004/01/30 17:57:08 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gplang',1,1,ibug)
c
c  get parameters
ccc      call getlan (po)
c
      rconv = 17.374
      nrn   = 6
c
      if (ibug.gt.0) write (iud,10) wsup
10    format (' wsup(i)=',6f6.1)
c
c.......................................................................
c
c  check debug option to override values to test format
c
c  override all lang values to test format
      if (iabug.eq.5) then
         do 20 i=1,nrn
            wsup(i) = rconv*i
20          continue
         wsup(6) = rconv*8
         endif
c
c.......................................................................
c
c  output in SHEF format
      if (ndes.gt.0) then
         nda = 5
         ndb = 0
         else
            nda = 0
            ndb = 0
         endif
      call wrdotb (wsid,nrn,nrn,wsup,nwid,ndec,nda,sdesc,ndb,strnam)
      nwsup = nwsup + 1
c
      return
c
      end
