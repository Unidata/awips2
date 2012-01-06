c  =====================================================================
c  pgm:  gpsup (po)
c
c   in: po     .... parameter array
c  =====================================================================
c
      subroutine gpsup (po)
c
c.......................................................................
c
c  Generate water supply guidance for parameter data type code wsup
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL                            Sept 1995
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'ffg_inc/wsparm'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/pfparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpsup.f,v $
     . $',                                                             '
     .$Id: gpsup.f,v 1.3 2004/01/30 17:57:19 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gpsup',1,1,ibug)
c
      call getsup (po)
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
c  override all wsup values to test format
      if (iabug.eq.5.or.iabug.eq.6) then
         do 20 i=1,nrn
            wsup(i) = rconv*i
20          continue
         wsup(6) = rconv*8
         endif
c
c.......................................................................
c
c  check FFG values
      do 50 i=1,nrn-1
         val1=wsup(i)
         if (val1.lt.0) then
            write (iutw,30) idurt(i),val1,wsid
30    format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.1,
     +   ') is less than zero for Water Supply area ',2a4,'.')
            if (iupr.ne.iutw) write (iupr,30) idurt(i),val1,wsid
            go to 50
            endif
         val2=wsup(i+1)
         if (val2.gt.0) then
            if (val2.lt.val1) then
               write (iutw,40) idurt(i+1),val2,
     +            idurt(i),val1,wsid
               if (iupr.ne.iutw) write (iupr,40) idurt(i+1),val2,
     +            idurt(i),val1,wsid
40    format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than value for ',
     +   i2,' hour duration (',f6.3,
     +   ') for Water Supply area ',2a4,'.')
               endif
            endif
50       continue
c
c  write to output file
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
