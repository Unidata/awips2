c  =====================================================================
c
      subroutine gparea (po)
c
c  =====================================================================
c
c  Generate single line for product for an area of record type AFFG
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*4 crnam(5)
c
      dimension po(*)
      dimension val(5)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'ffg_inc/arparm'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/pfparm'
      include 'prodgen_inc/poptns'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gparea.f,v $
     . $',                                                             '
     .$Id: gparea.f,v 1.4 2004/01/30 17:56:20 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gparea',1,1,ibug)
c
c  fill common arparm with parameters for area
      call getar (po)
c
c  determine number of durations
      idur = ndur
      if (nadur.lt.idur) idur = nadur
c
      if (ibug.gt.0) write (iud,10) ndur,nadur,idur,(affg(i),i=1,idur)
10    format (' ndur=',i2,' nadur=',i2,' idur=',i2,' affg(i)=',5f6.2)
c
c.......................................................................
c
c  check debug option to override values to test format
c
c  override all ffg values to test format
      if (iabug.eq.5) then
         affg(1) = 1.1
         affg(2) = 1.5
         affg(3) = 2.0
         affg(4) = 2.4
         affg(5) = 3.0
         else if (iabug.eq.6) then
            do 20 i=1,5
               affg(i) = aro(i)
20             continue
         endif
c
c.......................................................................
c
c  check if to created product for value
      nval = 0
      do 30 i=1,idur
         if (pming(i).ge.0.0) then
            nval = nval + 1
            val(nval) = affg(i)
            endif
30       continue
c
c  check FFG values
      if (nval.gt.1) then
         do 50 i=1,nval-1
            val1=val(i)
            if (val1.lt.0) then
               write (iutw,35) idurt(i),val1,areaid
35    format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.1,
     +   ') is less than zero for area ',2a4,'.')
               if (iupr.ne.iutw) write (iupr,35) idurt(i),val1,areaid
               go to 50
               endif
            val2=val(i+1) 
            if (val2.gt.0) then
               if (val2.lt.val1) then
                  write (iutw,40) idurt(i+1),val2,
     +               idurt(i),val1,areaid
                  if (iupr.ne.iutw) write (iupr,40) idurt(i+1),val2,
     +               idurt(i),val1,areaid
40    format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than value for ',
     +   i2,' hour duration (',f6.3,
     +   ') for area ',2a4,$)
                  if (i+2.gt.nval) then
                     write (iutw,45)
                     if (iupr.ne.iutw) write (iupr,45)
45    format ('.')
                     else
                        val3=val(i+2) 
                        valnew=(val3+val1)/2.0
                        write (iutw,47) valnew
                        if (iupr.ne.iutw) write (iupr,47) valnew
47    format (' and will be set to ',f4.1,'.')
                        val(i+1)=valnew
                     endif
                  endif
               endif
50          continue
         endif
c
c  output in SHEF format
      if (ndes.gt.0.or.nstr.gt.0) then
         nda = 5
         if (nstr.gt.0) ndb = 5
         else
            nda = 0
            ndb = 0
         endif
      mdur = ndur
      if (nval.lt.3) mdur = nval
      crnam(1)= ' '
      call wrdotb (areaid,mdur,nval,val,nwid,ndec,nda,adesc,ndb,crnam)
      narea = narea + 1
c
      return
c
      end
