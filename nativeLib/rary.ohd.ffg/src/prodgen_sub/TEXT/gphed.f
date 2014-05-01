c  =====================================================================
c  pgm:  gphed (po)
c
c   in: po     .... parameter array
c  =====================================================================
c
      subroutine gphed (po)
c
c.......................................................................
c
c  Generate single line of headwater guidance for headwater defined
c  by parameter data type code HFFG
c
c.......................................................................
c          Initially written by
c              Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      dimension po(*)
      dimension val(5),jdurt(5)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/pfparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gphed.f,v $
     . $',                                                             '
     .$Id: gphed.f,v 1.6 2005/07/07 19:49:17 xfan Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gphed',1,1,ibug)
c
c  get parameters
      call gethed (po)
c
      if (ibug.eq.1) write (iud,5) hdid
5     format (' in gphed - hdid=',2a4)
c
c  set number of durations
      idur = ndur
      if (nhdur.lt.idur) idur = nhdur
      ndurg = 0
      do 7 i=1,idur
         if (ibug.eq.1) write (iud,*) 'i=',i,' pminh(i)=',pminh(i)
         if (pminh(i).ge.0.0) ndurg = ndurg + 1
7        continue
c
      if (ibug.eq.1) write (iud,10) ndur,nhdur,idur,
     +   ndurg,(hffg(i),i=1,ndurg)
10    format (' in gphed - ndur=',i2,' nhdur=',i2,' idur=',i2,
     +   ' ndurg=',i2,' hffg(i)=',5(f7.2,1x))
c
c.......................................................................
c
c  check debug option to override values to test format
c
c  override all ffg values to test format
      if (iabug.eq.5) then
         hffg(1) = 0.3
         hffg(2) = 0.5
         hffg(3) = 0.8
         hffg(4) = 1.5
         hffg(5) = 2.8
         endif
c
c  override all ffg values with threshold runoffs to test formats
      if (iabug.eq.6) then
         if (fsflow.gt.10.0) then
            do 20 i=1,idur
               if (upk(i).lt.0.01) goto 20
               hffg(i) = fsflow/upk(i)
20             continue
         else if (rcid(1).eq.' ') then
c        upk is runoff times 100
            do 30 i=1,idur
               if (upk(i).lt.0.01) goto 30
               hffg(i) = upk(i)/100.
30             continue
            else
c           when rating curve option used insert fixed ffg values
               hffg(1) = 0.4
               hffg(2) = 0.6
               hffg(3) = 0.9
               hffg(4) = 1.6
               hffg(5) = 2.9
            endif
         endif
c
c.......................................................................
c
      if (ibug.eq.1) write (iud,39) idur,(hffg(i),i=1,idur)
39    format (' in gphed - idur=',i2,' hffg(i)=',5(f7.2,1x))
c
c  check if to create product for value
      nval = 0
      do 40 i=1,idur
         if (ibug.eq.1) write (iud,*) 'i=',i,' pminh(i)=',pminh(i)
         if (pminh(i).ge.0.0) then
            nval = nval + 1
cfan        val(nval) = hffg(nval)
            val(nval) = hffg(i)          !cfan 05/2005  HSD bug r26-23
            jdurt(nval) = idurt(i)
            endif
40       continue
c
      nerr2 = 0
      do 37 i=1,nval
         if (val(i).eq.-999.0) then
            write (iutw,35) jdurt(i),hdid
            if (iupr.ne.iutw) write (iupr,35) jdurt(i),hdid
35    format (' ERROR: FFG value for ',i2,' hour duration ',
     +   'never computed for Headwater area ',2a4,'.')
            nerr = nerr + 1
            nerr2 = nerr2 + 1
            endif
37       continue
      if (nerr2.eq.nval) go to 100
c
      mdur = ndur
      if (nval.lt.3) mdur = nval
c
      if (ibug.eq.1) write (iud,45) nval,(val(i),i=1,nval)
45    format (' in gphed - nval=',i2,' val(i)=',5(f7.2,1x))
c
c  check FFG values
      if (nval.gt.1) then
         do 90 i=1,nval-1
            val1=val(i)
            if (val1.lt.0) then
               write (iutw,50) jdurt(i),val1,hdid
               if (iupr.ne.iutw) write (iupr,50) jdurt(i),val1,hdid
50    format (' WARNING: FFG value for ',i2,' hour duration (',f6.1,
     +   ') is less than zero for Headwater area ',2a4,'.')
               nwarn = nwarn + 1
               go to 90
               endif
            val2=val(i+1)
            if (val2.gt.0) then
               if (val2.lt.val1) then
                  write (iutw,60) jdurt(i+1),val2,
     +               jdurt(i),val1,hdid
                  if (iupr.ne.iutw) write (iupr,60) jdurt(i+1),val2,
     +               jdurt(i),val1,hdid
60    format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than value for ',
     +   i2,' hour duration (',f6.3,
     +   ') for Headwater area ',2a4,$)
                  nwarn = nwarn + 1
                  if (i+2.gt.nval) then
                     write (iutw,70)
                     if (iupr.ne.iutw) write (iupr,70)
70    format ('.')
                     else
                        val3=val(i+2)
                        valnew=(val3+val1)/2.0
                        write (iutw,80) valnew
                        if (iupr.ne.iutw) write (iupr,80) valnew
80    format (' and will be set to ',f4.1,'.')
                        val(i+1)=valnew
                     endif
                  endif
               endif
90          continue
         endif
c
c  output in SHEF formatc
      if (ndes.gt.0.or.nstr.gt.0) then
         nda = 5
         ndb = 0
         if (nstr.gt.0) ndb = 5
         else
            nda = 0
            ndb = 0
         endif
      call wrdotb (hdid,mdur,nval,val,nwid,ndec,nda,desc,ndb,strnam)
      nhead = nhead + 1
c
100   call prbug2 ('gphed',1)
c
      return
c
      end
