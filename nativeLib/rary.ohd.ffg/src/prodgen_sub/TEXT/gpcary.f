c  =====================================================================
c  pgm:  subroutine gpcary (po)
c
c   in: po     .... array containing carryover information
c  =====================================================================
c
      subroutine gpcary (po)
c
c.......................................................................
c
c  Generate product for carryover of record type CARY
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL - Dec 1993
c.......................................................................
c
      character*1 line(80)
      character*4 dum,type
      character*4 cid(2),desc(5)
      character*4 opnarr(2),opnasn(2),optyrr(2),optysn(2),optyp(12)
c
      dimension co(20),po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/count'
      include 'prodgen_inc/pfparm'
      include 'prodgen_inc/poptns'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpcary.f,v $
     . $',                                                             '
     .$Id: gpcary.f,v 1.2 2004/01/30 17:56:25 scv Exp $
     . $' /
C    ===================================================================
C
      data optyp/'SAC-','SMA ','API-','CONT','API-','MKC ',
     +           'API-','CIN ','API-','HAR ','API-','HFD '/
c
c
      call prbug ('gpcary',1,1,ibug)
c
c  clear output array
      do 10 i=1,80
         line(i) = ' '
10       continue
c
c  identifier
      call umemov (po(2),cid(1),2)
c
c  type code
      call umemov (po(4),type,1)
c
c  description
      call umemov (po(6),desc(1),5)
c
c  rainfall-runoff model type
      lr = po(12)
      call umemov (po(lr),optyrr(1),2)
c
c  rainfall-runoff model name
      call umemov (po(lr+2),opnarr(1),2)
c
c  number of carryover values for this rainfall-runoff model
      irlc = lr + 4
      ncorr = po(irlc)
c
      if (ibug.gt.0) write (iud,20) cid,type,optyrr,opnarr
20    format (' cid=',2a4,' type=',a4,'  optyrr=',2a4,'  opnarr=',2a4)
c
c  determine rainfall-runoff model
      do 30 iopno=1,6
         j = (iopno-1)*2
         if (optyp(j+1).ne.optyrr(1) ) go to 30
         if (optyp(j+2).eq.optyrr(2) ) go to 50
30       continue
      write (iutw,40) optyrr,cid
      if (iupr.ne.iutw) write (iupr,40) optyrr,cid
40    format (' WARNING: no carryover for Operation ',2a4,
     +   ' for identifier ',2a4,'.')
      nwarn = nwarn + 1
      go to 100
c
c  transfer rainfall-runoff carryovers from po array to co array
50    nco = ncorr
      if (iopno.ne.2) then
         do 60 i=1,nco
            co(i) = po(irlc+i)
60          continue
         else if (iopno.eq.2) then
            ivopt = po(irlc+8)
            do 70 i=1,4
               co(i) = po(irlc+i)
70             continue
            k = 0
            if (ivopt.gt.0) then
               co(5) = po(irlc+5)
               k = 1
            endif
            do 80 i=6,8
               j = k + i - 1
               co(j) = po(irlc+i)
80             continue
            nco = j
         endif
c
      if (iopno.eq.1) nco = 6
c
c  get parameter codes and set size of each field
      call shefpe (type,0,iopno)
c
c  output in SHEF format
      if (mxel.le.8.and.iopno.ne.2) then
         call wrdotb (cid,nco,nco,co,nwid,ndec,0,desc,0,dum)
         else
            call wrdota (cid,nco,co,nwid,ndec,peunit,pets)
         endif
      nvar = nvar + 1
c
c  check for snow model
      ls = po(13)
c
c  control snow carryovers for product
      if (nstr.eq.0) ls = 0
      if (ls.le.0) go to 100
c
c  snow model type
      call umemov (po(ls),optysn(1),2)
c
c  snow model name
      call umemov (po(ls+2),opnasn(1),2)
c
c  number of carryover values for this snow model
      islc = ls + 4
      ncosn = po(islc)
c
c  transfer snow model carryovers from po array to co array
      do 90 i=1,ncosn
         co(i) = po(islc+i)
90       continue
c
c  get snow model parameter codes and field sizes
      iopno = 7
      call shefpe (type,m,iopno)
c
c  output snow carryovers in SHEF format
      call wrdota (cid,ncosn,co,nwid,ndec,peunit,pets)
      nvar = nvar + 1
c
100   return
c
      end
