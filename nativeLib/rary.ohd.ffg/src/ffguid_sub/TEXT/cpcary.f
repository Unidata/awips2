c  =====================================================================
c  pgm:  cpcary
c
c  variables are passed in common blocks
c
c  =====================================================================
c
      subroutine cpcary
c
c.......................................................................
c
c  Transfer rainfall-runoff and snow model state variables (carrhover) 
C  from the OFS database to an FFGS file
c
c.....................................................................
c     Program initially written by
c           Tim Sweeney, HRL - Dec 1993
c
c.....................................................................
c
      character*2 bname
      character*4 cnone
      character*4 ffgtyp,foptyp
      character*8 ffgid,lident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/count'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gpo'
      include 'ffg_inc/gbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/cpcary.f,v $
     . $',                                                             '
     .$Id: cpcary.f,v 1.4 2004/09/13 14:23:05 scv Exp $
     . $' /
C    ===================================================================
C
      data foptyp/ 'ffg '/
      data cnone/ 'none'/
      data bname/ '  ' /
c
c
      call prbug ('cpcary',1,1,ibug)
c
      nevar = nerr
      nwvar = nwarn
c
      ncary = 0
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,10)
         if (iupr.ne.iutw) write (iupr,10)
10    format (' WARNING: cannot transfer caryover ',
     +   'because user controls are not defined.')
         go to 110
         endif
c
c  values of iofs:
c    0 = use OFS files
c    1 = use alternate OFS files (FFGS directories)
c    2 = use OFS BASN files as control and FFGS ffg op files
c        (for testing)
c
c  get index for ffg rainfall-runoff files (OFS)
      if (iofs.eq.1) go to 30
         ffgid = ' '
         irec = 0
         write (iutw,20)
      if (iupr.ne.iutw) write (iupr,20)
20    format (/ ' NOTE: transferring carryover data from ',
     +   'OFS to FFGS.' /)
      if (iofs.eq.0) go to 60
c
c  alternate files
30    call getidx (foptyp,idxdv,mcidx,po,nof,cidx,istat)
      call upclos (idxdv,bname,ic)
      iptbb = 0
c
c  process each ffg area file
      irec = 0
40    irec = irec + 1
      if (irec.gt.nof) then
         if (iofs.eq.1) go to 110
         irec = 1
         endif
      if (cidx(1,irec).ne.cnone) go to 50
      if (cidx(2,irec).eq.cnone) go to 40
50    call umemov (cidx(1,irec),ffgid,2)
c
c  get a (next) ffg area id from nwsrfs ofs pppdb
c  contains basin boundary id and rainfall-runoff curves
60    if (iofs.eq.0) then
         ffgtyp = 'FFG'
         call rpprec (ffgid,ffgtyp,irec,mpo,po,nfill,irecnx,istat)
         else
            ffgtyp = 'ffg'
            kod = 0
            call rppfil (ffgid,ffgtyp,kod,iopdv,mpo,po,npo,istat)
          endif
      if (ibug.gt.0) write (iud,70) ffgid,ffgtyp,irec,istat
70    format (' ffgid=',a,' ffgtyp=',a,' mbx=',i5,' istat=',i2)
      if (istat.eq.0) then
         if (iofs.ne.0) call upclos (iopdv,bname,ic)
         else
            write (iutw,80) ffgid
            if (iupr.ne.iutw) write (iupr,80) ffgid
80    format (' ERROR: Flash Flood Guidance Operation not defined ',
     +   'for identifier ',a,'.')
            nerr = nerr + 1
            go to 100
         endif
c
c  check ffg computational dates
      call ckcpd (ffgid,po(17),lident,lffcpd,nwarn)
c
c  transfer carryover in FFG parameter array to file
      call trcary (ffgid,mpo,mbx,po,bx)
c
      write (iutw,90) ffgid
      if (iupr.ne.iutw) write (iupr,90) ffgid
90    format (' NOTE: carryover tranferred for FFG identifier ',a,'.')
      ncary = ncary +1
c
100   if (iofs.eq.0) then
         if (irecnx.eq.0) go to 110
         irec = irecnx
         ffgid = ' '
         go to 60
         else if (iofs.eq.1) then
            go to 40
         else
            if (inxbb.eq.0) go to 110
            iptbb = inxbb
            go to 40
         endif
c
110   write (iutw,120) ncary
      if (iupr.ne.iutw) write (iupr,120) ncary
120   format (/ ' NOTE: carryover tranferred for ',i3,
     +   ' FFG identifiers.')
c
      nevar = nerr - nevar
      nwvar = nwarn - nwvar
c
      return
c
      end
