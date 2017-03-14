c  =====================================================================
c  pgm:  xm2grib (iutw,iupr,iud,xver,user,proces,isearch,ivalhr,
c                 imo,ida,iyr,ihr,imn,
c                 mwcol,msrow, nsbctr,iocent,mptnum,lptnum,
c                 iresfl,rlav,rlov,iscan,iparm,modlid,ngrid,itunit,
c                 nturef,ntufc,itrang,ipkflg,inbmap,refval,ibinf,
c                 idec,iwidth,idatyp,wmo,senof,istat)
c
c   in:  iutw   .... unit number for print output
c   in:  iupr   .... unit number for log output
c   in:  iud    .... unit number for debug output
c
c  xmrg stuff:
c   in: xver    .... xmrg file format version number
c   in: user    .... not necessarily originating center
c   in: proces  .... process flag consisting of generating model
c                    and hours of accumulation or duration
c   in: isearch .... parameter table search sequence
c
c   in: ivalhr  .... valid hour
c
c  i/o: imo     .... reference month
c  i/o: ida     .... reference day
c  i/o: iyr     .... reference year
c  i/o: ihr     .... reference hour
c  i/o: imn     .... reference minute
c
c   in: mwcol   .... most west HRAP column
c   in: msrow   .... most south HRAP row
c
c  GRIB stuff:
c  out: nsbctr  .... sub-center number (Table C)
c  out: iocent  .... identification of originating center (Table 0)
c  out: mptnum  .... parameter table number
c  out: lptnum  .... local parameter table number (not used)
c  out: iresfl  .... resolution and component flag (Table 7)
c  out: rlav    .... latitude where Dx and Dy specified
c  out: rlov    .... orientation of grid (parallel longitude)
c  out: iscan   .... scanning mode flag (Table 8)
c  out: iparm   .... indicator of parameters and units (Table 2)
c  out: modlid  .... model identification (Table A)
c  out: ngrid   .... grid number of output
c  out: itunit  .... forecast time units (Table 4)
c  out: nturef  .... number of time units P1 from reference time
c  out: ntufc   .... number of time units P2
c  out: itrang  .... time range indicator (Table 5)
c  out: ipkflg  .... packing (0 - simple, 1 - second order)
c  out: inbmap  .... bit map indicator (0 = include bit map)
c  out: refval  .... reference value
c  out: ibinf   .... binary scale factor
c  out: idec    .... decimal scale factor (power of 10)
c  out: iwidth  .... field width for data
c  out: idatyp  .... data input type
c
c  comms info:
c  out: wmo    .... World Meteorological Oranization identifier
c  out: senof  .... sending office (ABRFC, etc.)
c
c  out: istat   .... status code
c  =====================================================================
c
      subroutine xm2grib (iutw,iupr,iud,xver,user,proces,isearch,ivalhr,
     +          imo,ida,iyr,ihr,imn,
     +          mwcol,msrow, nsbctr,iocent,mptnum,lptnum,
     +          iresfl,rlav,rlov,iscan,iparm,modlid,ngrid,itunit,
     +          nturef,ntufc,itrang,ipkflg,inbmap,refval,ibinf,idec,
     +          iwidth,idatyp,wmo,senof,istat)
c
c.......................................................................
c
c  This routine initializes variables needed by the GRIB encoder.
c  Uses info from XMRG files to define several variables for GRIB.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                 Jan 2000
c.......................................................................
c
c
      include 'xmrg_tbl'
      include 'grib_tbl'
c
      character cnum(2)
      character*4 wmo(2),senof
      character*5 agpm
      character*8 proces
      character*10 user
c
      dimension mon(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/xm2grib.f,v $
     . $',                                                             '
     .$Id: xm2grib.f,v 1.4 2006/10/02 14:29:36 lawrence Exp $
     . $' /
C    ===================================================================
C
      data mon / 31,28,31,30,31,30,31,31,30,31,30,31 /
c
c
      call prbug ('xm2grib',1,1,ibug)
ccc      ibug = 1
c
      istat = 0
c
c.......................................................................
c
c  SECTION 1 - IDENTIFICATION SECTION
c
      if (ibug.eq.1) write (iutw,*) 'in xm2grib - senof=',senof
      call subctr (iutw,iupr,iud,user,mwcol,nsbctr,senof)
      if (ibug.eq.1) write (iutw,*) 'in xm2grib - senof=',senof
      if (nsbctr.gt.0) then
         iocent = 9
         else
            istat = 1
            go to 150
         endif
c
c.......................................................................
c
c  SECTION 3 - GRID DEFINITION SECTION
c
c  variables defined in routine griddef
c
c.......................................................................
c
c  SECTION 4 - PRODUCT DEFINITION SECTION
c
c  determine xmrg process flag (check entire field)
      ipt = 0
      do 30 ixpm=1,mproc
         if (proces.eq.xmproc(ixpm)) go to 70
30       continue
c
c  determine xmrg process flag (check first ipt-1 characters)
      do 50 ixpm=1,mproc
         lxmproc = lenstr(xmproc(ixpm))
c         if (ibug.eq.1) write (iupr,*) 'mproc=',mproc,' ixpm=',ixpm,
c     +      ' xmproc(ixpm)=',xmproc(ixpm),' lxmproc=',lxmproc
c     +      ' xmproc(ixpm)=',xmproc(ixpm),' lxmproc=',lxmproc
         do 40 i=1,lxmproc
            k = lxmproc - i + 1
            if (xmproc(ixpm)(k:k).eq.'*') ipt = k
40          continue
         if (ipt.gt.0) then
            kk = ipt - 1
            if (proces(1:kk).eq.xmproc(ixpm)(1:kk)) go to 70
            endif
50       continue
      write (iutw,60) proces(1:lenstr(proces))
60    format (' ERROR: process flag ',a,' from xmrg file is invalid. ',
     *   ' Valid values are:')
      do 65 ixpm=1,mproc
         if (xmproc(ixpm).ne.'NONE') write (iupr,63) ixpm,xmproc(ixpm)
63    format (' ',i2,':',' ',a)
65       continue 
      istat = 2
      go to 150
c
c  set corresponding parameters
70    modlid = xmodlid(ixpm)
      ngrid = xngrid(ixpm)
      if (ngrid.eq.0) ngrid = 240
      itunit = xtunit(ixpm)
      itrang = xtrang(ixpm)
c
c     Additional logic to handle QPE01, QPE06, QPE24
c
c     if (xmproc(ixpm).eq.'QPE06'.or.xmproc(ixpm).eq.'QPE24')then
c        ipt=4
c     endif
c
      if (ipt.gt.0) then
         cnum(1) = proces(ipt:ipt)
         i = ipt + 1
         cnum(2) = proces(i:i)
         ipos = 1
         iwidth = 2
         num = 1
         call ffa2i (cnum,ipos,iwidth,num,istep,ierr)
         if (ierr.ne.0) istep = 1
         else
c        default 1 hr if no accumulation time provided
            istep = 1
         endif
c
      if (ivalhr.le.0) then
         nturef = 0
         ntufc = istep
c     recompute reference time
         mon(2) = 28 + ((iyr/4)*4)/iyr
         ndhr = -istep
         call datimi (ndhr,imo,ida,iyr,ihr,imn,jmo,jda,jyr,jhr,jmn,mon)
         imo = jmo
         ida = jda
         iyr = jyr
         ihr = jhr
         imn = jmn
         else
            nturef = ivalhr - istep
            ntufc = ivalhr
         endif
      if (xnturef(ixpm).ge.0) nturef = xnturef(ixpm)
      if (xntufc(ixpm).ge.0)  ntufc = xntufc(ixpm)
c
      if (ibug.gt.0) write (iupr,80) ivalhr,istep
80    format (' ivalhr=',i2,4x,'istep=',i2)
c
c  wmo id (TTAA00)
      if (proces(1:3).ne.'QPA'.or.proces(1:3).ne.'QPM') then
         wmo(1) = xwmo(ixpm)(1:4)
         wmo(2) = xwmo(ixpm)(5:6)
         else
c     select wmo id for particular 6-hour fcst period
            iper = ntufc/istep
            wmo(1) = qpfwmo(iper)(1:4)
            wmo(2) = qpfwmo(iper)(5:6)
         endif
c
c  append hours to base GRIB parmeter abbreviation, when '*' used
      agpm = gribpm(ixpm)
      la = lenstr(agpm)
      ipa = 0
      do 90 k=1,la
         i = la - k + 1
         if (agpm(i:i).eq.'*') ipa = i
90       continue
      if (ipa.gt.0.and.ipt.gt.0) then
c     insert 0 (zero) for hours less than 10
         i = ipt + 1
         if (proces(i:i).ne.' ') then
            agpm(ipa:ipa) = proces(ipt:ipt)
            ipt = ipt + 1
            else
               agpm(ipa:ipa) = '0'
            endif
         ipa = ipa + 1
         agpm(ipa:ipa) = proces(ipt:ipt)
         endif
c
c  determine GRIB parameter number from a Parameter Table,
c  then get Parameter Table number
      mptnum = -1
      iparm = -1
c
c  search table 128 for parameter
c     write (6,*) 'Searching for parameter from 128', agpm
      do 110 i=128,255
         if (agpm.eq.abv128(i)) then
            iparm = i
            mptnum = 128
            if (isearch.eq.1) go to 140
            endif
110      continue
c
c  search Table 2 for parameter (NCEP)
c     write (6,*) 'Searching for parameter from Table 2'
      do 120 i=1,255
         if (agpm.eq.abv2(i)) then
            iparm = i
            mptnum = 2
            endif
120      continue
      if (mptnum.gt.0.and.iparm.gt.0) go to 140
      write (iupr,130) agpm
130   format (' ERROR: parameter ',a,' not defined.')
c
c.......................................................................
c
c  SECTION 5 - DATA REPRESENTATION SECTION
c
c  packing data:  0 = simple
c                 1 = complex (second order)
c                 2 = second order spatial differencing
140   ipkflg = xpkflg(ixpm)
c
c  bit map indicator - 0 = include bit map
      inbmap = 0
c
c  reference value
      refval = 0
c
c  binary scale factor
      ibinf = xbinf(ixpm)
c
c  decimal scale factor (power of 10) - number of decimal positions
      idec = xdec(ixpm)
c
c  field width for data
      iwidth = xwidth(ixpm)
c
c  GRIB1 data input:  0 = floating point  1 = integer
      idatyp = 0
c
150   return
c
      end
