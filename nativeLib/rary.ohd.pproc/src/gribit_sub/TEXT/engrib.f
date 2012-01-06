C$PRAGMA C (GBF_WRIT)
C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  engrib (iupr,iuout,ibug,kyr,kmo,kda,khr,kmn,
c               lwcol,ncol,lsrow,nrow,igds,
c               mptnum,iocent,modlid,ngrid,iparm,itunit,ntup1,ntup2,
c               itrang,nsbctr,ibinf,idec,iresfl,rlov,iscan,ipkflg,
c               idatyp,kbuf,ifld,fld,wfld,ibmap,itot,istat)
c
c   in: iupr   .... unit number of output
c   in: iuout  .... unit number of GRIB encoded binary output file
c   in: ibug   .... debug control
c
c  computation time:
c   in: kyr    .... year (4 digits)
c   in: kmo    .... month (1 to 12)
c   in: kda    .... day (1 to 31)
c   in: khr    .... hour (0 to 23)
c   in: kmn    .... minute (0 to 59)
c
c   in: lwcol  .... local west most HRAP column
c   in: ncol   .... number of HRAP columns
c   in: lsrow  .... local south most HRAP row
c   in: nrow   .... number of HRAP rows
c
c   in: igds   .... grid definition section array
c
c  GRIB stuff:
c   in: mptnum .... parameter table number
c   in: iocent .... identification of originating center (Table 0)
c   in: modlid .... model identification (Table A)
c   in: ngrid  .... grid number (Table B)
c   in: iparm  .... indicator of parameters and units (Table 2)
c   in: itunit .... forecast time units (Table 4)
c   in: ntup1  .... number of time units P1
c   in: ntup2  .... number of time units P2
c   in: itrang .... time range indicator (Table 5)
c   in: nsbctr .... sub-center (Table C)
c   in: ibinf  .... binary scaling factor
c   in: idec   .... decimal scale factor (power of 10)
c   in: iresfl .... resolution and component flag (Table 7)
c   in: rlov   .... longitude of meridian parallel to y-axis
c   in: iscan  .... scanning mode flag (Table 8)
c   in: ipkflg .... packing (0 - simple, 1 - second order)
c   in: idatyp .... data input type:
c                      0 - floating point in array 'fld'
c                      1 - integer in array 'ifld'
c   in: kbuf   .... array to hold packed message
c   in: ifld   .... integer array containing gridded values
c   in: fld    .... real array containing gridded values (metric)
c   in: wfld   .... work real array
c   in: ibmap  .... bitmap array
c
c  out: itot   .... number of bytes output to array kbuf
c  out: istat  .... status code
c  =====================================================================
c
      subroutine engrib_sub (iupr,iuout,ibug, kyr,kmo,kda,khr,kmn,
     1      lwcol,ncol,lsrow,nrow, igds,
     2      mptnum,iocent,modlid,ngrid,iparm,itunit,ntup1,ntup2,
     3      itrang,nsbctr,ibinf,idec,iresfl,rlov,iscan,ipkflg,
     4      idatyp, kbuf,ifld,fld,wfld, ibmap, itot, istat)
c
c.......................................................................
c
c  This routine initializes variables needed by the GRIB encoder,
c  calls the GRIB encoder routine and outputs the GRIB encoded data.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                               Dec 10, 1992
c
c  Changed to pass many GRIB variables thru argument list.
c       Tim Sweeney, HRL                                   Nov 1998
c.......................................................................
c
      character*1 pds(28),kbuf(*)
      character*2 subcenter0
      character*25 appsvar

      real fld(*),wfld(*)
      integer ifld(*)
      integer id(25)
      integer igds(*)
      integer ibdsfl(12)
      integer ibmap(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/engrib.f,v $
     . $',                                                             '
     .$Id: engrib.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
      if (ibug.gt.0) write (iupr,*) 'enter engrib'
c
      if (ibug.gt.0) then
         write (iupr,10) kyr,kmo,kda,khr,kmn
10    format (' kyr=',i4,' kmo=',i2,' kda=',i2,' khr=',i2,' kmn=',i2)
         write (iupr,*) 'lwcol=',lwcol,' ncol=',ncol,
     +     ' lsrow=',lsrow,' nrow=',nrow
         endif
c
      istat = 0
c
c
c  data input: 0 - floating point in array 'fld'
c              1 - integer in array 'ifld'
      itype = idatyp
c
c===================================================================
c
c                 PRODUCT DEFINITION SECTION (PDS)
c
c===================================================================
c  ipflag control:   0 - make PDS from user supplied array (id)
c                        for w3fi68 input
c                    1 - user supplying PDS
      ipflag = 0
c
c  number of bytes in PDS
      id(1)  = 28
c
c  parameter table version number (NWS field - Oct 98)
      id(2)  = mptnum
c
c  identification of originating center (Table 0)
      id(3)  = iocent
c
c  model identification (Table A)
      id(4)  = modlid
c
c  grid identification (Table B)
      id(5)  = ngrid
      if (ngrid.eq.240) id(5) = 255
c
c  include GDS section (0-no, 1-yes)  (Table 1)
      id(6)  = 1
c
c  include BMS section (0-no, 1-yes)   (also Table 1)
      id(7)  = 1
c
c  indicator of parameters and units (Table 2)
      id(8)  = iparm
c
c  indicator of type of level (Table 3)
c                     1 - surface
      id(9)  = 1
c
c  value 1 of level   (0 for level 1)
      id(10) = 0
c
c  value 2 of level   (0 for level 1)
      id(11) = 0
c
c  elements 12-16 are year, month, day, hour, minute
c  model computation time (last data)
c          year   GRIB year
c          1998      98
c          1999      99
c          2000     100
c          2001       1
c          2002       2
      kcc    = kyr/100
      id(12) = kyr - kcc*100
      if (id(12).le.0) id(12) = 100
      id(13) = kmo
      id(14) = kda
      id(15) = khr
      id(16) = kmn
c
c  fcst time unit (Table 4)
      id(17) = itunit
c
c  p1 period of time (number of time units)
      id(18) = ntup1
c
c  p2 period of time (number of time units)
      id(19) = ntup2
c
c  time range indicator (Table 5)
      id(20) = itrang
c
c  number included in average when average or accumulated value
c  indicated in Table 5
      id(21) = 0
c
c  number missing from averages
      id(22) = 0
c
c  century of initial reference (20, change to 21 on Jan 1, 2001, etc.)
c           year   GRIB century
c           1998      20
c           1999      20
c           2000      20
c           2001      21
c           2002      21
      id(23) = (kyr-1)/100 + 1
c
c  identification of sub-center  Table C
c  for originating center = 9
c   RFCs 150 to 162
      id(24) = nsbctr
c
c  check value of grib_set_subcenter_0 token
c  if set to ON, then set id(24) to 0
c
      appsvar='grib_set_subcenter_0'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,subcenter0,lsub0)
      if(subcenter0.eq.'ON'.or.subcenter0.eq.'on'
     *      .or.subcenter0.eq.'On') id(24) = 0
c
c  decimal scale factor (power of 10)--number of decimal positions
      id(25) = idec
c
c      id(26) = 0
c      id(27) = 0
c      id(28) = 0
c  end PDS
c
c=======================================================================
c
c                   GRID DEFINTION SECTION (GDS)
c
c=======================================================================
c  igflag control:
c      0 - make GDS based on 'igrid' value
c      1 - make GDS from user supplied info in 'igds' and 'igrid' value
c          for w3fi74 input (but doc in w3fi71)
c
      igflag = 1
c
c  grid identification:  #   - Table B
c                        255 - user defined grid, 'igds' must be
c                              supplied and igflag must = 1
      igrid = 255
c
c  user defined grid  -  HRAP National CONUS
c
c  resolution and component flag for bit 5 of gds(17)
c                 0 = earth oriented winds, 1 = grid oriented winds
      icomp = 0
c              end GDS
c====================================================================
c
c  ibflag control:  0 - make bit map from user supplied data
c                   # - bit map predefined by center
      ibflag = 0
c
c  length of bit map to be used to verify length of field
      mbmap = ncol*nrow
c
c====================================================================
c
c            BINARY DATA SECTION -- PACK DATA
c
c====================================================================
c    Reference Table 11  FLAGS
c
c  data:  0 - grid point data, 1 - spherical harmonic coefficients
      ibdsfl(1) = 0
c
c  packing:  0 - simple,  1 - second order
      ibdsfl(2) = ipkflg
c
c  original data type:  0 - floating point values,  1 - integer
      ibdsfl(3) = idatyp
c
c  octet 14:  0 - no additional flags, 1 - contains flag bits 5-12
      ibdsfl(4) = 0
c
c  reserved
      ibdsfl(5) = 0
c
c  each grid point:  0 - single datum, 1 - matrix of values
      ibdsfl(6) = 0
c
c  secondary bit maps:  0 - none, 1 - secondary bit maps present
      ibdsfl(7) = 0
c
c  second order values have:  0 - constant width, 1 - different widths
      ibdsfl(8) = 0
c
      ibdsfl(9)  = 0
      ibdsfl(10) = 0
      ibdsfl(11) = 0
      ibdsfl(12) = 0
c
c  length for packing data from power of 2 (number of bits)
c     0 - best fit using variable bit packer w3fi58
c     8, 12, etc. - rescales to fit fixed number of bits using w3fi59
c
c  binary scaling (3=nearest eighth value)
      call getbit (id(7),ibinf,id(25),mbmap,ibmap,fld,
     +             wfld,fmin,fmax,nbit)
C
c==================================================================
c
c  encode using NCEP's GRIB routines
c
      if (ibug.gt.0) write (iupr,*) 'ibinf=',ibinf,' mbmap=',mbmap,
     +   ' fmin=',fmin,' fmax=',fmax,' nbit=',nbit
      if (ibug.gt.0) write (iupr,20) (ibmap(i),i=1,mbmap)
20    format (' in engrib - (ibmap(i),i=1,mbmap)=' / (1x,50i2))
      if (ibug.gt.0) write (iupr,30) (fld(i),i=1,mbmap)
30    format (' in engrib - (fld(i),j=1,mbmap)=' / (1x,15(f6.2,1x)))
      if (ibug.gt.0) write (iupr,*) 'in engrib - calling w3fi72'
      call w3fi72 (itype,wfld,ifld,nbit,ipflag,id,pds,
     +             igflag,igrid,igds,icomp,
     +             ibflag,ibmap,mbmap,ibdsfl,
     +             npts,kbuf,itot,istat)
      if (istat.eq.0) then
         if (ibug.gt.0) write (iupr,*) 'in engrib - ',
     +      'calling gbf_writ - itot=',itot
         call gbf_writ (itot,kbuf,ier)
         else
            write (iupr,40) istat
40    format (' ERROR: in GRIB encoding - w3fi72 istat=',i2 /
     +   ' Values for istat:' /
     +   5x,'1 = IPFLAG not 0 or 1' /
     +   5x,'2 = IGFLAG not 0 or 1' /
     +   5x,'3 = error converting IEEE F.P. number' /
     +   5x,'4 = W3FI71 error/IGRID not defined' /
     +   5x,'5 = W3FK74 error/grid represntation type not valid' /
     +   5x,'6 = grid too large for packer dimension array see NCEP' /
     +   5x,'7 = length of bit map not equal to size of FLD or IFLD' /
     +   5x,'8 = W3FI73 error, all values in IBMAP are zero')
         endif
c
      return
c
      end
