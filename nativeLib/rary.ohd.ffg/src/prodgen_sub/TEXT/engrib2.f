c  =====================================================================
c  pgm:  engrib2 (iupr,igout,iud,ibug,
c                kyr,kmo,kda,khr,kmn, nzyr,nzmo,nzda,nzhr,nzmn, nsbctr,
c                iocent,senof,mptver,lptver,lwcol,ncol,lsrow,nrow,
c                iresfl,rlav,rlov,iscan,iparm,modlid,nturef,itunit,
c                ntufc,ipkflg,inbmap,refval,ibinf,idecf,iwidth,
c                ibmap, mfld,fld, L3264B,istat)
c
c   in: iupr   .... unit number of terminal
c   in: igout  .... unit number of GRIB encoded binary output file
c   in: iud    .... unit number for debug output
c   in: ibug   .... debug control
c
c                     computation time
c   in: kyr    .... year (4 digits)
c   in: kmo    .... month (1 to 12)
c   in: kda    .... day (1 to 31)
c   in: khr    .... hour (0 to 23)
c   in: kmn    .... minute (0 to 59)
c
c                      current z time
c   in: nzyr   .... year (4 digits)
c   in: nzmo   .... month
c   in: nzda   .... day
c   in: nzhr   .... hour
c   in: nzmn   .... minute
c
c  GRIB stuff:
c   in: iocent .... identification of originating center (Table 0)
c   in: senof  .... sending office (ABRFC, etc.)
c   in: mptver .... master table version number
c   in: lptver .... local table version number
c   in: lwcol  .... local west most HRAP column
c   in: ncol   .... number of HRAP columns
c   in: lsrow  .... local south most HRAP row
c   in: nrow   .... number of HRAP rows
c
c   in: iresfl .... resolution and component flag (Table 7)
c   in: rlav   .... latitude where Dx and Dy are specified
c   in: rlov   .... orientation of the grid
c   in: iscan  .... scanning mode flag (Table 8)
c   in: iparm  .... indicator of parameters and units (Table 2)
c   in: modlid .... model identification (Table A)
c   in: nturef .... number of time units from reference time (P1)
c   in: itunit .... forecast time units (Table 4)
c
c   in: ntufc  .... number of time units to forecast (P2)
c   in: ipkflg .... packing (0 - simple, 1 - second order,
c                            2 - 2nd order spatial differencing)
c   in: inbmap .... bit map indicator (0 - bit map in section 6,
c                                     >0 - no bit map)
c   in: refvel .... reference value
c   in: ibinf  .... binary scale factor
c   in: idecf  .... decimal scale factor (power of 10)
c   in: iwidth .... field width of data
c   in: ibmap  .... bitmap array
c
c   in: mfld   .... number of words in array fld
c   in: fld    .... real array containing gridded values
c
c   in: L3264B .... integer word length of machine used
c
c  out: istat  .... status code
c  =====================================================================
      subroutine engrib2 (iupr,igout,iud,ibug,
     1          kyr,kmo,kda,khr,kmn, nzyr,nzmo,nzda,nzhr,nzmn, nsbctr,
     2             iocent,senof,mptver,lptver,lwcol,ncol,lsrow,nrow,
     3             iresfl,rlav,rlov,iscan,iparm,modlid,nturef,itunit,
     4             ntufc,ipkflg,inbmap,refval,ibinf,idecf,iwidth,
     5             ibmap, mfld,fld, L3264B,istat)
c.......................................................................
c  This routine initializes variables needed by the GRIB2 encoder,
c  calls the GRIB2 encoder routine, and outputs the GRIB2 encoded data.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                   May 1999
c
c.......................................................................
c
      character*1 ipack(252000)
      character*4 blnk
      character*8 sname
c
      real is0(16),is1(30),is2(1),is3(50),is4(20),is5(50),
     1     is6(250000),is7(250000)
      real rdata(250000),fld(*)
c
      dimension ibmap(250000)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ffg/src/prodgen_sub/RCS/engrib2.f,v $
     . $',                                                             '
     .$Id: engrib2.f,v 1.2 2002/05/15 17:50:25 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'engrib2 ' /
      data blnk/ '    ' /
c
      if(ibug.gt.0) write(iud,7000) sname
 7000 format('>> ',a8,' ENTERED' )
c
      nd5 = 252000
      ns1 = 30
      ns2 = 1
      ns3 = 50
      ns4 = 20
      ns5 = 50
      ns6 = 1
      ns7 = 250000

c
      if(ibug.gt.0) then
        write(iud,6910) nzyr,nzmo,nzda,nzhr,nzmn
 6910   format('  nzyr=',i4,3x,'nzmo=',i2,3x,'nzda=',i2,
     1         3x,'nzhr=',i2, 3x,'nzmn=',i2)
        write(iud,6920) kyr,kmo,kda,khr,kmn
 6920   format('  kyr=',i4,3x,'kmo=',i2,3x,'kda=',i2,3x,'khr=',i2,
     1         3x,'kmn=',i2)
        endif
c
      istat = 0
c
c
c===================================================================
c
c                 SECTION 0 - INDICATOR SECTION
c
c===================================================================
c
c  GRIB name
      is0(1) = 4hGRIB
c
c  discipline - master table number (table 0.0)
c                  1 - hydrological products
      is0(7) = 1
c
c  GRIB edition number
      is0(8) = 2
c
c
c===================================================================
c
c                 SECTION 1 - IDENTIFICATION SECTION
c
c===================================================================
c
c  length of section
      is1(1) = 30
c
c  number of section
      is1(5) = 1
c
c  identification of originating/generating center (Table 0)
c           7 - US Weather Service - National Met Center
c           9 - US Weather Service - Field Stations
      is1(6)  = iocent
c
c  identification of originating/generating sub-center  Table C
c  for originating center = 9
c   RFCs 150 to 162
c      call subcen (senof,is1(8) )
      is1(8) = nsbctr
c
c  GRIB Master Tables Version Number
c        0 - not used
      is1(10) = mptver
c
c  GRIB Local Tables Version Number
c     (might be 128 to 255)
c        0 - not used
      is1(11) = lptver
c
c  year (4 digit), month, day, hour, minute, second
c  model computation time (last data)
      is1(12) = kyr
      is1(14) = kmo
      is1(15) = kda
      is1(16) = khr
      is1(17) = kmn
      is1(18) = 0
c
c  production status of processed data (code table 1.0)
c         0 - operational products
      is1(19) = 0

c
c  type of processed data in message (code table 1.1)
c         1 = forecast product
      is1(20) = 1
c
c===================================================================
c
c                 SECTION 2 - LOCAL USE SECTION
c
c===================================================================
c
c  length of section
      is2(1) = ns2
c
c  number of section
      is2(5) = 2

c
c===================================================================
c
c                 SECTION 3 - GRID DEFINITION SECTION
c
c===================================================================
c
c  length of section
      is3(1) = ns3
c
c  number of section
      is3(5) = 3
c
c  grid definition template 3.0
c        5 - polar stereographic projection
      is3(6) = 5
c
c  grid definition template 3.5 - polar stereographic projection
c  Nx - number of points along X-axis
      is3(8) = ncol
c
c  Ny - number of points along Y-axis
      is3(12) = nrow
c
c  convert HRAP grid to local origin lat & lon
      illgd = 0
      npair = 1
      x = lwcol
      y = lsrow
      call cvllgd (rlon,rlat,npair,x,y,illgd,istat)
      if(ibug.gt.0) write(iud,8030) illgd,rlat,rlon,lsrow,lwcol
 8030 format(' illgd=',i1,'  rlat=',f7.3,'  rlon=',f8.3,'  lsrow=',i3,
     +       '  lwcol=',i3)
c
c  La1 - latitude of first grid point (10**-6 degrees)
      is3(16) = rlat*1000000.
c
c  Lo1 - longitude of first grid point (10**-6 degrees)
      is3(20) = -rlon*1000000.
c
c  resolution and component flag (flag table 3.1)
      is3(24) = iresfl
c
c  Lav - latitude where Dx and Dy are specified (10**-6 degrees)
      is3(25) = rlav*1000000.
c
c  Lov - orientation of the grid (10**-6 degrees)
      is3(29) = rlov*1000000.
c
c  Dx - X-direction grid length (units of 10**-5 meters)
      is3(33) = 476200000.
c
c  Dy - Y-direction grid length (units of 10**-5 meters)
      is3(37) = 476200000.
c
c  projection center flag
c        0 - north pole on the projection plane
      is3(41) = 0
c
c  scanning mode (flag table 3.2)
      is3(42) = iscan
c
c  reserved
      do 1110 i=43,ns3
 1110 is3(i) = 0
c
c===================================================================
c
c                 SECTION 4 - PRODUCT DEFINITION SECTION
c
c===================================================================
c
c  length of section
      is4(1) = ns4
c
c  Number of section
      is4(5) = 4
c
c  product definition template number (code table 4.0)
c        2 - forecast at a horizontal level at a point in time
      is4(6) = 2
c
c  product definition template:
c  for parameter category (code table 4.1)
c  Product Discipline 1
c         0 - hydrology
      is4(8) = 0
c
c  parameter number (code table 4.2)
      is4(9) = iparm
c
c  forecast generating process identifer (defined by originating
c  center)
      is4(11) = modlid
c
c  hours after reference time of data cutoff
      is4(12) = nturef
c
c  minutes after reference time of data cutoff
      is4(14) = 0
c
c  indicator of unit of time range
      is4(15) = itunit
c
c  forecast time in units defined by is4(15)
      is4(16) = ntufc
c
c  indicator of type and unit of level (code table 4.3)
c        1 - surface
      is4(18) = 1
c
c  vertical coordinate value of level
      is4(19) = 0
c
c=======================================================================
c
c                   SECTION 5 - DATA REPRESENTATION SECTION
c
c=======================================================================
c
c  length of section
      is5(1) = ns5
c
c  number of section
      is5(5) = 5
c
c  data representation template number (code table 5.0)
c       grid point data:  0 - simple packing
c                         1 - complex packing
c                         2 - second-order spatial differencing
      is5(6) = ipkflg
c
c  bit map indicator
c        0   - bit map applies and is supplied in Section 6
c       255  - no bit map, Section 6 is absent
c      1-254 - bit map predetermined by center, Section 6 is absent
      is5(7) = inbmap
c
c
c  data representation template:
      if(ipkflg.eq.0) then
c  simple packing:  Data Representation Template 5.0
c    reference value (R)
         is5(8) = refval
c
c   binary scale factor (E)
         is5(12) = ibinf
c
c   decimal scale factor (D)
         is5(13) = idecf
c
c   field width for data
         is5(14) = iwidth
c
      else if(ipkflg.eq.1) then
c  complex packing:  Data Representation Template 5.1


c
      else
c  2nd order spatial differencing (ipkflg=3):  Data Rep Template 5.2

      endif
c
c=======================================================================
c
c                   SECTION 6 - BIT-MAP SECTION (OPTIONAL)
c
c=======================================================================
c
      if(inbmap.gt.0) then
         ns6 = 0
      else
c
c  length of bit map
         mbmap = ncol*nrow
c
c  transfer bit map array
         do 1210 i=1,mbmap
 1210    is6(i+5) = ibmap(i)
      endif
c
c  length of section
      is6(1) = ns6
c
c  number of section
      is6(5) = 6
c
c=======================================================================
c
c                   SECTION 7 - BINARY DATA SECTION
c
c=======================================================================
c
c  length of section
      is7(1) = mfld + 5
c
c  number of section
      is7(5) = 7
c
c  binary data values
      do 1420 i=1,mfld
 1420 is7(i+5) = fld(i)
c
c======================================================================
c
c  encode using TDL's GRIB2 routines
c
      call pk_grib2(igout,fld,nrow,ncol,is0,is1,ns1,is2,ns2,is3,ns3,
     +              is4,ns4,is5,ns5,is7,ns7,ipack,nd5,xmissx,xmissp,
     +              xmisss,new,minpk,ioctet,L3264B,istat)
      if (ier.eq.0) then
         write(igout) nd5,(ipack(i),i=1,nd5)
         write(iupr,90) nd5
   90 format(' Wrote GRIB Bulletin of ',i6,' bytes.')
         else
            write(iupr,8010) istat
 8010 format (' ERROR: in GRIB encoding - pk_grib2 istat=',i2 /
     +   5x,'1 = IPFLAG not 0 or 1' /
     +   5x,'2 = IGFLAG not 0 or 1' /
     +   5x,'3 = error converting IEEE F.P. number' /
     +   5x,'4 = W3FI71 error/IGRID not defined' /
     +   5x,'5 = W3FK74 error/grid represntation type not valid' /
     +   5x,'6 = grid too large for packer dimension array' /
     +   5x,'7 = length of bit map not equal to size of FLD ',
     +      'or IFLD' /
     +   5x,'8 = W3FI73 error, all values in IBMAP are zero' /)
         endif
c
      if (ibug.gt.2) then
c     print grib file information
         call ungrib2 (iupr,iud,ibug,nd5,ipack,mfld,fld,L3264B,ier)
         endif
c
 9999 return
c
      end
