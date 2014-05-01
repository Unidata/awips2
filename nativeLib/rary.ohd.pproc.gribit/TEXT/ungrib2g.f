c  =====================================================================
c  pgm:  ungrib2g (iupr,iud,ibug,nd5,ipack,mfld,fld,L3264B,gridf,ier)
ccc
ccc next appears to be old version
ccc
ccc  pgm:  ungrib2 (iupr,igout,iud,ibug,
ccc                kyr,kmo,kda,khr,kmn, nyr,nmo,nda,nhr,nmn,
ccc                iocent,senof,mptver,lptver,lwcol,ncol,lsrow,nrow,
ccc                iresfl,rlav,rlov,iscan,iparm,modlid,nturef,itunit,
ccc                ntufc,ipkflg,inbmap,refval,ibinf,idecf,iwidth,
ccc                ibmap, mfld,fld, ichd,pid,cct,wmo,gridf,jerr)
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
c                      now time
c   in: nyr    .... year (4 digits)
c   in: nmo    .... month
c   in: nda    .... day
c   in: nhr    .... hour
c   in: nmn    .... minute
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
c  comms info:
c   in: ichd   .... comms header control
c   in: pid    .... communications identifier for product
c   in: cct    .... comms circuits
c   in: wmo    .... World Meteorological Oranization identifier
c
c   in: gridf  .... grid factor 1=HRAP 4= 1/4 HRAP
c  out: jerr   .... status code
c  =====================================================================
      subroutine ungrib2g (iupr,iud,ibug,nd5,ipack,mfld,fld,L3264B,
     +                     gridf,ier)
ccc      subroutine ungrib2 (iupr,igout,iud,ibug,
ccc     1             kyr,kmo,kda,khr,kmn, nyr,nmo,nda,nhr,nmn,
ccc     2             iocent,senof,mptver,lptver,lwcol,ncol,lsrow,nrow,
ccc     3             iresfl,rlav,rlov,iscan,iparm,modlid,nturef,itunit,
ccc     4             ntufc,ipkflg,inbmap,refval,ibinf,idecf,iwidth,
ccc     5             ibmap, mfld,fld, ichd,pid,cct,wmo,jerr)
c.......................................................................
c  This routine calls the GRIB2 unpacker routine, and outputs unpacked
c  data from GRIB2.
c
c.......................................................................
c  ungrib2 Initially written by
c       Tim Sweeney, HRL                                   May 1999
c
c  Adapted from ungrib2 by
c       David T. Miller, RSIS, OHD/HSEB                    Nov 2007
c       added a grid factor, gridf, to account for 1/4 
c       HRAP grid used by EMPE and used exclusively by new
c       gribit routine
c.......................................................................
c
      character*1 ipack(*)
      character*4 blnk,senof,pid(3),cct,wmo(2)
c
      real is0(16),is1(30),is2(1),is3(50),is4(20),is5(50),
     *     is6(250000),is7(250000)
      real fld(*)
      real*8 gridf
c
      dimension ibmap(250000)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C

      data blnk/ ' ' /
c
c
      call prbug ('ungrib2g',1,1,ibug)
c
      ns1 = 30
      ns2 = 1
      ns3 = 50
      ns4 = 20
      ns5 = 50
      ns6 = 1
      ns7 = 250000
c
ccc      if (ibug.gt.0) write (iud,*) nyr,nmo,nda,nhr,nmn
c
      jerr = 0
c
C ***********************************************************************
C *************** DEBUG - CALL UNPACKER...PRINT OUT INFO ****************
C ***********************************************************************
      write (IUD,20)
20    format (// 10X,'UNPACKER OUTPUT - GRIB2' //)
C
      call unpk_grib2(iupr,fld,nrow,ncol,is0,is1,ns1,is2,ns2,is3,ns3,
     +                is4,ns4,is5,ns5,is7,ns7,ipack,nd5,xmissx,xmissp,
     +                xmisss,new,minpk,ioctet,L3264B,end,ier)
      write (iud,30) ier
30    FORMAT (1X,'ERROR RETURN FLAG =',I4)
C
C ***********************************************************************
      write (iud,40)
40    format (/ ' SECTION 0 - INDICATOR SECTION' /)
c
C ***********************************************************************
      write (iud,50) is0(1),is0(7),is0(8)
50    format (' is0(1) name                             ',a4 /,
     +       ' is0(7) discipline, master table number  ',i4 /,
     +       '         (1 - hydrological products)',/
     +       ' is0(8) GRIB edition number              ',i4 /)
c
C ***********************************************************************
      write (iud,60)
60    format (/ ' SECTION 1 - IDENTIFICATION SECTION' /)
c
      iocent = is1(6)
      mptver = is1(10)
      lptver = is1(11)
      kyr = is1(12)
      kmo = is1(14)
      kda = is1(15)
      khr = is1(16)
      kmn = is1(17)
      ksc = is1(18)
c
      write (iud,70) ns1,is1(5),iocent,is1(8),mptver,lptver,kyr,kmo,
     +              kda,khr,kmn, is1(19),is1(20)
70    format (' ns1    length of section                ',i4 /,
     +       'is1(5)  number of section                ',f4.0 /,
     +       'iocent  originating center               ',i4 /,
     +       'is1(8)  sending office                   ',f4.0 /,
     +       'mptver  master tables version number     ',i4 /,
     +       'lptver  local tables version number      ',i4 /,
     +       'kyr     computation year                 ',i4 /,
     +       'kmo     computation month                ',i4 /,
     +       'kda     computation day                  ',i4 /,
     +       'khr     computation hour                 ',i4 /,
     +       'kmn     computation minute               ',i4,//,
     +       'is1(19) production status                ',f4.0 /,
     +       'is1(20) type of processed data (1=fcst)  ',f4.0 /)
c
C ***********************************************************************
      write (iud,80)
80    FORMAT (/ ' SECTION 3 - GRID DEFINITION SECTION' /)
c
c      write (iud,7310) (is3(i),i=1,ns3)
c 7310 format ( 5(1x,5(i8,2x) /) )
c
      rlat = is3(16)/1000000.
      rlon = -is3(20)/1000000.
      illgd = 1
      npair = 1
      call cvllgdg(rlon,rlat,npair,x,y,illgd,gridf,istat)
      lwcol = int(x + 0.05)
      lsrow = int(y + 0.05)
      lncol = is3(8)
      lnrow = is3(12)
      iresfl = is3(24)
      rlav   = is3(25)/1000000.
      rlov   = is3(29)/1000000.
      dx     = is3(33)/100000.
      dy     = is3(37)/100000.
      iscan  = is3(42)
c
      write (iud,90) ns3,is3(5),is3(6),lncol,lnrow,rlat,rlon,
     +              iresfl,rlav,rlov,dx,dy,is3(41),iscan
90    format (' ns3    length of section                ',i4 /,
     +       'is3(5)  number of section                ',f4.0 /,
     +       'is3(6)  grid definition template         ',f4.0 /,
     +       'lwcol   local west column                ',i5 /,
     +       'lsrow   local south row                  ',i5 /,
     +       'lncol   local number of columns          ',i5 /,
     +       'lnrow   local number of rows             ',i5 /,
     +       'rlat    latitude of first grid point     ',f10.6 /,
     +       'rlon    longitude of first grid point    ',f10.6 /,
     +       'iresfl  resolution & component flag      ',i4 /,
     +       'rlav    lat for Dx & Dy                  ',f10.6 /,
     +       'rlov    orientation of grid              ',f10.6 /,
     +       'dx      x-direction grid length          ',f10.4 /,
     +       'dy      y-direction grid length          ',f10.4 /,
     +       'is3(41) projection center flag           ',f4.0 /,
     +       'iscan   scanning mode                    ',i4 /)
c
C ***********************************************************************
      write (iud,100)
100   FORMAT (/ ' SECTION 4 - PRODUCT DEFINITION SECTION' /)
c
      write (iud,110) (is4(i),i=1,ns4)
110   format ( 5(1x,5(i8,2x) /) )
c
      iparm = is4(9)
      modlid = is4(11)
      nturef = is4(12)
      itunit = is4(15)
      ntufc = is4(16)
c
      write (iud,120) is4(1),is4(5),is4(6),is4(8),iparm,modlid,nturef,
     +               is4(14),itunit,ntufc,is4(18),is4(19)
120   format (' ns4    length of section 4              ',i4 /,
     +       ' is4(5)  number of section                ',f4.0 /,
     +       ' is4(6)  product def template number      ',f4.0 /,
     +       ' is4(8)  product discipline 1             ',f4.0 /,
     +       ' iparm   parameter number                 ',i4 /,
     +       ' modlid  fcst generating process id       ',i4 /,
     +       ' nturef  hours after reference time       ',i4 /,
     +       ' is4(14) minutes after ref time           ',f4.0 /,
     +       ' itunit  indicator of unit of time        ',i4 /,
     +       ' ntufc   fcst time in units               ',i4 /,
     +       ' is4(18) indicator type & unit of level   ',f4.0 /,
     +       ' is4(19) vertical coordinte value of level',f4.0 /)
c
C ***********************************************************************
      write (iud,130)
130   format (/ ' SECTION 5 - DATA REPRESENTATION SECTION' /)
      ipkflg = is5(6)
      inbmap = is5(7)
      refval = is5(8)
      ibinf = is5(12)
      idecf = is5(13)
      iwidth = is5(14)
      write (iud,140) ns5,is4(5),ipkflg,inbmap,refval,ibinf,idecf,iwidth
140   format (' ns5     length of section 5              ',i4 /,
     +       ' is4(5)  number of section                ',f4.0 /,
     +       ' ipkflg  data representation template num ',i4 /,
     +       ' inbmap  bit map indicator                ',i4 /,
     +       ' refval  reference value                  ',f8.2 /,
     +       ' ibinf   binary scale factor              ',f8.3 /,
     +       ' idecf   decimal scale factor             ',f5.2 /,
     +       ' iwidth field width for data              ',f5.0 //)
c
C ***********************************************************************
      if (inbmap.gt.0) goto 200
      write (iud,150)
150   FORMAT (/ ' SECTION 6 - BIT-MAP SECTION' /)
c
      write (iud,160) is6(1),is6(5)
160   format (' is6(1)  length of section 6              ',f4.0 /,
     +       ' is6(5)  number of section                ',f4.0 //)
c
      j = is6(1) - 5
      write (iud,170) j
170   format ( 2x,i7,' ELEMENTS    (north row first)' )
      do 190 krow=lnrow,1,-1
        is = (krow - 1)*lncol + 6
        ie = is + lncol - 1
        write (iud,180) (is6(j),j=is,ie)
180     format (1X,100I1)
190   CONTINUE
c
C ***********************************************************************
200   write (iud,210)
210   format (/ ' SECTION 7 - BINARY DATA SECTION' /)
c
      write (iud,220) is7(1),is7(5)
220   format (' is7(1)  length of section 7              ',i4 /,
     +        ' is7(5)  number of section                ',f4.0 //,
     +        ' north row first' /)
c
c  insert units label
      if (iparm.eq.191) then
         write (iud,230)
230   format (5x,'Total precip in kg/m2 (mm)')
      else if (iparm.eq.225) then
         write (iud,240)
240   format (5x,'1-hour flash flood guidance in kg/m2 (mm)')
      else if (iparm.eq.226) then
         write (iud,250)
250   format (5x,'3-hour flash flood guidance in kg/m2 (mm)')
      else if (iparm.eq.227) then
         write (iud,260)
260   format (5x,'6-hour flash flood guidance in kg/m2 (mm)')
      else if (iparm.eq.228) then
         write (iud,270)
270   format (5x,'12-hour flash flood guidance in kg/m2 (mm)')
      else if (iparm.eq.229) then
         write (iud,280)
280   format (5x,'24-hour flash flood guidance in kg/m2 (mm)')
      else if (iparm.eq.235) then
         write (iud,290)
290   format (5x,'Multisensor precipitation in kg/m2 x 100 ',
     +          '(mm x 100)' )
      endif
c
      j = is7(1) - 5
      do 310 krow=lnrow,1,-1
         is = (krow - 1)*lncol + 6
         ie = is + lncol - 1
         write (iud,300) (is7(j),j=is,ie)
300   format (10(1x,f7.2))
310      continue
c
320   return
c
      end
