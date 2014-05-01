c  =====================================================================
c  pgm:  prgrib (iupr,iud,ibug,fld,mkptr,kptr,mkpds,kpds,mkgds,
c                kgds,kbms,lwcol,lsrow,lncol,lnrow,ic)
c
c   in: iupr   .... unit number of output
c   in: ibug   .... debug control
c   in: fld    .... real array of unpacked values
c   in: mkptr  .... maximum size of array kptr
c   in: kptr   .... array containing pointers in GRIB encoded file
c   in: mkpds  .... maximum size of array kpds
c   in: kpds   .... array containing parameters in Product Definition
c                   Section
c   in: mkgds  .... maximum size of array kgds
c   in: kgds   .... array containing parameters in Grid Definition
c                   Section
c   in: kbms   .... array containing bitmaps
c   in: lwcol  .... most west HRAP column
c   in: lsrow  .... most south HRAP row
c   in: lncol  .... number of HRAP columns
c   in: lnrow  .... number of HRAP rows
c  i/o: ic     .... control code
c  =====================================================================
      subroutine prgrib(iupr,iud,ibug,fld,mkptr,kptr,mkpds,kpds,mkgds,
     +                  kgds,kbms,lwcol,lsrow,lncol,lnrow,ic)
c.......................................................................
c  This routine prints decoded GRIB parameters.
c
c.......................................................................
c  Initially derived from routine ungrib
c        Tim Sweeney, HRL                                   Jan 2001
c.......................................................................
c
      include 'tbl_pm'
      include 'grib_tbl'
c
      logical*1 kbms(*)
      character*8 sname
c
      real fld(*)
c
      integer kptr(mkptr)
      integer kpds(mkpds)
      integer kgds(mkgds)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/prgrib.f,v $
     . $',                                                             '
     .$Id: prgrib.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
c
      data sname / 'prgrib  ' /
c
      if (ibug.gt.0) write (iud,*) 'enter prgrib - ic=',ic
c
      if (ic.eq.20) go to 230
c
      write (iupr,10)
10    format (/ ' SECTION 1 - PRODUCT DEFINITION SECTION (PDS)' /)
      if (ibug.gt.0) write (iupr,20) kpds
20    format (' kpds:' / (1x,5(i8,2x)/))
      write (iupr,30) kpds
30    format (
     +   ' kpds(3)  originating center             = ',i4 /
     +   ' kpds(4)  model id (table A)             = ',i4 /
     +   ' kpds(5)  grid identification (table B)  = ',i4 /
     +   ' kpds(6)  GDS/BMS flag                   = ',i4 /
     +   ' kpds(8)  parameter & units              = ',i4 /
     +   ' kpds(9)  type of level (table 3)        = ',i4 /
     +   ' kpds(10) value of level                 = ',i4 /
     +   ' kpds(12) computation time, year         = ',i4 /
     +   ' kpds(13) comp time, month               = ',i4 /
     +   ' kpds(14) comp time, day                 = ',i4 /
     +   ' kpds(15) comp time, hour                = ',i4 /
     +   ' kpds(16) comp time, minutes             = ',i4 /
     +   ' kpds(17) forecast time unit (table 4)   = ',i4 /
     +   ' kpds(18) p1 period of time              = ',i4 /
     +   ' kpds(19) p2 period of time              = ',i4 /
     +   ' kpds(20) time range indicator (table 5) = ',i4 /
     +   ' kpds(21) number included in average     = ',i4 /
     +   '          edition no. of grib spec       = ',i4 /
     +   ' kpds(2)  version no. of parameter table = ',i4 /
     +   ' kpds(22) no. missing from avg/accum     = ',i4 /
     +   ' kpds(23) century                        = ',i4 /
     +   ' kpds(25) decimal scale factor           = ',i4 /
     +   ' kpds(24) sub-center (table C)           = ',i4 /
     +   ' reserved for future use                 = ',i4 /
     +   ' reserved for future use                 = ',i4)
c
      write (iupr,40)
40    format (/ ' SECTION 2 - GRID DESCRIPTION SECTION (GDS)' /)
      if (ibug.gt.0) write (iupr,50) kgds
50    format (' kgds:' / (1x,5(i8,2x)/))
      write (iupr,60) kgds
60    format (
     +   ' kgds(3)  projection grid (table 6)         = ',I8 /
     +   ' kgds(4)  number of points, x-axis          = ',I8 /
     +   ' kgds(5)  number of points, y-axis          = ',I8 /
     +   ' kgds(6)  latitude of origin (x 1000)       = ',I8 /
     +   ' kgds(7)  longitude of origin (x 1000)      = ',I8 /
     +   ' kgds(8)  resolution & component (table 7)  = ',I8 /
     +   ' kgds(9)  meridian parallel to y-axis       = ',I8 /
     +   ' kgds(10) x-direction grid length           = ',I8 /
     +   ' kgds(11) y-direction grid length           = ',I8 /
     +   ' kgds(12) projection center flag            = ',I8 /
     +   ' kgds(13) scanning mode flag (table 8)      = ',I8 /
     +   ' kgds(14) not used                          = ',I8 /
     +   ' kgds(15) lat secant cone intersect         = ',I8 /
     +   '                                            = ',I8 /
     +   '                                            = ',I8 /
     +   '                                            = ',I8 /
     +   '                                            = ',I8 /
     +   '                                            = ',I8 /
     +   '                                            = ',I8 /
     +   ' kgds(2) set at 255 user supplied grid      = ',I8)
c
      write (iupr,70) lwcol,lncol,lsrow,lnrow
70    format (/ ' Grid subset:' /
     +   5x,'west column       = ',i5 /
     +   5x,'number of columns = ',i5 /
     +   5x,'south row         = ',i5 /
     +   5x,'number of rows    = ',i5)
c
      if (ibug.gt.0) write (iupr,80) (kptr(i),i=1,20)
80    format (/' kptr:' / (1x,5(i8,2x)/))
      write (iupr,90) (kptr(i),i=1,15)
90    format (/
     +   ' kptr(1)  total length of GRIB message        = ',i7 /
     +   ' kptr(2)  length of indicator (section 0)     = ',i7 /
     +   ' kptr(3)  length of PDS       (section 1)     = ',i7 /
     +   ' kptr(4)  length of GDS       (section 2)     = ',i7 /
     +   ' kptr(5)  length of BMS       (section 3)     = ',i7 /
     +   ' kptr(6)  length of BDS       (section 4)     = ',i7 /
     +   ' kptr(7)  value of current byte               = ',i7 /
     +   ' kptr(8)  bit pointer                         = ',i7 /
     +   ' kptr(9)  GRIB start bit number               = ',i7 /
     +   ' kptr(10) GRIB/grid element count             = ',i7 /
     +   ' kptr(11) Number unused bits end of section 3 = ',i7 /
     +   ' kptr(12) bit map flag                        = ',i7 /
     +   ' kptr(13) number unused bits end of section 2 = ',i7 /
     +   ' kptr(14) BDS flags                           = ',i7 /
     +   ' kptr(15) number unused bits end of section 4 = ',i7)
c
      iall=1
      if (iall.eq.1) then
c     print all rows
         mprow=1
         else
c        print first numrow rows
            numrow=8
            mprow=lnrow-numrow
            write (iupr,100) numrow,lnrow
100   format (/ ' NOTE: printing only the first ',i2,' rows of ',i3,
     +   ' rows.')
         endif
c
      write (iupr,110)
110   format (/ ' SECTION 3 - BIT MAP SECTION (BMS)' /)
      write (iupr,120) 'BMS'
120   format (' ',a,' values (starting with north row)')
      if (kptr(5).gt.0) then
         do 140 krow=lnrow,mprow,-1
            is = (krow-1)*lncol + 1
            ie = is + lncol - 1
            if (ibug.gt.0) write (iud,*) 'in prgrib - is=',is,
     +         ' kbms(is)=',kbms(is)
            write (iupr,130) krow
130   format (' row=',i4,': ',$)
            icol=0
            do 137 j=is,ie
               ibms=0
               if (kbms(j).eqv..TRUE.) ibms=1
               write (iupr,135) ibms
135   format (i1,$)
               icol=icol+1
               nper=100
               if (icol.gt.nper) then
                  write (iupr,*)
                  write (iupr,136)
136   format (5x,4x,'  ',$)
                  icol=0
                  endif
137            continue
            write (iupr,*)
140         continue
         else
            write (iupr,150)
150   format (/ ' NOTE: No Bit Map Section.' )
         endif
c
      write (iupr,160)
160   format (/ ' SECTION 4 - BINARY DATA SECTION (BDS)' /)
c
c  insert process label
      write (iupr,170) kpds(2),tab_a(kpds(2)),kpds(19)
170   format ('   Process>  ',i4,':',a /
     +   '   Table Num>',i4)
c
c  insert units label
      if (kpds(19).eq.128) then
         write (iupr,180) kpds(5),abv128(kpds(5)),des128(kpds(5))
         else if (kpds(19).eq.2) then
            write (iupr,180) kpds(5),abv2(kpds(5)),des2(kpds(5))
         else
            write (iupr,180) kpds(5),tabpm(kpds(5)),tabdes(kpds(5))
180   format ('   Parameter>',i4,':',a,3x,a)
         endif
c
c  print data values
      write (iupr,*)
      write (iupr,120) 'BDS'
      nper=10
      do 220 krow=lnrow,mprow,-1
         is = (krow-1)*lncol + 1
         ie = is + lncol - 1
         iea = is + min(nper-1,lncol)
         iref = 1
         write (iupr,190) krow,iref,(fld(j),j=is,iea)
190   format (' row=',i4,' col=',i4,':',15(1x,f8.2) )
         if (lncol.gt.nper) then
            is = is + nper
            do 200 isb=is,ie,nper
               ieb = min((isb+nper-1),ie)
               iref = iref + nper
               write (iupr,210) iref,(fld(j),j=isb,ieb)
200            continue
210   format (5x,4x,' col=',i4,': ',15(f8.2,1x))
            endif
220      continue
c
230   return
c
      end
