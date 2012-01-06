      subroutine cnvgrib(gfilein, gfileout)
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  cnvgrib
C   PRGMMR: Gilbert        ORG: NP11        DATE: 2003-06-06
C
C ABSTRACT: This program converts every GRIB field in a file from
C   (1) GRIB1 to GRIB2   (2) GRIB2 to GRIB1  or (3) GRIB2 to GRIB2.
C
C PROGRAM HISTORY LOG:
C 2003-06-06  Gilbert
C 2008-05-14  Vuong    Added the option -m0 (No explicit missing values 
C                      included within the datavalues, modified the options
C                      and help messages
C
C USAGE:    CALL usage(gfilein, gfileout)
C   INPUT ARGUMENT LIST:
C     gfilein   - ouput option:
C                   1 = print description of arguments
C                   otherwise, print command usage summary
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$

      integer :: inver=0,outver=0,ipack=-1
      character(len=500) :: gfilein,gfileout,copt
      INTEGER(4) NARG,IARGC
      logical :: usemiss=.false., uvvect=.true.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      inver=1
      outver=2
      ipack=0
      uvvect=.false.
      imiss=0
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Open input and output grib files
!
      IFL1=10
      IFL2=50
      NCGB=LEN_TRIM(gfilein)
      CALL BAOPENR(ifl1,gfilein(1:NCGB),IOS)
      if (IOS.NE.0) then
         call errmsg('cnvgrib: cannot open input GRIB file '//
     &               gfilein(1:NCGB))
         call errexit(3)
      endif
      NCGB=LEN_TRIM(gfileout)
      CALL BAOPENW(ifl2,gfileout(1:NCGB),IOS)
      if (IOS.NE.0) then
         call errmsg('cnvgrib: cannot open output GRIB file '//
     &               gfileout(1:NCGB))
         call errexit(4)
      endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  convert grib file
!
      if ((inver.eq.1).AND.(outver.eq.2)) then
         call cnv12(ifl1,ifl2,ipack,usemiss,imiss,uvvect)
      elseif ((inver.eq.2).AND.(outver.eq.1)) then
         call cnv21(ifl1,ifl2)
      elseif ((inver.eq.2).AND.(outver.eq.2)) then
         call cnv22(ifl1,ifl2,ipack,usemiss,imiss)
      else
         print *,' Unknown conversion option.'
         call errexit(5)
      endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  close grib files
!
      CALL BACLOSE(ifl1,IOS)
      CALL BACLOSE(ifl2,IOS)

      end

      subroutine usage(iopt)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    usage
C   PRGMMR: Gilbert     ORG: W/NP11    DATE: 2003-06-06
C
C ABSTRACT: This routine prints out the command "usage" 
C   or a brief description of the command line options.
C
C PROGRAM HISTORY LOG:
C 2003-06-06  Gilbert
C 2007-04-25  Vuong   -  Changed the cnvgrib_ver
C 2008-08-12  Vuong   -  Changed the cnvgrib_ver
C
C USAGE:    CALL usage(iopt)
C   INPUT ARGUMENT LIST:
C     iopt   - ouput option:
C                   1 = print description of arguments
C                   otherwise, print command usage summary
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
         character(len=15) :: cnvgrib_ver="cnvgrib-1.1.8"
         integer,intent(in) :: iopt 

         if ( iopt.eq.0 ) then
         call errmsg ('  ')
         call errmsg('Usage: cnvgrib [-h] {-g12|-g21|-g22} [-m|-m0]'//
     &                             ' [-nv]')
         call errmsg('               [{-p0|-p2|-p31|-p32|-p40'//
     &               '|-p41}]  ingribfile   outgribfile')
         call errmsg ('  ')
         call errmsg('Usage: cnvgrib  -h  For helps and shows all'//
     &                                  ' options') 
         call errmsg ('  ')
         endif

         if ( iopt.eq.1 ) then
            call errmsg ('  ')
            call errmsg('cnvgrib:  version '//cnvgrib_ver)
            call errmsg ('  ')
            call errmsg('Must use one of the following options:')
            call errmsg('   -g12     converts GRIB1 to GRIB2')
            call errmsg('   -g21     converts GRIB2 to GRIB1')
            call errmsg('   -g22     converts GRIB2 to GRIB2 '//
     &                  ' (used to change packing option)')
            call errmsg ('  ')
            call errmsg('Optional packing options: (for use with '//
     &                   ' -g12 and -g22 only)')
            call errmsg('   -p0      simple packing')
            call errmsg('   -p2      complex packing')
            call errmsg('   -p31     complex pack with 1st order diffs')
            call errmsg('   -p32     complex pack with 2nd order diffs')
            call errmsg('   -p40     JPEG2000 encoding')
            call errmsg('   -p41     PNG encoding')
            call errmsg ('  ')
            call errmsg('Other Optional options: ')
         call errmsg('   -nv      Do not combine U, V wind components')
         call errmsg ('  ')
         call errmsg('   Use missing value management'//
     &                 ' instead of bitmap')
         call errmsg('   (ONLY valid with Complex Packing options:'//
     &               ' -p2, -p31 or -p32 )')
         call errmsg ('  ')
         call errmsg('   -m      Primary missing values'//
     &               ' included within the data values')
         call errmsg('   -m0     No explicit missing values'//
     &               ' included within the data values')
         call errmsg ('  ')
         endif
      return
      end
