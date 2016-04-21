C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (UDATL)
C-----------------------------------------------------------------------
C      PROGRAM COPYGB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: COPYGB       COMPARE GRIB FILES
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-10-22
C
C ABSTRACT: The command copygb copies all or part of one GRIB file
C   to another GRIB file, interpolating if necessary.  Unless
C   otherwise directed (-x option), the GRIB index file is also used
C   to speed the reading. The fields are interpolated to an output grid
C   if specified (-g option). The interpolation type defaults to
C   bilinear but may be specified directly (-i option).  The copying
C   may be limited to specific fields (-k option). It may also be
C   limited to a specified subgrid of the output grid or to a subrange
C   of the input fields (-B and -b, -A, and -K options). Fields can be
C   identified as scalars or vectors (-v option), which are interpolated
C   differently.  The invalid data in the output field can be filled
C   with mask values or merged with a merge field (-M and -m options).
C   The output GRIB message can also be appended to a file (-a option).
C   Some defaults can be overridden in a namelist file (-N option).
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C   97-03-05  IREDELL  CORRECTED THE COPYING OF THE V-WIND FIELD
C                      WHEN NO INTERPOLATION IS DONE
C   97-03-18  IREDELL  INCREASED VERBOSITY
C 1998-09-03  IREDELL  INSTRUMENTED AND MADE PLATFORM-INDEPENDENT
C 1999-10-05  IREDELL  ADDED APPEND OPTION AND WGRIB REQUEST OPTION
C 1999-10-06  IREDELL  ADDED MERGE OPTION
C 2000-01-19  IREDELL  ADDED NAMELIST OPTION
C 2001-03-16  IREDELL  ADDED ENSEMBLE EXTENSION OPTION
C 2002-01-10  IREDELL  CORRECTED V-WIND SEARCH TO INCLUDE SUBCENTER
C 2006-02-07  GILBERT  CHANGED V-WIND SEARCH TO EXCLUDE DECIMAL SCALE FACTOR
C
C COMMAND LINE OPTIONS:
C   -a
C      Appends rather than overwrites the output GRIB file.
C
C   -A "<> mapthreshold"
C      Inequality and threshold used in determining
C      where on the map the data will be copied.
C      The data are copied only where the given 
C      map field is on the correct side of the threshold.
C      The mapthreshold defaults to '>-1.e30'; in this case,
C      only the map field's bitmap will limit the domain.
C
C   -b mapindex   
C      Optional index file used to get the map field.
C
C   -B mapgrib    
C      GRIB file used to get the map field.  The map field
C      is read from the GRIB file and compared to the
C      map threshold to determine for which region on the map
C      the data will be copied.  The mapgrib can be the name
C      of an actual GRIB file (in which case the index
C      file may be specified with the -b option) or it can
C      be '-1'.  If mapgrib is '-1', then the input GRIB file
C      (first positional argument) is used.
C      The -K option specifies which field to read from
C      the mapgrib GRIB file.  If mapgrib is an actual file,
C      then the first field is taken if -K is not specified.
C      On the other hand, if mapgrib is '-1', then if the
C      if -K is not specified, the current field is taken
C      as the map field.  A special exception is if -K '-1'
C      is specified, in which case the current field is
C      taken as the map field and it is applied before any
C      interpolation; otherwise the map field is always
C      applied after interpolation.
C
C   -g "grid [kgds]"
C      Output grid identification.  If grid=-1 (the default),
C      then the output grid is the same as the input grid.
C      If grid=-4, then the grid is that of the map field.
C      If grid=-5, then the grid is that of the merge field.
C      If 0<grid<255, then grid designates an NCEP grid.
C      If grid=255, then the grid must be specified by the
C      full set of kgds parameters determining a GRIB GDS
C      (grid description section) in the W3FI63 format.
C
C   -i "ip [ipopts]"
C      Interpolation options.  The default is bilinear
C      interpolation (ip=0).  Other interpolation options
C      are bicubic (ip=1), neighbor (ip=2), budget (ip=3),
C      and spectral (ip=4).  Spectral interpolation is forced
C      even if the input and output grids are the same.
C      See the documentation for iplib for further details.
C
C   -k "kpds"
C      Full set of kpds parameters determing a GRIB PDS
C      (product definition section) in the W3FI63 format
C      determining the field(s) to be copied.  Note that
C      kpds(5) is the parameter indicator (PDS octet 9).
C      A wildcard is specified by -1 (the defaults).
C      If "kpds" is equal to "w", then the requested record
C      numbers are read in from standard input instead,
C      one record per line and possibly delimited by a colon
C      (so that wgrib could pipe requests to copygb).
C      If the -k is not specified, then copygb will attempt
C      to copy every field in the input GRIB file.
C
C   -K "mapkpds"
C      Full set of kpds parameters determing a GRIB PDS
C      (product definition section) in the W3FI63 format
C      determining the map field to be used to determine
C      where on the map the data will be copied.  
C      A wildcard is specified by -1 (the defaults).
C
C   -m mergeindex   
C      Optional index file used to get the merge field.
C
C   -M "mask"/mergegrib
C      Mask used to fill out bitmapped areas of the map.
C      If specified, there will be no bitmap in the output.
C      The mask must be in the format '#value' where value
C      is the real number used to fill out the field.
C      Otherwise, the argument is interpreted as a merge
C      GRIB file.  Then for each GRIB message copied,
C      a merge field is found in the merge GRIB file
C      with the same parameter and level indicators
C      as the copied field.  This merge field is interpolated
C      to the output grid and used to fill out the bitmapped
C      areas of the map, at least where the merge field
C      is not bitmapped.  No merging is done if no merge
C      field is found.
C
C   -N namelist
C      Namelist file to override default output options.
C      The namelist must start with " &NLCOPYGB" and end with "/".
C      Namelist variables are
C        IDS(255)      Output decimal scaling by parameter
C        IBS(255)      Output binary scaling by parameter
C        NBS(255)      Output number of bits by parameter
C      
C   -v "uparms"
C      Parameter indicator(s) for the u-component of vectors.
C      The parameter indicator for the v-component is assumed
C      to be one more than that of the u-component.
C      If the -v option is not specified, then the wind
C      components (parameters 33 and 34) are the only fields
C      assumed to be vector components in the GRIB file.
C
C   -x
C      Turns off the use of an index file.  The index records
C      are then extracted from the GRIB file, which
C      will increase the time taken by copygb.
C
C   -X
C      Turns on verbose printout.  This option is
C      incompatible with GRIB output to standard output.
C
C INPUT FILES:
C   UNIT   11    INPUT GRIB FILE
C   UNIT   14    MAP GRIB FILE
C   UNIT   15    MERGE GRIB FILE
C   UNIT   31    INPUT GRIB INDEX FILE
C   UNIT   34    MAP GRIB INDEX FILE
C   UNIT   35    MERGE GRIB INDEX FILE
C
C OUTPUT FILES:
C   UNIT   51    OUTPUT GRIB FILE
C
C SUBPROGRAMS CALLED:
C   IARGC
C   GETARG
C   ERRMSG
C   EUSAGE
C   ERREXIT
C   FPARSEI
C   FPARSER
C   BAOPENR
C   BAOPENWT
C   BAOPENWA
C   CPGB
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$

      subroutine copygb_main
      
      CHARACTER*256 CARG,CG1,CX1,CGB,CXB,CGM,CXM,CG2,CNL
      INTEGER KARG(100)
      INTEGER KGDSI(200),IPOPT(20),JPDS1(200),JPDSB(200),IUV(100)
      CHARACTER*400 GDS
      DATA IGI/-1/,KGDSI/19*0,255,180*0/
      DATA IP/0/,IPOPT/20*-1/
      DATA JPDS1/200*-1/,JPDSB/200*-1/,IUV/33,99*0/,NUV/1/
      DATA LWG/0/,LAPP/0/,LXX/0/,LX/1/,KZ1/-1/,KZ2/-2/
      DATA JB/0/,JBK/0/,LAB/1/,AB/-1.E30/,LAM/0/,AM/0./
      DATA CGB/' '/,CXB/' '/,CGM/' '/,CXM/' '/,CNL/' '/
      INTEGER IDS(255),IBS(255),NBS(255)
      NAMELIST/NLCOPYGB/ IDS,IBS,NBS
      DATA IDS/255*-9999/,IBS/255*-9999/,NBS/255*-9999/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PARSE COMMAND LINE OPTIONS
      NARG=IARGC()
      IARG=1
      LSTOPT=0
      DOWHILE(IARG.LE.NARG.AND.LSTOPT.EQ.0)
        CALL GETARG(IARG,CARG)
        LARG=LEN_TRIM(CARG)
        IARG=IARG+1
        IF(CARG(1:1).NE.'-') THEN
          LSTOPT=1
          IARG=IARG-1
        ELSEIF(LARG.EQ.1) THEN
          CALL ERRMSG('copygb: invalid option -')
          CALL EUSAGE
          CALL ERREXIT(1)
        ELSE
          L=2
          DOWHILE(L.LE.LARG)
            IF(CARG(L:L).EQ.'-') THEN
              LSTOPT=1
            ELSEIF(CARG(L:L).EQ.'a') THEN
              LAPP=1
            ELSEIF(CARG(L:L).EQ.'A') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              IF(CARG(L+1:L+1).EQ.'>') THEN
                LAB=1
                L=L+1
              ELSEIF(CARG(L+1:L+1).EQ.'<') THEN
                LAB=-1
                L=L+1
              ELSE
                CALL ERRMSG('copygb: invalid threshold '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              CALL FPARSER(CARG(L+1:LARG),1,AB)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'B') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCGB=LARG-L
              CGB=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'b') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCXB=LARG-L
              CXB=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'g') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              KARG(1)=IGI
              KARG(2:100)=KGDSI(1:99)
              CALL FPARSEI(CARG(L+1:LARG),100,KARG)
              IGI=KARG(1)
              IF(IGI.GT.0.AND.IGI.LT.255) THEN
                CALL MAKGDS(IGI,KGDSI,GDS,LGDS,IRET)
                IF(IRET.NE.0) IGI=-1
              ELSEIF(IGI.EQ.255) THEN
                KGDSI(1:99)=KARG(2:100)
              ENDIF
              IF(IGI.LT.-4.OR.IGI.EQ.0.OR.IGI.GT.255) THEN
                CALL ERRMSG('copygb: invalid output grid '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              MI=LENGDS(KGDSI)
              IF(MI.LE.0) THEN
                CALL ERRMSG('copygb: unsupported output grid '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'i') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              KARG(1)=IP
              KARG(2:21)=IPOPT
              CALL FPARSEI(CARG(L+1:LARG),21,KARG)
              IP=KARG(1)
              IPOPT=KARG(2:21)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'K') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              JBK=1
              CALL FPARSEI(CARG(L+1:LARG),100,JPDSB)
              IF(JPDSB(5).EQ.0) THEN
                CALL ERRMSG('copygb: invalid PDS parms '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'k') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              IF(CARG(L+1:LARG).EQ.'w') THEN
                LWG=1
              ELSE
                CALL FPARSEI(CARG(L+1:LARG),100,JPDS1)
                IF(JPDS1(5).EQ.0) THEN
                  CALL ERRMSG('copygb: invalid PDS parms '//
     &                        CARG(L+1:LARG))
                  CALL EUSAGE
                  CALL ERREXIT(1)
                ENDIF
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'M') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              IF(CARG(L+1:L+1).EQ.'#') THEN
                L=L+1
                CALL FPARSER(CARG(L+1:LARG),1,AM)
                LAM=1
              ELSE
                LCGM=LARG-L
                CGM=CARG(L+1:LARG)
                LAM=5
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'m') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCXM=LARG-L
              CXM=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'N') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCNL=LARG-L
              CNL=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'v') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              CALL FPARSEI(CARG(L+1:LARG),100,IUV)
              NUV=1
              DO JUV=2,100
                IF(IUV(JUV).NE.0) NUV=JUV
              ENDDO
              L=LARG
            ELSEIF(CARG(L:L).EQ.'x') THEN
              LX=0
            ELSEIF(CARG(L:L).EQ.'X') THEN
              LXX=1
            ELSE
              CALL ERRMSG('copygb: invalid option '//CARG(L:L))
              CALL EUSAGE
              CALL ERREXIT(1)
            ENDIF
            L=L+1
          ENDDO
        ENDIF
      ENDDO

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PARSE COMMAND LINE POSITIONAL ARGUMENTS
      NXARG=LX+2
      IF(NARG-IARG+1.NE.NXARG) THEN
        CALL ERRMSG('copygb: incorrect number of arguments')
        CALL EUSAGE
        CALL ERREXIT(NXARG)
      ENDIF
      CALL GETARG(IARG,CG1)
      LCG1=LEN_TRIM(CG1)
      IARG=IARG+1
      LG1=11
      CALL BAOPENR(LG1,CG1(1:LCG1),IRETBA)
      IF(IRETBA.NE.0) THEN
        CALL ERRMSG('copygb:  error accessing file '//CG1(1:LCG1))
        CALL ERREXIT(8)
      ENDIF
      IF(LX.GT.0) THEN
        CALL GETARG(IARG,CX1)
        LCX1=LEN_TRIM(CX1)
        IARG=IARG+1
        LX1=31
        CALL BAOPENR(LX1,CX1(1:LCX1),IRETBA)
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('copygb:  error accessing file '//CX1(1:LCX1))
          CALL ERREXIT(8)
        ENDIF
      ELSE
        LX1=0
      ENDIF
      CALL GETARG(IARG,CG2)
      LCG2=LEN_TRIM(CG2)
      IARG=IARG+1
      IF(CG2(1:LCG2).EQ.'-') THEN
        IF(LXX.GT.0) THEN
          CALL ERRMSG('copygb:  piping incompatible with the X option')
          CALL ERREXIT(1)
        ENDIF
        LG2=6
      ELSE
        LG2=51
        IF(LAPP.EQ.0) THEN
          CALL BAOPENWT(LG2,CG2(1:LCG2),IRETBA)
        ELSE
          CALL BAOPENWA(LG2,CG2(1:LCG2),IRETBA)
        ENDIF
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('copygb:  error accessing file '//CG2(1:LCG2))
          CALL ERREXIT(8)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN MAP FILE
      IF(CGB.NE.' ') THEN
        IF(CGB(1:2).EQ.'-1') THEN
          IF(JPDSB(5).EQ.-1) THEN
            JB=1
          ELSE
            JB=4
            LGB=LG1
            LXB=LX1
          ENDIF
        ELSE
          JB=4
          LGB=14
          CALL BAOPENR(LGB,CGB(1:LCGB),IRETBA)
          IF(IRETBA.NE.0) THEN
            CALL ERRMSG('copygb:  error accessing file '//CGB(1:LCGB))
            CALL ERREXIT(8)
          ENDIF
          IF(CXB(1:1).NE.' ') THEN
            LXB=34
            CALL BAOPENR(LXB,CXB(1:LCXB),IRETBA)
            IF(IRETBA.NE.0) THEN
              CALL ERRMSG('copygb:  error accessing file '//CXB(1:LCXB))
              CALL ERREXIT(8)
            ENDIF
          ELSE
            LXB=0
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN MERGE FILE
      IF(CGM.NE.' ') THEN
        LAM=5
        LGM=15
        CALL BAOPENR(LGM,CGM(1:LCGM),IRETBA)
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('copygb:  error accessing file '//CGM(1:LCGM))
          CALL ERREXIT(8)
        ENDIF
        IF(CXM(1:1).NE.' ') THEN
          LXM=35
          CALL BAOPENR(LXM,CXM(1:LCXM),IRETBA)
          IF(IRETBA.NE.0) THEN
            CALL ERRMSG('copygb:  error accessing file '//CXM(1:LCXM))
            CALL ERREXIT(8)
          ENDIF
        ELSE
          LXM=0
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN AND READ NAMELIST FILE
      IF(CNL.NE.' ') THEN
        LNL=2
        OPEN(LNL,FILE=CNL(1:LCNL),STATUS='OLD',IOSTAT=IRET)
        IF(IRET.NE.0) THEN
          CALL ERRMSG('copygb:  error accessing file '//CNL(1:LCNL))
          CALL ERREXIT(8)
        ENDIF
        READ(LNL,NLCOPYGB,IOSTAT=IRET)
        IF(IRET.NE.0) THEN
          CALL ERRMSG('copygb:  error reading namelist from file '//
     &                CNL(1:LCNL))
          CALL ERREXIT(8)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GO
      IF(LXX.GT.0) THEN
        CALL W3TAGB('COPYGB  ',1998,0295,0047,'NP23   ')
      ENDIF
      CALL CPGB(LG1,LX1,LGB,LXB,LGM,LXM,LG2,
     &          IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
     &          JPDSB,JB,JBK,LAB,AB,LAM,AM,LXX,LWG,
     &          IDS,IBS,NBS)
      IF(LXX.GT.0) THEN
        CALL W3TAGE('COPYGB  ')
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE EUSAGE
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    EUSAGE      PRINT PROPER USAGE TO STDERR
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: PRINT PROPER USAGE TO STDERR.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL EUSAGE
C
C SUBPROGRAMS CALLED:
C   ERRMSG
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL ERRMSG('Usage: copygb'//
     & ' [-g "grid [kgds]"] [-i "ip [ipopts]"]'//
     & ' [-k "kpds"] [-v "uparms"]')
      CALL ERRMSG('             '//
     & ' [-B mapgrib [-b mapindex] [-A "<> mapthreshold"]'//
     & ' [-K "mapkpds"]]')
      CALL ERRMSG('             '//
     & ' [-M "mask"/mergegrib [-m mergeindex]] [-X] [-a]'//
     & ' [-N namelist]')
      CALL ERRMSG('       then either:')
      CALL ERRMSG('             '//
     & ' grib1 index1 grib2')
      CALL ERRMSG('            or:')
      CALL ERRMSG('             '//
     & ' -x grib1 grib2')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE CPGB(LG1,LX1,LGB,LXB,LGM,LXM,LG2,
     &                IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
     &                JPDSB,JB,JBK,LAB,AB,LAM,AM,LXX,LWG,
     &                IDS,IBS,NBS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CPGB        COPY GRIB FILES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: COPY GRIB FILES.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL CPGB(LG1,LX1,LGB,LXB,LGM,LXM,LG2,
C    &                IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
C    &                JPDSB,JB,JBK,LAB,AB,LAM,AM,LXX,LWG,
C    &                IDS,IBS,NBS)
C   INPUT ARGUMENTS:
C     LG1          INTEGER UNIT NUMBER FOR GRIB FILE 1
C     LX1          INTEGER UNIT NUMBER FOR GRIB INDEX FILE 1
C     LGB          INTEGER UNIT NUMBER FOR GRIB FILE MAP
C     LXB          INTEGER UNIT NUMBER FOR GRIB INDEX FILE MAP
C     LGM          INTEGER UNIT NUMBER FOR GRIB FILE MERGE
C     LXM          INTEGER UNIT NUMBER FOR GRIB INDEX FILE MERGE
C     LG2          INTEGER UNIT NUMBER FOR GRIB FILE 2
C     IGI          INTEGER OUTPUT GRID IDENTIFICATION
C     KGDSI        INTEGER (200) OUTPUT GRID PARAMETERS
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     JPDS1        INTEGER (100) KPDS SEARCH OPTIONS
C     NUV          INTEGER NUMBER OF VECTOR PARAMETER IDS
C     IUV          INTEGER (100) VECTOR PARAMETER IDS
C     JPDSB        INTEGER (100) KPDS SEARCH OPTIONS (MAP)
C     JB           INTEGER FLAG FOR MAP OPTIION
C     JBK          INTEGER FLAG FOR MAP OPTIION
C     LAB          INTEGER FLAG FOR MAP THRESHOLD INEQUALITY
C     AB           REAL MAP THRESHOLD
C     LAM          INTEGER FLAG FOR MASK VALUE
C     AM           REAL MASK VALUE
C     LXX          INTEGER FLAG FOR VERBOSE OUTPUT
C     LWG          INTEGER FLAG FOR STDIN SELECTION
C     IDS          INTEGER (255) DECIMAL SCALING (-9999 FOR NO CHANGE)
C     IBS          INTEGER (255) BINARY SCALING (-9999 FOR NO CHANGE)
C     NBS          INTEGER (255) NUMBER OF BITS (-9999 FOR NO CHANGE)
C
C SUBPROGRAMS CALLED:
C   GETGBEMH
C   CPGB1  
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      PARAMETER(MBUF=256*1024)
      CHARACTER CBUF1(MBUF),CBUFB(MBUF),CBUFM(MBUF)
      INTEGER JPDS1(200),JPDSB(200),IUV(100)
      INTEGER KGDSI(200)
      INTEGER IPOPT(20)
      INTEGER IDS(255),IBS(255),NBS(255)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS1(200),KGDS1(200),KENS1(200)
      INTEGER KPDSB(200),KGDSB(200),KENSB(200)
      INTEGER KPDSM(200),KGDSM(200),KENSM(200)
      CHARACTER*80 CIN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ GRIB HEADERS
      IF(LXX.GT.0) CALL INSTRUMENT(6,KALL0,TTOT0,TMIN0,TMAX0)
      IF(JB.EQ.4) THEN
        JGDS=-1
        JENS=-1
        KRB=-1
        KPDSB=0
        KGDSB=0
        CALL GETGBEMH(LGB,LXB,KRB,JPDSB,JGDS,JENS,
     &                MBUF,CBUFB,NLENB,NNUMB,MNUMB,
     &                KB,MB,KRBX,KPDSB,KGDSB,KENSB,IRET)
        IF(IRET.EQ.0.AND.MB.LE.0) IRET=255
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.99) THEN
            PRINT *,'copygb map field not found'
          ELSEIF(IRET.NE.0) THEN
            PRINT *,'copygb map field retrieval error code ',IRET
          ENDIF
        ENDIF
      ELSE
        MB=1
        IRET=0
      ENDIF
      IF(IRET.EQ.0) THEN
        KR1=-1
        IF(LWG.EQ.1) THEN
          READ (*,*,IOSTAT=IRET) CIN
          IF(IRET.EQ.0) THEN
            NDEL=SCAN(CIN,":")
            IF(NDEL.GT.0) CIN=CIN(:NDEL-1)
            READ(CIN,*) KR1
            KR1=-KR1
          ENDIF
        ENDIF
        IF(IRET.EQ.0) THEN
          JGDS=-1
          JENS=-1
          KPDS1=0
          KGDS1=0
          CALL GETGBEMH(LG1,LX1,KR1,JPDS1,JGDS,JENS,
     &                  MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &                  K1,M1,KR1X,KPDS1,KGDS1,KENS1,IRET)
          IF(IRET.EQ.0.AND.M1.LE.0) IRET=255
          KR1=KR1X
          IF(LXX.GT.0) THEN
            IF(IRET.EQ.99) THEN
              PRINT *,'copygb field not found'
            ELSEIF(IRET.NE.0) THEN
              PRINT *,'copygb header retrieval error code ',IRET
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOOP UNTIL DONE
      NO=0
      DOWHILE(IRET.EQ.0)
        mm = 0
        IF(LAM.EQ.5) THEN
          JPDS=-1
          JPDS(5:7)=KPDS1(5:7)
          JGDS=-1
          JENS=-1
          KRM=-1
          KPDSM=0
          KGDSM=0
          CALL GETGBEMH(LGM,LXM,KRM,JPDS,JGDS,JENS,
     &                  MBUF,CBUFM,NLENM,NNUMM,MNUMM,
     &                  KM,MM,KRMX,KPDSM,KGDSM,KENSM,IRET)
          IF(IRET.EQ.0.AND.MM.LE.0) IRET=255
          IF(IRET.NE.0) THEN
            MM=0
            KPDSM=0
            KGDSM=0
            IRET=0
          ENDIF
        ENDIF
        IF(IGI.EQ.-1) THEN
          IGI=KPDS1(3)
          KGDSI=KGDS1
          MI=M1
        ELSEIF(IGI.EQ.-4.AND.JB.EQ.4) THEN
          IGI=KPDSB(3)
          KGDSI=KGDSB
          MI=MB
        ELSEIF(IGI.EQ.-5.AND.LAM.EQ.5) THEN
          IGI=KPDSM(3)
          KGDSI=KGDSM
          MI=MM
        ELSE
          MI=LENGDS(KGDSI)
        ENDIF
        IF(LXX.GT.0) CALL INSTRUMENT(1,KALL1,TTOT1,TMIN1,TMAX1)
        IF(IGI.GT.0.AND.IGI.LE.255) THEN
	
          MF=MAX(M1,MB,MM)
          CALL CPGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
     &               MBUF,MF,MI,
     &               IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
     &               JPDSB,JB,JBK,LAB,AB,LAM,AM,
     &               IDS,IBS,NBS,
     &               LGB,LXB,MB,CBUFB,NLENB,NNUMB,MNUMB,
     &               LGM,LXM,MM,CBUFM,NLENM,NNUMM,MNUMM,
     &               LG2,LXX,KR1-1,NO,IRET1)
        ENDIF
        IF(LWG.EQ.1) THEN
          READ (*,*,IOSTAT=IRET) CIN
          IF(IRET.EQ.0) THEN
            NDEL=SCAN(CIN,":")
            IF(NDEL.GT.0) CIN=CIN(:NDEL-1)
            READ(CIN,*) KR1
            KR1=KR1-1
          ENDIF
        ENDIF
        IF(IRET.EQ.0) THEN
          JGDS=-1
          JENS=-1
          KPDS1=0
          KGDS1=0
          CALL GETGBEMH(LG1,LX1,KR1,JPDS1,JGDS,JENS,
     &                  MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &                  K1,M1,KR1X,KPDS1,KGDS1,KENS1,IRET)
          IF(IRET.EQ.0.AND.M1.LE.0) IRET=255
          KR1=KR1X
          IF(LXX.GT.0) THEN
            IF(IRET.NE.0.AND.IRET.NE.99) THEN
              PRINT *,'copygb header retrieval error code ',IRET
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LXX.GT.0) THEN
        PRINT *,'copygb wrote ',NO,' total records'
C        CALL INSTRUMENT(1,KALL1,TTOT1,TMIN1,TMAX1)
C        PRINT *,'Instrumentation Report'
C        PRINT '(F10.3," seconds spent searching headers")',TTOT1
C        CALL INSTRUMENT(-2,KALL2,TTOT2,TMIN2,TMAX2)
C        PRINT '(F10.3," seconds spent reading and unpacking")',TTOT2
C        CALL INSTRUMENT(-3,KALL3,TTOT3,TMIN3,TMAX3)
C        PRINT '(F10.3," seconds spent manipulating masks")',TTOT3
C        CALL INSTRUMENT(-4,KALL4,TTOT4,TMIN4,TMAX4)
C        PRINT '(F10.3," seconds spent interpolating or copying")',TTOT4
C        CALL INSTRUMENT(-5,KALL5,TTOT5,TMIN5,TMAX5)
C        PRINT '(F10.3," seconds spent merging")',TTOT5
C        CALL INSTRUMENT(-6,KALL6,TTOT6,TMIN6,TMAX6)
C        PRINT '(F10.3," seconds spent packing and writing")',TTOT6
C        TTOTT=TTOT1+TTOT2+TTOT3+TTOT4+TTOT5+TTOT6
C        PRINT '(F10.3," total seconds spent in copygb")',TTOTT
      ENDIF
	return
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE CPGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
     &                 MBUF,MF,MI,
     &                 IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
     &                 JPDSB,JB,JBK,LAB,AB,LAM,AM,
     &                 IDS,IBS,NBS,
     &                 LGB,LXB,MB,CBUFB,NLENB,NNUMB,MNUMB,
     &                 LGM,LXM,MM,CBUFM,NLENM,NNUMM,MNUMM,
     &                 LG2,LXX,KS1,NO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CPGB1       COPY ONE GRIB FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: COPY ONE GRIB FIELD.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL CPGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
C    &                 MBUF,MF,MI,
C    &                 IGI,KGDSI,IP,IPOPT,JPDS1,NUV,IUV,
C    &                 JPDSB,JB,JBK,LAB,AB,LAM,AM,
C    &                 IDS,IBS,NBS,
C    &                 LGB,LXB,MB,CBUFB,NLENB,NNUMB,MNUMB,
C    &                 LGM,LXM,MM,CBUFM,NLENM,NNUMM,MNUMM,
C    &                 LG2,LXX,KS1,NO,IRET)
C   INPUT ARGUMENTS:
C     LG1          INTEGER UNIT NUMBER FOR GRIB FILE 1
C     LX1          INTEGER UNIT NUMBER FOR GRIB INDEX FILE 1
C     M1           INTEGER DIMENSION OF GRIB FIELD 1
C     CBUF1        CHARACTER (MBUF) INDEX BUFFER 1
C     NLEN1        INTEGER RECORD LENGTH OF INDEX BUFFER 1
C     NNUM1        INTEGER NUMBER OF RECORDS IN INDEX BUFFER 1
C     NLEN1        INTEGER LENGTH OF EACH INDEX RECORD 1
C     NNUM1        INTEGER NUMBER OF INDEX RECORDS 1
C     MNUM1        INTEGER NUMBER OF INDEX RECORDS 1 SKIPPED
C     MBUF         INTEGER DIMENSION OF INDEX BUFFERS
C     MF           INTEGER DIMENSION OF FIELD
C     MI           INTEGER DIMENSION OF OUTPUT GRID
C     IGI          INTEGER OUTPUT GRID IDENTIFICATION
C     KGDSI        INTEGER (200) OUTPUT GRID PARAMETERS
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     JPDS1        INTEGER (100) KPDS SEARCH OPTIONS
C     NUV          INTEGER NUMBER OF VECTOR PARAMETER IDS
C     IUV          INTEGER (100) VECTOR PARAMETER IDS
C     JPDSB        INTEGER (100) KPDS SEARCH OPTIONS (MAP)
C     JB           INTEGER FLAG FOR MAP OPTIION
C     JBK          INTEGER FLAG FOR MAP OPTIION
C     LAB          INTEGER FLAG FOR MAP THRESHOLD INEQUALITY
C     AB           REAL MAP THRESHOLD
C     LAM          INTEGER FLAG FOR MASK VALUE
C     AM           REAL MASK VALUE
C     IDS          INTEGER (255) DECIMAL SCALING (-9999 FOR NO CHANGE)
C     IBS          INTEGER (255) BINARY SCALING (-9999 FOR NO CHANGE)
C     NBS          INTEGER (255) NUMBER OF BITS (-9999 FOR NO CHANGE)
C     LGB          INTEGER UNIT NUMBER FOR GRIB FILE MAP
C     LXB          INTEGER UNIT NUMBER FOR GRIB INDEX FILE MAP
C     MB           INTEGER DIMENSION OF GRIB FIELD MAP
C     CBUFB        CHARACTER (MBUF) INDEX BUFFER MAP
C     NLENB        INTEGER RECORD LENGTH OF INDEX BUFFER MAP
C     NNUMB        INTEGER NUMBER OF RECORDS IN INDEX BUFFER MAP
C     NLENB        INTEGER LENGTH OF EACH INDEX RECORD MAP
C     NNUMB        INTEGER NUMBER OF INDEX RECORDS MAP
C     MNUMB        INTEGER NUMBER OF INDEX RECORDS MAP SKIPPED
C     LGM          INTEGER UNIT NUMBER FOR GRIB FILE MERGE
C     LXM          INTEGER UNIT NUMBER FOR GRIB INDEX FILE MERGE
C     MM           INTEGER DIMENSION OF GRIB FIELD MERGE
C     CBUFM        CHARACTER (MBUF) INDEX BUFFER MERGE
C     NLENM        INTEGER RECORD LENGTH OF INDEX BUFFER MERGE
C     NNUMM        INTEGER NUMBER OF RECORDS IN INDEX BUFFER MERGE
C     NLENM        INTEGER LENGTH OF EACH INDEX RECORD MERGE
C     NNUMM        INTEGER NUMBER OF INDEX RECORDS MERGE
C     MNUMM        INTEGER NUMBER OF INDEX RECORDS MERGE SKIPPED
C     LG2          INTEGER UNIT NUMBER FOR GRIB FILE 2
C     LXX          INTEGER FLAG FOR VERBOSE OUTPUT
C     KS1          INTEGER INPUT RECORD COUNTER
C     NO           INTEGER OUTPUT RECORD COUNTER
C   OUTPUT ARGUMENTS:
C     NO           INTEGER OUTPUT RECORD COUNTER
C     IRET         INTEGER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   GETGBEM
C   INTGRIB
C   PUTGBEN
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      CHARACTER CBUF1(MBUF),CBUFB(MBUF),CBUFM(MBUF)
      INTEGER JPDS1(200),JPDSB(200),IUV(100)
      INTEGER KGDSI(200)
      INTEGER IPOPT(20)
      INTEGER IDS(255),IBS(255),NBS(255)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS1(200),KGDS1(200),KENS1(200)
      INTEGER KPDSB(200),KGDSB(200),KENSB(200)
      INTEGER KPDSM(200),KGDSM(200),KENSM(200)
      LOGICAL*1 LR(MF),L1I(MI),LBI(MI)
      REAL FR(MF),F1I(MI),FBI(MI)
      REAL GR(MF),G1I(MI),GBI(MI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET FIELD FROM FILE 1
      JGDS=-1
      KPDS1=0
      KGDS1=0
      CALL GETGBEM(LG1,LX1,M1,KS1,JPDS1,JGDS,JENS,
     &             MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &             K1,KR1,KPDS1,KGDS1,KENS1,LR,FR,IRET)
      IDS2=KPDS1(22)
      IV=0
      KRV=0
      IF(IRET.EQ.0) THEN
        JUV=1
        DOWHILE(JUV.LE.NUV.AND.KPDS1(5).NE.IUV(JUV).AND.
     &          KPDS1(5).NE.IUV(JUV)+1)
          JUV=JUV+1
        ENDDO
        IF(JUV.LE.NUV.AND.KPDS1(5).EQ.IUV(JUV)) THEN
          IV=1
          JPDS=-1
          JPDS(1:24)=KPDS1(1:24)
          JPDS(22) = -1
          JPDS(5)=KPDS1(5)+1
          JGDS=KGDS1
          JENS=KENS1
          CALL GETGBEM(LG1,LX1,M1,KRV,JPDS,JGDS,JENS,
     &                MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &                K1,KRVX,KPDS1,KGDS1,KENS1,LR,GR,IRET)
          KRV=KRVX
          KPDS1(5)=JPDS(5)-1
          KPDS1(22)=MAX(IDS2,KPDS1(22))
        ELSEIF(JUV.LE.NUV.AND.KPDS1(5).EQ.IUV(JUV)+1) THEN
          IRET=-1
        ENDIF
      ENDIF
      IF(LXX.GT.0) THEN
        IF(IRET.EQ.-1) THEN
          PRINT *,'copygb skipping 2nd vector component field'
        ELSEIF(IRET.NE.0) THEN
          PRINT *,'copygb data retrieval error code ',IRET
        ELSEIF(KRV.EQ.0) THEN
          PRINT *,'copygb read scalar field from record ',KR1
          PRINT *,'       ...KPDS(1:24)=',(KPDS1(I),I=1,24)
        ELSE
          PRINT *,'copygb read vector field from records ',KR1,KRV
          PRINT *,'       ...KPDS(1:24)=',(KPDS1(I),I=1,24)
          PRINT *,'       ...KPDS(1:24)=',(KPDS1(I),I=1,4),
     &            KPDS1(5)+1,(KPDS1(I),I=6,24)
        ENDIF
        CALL INSTRUMENT(2,KALL2,TTOT2,TMIN2,TMAX2)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INVOKE MAP MASK BEFORE INTERPOLATION
      IF(IRET.EQ.0.AND.JBK.EQ.1.AND.JB.EQ.1) THEN
        DO I=1,K1
          IF(LR(I)) THEN
            IF((LAB.EQ.1.AND.FR(I).LE.AB).OR.
     &         (LAB.EQ.-1.AND.FR(I).GE.AB)) THEN
              IB1=1
              LR(I)=.FALSE.
            ENDIF
          ENDIF
        ENDDO
        IF(LXX.GT.0) THEN
          PRINT *,'       applied pre-interpolation map mask'
          CALL INSTRUMENT(3,KALL3,TTOT3,TMIN3,TMAX3)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE FIELD
      IF(IRET.EQ.0) THEN
        IB1=MOD(KPDS1(4)/64,2)
c	ib1i = 99
        CALL INTGRIB(IV,IP,IPOPT,KGDS1,K1,IB1,LR,FR,GR,KGDSI,MI,
     &               IB1I,L1I,F1I,G1I,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
            PRINT *,'       interpolated to grid ',IGI
          ELSEIF(IRET.GT.0) THEN
            PRINT *,'       interpolation error code ',IRET
          ENDIF
          CALL INSTRUMENT(4,KALL4,TTOT4,TMIN4,TMAX4)
        ENDIF
        IF(IRET.EQ.-1) IRET=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET MAP FIELD
      IF(IRET.EQ.0.AND.JB.EQ.4) THEN
        KRB=0
        JGDS=-1
        JENS=-1
        CALL GETGBEM(LGB,LXB,MB,KRB,JPDSB,JGDS,JENS,
     &               MBUF,CBUFB,NLENB,NNUMB,MNUMB,
     &               KB,KRBX,KPDSB,KGDSB,KENSB,LR,FR,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
            PRINT *,'       map field retrieved'
            PRINT *,'       ...KPDS(1:24)=',(KPDSB(I),I=1,24)
          ELSEIF(IRET.EQ.99) THEN
            PRINT *,'       map field not found'
          ELSE
            PRINT *,'       map field retrieval error code ',IRET
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE MAP FIELD
        IF(IRET.EQ.0) THEN
          IBB=MOD(KPDSB(4)/64,2)
          CALL INTGRIB(0,IP,IPOPT,KGDSB,KB,IBB,LR,FR,GR,KGDSI,MI,
     &                 IBBI,LBI,FBI,GBI,IRET)
          IF(LXX.GT.0) THEN
            IF(IRET.EQ.0) THEN
              PRINT *,'       interpolated to grid ',IGI
            ELSEIF(IRET.GT.0) THEN
              PRINT *,'       interpolation error code ',IRET
            ENDIF
          ENDIF
          IF(IRET.EQ.-1) IRET=0
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INVOKE MAP MASK
      IF(IRET.EQ.0) THEN
        IF(JBK.EQ.0.AND.JB.EQ.1) THEN
          DO I=1,MI
            IF(L1I(I)) THEN
              IF((LAB.EQ.1.AND.F1I(I).LE.AB).OR.
     &           (LAB.EQ.-1.AND.F1I(I).GE.AB)) THEN
                IB1I=1
                L1I(I)=.FALSE.
              ENDIF
            ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
            PRINT *,'       applied post-interpolation map mask'
          ENDIF
        ELSEIF(JB.EQ.4) THEN
          DO I=1,MI
            IF(LBI(I)) THEN
              IF((LAB.EQ.1.AND.FBI(I).LE.AB).OR.
     &           (LAB.EQ.-1.AND.FBI(I).GE.AB)) THEN
                IB1I=1
                L1I(I)=.FALSE.
              ENDIF
            ELSE
              IB1I=1
              L1I(I)=.FALSE.
            ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
            PRINT *,'       applied fixed map mask'
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MASK VALUES
        IF(LAM.EQ.1.AND.IB1I.EQ.1) THEN
          IB1I=0
          DO I=1,MI
            IF(.NOT.L1I(I)) THEN
              L1I(I)=.TRUE.
              F1I(I)=AM
              IF(KRV.GT.0) G1I(I)=AM
            ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
            PRINT *,'       substituted mask fill value'
          ENDIF
        ENDIF
        IF(LXX.GT.0) CALL INSTRUMENT(3,KALL3,TTOT3,TMIN3,TMAX3)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MERGE FIELD
        IF(LAM.EQ.5.AND.IB1I.EQ.1) THEN
          KRM=0
          JPDS=-1
          JPDS(5:7)=KPDS1(5:7)
          JGDS=-1
          JENS=-1
          CALL GETGBEM(LGM,LXM,MM,KRM,JPDS,JGDS,JENS,
     &                 MBUF,CBUFM,NLENM,NNUMM,MNUMM,
     &                 KM,KRMX,KPDSM,KGDSM,KENSM,LR,FR,IRET)
          IF(IRET.EQ.0.AND.KRV.GT.0) THEN
            JPDS=-1
            JPDS(1:24)=KPDSM(1:24)
            JPDS(5)=KPDSM(5)+1
            JGDS=KGDSM
            JENS=KENSM
            CALL GETGBEM(LGM,LXM,MM,KRM,JPDS,JGDS,JENS,
     &                   MBUF,CBUFM,NLENM,NNUMM,MNUMM,
     &                   KM,KRMX,KPDSM,KGDSM,KENSM,LR,GR,IRET)
            KPDSM(5)=JPDS(5)-1
          ENDIF
          IF(LXX.GT.0) THEN
            IF(IRET.EQ.0) THEN
              PRINT *,'       merge field retrieved'
              PRINT *,'       ...KPDS(1:24)=',(KPDSM(I),I=1,24)
              IF(KRV.GT.0)
     &         PRINT *,'       ...KPDS(1:24)=',(KPDSM(I),I=1,4),
     &                 KPDSM(5)+1,(KPDSM(I),I=6,24)
            ELSEIF(IRET.EQ.99) THEN
              PRINT *,'       merge field not found'
            ELSE
              PRINT *,'       merge field retrieval error code ',IRET
            ENDIF
          ENDIF
          IF(IRET.EQ.0) THEN
            IBM=MOD(KPDSM(4)/64,2)
            CALL INTGRIB(IV,IP,IPOPT,KGDSM,KM,IBM,LR,FR,GR,KGDSI,MI,
     &                   IBBI,LBI,FBI,GBI,IRET)
            IF(LXX.GT.0) THEN
              IF(IRET.EQ.0) THEN
                PRINT *,'       interpolated to grid ',IGI
              ELSEIF(IRET.GT.0) THEN
                PRINT *,'       interpolation error code ',IRET
              ENDIF
            ENDIF
            IF(IRET.EQ.-1) IRET=0
          ENDIF
          IF(IRET.EQ.0) THEN
            DO I=1,MI
              IF(.NOT.L1I(I).AND.LBI(I)) THEN
                L1I(I)=.TRUE.
                F1I(I)=FBI(I)
                IF(KRV.GT.0) G1I(I)=GBI(I)
              ENDIF
            ENDDO
            IF(LXX.GT.0) THEN
              PRINT *,'       merged output field with merge field'
            ENDIF
          ENDIF
          IRET=0
        ENDIF
        IF(LXX.GT.0) CALL INSTRUMENT(5,KALL5,TTOT5,TMIN5,TMAX5)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE OUTPUT FIELD
      IF(IRET.EQ.0) THEN
        KPDS1(3)=IGI
        KPDS1(4)=128+64*IB1I
        K5=KPDS1(5)
        IDS1=KPDS1(22)
        IBS1=0
        NBS1=0
        IF(K5.GT.0.AND.K5.LT.256) THEN
          IF(IDS(K5).GE.-128.AND.IDS(K5).LT.128) IDS1=IDS(K5)
          IF(IBS(K5).GE.-128.AND.IBS(K5).LT.128) IBS1=IBS(K5)
          IF(NBS(K5).GE.0.AND.NBS(K5).LT.256) NBS1=NBS(K5)
        ENDIF
        KPDS1(22)=IDS1
        CALL PUTGBEN(LG2,MI,KPDS1,KGDSI,KENS1,IBS1,NBS1,L1I,F1I,IRET)
        IF(IRET.EQ.0) NO=NO+1
        IF(IRET.EQ.0.AND.KRV.GT.0) THEN
          KPDS1(5)=K5+1
          CALL PUTGBEN(LG2,MI,KPDS1,KGDSI,KENS1,IBS1,NBS1,L1I,G1I,IRET)
          IF(IRET.EQ.0) NO=NO+1
          KPDS1(5)=K5
        ENDIF
        IF(LXX.GT.0) THEN
          IF(IRET.NE.0) THEN
            PRINT *,'       packing error code ',IRET
          ELSEIF(KRV.EQ.0) THEN
            PRINT *,'       wrote scalar field to record ',NO
            PRINT *,'       ...KPDS(1:24),IDS,IBS,NBS=',
     &                      (KPDS1(I),I=1,24),IDS1,IBS1,NBS1
          ELSE
            PRINT *,'       wrote vector field to records ',NO-1,NO
            PRINT *,'       ...KPDS(1:24)=',(KPDS1(I),I=1,24)
            PRINT *,'       ...KPDS(1:24)=',(KPDS1(I),I=1,4),
     &              KPDS1(5)+1,(KPDS1(I),I=6,24)
          ENDIF
          CALL INSTRUMENT(6,KALL6,TTOT6,TMIN6,TMAX6)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE INTGRIB(IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                   IB2,L2,F2,G2,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INTGRIB     INTERPOLATE FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: INTERPOLATE FIELD.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL INTGRIB(IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
C    &                   IB2,L2,F2,G2,IRET)
C   INPUT ARGUMENTS:
C     IV           INTEGER VECTOR FLAG
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     KGDS1        INTEGER (200) INPUT GRID PARAMETERS
C     K1           INTEGER INPUT DIMENSION
C     IB1          INTEGER INPUT BITMAP FLAG
C     L1           LOGICAL*1 (K1) INPUT BITMAP IF IB1=1
C     F1           REAL (K1) INPUT FIELD
C     G1           REAL (K1) INPUT Y-COMPONENT IF IV=1
C     KGDS2        INTEGER (200) OUTPUT GRID PARAMETERS
C     K2           INTEGER OUTPUT DIMENSION
C     IB2          INTEGER OUTPUT BITMAP FLAG
C     L2           LOGICAL*1 (K2) OUTPUT BITMAP
C     F2           REAL (K2) OUTPUT FIELD
C     G2           REAL (K2) OUTPUT Y-COMPONENT IF IV=1
C
C SUBPROGRAMS CALLED:
C   LENGDSF
C   INTGRIB1
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER IPOPT(20)
      INTEGER KGDS1(200),KGDS2(200)
      LOGICAL*1 L1(K1),L2(K2)
      REAL F1(K1),F2(K2)
      REAL G1(K1),G2(K2)
      INTEGER KGDS1F(200),KGDS2F(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INTERPOLATION IS NECESSARY
      IF(IP.EQ.4) THEN
        INT=1
      ELSE
        INT=0
        DO I=1,200
          INT=MAX(INT,ABS(KGDS1(I)-KGDS2(I)))
        ENDDO
        INT=MIN(INT,1)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COPY FIELD
      IF(INT.EQ.0) THEN
        IB2=IB1
        DO I=1,K1
          L2(I)=L1(I)
          F2(I)=F1(I)
          IF(IV.NE.0) G2(I)=G1(I)
        ENDDO
        IRET=-1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE REGULARIZED GRIDS AND INTERPOLATE FIELD
      ELSE
        K1F=LENGDSF(KGDS1,KGDS1F)
        IF(K1F.EQ.K1) K1F=1
        K2F=LENGDSF(KGDS2,KGDS2F)
        IF(K2F.EQ.K2) K2F=1
        MRL=MAX(K2,K2F)
        IF(IV.EQ.0) THEN
          MRO=1
        ELSE
          MRO=MRL
        ENDIF
        IF(K1F.GT.0.AND.K2F.GT.0) THEN
          CALL INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
     &                  IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                  IB2,L2,F2,G2,IRET)
        ELSE
          IRET=101
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
     &                    IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                    IB2,L2,F2,G2,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INTGRIB1    INTERPOLATE FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: INTERPOLATE FIELD.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
C    &                    IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
C    &                    IB2,L2,F2,G2,IRET)
C   INPUT ARGUMENTS:
C     K1F          INTEGER REGULARIZED INPUT DIMENSION
C     KGDS1F       INTEGER (200) REGULARIZED INPUT GRID PARAMETERS
C     K2F          INTEGER REGULARIZED OUTPUT DIMENSION
C     KGDS2F       INTEGER (200) REGULARIZED OUTPUT GRID PARAMETERS
C     MRL          INTEGER DIMENSION OF RLAT AND RLON
C     MRO          INTEGER DIMENSION OF CROT AND SROT
C     IV           INTEGER VECTOR FLAG
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     KGDS1        INTEGER (200) INPUT GRID PARAMETERS
C     K1           INTEGER INPUT DIMENSION
C     IB1          INTEGER INPUT BITMAP FLAG
C     L1           LOGICAL*1 (K1) INPUT BITMAP IF IB1=1
C     F1           REAL (K1) INPUT FIELD
C     G1           REAL (K1) INPUT Y-COMPONENT IF IV=1
C     KGDS2        INTEGER (200) OUTPUT GRID PARAMETERS
C     K2           INTEGER OUTPUT DIMENSION
C     IB2          INTEGER OUTPUT BITMAP FLAG
C     L2           LOGICAL*1 (K2) OUTPUT BITMAP
C     F2           REAL (K2) OUTPUT FIELD
C     G2           REAL (K2) OUTPUT Y-COMPONENT IF IV=1
C
C SUBPROGRAMS CALLED:
C   IPOLATES
C   IPOLATEV
C   IPXWAFS2
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER IPOPT(20)
      INTEGER KGDS1(200),KGDS2(200)
      LOGICAL*1 L1(K1),L2(K2)
      REAL F1(K1),F2(K2),G1(K1),G2(K2)
      INTEGER KGDS1F(200),KGDS2F(200)
      LOGICAL*1 L1F(K1F),L2F(K2F)
      REAL F1F(K1F),F2F(K2F),G1F(K1F),G2F(K2F)
      REAL RLAT(MRL),RLON(MRL),CROT(MRO),SROT(MRO)

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO REGLR SCALAR
      IF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1,
     &                KI,RLAT,RLON,IB2,L2,F2,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO REGLR SCALAR
      ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1,IB1F,L1F,F1F,
     &                  KI,RLAT,RLON,IB2,L2,F2,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO IRREG SCALAR
      ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1,
     &                KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO IRREG SCALAR
      ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1,IB1F,L1F,F1F,
     &                  KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
          IF(IRET.EQ.0) THEN
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO REGLR VECTOR
      ELSEIF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
        CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1,G1,
     &                KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
        IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
          F2(K2)=0
          G2(K2)=0
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO REGLR VECTOR
      ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1,
     &                  IB1F,L1F,F1F,G1F,
     &                  KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
          IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
            F2(K2)=0
            G2(K2)=0
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO IRREG VECTOR
      ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
        CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1,G1,
     &                KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO IRREG VECTOR
      ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1,
     &                  IB1F,L1F,F1F,G1F,
     &                  KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
          IF(IRET.EQ.0) THEN
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      FUNCTION LENGDSF(KGDS,KGDSF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LENGDSF     RETURN THE LENGTH OF A FILLED GRID
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: GIVEN A GRID DESCRIPTION SECTION (IN W3FI63 FORMAT),
C   RETURN THE GRID DESCRIPTION SECTION AND SIZE OF ITS REGULARIZED
C   COUNTERPART.  THAT IS, IF THE INPUT GRID IS REGULAR, THEN ITSELF
C   IS RETURNED ALONG WITH ITS GRID SIZE; HOWEVER IF THE INPUT GRID IS
C   ONLY QUASI-REGULAR (SUCH AS THE WAFS GRIDS), THEN ITS FILLED REGULAR
C   VERSION IS RETURNED ALONG WITH ITS FILLED GRID SIZE.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL LENGDSF(KGDS,KGDSF)
C   INPUT ARGUMENTS:
C     KGDS         INTEGER (200) GDS PARAMETERS IN W3FI63 FORMAT
C   OUTPUT ARGUMENTS:
C     KGDSF        INTEGER (200) REGULAR GDS PARAMETERS IN W3FI63 FORMAT
C     LENGDSF      INTEGER SIZE OF REGULARIZED GRID
C
C SUBPROGRAMS CALLED:
C   IPXWAFS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER KGDS(200),KGDSF(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDS(1).EQ.201) THEN
        KGDSF=KGDS
        LENGDSF=KGDS(7)*KGDS(8)-KGDS(8)/2
      ELSEIF(KGDS(1).EQ.202) THEN
        KGDSF=KGDS
        LENGDSF=KGDS(7)*KGDS(8)
      ELSEIF(KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
        CALL IPXWAFS(1,1,1,0,KGDS,DUM,KGDSF,DUMF,IRET)
        IF(IRET.EQ.0) THEN
          LENGDSF=KGDSF(2)*KGDSF(3)
        ELSE
          LENGDSF=0
        ENDIF
      ELSE
        KGDSF=KGDS
        LENGDSF=KGDS(2)*KGDS(3)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
