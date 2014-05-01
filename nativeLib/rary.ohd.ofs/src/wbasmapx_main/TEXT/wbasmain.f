C-----------------------------------------------------------------------
C
      SUBROUTINE WBASMAIN_MAIN
C
      PARAMETER (LARRAY=1000)
      DIMENSION ARRAY1(LARRAY),ARRAY2(LARRAY)
      INCLUDE 'updaio'
C
C-----------------------------------------------------------------------

      CHARACTER*100 FILENAME1, FILENAME2

C    ===================================================================
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      NO_OF_ARGUMENTS = IARGC()
C
      IF ( NO_OF_ARGUMENTS .NE. 2 ) THEN
         WRITE(*,*) '--------------------------------------------------'
         WRITE(*,*) 'Usage: rwNWSRFS <input file name> <log file name> '
         WRITE(*,*) '--------------------------------------------------'
         CALL EXIT(1)
      ELSE
         N1 = 1
         CALL GETARG( N1, FILENAME1)
         N2 = 2
         CALL GETARG( N2, FILENAME2)
      ENDIF
C

      CALL UPINIO()

      OPEN(59,FILE=TRIM(FILENAME1))
      OPEN( 7,FILE=trim(FILENAME2))

      WRITE(*,*)'-----PROGRAM BEGINNING----'

      CALL WBASTOP(ARRAY1,ARRAY2,LARRAY,ISTAT)

      WRITE(*,*)'-----PROGRAM FINISHED ----'

      STOP

      END

C-----------------------------------------------------------------------
C
      SUBROUTINE WBASTOP(ARRAY1,ARRAY2,LARRAY,ISTAT)
C
C-----------------------------------------------------------------------
C
      CHARACTER PARMID*8,PARMTP*4
      CHARACTER STAID*8,STATE*4,GPS(5)*4
      CHARACTER*100 CARD
      DIMENSION ARRAY1(LARRAY),ARRAY2(LARRAY)
      CHARACTER*4 DISP,TDISP,PRPARM
      PARAMETER (LCHAR=10,LCHK=5)
      PARAMETER (MBPTS=3000)
      PARAMETER (LXBUF=1)
      PARAMETER (LWKBUF=2000)
      DIMENSION XBUF(LXBUF)
      DIMENSION IWKBUF(LWKBUF)
      CHARACTER*4 CHAR(LCHAR),CHK(LCHK)
      CHARACTER*8 STRING,FTSID
      DIMENSION MAPXID(2), DESCRP0(5),DESCRP(5), FMAPID(2), BASNID(2)
      DIMENSION FLAT(MBPTS),FLON(MBPTS)
      DIMENSION FLTLN(2),PID(2),TID(2),PXID(2)
      REAL      ELEV, AREA
      INTEGER*4 XC, YC, IY(MBPTS), IXB(MBPTS), IXE(MBPTS)
      INTEGER*4 IVMAPX, NBPTS, NSEGS
      DATA      BLANK/4H    /
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'upagex'
CFAN
      INCLUDE 'uunits'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/sntwfx'
      INCLUDE 'ufreex'
c      INCLUDE 'uio'
CFAN
      DO I=1,LARRAY
      ARRAY1(I)=BLANK
      ARRAY2(I)=BLANK
      ENDDO

      LDEBUG=1
      NFLD=0
      IREAD=1
      LMXCRD=9999999
C
C---- OPEN INPUT/OUTPUT UNITS
      CALL UPRIMO0()
C
C---- GET USER IDENTIFIER
C     CALL HGTUSR (PUSRID,ISTAT)
C
C---- PRINT USER IDENTIFIER
C     WRITE (*,*) 'USERID = ',PUSRID
C
C---- READ DATA BASE CONTROL RECORDS
      CALL RPPPCO (ISTAT)
      IF (ISTAT.GT.0) GO TO 40
C
C---- GET INPUT FIELD
C1111 FOR MAP
      CALL UFLDRD (NFLD,CHAR,LCHAR,LDEBUG,IERR)
      WRITE(*,*)'READ DATA FOR ',CHAR

      IF (CHAR(1) .EQ. 'MAPX') THEN            !--------------if---

      IVMAPX   = 2            ! for MAP1 parameter array version # =1
      INTERVAL = 1            ! Data time interval (unit are HOUR)

      ARRAY1(1) =IVMAPX
      ARRAY1(9) =INTERVAL
      ARRAY1(11)=-999.0

      DO I=1,4
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
        IF (I.EQ.1 .AND. ITYPE.EQ.2) THEN
          CALL SUBSTR (CHAR,1,8,MAPXID,1)
          CALL SUBSTR (MAPXID(1),1,4,ARRAY1(2),1)
          CALL SUBSTR (MAPXID(2),1,4,ARRAY1(3),1)
        ELSEIF (I.EQ.2 .AND. ITYPE.EQ.2) THEN
          CALL SUBSTR (CHAR,1,20,DESCRP0,1)
          CALL SUBSTR (DESCRP0(1),1,4,ARRAY1(4),1)
          CALL SUBSTR (DESCRP0(2),1,4,ARRAY1(5),1)
          CALL SUBSTR (DESCRP0(3),1,4,ARRAY1(6),1)
          CALL SUBSTR (DESCRP0(4),1,4,ARRAY1(7),1)
          CALL SUBSTR (DESCRP0(5),1,4,ARRAY1(8),1)
        ELSEIF (I.EQ.3 .AND. ITYPE.EQ.0) THEN
          NUMOFBASN=INTEGR
          ARRAY1(10)=NUMOFBASN
        ELSEIF (I.EQ.4 .AND. ITYPE.EQ.2) THEN
          CALL SUBSTR (CHAR,1,8,FMAPID,1)
          CALL SUBSTR (FMAPID(1),1,4,ARRAY1(12),1)
          CALL SUBSTR (FMAPID(2),1,4,ARRAY1(13),1)
        ELSE
          STOP 'WRONG FORMAT FOR MAPX'
        ENDIF
      ENDDO

      ENDIF                                      !-----------endif---

C--------------------------------------------------------------------
C MAPX ARRAY:
C   starting  dim type input/     description
C   position           generated
C      1       1  I*4    G        Parameter array version number
C      2       1  A8     I        MAPX area identifier
C      4       1  A20    I        Desciption
C      9       1  I*4    G        Data time interval (only be 1 hour)
C      10      1  I*4    I        Number of basin in area
C      11      1  R*4    I        Unused
C      12      1  A8     I        Identifier of Future MAP area
C      14      1  A8     I        BASN ID
C
C INPUT FORMAT
C   MAP
C   test 'testab' 1 test21
C  (mapxid decsription no._of_basins fmapid)
C
C--------------------------------------------------------------------

C2222 FOR BASIN


      DO II=1,NUMOFBASN                             !---------do-----


      IVMAPX   = -1            ! for MAP1 parameter array version # =1
      ARRAY2(1) =IVMAPX

      CALL UFLDRD (NFLD,CHAR,LCHAR,LDEBUG,IERR)
      WRITE(*,*)'READ DATA FOR ',CHAR

      IF (CHAR(1)(1:4) .EQ. 'BASN') THEN            !---------if-----
C---- first field ----
      DO I=1,8
35    CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
        IF (I.EQ.1 .AND. ITYPE.EQ.2) THEN
          CALL SUBSTR (CHAR,1,8,BASNID,1)
          CALL SUBSTR (BASNID(1),1,4,ARRAY2(2),1)
          CALL SUBSTR (BASNID(2),1,4,ARRAY2(3),1)
C
          CALL SUBSTR (BASNID(1),1,4,ARRAY1(14+(II-1)*2),1)
          CALL SUBSTR (BASNID(2),1,4,ARRAY1(14+(II-1)*2+1),1)
C
        ELSEIF (I.EQ.2 .AND. ITYPE.EQ.2) THEN
          CALL SUBSTR (CHAR,1,20,DESCRP,1)
          CALL SUBSTR (DESCRP(1),1,4,ARRAY2(4),1)
          CALL SUBSTR (DESCRP(2),1,4,ARRAY2(5),1)
          CALL SUBSTR (DESCRP(3),1,4,ARRAY2(6),1)
          CALL SUBSTR (DESCRP(4),1,4,ARRAY2(7),1)
          CALL SUBSTR (DESCRP(5),1,4,ARRAY2(8),1)
        ELSEIF (I.EQ.3 .AND. ITYPE.EQ.1) THEN
          AREA=REAL
          ARRAY2(10)=AREA
          ARRAY2(11)=AREA
        ELSEIF (I.EQ.4 .AND. ITYPE.EQ.1) THEN
          ELEV=REAL
          ARRAY2(9)=ELEV
        ELSEIF (I.EQ.6 .AND. ITYPE.EQ.0) THEN
          YC  =INTEGR
          ARRAY2(13)=YC
        ELSEIF (I.EQ.7 .AND. ITYPE.EQ.0) THEN
          XC  =INTEGR
          ARRAY2(12)=XC
        ELSE
        ENDIF
      ENDDO
C
          ARRAY2(18)=0
          ARRAY2(19)=0
          CALL SUBSTR (MAPXID(1),1,4,ARRAY2(20),1)
          CALL SUBSTR (MAPXID(2),1,4,ARRAY2(21),1)
          ARRAY2(22)=1
C
C---- second field ----
36    CONTINUE

      DO I=1,2
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
      IF (ITYPE .EQ. 0) THEN
       NSEGS=INTEGR
       ARRAY2(24)=NSEGS
CCC    CALL SFCONV (ARRAY2(23),NSEGS,1)
      ENDIF

      ENDDO

      DO J=1,NSEGS
      DO K=1,4
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
        IF ( K.EQ.2 ) THEN
          IY(J)=INTEGR
        ELSEIF ( K.EQ.3 ) THEN
          IXB(J)=INTEGR
        ELSEIF ( K.EQ.4 ) THEN
          IXE(J)=INTEGR
        ELSE
        ENDIF

      ENDDO
      ENDDO

C---- third field ----
37    CONTINUE

      DO I=1,2
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ITYPE .EQ. 0) THEN
       NBPTS = INTEGR
       ARRAY2(23)=NBPTS
      ENDIF

      IF (NBPTS .GT. 400) WRITE(*,*)
     *   'WARNING !!!!! NBPTS > 400 !!!!!'

      ENDDO
C
      DO J=1,2*NBPTS
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
       IF (ITYPE .EQ. 1 .AND. J/2*2 .EQ. J) THEN
         FLON(J/2)=REAL
       ELSEIF (ITYPE .EQ. 0 .AND. J/2*2 .EQ. J) THEN
         FLON(J/2)=INTEGER
       ELSE IF (ITYPE .EQ. 1 .AND. J/2*2 +1 .EQ. J) THEN
         FLAT(J/2+1)=REAL
       ELSE IF (ITYPE .EQ. 0 .AND. J/2*2 +1 .EQ. J) THEN
         FLAT(J/2+1)=INTEGER
       ELSE
         WRITE(*,*)'WRONG TYPE in getting FLAT and FLON', itype
       ENDIF

      ENDDO

      CALL SFCONV (ARRAY2(25),FLAT,NBPTS)
      CALL SFCONV (ARRAY2(25+NBPTS),FLON,NBPTS)

c      DO K=1,NBPTS
c      ARRAY2(24+K)=FLAT(K)
c      ARRAY2(24+NBPTS+K)=FLON(K)
c      ENDDO

      CALL SFCONV (ARRAY2(25+2*NBPTS),IY,NSEGS)
      CALL SFCONV (ARRAY2(25+2*NBPTS+NSEGS),IXB,NSEGS)
      CALL SFCONV (ARRAY2(25+2*NBPTS+2*NSEGS),IXE,NSEGS)

c      DO K=1,NSEGS
c      ARRAY2(24+2*NBPTS+K)        =IY(K)
c      ARRAY2(24+2*NBPTS+NSEGS+K)  =IXB(K)
c      ARRAY2(24+2*NBPTS+2*NSEGS+K)=IXE(K)
c      ENDDO


      ENDIF                                        !-------endif-----

      NPOS=24+2*NBPTS+3*NSEGS
      IPTR=II-1

      IF (NPOS .GT. 1000) WRITE(*,*)
     *   'WARNING !!!!! NPOS > 1000 !!!!!'

      CALL WPPREC (BASNID,'BASN',NPOS,ARRAY2,IPTR,IERR)

      CALL WPPPCO(IERR)

      CALL UCLOSL

      ENDDO                                        !-------enddo-----

C--------------------------------------------------------------------
C BASIN ARRAY:
C   starting  dim     type input/  description
C   position             generated
C      1       1       I*4  G  Parameter array version number
C      2       1       A8   I  Basin boundary identifier
C      4       1       A20  I  Desciption
C      9       1       R*4  I  Mean elevation (units are M)
C      10      1       R*4  I  Basin area (unit are KM^2)
C      11      1       R*4  G  Computed basin area (unit are KM^2)
C      12      2       I*4  G  Centroid of area (NWSRFS/HRAP
C                              coordinates stored as (X,Y))
C      14      1       A8   G  Identifier of MAP that uses
C                              this basin boundary
C      16      1       A8   G  Identifier of MAT that uses
C                              this basin boundary
C      18      1       I*4  G  Update indicator for MAP parameters
C                               0=not updated; 1=updated
C      19      1       I*4  G  Update indicator for MAT parameters
C                               0=not updated; 1=updated
C      20      1       A8   G  Identifier of NEXRAD MAP (MAPX)
C                              that uses this basin boundary
C      22      1       I*4  G  NWSRFS/HRAP grid spacing factor
C      23      1       I*4  G  Number of pairs of basin boundary points
C                              (NBPTS)
C      24      1       I*4  G  Number of NWSRFS/HRAP grid segments
C                              used to define the basin(BSEGS)
C      25    (NBPTS,2) R*4  I  Pairs of basin boundary points (lat/lon)
C                              stored in clockwise order (in decimal deg)
C 25+2*NBPTS (NSEGS,3) I*4  G  Grid point definition
C INPUT FRMAT
C   BASIN
C   upper 'this is number 1' 35669589.68 1000 (40.578, -77.5605)
C  (basinid description area mean_elevation (lat/lon_of_centroid))
C   1 306 473 475
C   2 305 472 474
C   3 304 472 474
C   4 303 472 473
C   5 302 472 472
C  (line_seg_no HRAP_row Begining_HRAP_col ENDING_HRAP_col)
C   LAT/LON 7
C  (LAT/LON no._of_lat/lon_pairs)
C   35.1137,-101.813 35.1303,-101.679 35.126,-101.665 34.9373,-101.837 34.9632,-101.877
C   35.0688, -101.852 35.1137,-101.813
C  (lat/lon_pairs (max of 5 pairs per row)
C
C--------------------------------------------------------------------

40    CONTINUE

      NFILL=13+2*NUMOFBASN
      IPTR =0

      CALL WPPREC (MAPXID,'MAPX',NFILL,ARRAY1,IPTR,IERR)
C
      CALL WPPPCO(IERR)

      CALL UCLOSL

CFAN (behin)
c---------------------------------------------------------------------
C
C  CHECK TO SEE IF TIME SERIES HEADER EXISTS
C
      INDPM=0
      ITIME=1
      TDISP='NEW'

      CALL SFTSCK (MAPXID,'MAPX',LXBUF,INDPM,IPMWRT,ITSWRT,ITIME,
     *   TDISP,NUMERR,NUMWRN,IERR)
C
C     IPMWRT ITSWRT : output, 1=no record
C

      IF (IERR.GT.0) THEN
         STOP 'AFTER SFTSCK !!!!!!!!!!!!!!!'
         ENDIF

      IF (IPMWRT .NE. 1 ) GOTO 666

C
C     CHECK IF BUFFER FOR PROCESSED DATA BASE R/W ROUTINE LARGE ENOUGH
C
      NPDT=1
      NX=0
C
      CALL SUPRDW ('MAPX',LWKBUF,ITIME,NPDT,NX,1,LWKNED,NUMERR,IERR)

      IF (IERR.GT.0) THEN
         STOP 'AFTER SUPRDW !!!!!!!!!!!!!!!'
         ENDIF
C
C     READS CONTROL INFORMATION FROM PROCESSED DATA BASE DATA FILES
C     INTO COMMON BLOCKS
C
      CALL RPDBCI(ISTAT0)

      IF (ISTAT0.GT.0) THEN
         STOP 'AFTER RPDBCI !!!!!!!!!!!!!!!'
         ENDIF
C
C     CREATE TIME SERIES IN THE PROCESSED DATABASE
C
      ITIME=1      !data tiem interval
      NVALX=0      !number of values per tiem interval
      FTSID=' '    !future time series identifier
C
C  CONVERT FROM GRID COORIDINATES to LAT/LON
C
      X=XC*1.00
      Y=YC*1.00
      CALL SBLLGD (FLTLN(2),FLTLN(1),1,X,Y,0,IERR)

      IF (ITSWRT .NE. 1) GOTO 555

      CALL WPRDH (MAPXID,'MAPX',ITIME,'MM  ',NVALX,FLTLN,FTSID,
     *   DESCRP0,NX,XBUF,LWKBUF,IWKBUF,IREC,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPRST ('WPRDH   ',MAPXID,'MAPX',ITIME,'MM  ',FTSID,
     *      LWKBUF,NVALX,IERR)
         STOP 'AFTER WPRDH !!!!!!!!!!!!!!!'
         ENDIF

555   CONTINUE

C
C     WRITE TIME SERIES TO THE PROCESSED DATABASE
C

      IF (ITSWRT .NE. 2) GOTO 666


      CALL WPRDC (MAPXID,'MAPX','    ',FLTLN,DESCRP0,FTSID,NX,
     *   XBUF,LWKBUF,IWKBUF,IREC,IERR)

      IF (IERR.GT.0 .AND. IERR.NE.8) THEN
         CALL SWPRST ('WPRDC   ',MAPXID,'MAPX',ITIME,'MM  ',FTSID,
     *      LWKBUF,NVALX,IERR)
         STOP 'AFTER WPRDC !!!!!!!!!!!!!!!'
      ELSEIF (IERR.EQ.8) THEN
         WRITE(*,*) 'IERR=8, NO CHANGES NEED TO BE MADE'
         ENDIF

c---------------------------------------------------------------------
CFAN (end)

666   CONTINUE


c---------------------------------------------------------------------

      CALL WPDBCO(ISTAT)
      IF (ISTAT.GT.0) THEN
         WRITE(*,*)'ERROR IN WRITING TO PROCESSED DATA BASE'
         ENDIF

      WRITE(*,*)'-----PROGRAM FINISHED-----'

      STOP
C
      END
C
C-----------------------------------------------------------------------
C MODULE UPRIMO0
C  =====================================================================
C  pgm: UPRIMO0 .. Initialize I/O units
C
C  rqd: URTIMR,UPINIO,UPRIMR,UPRIMW,UPRIMT,UPPFIX,UPFNCU,UDOPEN,UPCHKD
C  rqd: UPCLOS
C  rqd: COMMON: UPDAIO and other nwsrfs-ofs common
C  =====================================================================

      SUBROUTINE UPRIMO0()

      EXTERNAL   URTIMR,UPINIO,UDOPEN
      EXTERNAL   UPRIMR,UPRIMW,UPRIMT,UPFNCU,UPPFIX,UPCHKD,UPCLOS

      CHARACTER*1   FM
      CHARACTER*39  INSTMT,OTSTMT,PUSTMT
      CHARACTER*32  FILNAM
      CHARACTER*128 NEWNAM
      CHARACTER*24  GLODIR
      INTEGER       FNUM,RNUM,BNUM,ISTAT,N,KOD,KOND

      INTEGER       JPRIM,JEBBB
      CHARACTER*4   CDEFN,CPRIM,CEBBB
      EQUIVALENCE ( CPRIM,JPRIM ),( CEBBB,JEBBB )

      CHARACTER*5   LOCK_TYPE

      INCLUDE 'updaio'

      INCLUDE 'uio'
C  "UIOX" is the shared library version of uio
      COMMON /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
      common /CMWBASMAPX/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'upagex'
      INCLUDE 'udebug'
      INCLUDE 'udsi'
      INCLUDE 'uunits'
      INCLUDE 'udsatx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/eunit'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'dscommon/dsunts'
      INCLUDE 'prdcommon/pmaxdm'

C   "ERRDAT" is in ...common/errdat
C   "UCMDBX" is for the shared library debug

      COMMON /ERRDAT/ IOERR_,NWARN_,NERRS_
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/wbasmapx_main/RCS/wbasmain.f,v $
     . $',                                                             '
     .$Id: wbasmain.f,v 1.2 2004/07/21 18:16:11 dsa Exp $
     . $' /
C    ===================================================================
C

      DATA  CDEFN,CPRIM,CEBBB / 'DEFN','PRIM','E   ' /
      DATA  FM / 'F' /
      DATA  INSTMT / ' *** Enter input filename, TTY, or Q:  ' /
      DATA  OTSTMT / ' *** Enter output filename, TTY, or Q: ' /
      DATA  PUSTMT / ' *** Enter punch filename, TTY, or Q:  ' /

C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C  Set cpu timer, sys level i/o unit numbers
        CALL URTIMR (LAPSE,ITMBEG)
CCC     CALL UPINIO()

C  Try to establish the OFS file lock
        LOCK_TYPE='write'
        CALL SET_OFS_LOCK (LOCK_TYPE,KOND)
        IF (KOND .GT. 0) THEN
           STOP 16
           ENDIF

C  Set user id
        PUSRID = ' '

C  Initialize common for function "ifbug".
        NDEBGS = 0
        IALL   = 0
C
C  Set unit number for system files
        KDTYPE = 3
        LSYS2  = KDTYPE

        ITRACE = 0
        IOPCLG(2) = 1

C  Set the two globals needed by every program
        GLODIR = 'SYST OPER'
        IF (PGMNAM.EQ.'FCST'   ) GLODIR(11:19) = 'MODS GRID'
        IF (PGMNAM.EQ.'REORDER') GLODIR(11:24) = 'MODS GRID REOR'
        CALL UPCHKD (GLODIR,ISTAT)
        IF (ISTAT .NE. 0) GO TO 30

        ICD    = 0
        LP     = 0
        ICDPUN = 0

C  Set i/o units for specific programs

       ICD=59
       LP =98
       ICDPUN=8

          USERPP(1) = JPRIM
          USERPP(2) = JEBBB
          IPRMPT = 1
          IF ( ICD .NE. 1 ) IPRMPT = 0
          FILNAM = 'TEMP.PPINIT.02'
          CALL UPPFIX ('OPER',FILNAM,NEWNAM,NOFC)
          ICDTMP = 2
          CALL UPRIMT (NEWNAM,FM,ICDTMP,I)
C      GET UNIT NUMBER FOR FILE SASM.CONTROL
          FILNAM = 'SASM.CONTROL'
          CALL UPFNCU ('NORM',FILNAM,FNUM,NEWNAM,N,RNUM,BNUM,ISTAT)
          IF ( ISTAT.GT.0 .AND. UE.GE.0 ) WRITE(UE,10)
10        FORMAT(' uprimo      ** ERROR = error returned from upfncu')
          IF ( ISTAT.LE.0 ) KDSRCF = N
            IPRCRD = 0
            IOPNWP = 0
            LPE    = LP
            KUPARM = 4
            IOGDB  = LP
            IUTLTR = 0
            IUTLDB = 0
            IOSDBG = LP
            ISTRCE = 0
            ISDBUG = 0
            ISALL  = 0
            NSDBUG = 0
            SDBUG(1) = CDEFN
            SONEWP = 0
            NFLD   = 0
            LPD    = LP
            IOERR  = LP
            IODBUG = LP
            IN     = ICD
            IPR    = LP
            IPU    = ICDPUN
            ISDBGL = 0
            GO TO 40

C  Error setting i/o units

30    IF (LP   .GT. 0) CALL UPCLOS (LP,' ',IERR)
      IF (ICD  .GT. 0) CALL UPCLOS (ICD,' ',IERR)
      IF (ICDPUN.GT.0) CALL UPCLOS (ICDPUN,' ',IERR)
      CALL FREE_OFS_LOCK (KOND)
      CALL EXIT

C  Set second common area for i/o for routines using UIOX instead of UIO
40    LP2    = LP
      ICD2   = ICD
      LPD2   = LPD
      LPE2   = LPE
      ICDPU2 = ICDPUN

C  Set shared library debug variables
      ICMPRU = LP
      ICMTRC = 0
      ICMDBG = 0

C  Make sure DSUNIT is 3380
      DSUNIT = '3380'

      IOPDBG = LP

      RETURN

      END
C-----------------------------------------------------------------------
C        Stub out SUEND to avoid calls to many unused routines
C-----------------------------------------------------------------------
C      SUBROUTINE SUEND()
C      RETURN
C      END
