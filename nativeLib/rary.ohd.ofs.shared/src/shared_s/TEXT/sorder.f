C MODULE SORDER
C-----------------------------------------------------------------------
C
C  ROUTINE TO DETERMINE MAP, MAPX AND FMAP COMPUTATIONAL ORDER
C  INFORMATION.
C
      SUBROUTINE SORDER (LARRAY,ARRAY,NFLD,IPRINT,ISTAT)
C
      CHARACTER*4 WDISP,UNITS,TCODE,DTYPE,XTYPE,PTYPE
      CHARACTER*8 TYPERR,CGID,SEGID,BASNID,XNAME
      CHARACTER*8 FMAPID,PMID,TID,PXID
      CHARACTER*20 STRNG/' '/,STRNG2/' '/
      CHARACTER*20 DESCRP,BDESC
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION CENTRD(2)
C 
      PARAMETER (MRIDCK=1000)
      CHARACTER*8 RIDCK(MRIDCK)
      DIMENSION NFGCK(MRIDCK)

C  JTO - MARFC - 8/2004 

C     Increased number of MAPX areas that can be processed from 1500 to
C     20000 to accomodate increased number of MAPX areas defined at
C     MARFC with their FFG experiment (i.e. creating one MAPX area per
C     HRAP cell).  Their HRAP bounds are 200x200 but only about 14000
C     are within their RFC boundary.  May have to be increased if any
C     other RFC follows this approach as MARFC is smallest
C     geographically.
C
      PARAMETER (MMPXID=20000)
      CHARACTER*8 MPXID(MMPXID)
      PARAMETER (MAXBASN=99)                                   !cfan
      DIMENSION BASNID00(MAXBASN*2)                            !cfan
      INTEGER NUMB,TmpNum
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sordrx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fd'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/fcsegn'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sorder.f,v $
     . $',                                                             '
     .$Id: sorder.f,v 1.14 2005/03/18 21:01:38 leecr Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('ORDR','ORDRMAIN','SORDER',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,280)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('ORDR','ORDRMAIN','SORDER',LDEBUG)
C
      ISTAT=0
C      
      LSTRNG=LEN(STRNG)/4     
      LSTRNG2=LEN(STRNG2)/4
C
      NUMERR=0
      NUMWRN=0
C      
C  PRINT CARD
      IF (NFLD.NE.-1) CALL SUPCRD
C
C  PRINT HEADING
      WRITE (LP,290)
      CALL SULINE (LP,2)
      WRITE (LP,300)
      CALL SULINE (LP,2)
C
      NFMPID=0
      NRMPDP=0
      IMAPX=1
      NMPXID=0
      NXA=0
      UNSD=-999.
C
C  SET SIZE OF WORK ARRAYS
CMODIFIED BY GZHOU 10/17/2003 R24-7
C
CEW making this bigger again for Joe O.
C      LWXGRD=20000
c      LWXGRD=40000
      LWXGRD=60000
CEND OF MODIFICATION
C
      IF (LWXGRD.GE.LSWORK) THEN
         WRITE (LP,310) LWXGRD,LSWORK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 270
         ENDIF
      LWORK=(LSWORK-LWXGRD)/4
      IDIV=5
      LARAY=LSWRK2/IDIV
C  IY:
      ID1=1
C  IXB:
      ID2=LARAY*1+1
C  IXE:
      ID3=LARAY*2+1
C  LATITUDES:
      ID4=LARAY*3+1
C  LONGITUDES:
      ID5=LARAY*4+1
C 
      MBPTS=LARAY
      MSEGS=LARAY
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IN SORDER -',
     *      ' LSWRK2=',LSWRK2,
     *      ' IDIV=',IDIV,
     *      ' MBPTS=',MBPTS,
     *      ' MSEGS=',MSEGS,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF   
C
C  INITIALIZE ARRAYS
      DO 10 I=1,MRMPID
         RMPID(I)=' '
10       CONTINUE
      DO 20 I=1,MFMPID
         FMPID(I)=' '
20       CONTINUE
C
C  SET PARAMETER ARRAY VERSION NUMBERS
      IVMPCO=1
      IVMPFO=1
      IVFMPO=1
      IVMXCO=1
      IVXGRD=1
C
C  SET FORECAST COMPONENT READ INDICATORS
      IDTYPE=2
      IOPT=1
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR ORDER OPTIONS
C
      ISTRT=1
C
30    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 60
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,340) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 30
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 60
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR OPTION
      IF (STRNG2.EQ.'MAPX') GO TO 40
C
C  INVALID OPTION
      WRITE (LP,330) STRNG
      CALL SUERRS (LP,2,NUMERR)
      GO TO 30
C
C  MAPX OPTION
40    CALL ULENTH (STRNG2,LEN(STRNG2),LSTRNG2)
      IF (LLPAR.EQ.0) THEN
         STRNG2='YES'
         CALL ULENTH (STRNG2,LEN(STRNG2),LSTRNG2)
         WRITE (LP,350) 'MAPX',STRNG2(1:LSTRNG2)
         CALL SULINE (LP,2)
         GO TO 50
         ENDIF
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,360) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LSTRNG2,STRNG2,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (STRNG2.EQ.'YES'.OR.STRNG2.EQ.'NO') GO TO 50
         WRITE (LP,370) 'MAPX',STRNG2(1:LSTRNG2)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 30
50    IF (STRNG2.EQ.'YES') IMAPX=1
      IF (STRNG2.EQ.'NO') IMAPX=0
      WRITE (LP,380) 'MAPX',STRNG2(1:LSTRNG2)
      CALL SULINE (LP,2)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,390) 'MAPX',IMAPX
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 30
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
60    ICKRGN=0
      ICKCPU=1
      MINCPU=15
      IPRERR=1
      IPUNIT=LP
      TYPERR='ERROR'
      INCLUDE 'clugtres'
      IF (IERR.GT.0) THEN
         CALL SUFATL
         CALL SUEND
         ENDIF
C
C  CHECK IF PARAMETRIC, PROCESSED AND FORECAST DATA BASES ALLOCATED
      IDPPP=1
      IDPRD=1
      IDFC=1
      CALL SUDALC (0,0,0,0,IDPPP,IDPRD,IDFC,0,0,0,NUMERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,400)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 270
         ENDIF
C
C  OPEN DATA BASES
      CALL SUDOPN (1,'PPP ',IERR)
      CALL SUDOPN (1,'PRD ',IERR2)
      CALL SUDOPN (1,'FC  ',IERR3)
      IF (IERR.GT.0.OR.IERR2.GT.0.OR.IERR3.GT.0) GO TO 270
C
C  READ USER GENERAL PARAMETERS
      CALL SUGTUG (LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,410)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 270
         ENDIF
C
C  SET MAP STATUS FLAG TO INCOMPLETE
      ICUGNL(1)=1
      WDISP='OLD'
      INCLUDE 'scommon/callswugnl'
      IF (IERR.GT.0) THEN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 270
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         UNITS='ENGL'
         INCLUDE 'scommon/callspugnl'
         ENDIF
C
C  FORCE WRITE PARAMETER RECORD TO FILE
      CALL SUPCLS (1,'UGNL',IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  WRITE GENERAL COMPUTATIONAL ORDER PARAMETERS
      INCLUDE 'scommon/callsuwtor'
      IF (IERR.GT.0) THEN
         WRITE (LP,420) IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 270
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  MODIFY CONTROL INFORMATION FOR FILES WITH ONLY COMPUTATIONAL ORDER
C  PARAMETER TYPES TO INDICATE FILE IS EMPTY
      CALL WPPCOF (IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,430)
            CALL SULINE (LP,2)
            GO TO 80
            ELSE
               WRITE (LP,440) IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 270
            ENDIF
         ENDIF
C
      WRITE (LP,450)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    IF (NUMERR.GT.0) GO TO 270
C
C  GET ORDERED LIST OF FORECAST GROUPS FOR EACH CARRYOVER GROUP
C
C  GET NUMBER OF CARRYOVER GROUPS AND NAMES
      CALL UREADT (KFCGD,1,NSLOTS,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NCG=',NCG
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (NCG.EQ.0) THEN
         WRITE (LP,650)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 270
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,670) ((CGIDS(I,J),I=1,2),J=1,NCG)
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      CALL UREADT (KFFGST,1,FGID,IERR)
      NFGREC=IDUMYG
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NFGREC=',NFGREC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NUMCG=0
C
C      -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
      IFPERR=0
C
      IMXFST=1
      IMXLST=0
C
C  PROCESS EACH CARRYOVER GROUP
C
      DO 190 ICG=1,NCG
         INDERR=0
         CALL SUBSTR (CGIDS(1,ICG),1,8,CGID,1)
         CHKID=1
         IF (CHKID.EQ.1) THEN
            IF (CGID.EQ.'MARFCADV') THEN
               CHKID=0
               ENDIF
            ENDIF
         NFGMAP=0
         NFGMPX=0
C     GET NUMBER OF FORECAST GROUPS IN CARRYOVER GROUP
         CALL UREADT (KFCGD,ICOREC(ICG),CGIDC,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'NFG=',NFG
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (NFG.EQ.0) THEN
            WRITE (LP,665) CGID
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 190
            ENDIF
         NREC1=1
         IMPFST=1
         IMPLST=0
C     GET LIST OF FORECAST GROUPS IN COMPUTATIONAL ORDER
         DO 110 IFG=1,NFG
            IF (NREC1.GT.NFGREC) THEN
               WRITE (LP,700) CGIDS(1,ICG),ICOSEQ,NFG
               CALL SUERRS (LP,2,NUMERR)
               GO TO 190
               ENDIF
            DO 90 NREC=NREC1,NFGREC
               CALL UREADT (KFFGST,NREC,FGID,IERR)
               IF (ISPEC.EQ.1) GO TO 90
               IF (CGIDS(1,ICG).NE.CGIDF(1).OR.
     *             CGIDS(2,ICG).NE.CGIDF(2)) GO TO 90
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,680) FGID
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ICOSEQ.GT.MFGIDS) THEN
                  WRITE (LP,690) MFGIDS
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 270
                  ENDIF
               CALL UMEMOV (FGID,FGIDS(ICOSEQ),2)
               GO TO 100
90             CONTINUE
100         NREC1=NREC+1
110         CONTINUE
C     GET LIST OF SEGMENTS IN ORDER FOR EACH FORECAST GROUP
         NRIDCK=0
         XTYPE=' '
         DO 180 IFG=1,NFG
            CALL FCORDR (IDTYPE,FGIDS(IFG),IERR,D,MD)
            IF (NSEGEX.LE.0) THEN
               WRITE (LP,710) FGIDS(IFG),NSEGEX
               CALL SUERRS (LP,2,NUMERR)
               GO TO 270
               ENDIF
            NRMPID=0
C        CHECK EACH SEGMENT FOR MAP, MAPX OR FMAP TIME SERIES TYPES
            DO 160 NSG=1,NSEGEX
               NOPARM=0
               CALL FGETSG (SEGID,IRSGEX(NSG),MP,P,MT,T,MTS,TS,IOPT,
     *            NOPARM,IERR)
               IF (IERR.GT.0) THEN
                  IF (IERR.EQ.1) THEN
                     WRITE (LP,720) IOPT
                     CALL SUERRS (LP,2,NUMERR)
                     ENDIF
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,730) MP,MT,MTS
                     CALL SUERRS (LP,2,NUMERR)
                     ENDIF
                  GO TO 270
                  ENDIF
               NTPOS=0
C           CHECK TIME SERIES TYPE
120            ITSTYP=TS(NTPOS+1)
               IF (LDEBUG.GT.1) THEN
                  WRITE (IOSDBG,*) 'ITSTYP=',ITSTYP
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ITSTYP.GE.0.AND.ITSTYP.LE.4) THEN
                  ELSE
                     WRITE (LP,735) ITSTYP,NSG,FGID,CGID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 160
                  ENDIF
               IF (ITSTYP.EQ.0) GO TO 160
               IF (ITSTYP.NE.1) GO TO 150
C           CHECK TIME SERIES DATA TYPE
               CALL UMEMOV (TS(NTPOS+10),TCODE,1)
               IF (TCODE.NE.'FPDB') THEN
                  IF (LDEBUG.GT.1) THEN
                     WRITE (IOSDBG,*) 'TCODE=',TCODE
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  GO TO 150
                  ENDIF
C           CHECK TIME SERIES DATA TYPE
               CALL UMEMOV (TS(NTPOS+15),DTYPE,1)
               IF (DTYPE.EQ.'MAP'.OR.DTYPE.EQ.'MAPX') GO TO 130
                  NTPOS2=NTPOS+13
                  NTPOS3=NTPOS+14
                  IF (LDEBUG.GT.1) THEN
                     WRITE (IOSDBG,750) (TS(I),I=NTPOS2,NTPOS3),DTYPE
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  GO TO 150
C           SET DATA TYPE AND IDENTIFIER
130            XTYPE=DTYPE
               CALL UMEMOV (TS(NTPOS+13),XNAME,2)
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,510) XTYPE,XNAME
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (XTYPE.EQ.'MAP') THEN
C              STORE MAP IDENTIFIER
                  IF (NRMPID+1.GT.MRMPID) THEN
                     WRITE (LP,770) MRMPID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 270
                     ENDIF
                  NRMPID=NRMPID+1
                  CALL UMEMOV (XNAME,RMPID(NRMPID),2)
                  IF (NRIDCK+1.GT.MRIDCK) THEN
                     WRITE (LP,770) MRIDCK
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 270
                     ENDIF
                  NRIDCK=NRIDCK+1
                  RIDCK(NRIDCK)=XNAME
                  NFGCK(NRIDCK)=IFG
                  IF (LDEBUG.GT.1) THEN
                     WRITE (IOSDBG,760) RIDCK(NRIDCK),NFGCK(NRIDCK)
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  ENDIF
C           GET FUTURE MAP IDENTIFIER
               CALL SOFPID (XTYPE,XNAME,MFMPID,NFMPID,FMPID,
     *            LARRAY,ARRAY,NUMERR,IERR)
               IF (IERR.GT.0) THEN
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,470) IERR
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  INDERR=1
                  IFPERR=1
                  ENDIF
               IF (XTYPE.EQ.'MAPX') GO TO 140
C              -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C           CHECK IF MAP AND MAPS PARAMETERS EXIST
               PTYPE='MAP'
               IPTR=0
               IFMAP=1
               CALL RPPREC (XNAME,PTYPE,IPTR,LARRAY,ARRAY,
     *            NFILL,IPTRNX,IERR)
               IF (IERR.GT.0) THEN
                  IERR=-IERR
                  CALL SRPPST (XNAME,PTYPE,IPTR,LARRAY,
     *               NFILL,IPTRNX,IERR)
                  NUMWRN=NUMWRN+1
                  IFMAP=0
                  ENDIF
               PTYPE='MAPS'
               IPTR=0
               IFMAPS=1
               CALL RPPREC (XNAME,PTYPE,IPTR,LARRAY,ARRAY,
     *            NFILL,IPTRNX,IERR)
               IF (IERR.GT.0) THEN
                  IERR=-IERR
                  CALL SRPPST (XNAME,PTYPE,IPTR,LARRAY,
     *               NFILL,IPTRNX,IERR)
                  NUMWRN=NUMWRN+1
                  IFMAPS=0
                  ENDIF
               IF (LDEBUG.GT.1) THEN
                  WRITE (IOSDBG,*)
     *               ' IFMAP=',IFMAP,
     *               ' IFMAPS=',IFMAPS,
     *               ' NRMPID=',NRMPID,
     *               ' NRIDCK=',NRIDCK,
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (IFMAP.EQ.1.AND.IFMAPS.EQ.1) THEN
                  ELSE
                     NRMPID=NRMPID-1
                     NRIDCK=NRIDCK-1
                  ENDIF
               GO TO 150
C              -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
140            IF (IMAPX.EQ.0) GO TO 150
C           CHECK IF MAPX AREA ALREADY PROCESSED
               IF (NMPXID.GT.1) THEN
                  DO 145 I=1,NMPXID
                     IF (XNAME.EQ.MPXID(I)) THEN
                        IF (LDEBUG.GT.0) THEN
                           WRITE (IOSDBG,*)
     *                        'XNAME=',XNAME,
     *                        'I=',I,
     *                        'MPXID(I)=',MPXID(I),
     *                        ' '                           
                           CALL SULINE (IOSDBG,1)
                           ENDIF
                        GO TO 150
                        ENDIF
145                  CONTINUE                        
                  ENDIF
C           READ MAPX PARAMETER RECORD
               IPTR=0
               IPRERR=0
	       ITIME=1
	       NUMB=0
               CALL SRMAPX (IVMAPX,XNAME,ITIME,DESCRP,BASNID00,FMAPID,
     *            UNUSED,LARRAY,ARRAY,IPTR,IPRERR,IPTRNX,IERR,NUMB)
               IF (IERR.GT.0) THEN
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,550) 'MAPX AREA',XNAME
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 150
                     ENDIF
                  IF (IERR.EQ.3) THEN
                     WRITE (LP,540) LARRAY,'MAPX',XNAME
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 150
                     ENDIF
                  WRITE (LP,560) 'MAPX',XNAME,IERR
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
C           READ BASIN PARAMETER RECORD
cckwz loop BEGIN here
               IF (IVMAPX.EQ.1) NUMB=1
               NSEGS = 0
               DO 159 Index=1,NUMB
                 IPTR=0
                 IPRERR=0
	         TmpNum=NSEGS
                 CALL SUBSTR (BASNID00,(index*8)-7,8,BASNID,1)
                 CALL SRBASN (ARRAY,LARRAY,IVBASN,BASNID,
     *            BDESC,SWRK2(ID4),SWRK2(ID5),LARAY,NBPTS,AREA,
     *            CAREA,ELEV,CENTRD(1),CENTRD(2),
     *            MAPFLG,MATFLG,PMID,TID,PXID,LARAY,TmpNum,
     *            SWRK2(ID1+TmpNum),SWRK2(ID2+TmpNum),SWRK2(ID3+TmpNum),
     *            LFACTR,IPTR,IPTRNX,IPRERR,IERR)
               IF (IERR.GT.0) THEN
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,550) 'BASIN',BASNID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 150
                     ENDIF
                  IF (IERR.EQ.3) THEN
                     WRITE (LP,540) LARRAY,'BASN',BASNID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 150
                     ENDIF
                  WRITE (LP,560) 'BASN',BASNID,IERR
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
	       NSEGS=NSEGS+TmpNum
159            CONTINUE
cckwz loop END here
C           WRITE MAPX COMPUTATIONAL ORDER PARAMETERS
               CALL SWMXCO (IVMXCO,NXA,XNAME,NXGPOS,
     *            IMXFST,IMXLST,NMXPOS,
     *            UNSD,LWORK,SWORK(LWORK*3+1),NUMERR,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (LP,460) 'MXCO'
                  CALL SULINE (LP,2)
                  GO TO 270
                  ENDIF
C           WRITE MAPX GRID POINT DEFINITION PARAMETERS
               CALL SWXGRD (IVXGRD,IMXFST,IMXLST,NXGPOS,NXA,
     *            NSEGS,SWRK2(ID1),SWRK2(ID2),SWRK2(ID3),
     *            UNSD,LWXGRD,SWORK(LWORK*4+1),SWORK(LWORK*4+1),
     *            NUMERR,IERR)
               IF (IERR.NE.0) THEN
                  IF (IERR.EQ.-1) THEN
C                 WORK ARRAY TOO SMALL - CONTINUE PROCESSING
                     ELSE
                        WRITE (LP,460) 'XGRD'
                        CALL SULINE (LP,2)
                        GO TO 270
                     ENDIF
                  ENDIF
               IMXFST=0
               NFGMPX=NFGMPX+1
               IF (NMPXID+1.GT.MMPXID) THEN
                  WRITE (LP,565) MMPXID
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 270
                  ENDIF
               NMPXID=NMPXID+1
               MPXID(NMPXID)=XNAME
C              -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C           SET NEXT POSITION IN TS ARRAY
150            NTPOS=TS(NTPOS+2)-1
               IF (NTPOS.LE.MTS) GO TO 120
160            CONTINUE
            IMPFO=0
C        CHECK IF ERRORS ENCOUNTERED
            IF (INDERR.GT.0) THEN
               WRITE (LP,570) 'MPFO','FORECAST ',FGID
               CALL SULINE (LP,2)
               GO TO 170
               ENDIF
C           -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C        CHECK NUMBER OF MAP TIME SERIES FOUND IN SEGMENTS
            IF (NRMPID.EQ.0) THEN
               WRITE (LP,480) FGID
               CALL SUWRNS (LP,2,NUMWRN)
               IMPFO=-1
               GO TO 170
               ENDIF
C        CHECK FOR DUPLICATE MAP IDENTIFIERS
            CALL SOIDUP (NRMPID,RMPID,NRMPDP,IERR)
C        WRITE MAP COMPUTATIONAL ORDER FOR FORECAST GROUP
            CALL SWMPFO (IVMPFO,UNSD,FGID,NRMPID,RMPID,LWORK,SWORK(1),
     *         LWORK,SWORK(LWORK+1),NUMERR,IERR)
            IF (IERR.GT.0) THEN
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,520) IERR
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               GO TO 170
               ENDIF
            NFGMAP=NFGMAP+1
            IMPFO=1
C        CHECK IF LAST FORECAST GROUP PROCESSED
            IF (IFG.EQ.NFG) THEN
C           CHECK IF MAP AREA IS IN MORE THAN ONE FORECAST GROUP
               CALL SORDUP (NRIDCK,RIDCK,NFGCK,MRMPDP,NRMPDP,RMPDP,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (LP,530) IERR
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 180
                  ENDIF
               ENDIF
C           -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C        CHECK IF LAST FORECAST GROUP PROCESSED
170         IF (IFG.EQ.NFG) THEN
C           CHECK IF ERRORS ENCOUNTERED
               IF (INDERR.GT.0) THEN
                  WRITE (LP,570) 'MPCO','CARRYOVER',CGID
                  CALL SULINE (LP,2)
                  GO TO 180
                  ENDIF
C           CHECK IF ANY FORECAST GROUPS WITH MAP TIME SERIES FOUND
               IF (NFGMAP.EQ.0) THEN
                  IF (NFGMPX.EQ.0) THEN
                     WRITE (LP,490) 'WARNING',CGID
                     CALL SUWRNS (LP,2,NUMWRN)
                     ELSE
                        WRITE (LP,490) 'NOTE',CGID
                        CALL SULINE (LP,2)
                     ENDIF
                  GO TO 180
                  ENDIF
               ENDIF
C        CHECK IF MPFO PARAMETER RECORD WRITTEN FOR THIS FORECAST GROUP
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,590) XTYPE,IMPFO
               CALL SULINE (IOSDBG,1)
               ENDIF
C           -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
            IF (IMPFO.EQ.1) THEN
C           CHECK IF ERRORS ENCOUNTERED
               IF (INDERR.GT.0) THEN
                  WRITE (LP,570) 'MPCO','CARRYOVER',CGID
                  CALL SULINE (LP,2)
                  GO TO 180
                  ENDIF
C           UPDATE MAP COMPUTATIONAL ORDER FOR CARRYOVER GROUP
               CALL SWMPCO (IVMPCO,UNSD,IMPFST,IMPLST,
     *            ICG,NFG,FGID,NFGMAP,NRMPDP,RMPDP,NRMPID,
     *            LWORK,SWORK(LWORK*2+1),NCOPOS,NUMERR,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (LP,600) IERR
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 180
                  ENDIF
               IMPFST=0
               ENDIF
C           -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
180         CONTINUE
         NUMCG=NUMCG+1
         IF (INDERR.EQ.0.AND.NFGMAP.GT.0) THEN
            IMPLST=1
C        WRITE MAP COMPUTATIONAL ORDER FOR CARRYOVER GROUP
            CALL SWMPCO (IVMPCO,UNSD,IMPFST,IMPLST,
cfan *         ICG,NFG,FGID,NFGMAP,NRMPDP,RMPDP,NRMPID,
     *         ICG,NFG,FGID,NFGMAP,NRMPDP,RMPID,NRMPID,        !cfan
     *         LWORK,SWORK(LWORK*2+1),NCOPOS,NUMERR,IERR)
cfan
c    HSD Bug r24-24:
C    When running ORDER there are some strange results for the forecast Group WAPSIFW.
c    The printed out list of those MAP identifiers are just a bunch of "start of header"
c    characters (view in nedit) to see them).
c    Find:
c    Varibal transfer should be RMPID instead of RMPDP in the subroutine SWMPCO calling.
cfan
            IF (IERR.GT.0) THEN
               WRITE (LP,600) IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 190
               ENDIF
            WRITE (LP,610) NFGMAP,CGID
            CALL SULINE (LP,2)
C           -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
            ENDIF
190      CONTINUE
C
C      -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  CHECK IF ANY CARRYOVER GROUPS SUCCESSFULLY PROCESSED
      IF (NUMCG.GT.0) THEN
         WRITE (LP,630) NUMCG
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,640)
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 270
         ENDIF
C
C  CHECK IF ANY CARRYOVER GROUPS SUCCESSFULLY PROCESSED
      IF (NFMPID.EQ.0) THEN
         WRITE (LP,580) 'FMPO'
         CALL SULINE (LP,2)
         GO TO 270
         ENDIF
C
C  CHECK IF ERROR CREATING FUTURE MAPID ARRAY
      IF (IFPERR.EQ.1) GO TO 200
C
C  CHECK FOR DUPLICATE FUTURE MAP IDENTIFIERS
      CALL SOIDUP (NFMPID,FMPID,NFDUP,IERR)
        IF (IERR.GT.0) THEN
           WRITE (LP,620) IERR
           CALL SULINE (LP,1)
           ENDIF
C
C  WRITE FUTURE MAP COMPUTATIONAL ORDER PARAMETERS
      CALL SWFMPO (IVFMPO,UNSD,NFMPID,FMPID,LARRAY,ARRAY,NUMERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,460) 'FMPO'
         CALL SULINE (LP,2)
         ENDIF
C
C  CHECK IF ANY MAPX AREAS PROCESSED
200   IF (NXA.EQ.0) GO TO 210
C
C  WRITE MAPX COMPUTATIONAL ORDER PARAMETERS
      IMXLST=1
      CALL SWMXCO (IVMXCO,NXA,XNAME,NXGPOS,
     *   IMXFST,IMXLST,NMXPOS,
     *   UNSD,LWORK,SWORK(LWORK*3+1),NUMERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,460) 'MXCO'
         CALL SULINE (LP,2)
         GO TO 270
         ENDIF
      CALL SWXGRD (IVXGRD,IMXFST,IMXLST,NXGPOS,NXA,
     *   NSEGS,SWRK2(ID1),SWRK2(ID2),SWRK2(ID3),
     *   UNSD,LWXGRD,SWORK(LWORK*4+1),SWORK(LWORK*4+1),
     *   NUMERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,460) 'XGRD'
         CALL SULINE (LP,2)
         GO TO 270
         ENDIF
C
C  SET COMPLETE FLAG IN GENERAL ORDER PARAMETER ARRAY TO COMPLETE AND
C  SET NUMBER OF AREAS ADDED TO ZERO
210   ICOMPL=1
      NAMAP=0
      NAFMAP=0
      NAMAPX=0
      ICBASN=0
      ICFMAP=0
      INCLUDE 'scommon/callsuwtor'
      IF (IERR.GT.0) THEN
         WRITE (LP,420) IERR
         CALL SULINE (LP,1)
         ENDIF
C
C  SET MAP STATUS FLAG TO COMPLETE
      ICUGNL(1)=0
      INCLUDE 'scommon/callswugnl'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IPRINT.EQ.0) GO TO 270
C
C  PRINT COMPUTATIONAL ORDER PARAMETERS

      IPRNT=1
C
C  READ GENERAL COMPUTATIONAL ORDER PARAMETERS
      IPRERR=1
      INCLUDE 'scommon/callsrordr'
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,830)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 220
            ENDIF
         WRITE (LP,820) IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
C
C  PRINT GENERAL COMPUTATIONAL ORDER PARAMETERS
      INCLUDE 'scommon/callspordr'
      IF (IERR.GT.0) THEN
         WRITE (LP,810) 'ORDR'
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
C
C      -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  PRINT MAP COMPUTATIONAL ORDER PARAMETERS
C
220   IPTRNF=0
      IPTRNC=0
C
230   XNAME=' '
      IPTRCG=IPTRNC
      IPTRFG=IPTRNF
C
      WRITE (LP,300)
      CALL SULINE (LP,1)
      WRITE (LP,320)
      CALL SULINE (LP,1)
C
C  READ MAP CARRYOVER GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      IPRERR=0
      CALL SRMPCO (XNAME,IPTRCG,LARRAY,ARRAY,IVMPCO,CGID,UNUSED,
     *   MFGIDS,FGIDS,NFGIDS,NRMPFG,MRMPDP,RMPDP,NRMPDP,IPRERR,IPTRNC,
     *   IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,780) 'MPCO'
            CALL SULINE (LP,2)
            GO TO 250
            ENDIF
         WRITE (LP,560) 'MPCO',XNAME,IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 250
         ENDIF
C
C  PRINT MAP CARRYOVER GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      CALL SPMPCO (IPRNT,NUMCG,IVMPCO,CGID,
     *   NFGIDS,FGIDS,RMPDP,NRMPFG,NRMPDP,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,800) 'MPCO',XNAME
         CALL SUERRS (LP,2,NUMERR)
         GO TO 250
         ENDIF
C
C  READ AND PRINT MAP FORECAST GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      IPTRFG=0
      DO 240 N=1,NFGIDS
         CALL UMEMOV (FGIDS(N),XNAME,2)
         CALL SRMPFO (XNAME,IPTRFG,LARRAY,ARRAY,IVMPFO,FGID,UNUSED,
     *      MRMPID,RMPID,NRMPID,MRMPID,MAPSR,NMAPSR,IPRERR,IPTRNF,IERR)
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) THEN
               WRITE (LP,780) 'MPFO'
               CALL SULINE (LP,2)
               GO TO 240
               ELSE
                  WRITE (LP,560) 'MPFO',XNAME,IERR
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 250
               ENDIF
            ENDIF
         IPTRFG=IPTRNF
         CALL SPMPFO (IPRNT,N,IVMPFO,FGID,
     *      RMPID,NRMPID,MAPSR,NMAPSR,IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,800) 'MPFO',XNAME
            CALL SUERRS (LP,2,NUMERR)
            GO TO 250
            ENDIF
         IPTRCG=IPTRNF
240      CONTINUE
C
      IPTRCG=IPTRNC
C
C  CHECK IF LAST CARRYOVER GROUP PROCESSED
      IF (IPTRNC.GT.0) THEN
         WRITE (LP,320)
         CALL SULINE (LP,1)
         WRITE (LP,300)
         CALL SULINE (LP,1)
         GO TO 230
         ENDIF
C
C      -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  PRINT MAPX COMPUTATIONAL ORDER PARAMETERS
C
250   WRITE (LP,300)
      CALL SULINE (LP,1)
      WRITE (LP,320)
      CALL SULINE (LP,1)
C
      XNAME='**NONE**'
C
C  READ MAPX CARRYOVER GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      MXA=LSWORK/3
      IP1=1
      IP2=MXA*2+1
      IPRERR=0
      CALL SRMXCO (IVMXCO,UNUSED,MXA,NXA,SWORK(IP1),SWORK(IP2),
     *   LARRAY,ARRAY,IPRERR,IERR)
      IF (IERR.GT.0) THEN
         IF (IPRERR.EQ.1) GO TO 260
         IF (IERR.EQ.2) THEN
            WRITE (LP,780) 'MXCO'
            CALL SULINE (LP,2)
            GO TO 260
            ENDIF
         IF (IERR.EQ.4) THEN
            WRITE (LP,790) 'MXCO'
            CALL SULINE (LP,2)
            GO TO 260
            ENDIF
         WRITE (LP,560) 'MXCO',XNAME,IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 260
         ENDIF
C
C  PRINT MAPX CARRYOVER GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      CALL SPMXCO (IPRNT,NXA,IVMXCO,SWORK(IP1),SWORK(IP2),IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,800) 'MXCO',XNAME
         CALL SUERRS (LP,2,NUMERR)
         GO TO 260
         ENDIF
C
C  READ XGRD PARAMETERS
      CALL SRXGRD (IVXGRD,LWORK,NXA,SWORK(1),
     *   LARAY,SWRK2(ID1),SWRK2(ID2),SWRK2(ID3),
     *   UNUSED,LARRAY,ARRAY,ARRAY,IPRERR,IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,780) 'XGRD'
            CALL SULINE (LP,2)
            GO TO 260
            ELSE
               WRITE (LP,560) 'XGRD',XNAME,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 260
            ENDIF
         ENDIF
C
C  PRINT XGRD PARAMETERS
      LEVEL=1
      CALL SPXGRD (IVXGRD,NXA,SWORK(1),
     *   SWRK2(ID1),SWRK2(ID2),SWRK2(ID3),
     *   LEVEL,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,800) 'XGRD',XNAME
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
C
C      -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C
C  READ FUTURE MAP COMPUTATIONAL ORDER PARAMETERS
260   CALL SRFMPO (LARRAY,ARRAY,IVFMPO,UNUSED,MFMPID,FMPID,NFMPID,
     *   IPRERR,IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,780) 'FMPO'
            CALL SULINE (LP,2)
            ELSE
               CALL SUBLID (XNAME,IERR)
               WRITE (LP,563) 'FMPO',IERR
               CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 270
         ENDIF
C
C  PRINT FUTURE MAP COMPUTATIONAL ORDER PARAMETERS
      CALL SPFMPO (IPRNT,IVFMPO,FMPID,NFMPID,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,805) 'FMPO'
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
270   IF (NUMERR.GT.0) THEN
         WRITE (LP,840) NUMERR
         CALL SULINE (LP,2)
         ISTAT=1
         ENDIF
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,850)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
280   FORMAT (' *** ENTER SORDER')
290   FORMAT ('0*--> DETERMINE ',
     *   'MAP, ',
     *   'MAPX AND ',
     *   'FMAP ',
     *   'COMPUTATIONAL ORDER')
300   FORMAT (' ')
310   FORMAT ('0*** ERROR - IN SORDER - ',
     *   'THE VALUE OF LWXGRD (',I5,
     *   ') IS GREATER THAN OR EQUAL TO THE VALUE OF LSWORK (',I5,').')
320   FORMAT (' ',132('#'))
330   FORMAT ('0*** ERROR - INVALID ORDER OPTION : ',A)
340   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
350   FORMAT ('0*** NOTE - NO LEFT PARENTHESIS FOUND. ',A,
     *   ' OPTION SET TO ',A,'.')
360   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,'.')
370   FORMAT ('0*** ERROR - INVALID ',A,' OPTION : ',A)
380   FORMAT ('0*** NOTE - ',A,' OPTION SET TO ',A,'.')
390   FORMAT (' ',A,' OPTION SET TO ',I2)
400   FORMAT ('0*** ERROR - ONE OR MORE OF THE FOLLOWING DATA BASES ',
     *   'ARE NOT ALLOCATED :  PARAMETRIC, PROCESSED DATA AND ',
     *   'FORECAST.')
410   FORMAT ('0*** ERROR - UGNL PARAMETERS NOT DEFINED. PARAMETERS ',
     *   'NEEDED TO RUN ORDER COMMAND.')
420   FORMAT (' SWORDR STATUS CODE=',I3)
430   FORMAT ('0*** NOTE - COMPUTATIONAL ORDER PARAMETER FILE(S) NOT ',
     *   'RESET TO EMPTY BECAUSE ONE OR MORE NON-COMPUTATIONAL ORDER ',
     *   'TYPES FOUND.')
440   FORMAT ('0*** ERROR - SYSTEM ERROR ACCESSING FILES WHEN CALLING ',
     *   'SUBROUTINE WPPCOF : STATUS CODE=',I3)
450   FORMAT ('0*** NOTE - COMPUTATIONAL ORDER PARAMETER FILE ',
     *   'RESET TO EMPTY.')
460   FORMAT ('0*** NOTE - ',A,' PARAMETERS ',
     *     'NOT SUCCESSFULLY WRITTEN BEACUSE ERRORS ENCOUNTERED.')
470   FORMAT (' SOFPID STATUS CODE=',I3)
480   FORMAT ('0*** WARNING - NO MPFO PARAMETERS WRITTEN FOR ',
     *   'FORECAST  GROUP ',2A4,' BECAUSE NO MAP TIME SERIES ',
     *   'FOUND IN ANY SEGMENTS.')
490   FORMAT ('0*** ',A,' - NO MPCO PARAMETERS WRITTEN FOR ',
     *   'CARRYOVER GROUP ',A,' BECAUSE NO MPFO PARAMETERS ',
     *   'SUCCESSFULLY WRITTEN.')
510   FORMAT (' XTYPE=',A,3X,'XNAME=',A)
520   FORMAT (' SWMPFO STATUS CODE=',I3)
530   FORMAT (' SORDUP STATUS CODE=',I3)
540   FORMAT ('0*** ERROR - PARAMETER ARRAY DIMENSION (',I4,
     *   ') IS TOO SMALL TO READ ',A,' PARAMETERS FOR IDENTIFIER ',A,
     *   ' .')
550   FORMAT ('0*** ERROR - ',A,' ',A,' IS NOT DEFINED.')
560   FORMAT ('0*** ERROR - READING ',A,' PARAMETERS FOR ',
     *   'IDENTIFIER ',A,'. STATUS CODE=',I2)
563   FORMAT ('0*** ERROR - READING ',A,' PARAMETERS. STATUS CODE=',I2)
565   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF MAPX AREA IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I4,') EXCEEDED.')
570   FORMAT ('0*** NOTE - ',A,' PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN FOR ',A,' GROUP ',2A4,' BECAUSE ERRORS ',
     *   'ENCOUNTERED.')
580   FORMAT ('0*** NOTE - ',A,' PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN BECAUSE NO FUTURE MAP IDENTIFIERS SUCCESSFULLY ',
     *   'PROCESSED.')
590   FORMAT (' XTYPE=',A,3X,'IMPFO=',I3)
600   FORMAT (' SWMPCO STATUS CODE=',I3)
610   FORMAT ('0*** NOTE - ',I3,' FORECAST GROUPS SUCCESSFULLY ',
     *   'PROCESSED FOR CARRYOVER GROUP ',A,'.')
620   FORMAT (' SOIDUP STATUS CODE=',I3)
630   FORMAT ('0*** NOTE - ',I3,' CARRYOVER GROUPS SUCCESSFULLY ',
     *   'PROCESSED.')
640   FORMAT ('0*** WARNING - NO CARRYOVER GROUPS SUCCESSFULLY ',
     *   'PROCESSED.')
650   FORMAT ('0*** WARNING - NO CARRYOVER GROUPS ARE DEFINED IN ',
     *   'THE FORECAST COMPONENT DATA BASE.')
665   FORMAT ('0*** WARNING - CARRYOVER GROUP ',A,' HAS NO ',
     *   'FORECAST GROUPS.')
670   FORMAT (' CARRYOVER GROUP IDS=' / 10(1X,2A4))
680   FORMAT (' FGID=',2A4)
690   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF FORECAST GROUPS THAT ',
     *   'CAN BE PROCESSED (',I5,') EXCEEDED.')
700   FORMAT ('0*** ERROR - NOT ENOUGH FORECAST GROUPS FOUND FOR ',
     *   'CARRYOVER GROUP ',A,'. ',I3,' FORECAST GROUPS FOUND. ',
     *   I3,' FORECAST GROUPS EXPECTED.')
710   FORMAT ('0*** ERROR - FORECAST GROUP ',A,' HAS NO SEGMENTS.',
     *   ' NSEGEX=',I5)
720   FORMAT ('0*** ERROR - ILLEGAL ARGUMENTS : IOPT=',I2,' IREC=',I5)
730   FORMAT ('0*** ERROR - UNABLE TO DEFINE P, T, OR TS ARRAY DUE TO ',
     *   'LACK OF SPACE : MP=',I5,' MT=',I5,' MTS=',I5)
735   FORMAT ('0*** ERROR - INVALID TS TYPE (',I3,')',
     *   ' FOR SEGMENT NUMBER ',I3,
     *   ' OF FORECAST GROUP ',2A4,
     *   ' AND CARRYOVER GROUP ',A,'.') 
750   FORMAT (' (TS(I),I=NTPOS2,NTPOS3)=',2A4,3X,'DTYPE=',A)
760   FORMAT (' RIDCK(NRIDCK)=',A,3X,'NFGCK(NRDICK)=',I3)
770   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF REGULAR TIME SERIES ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
780   FORMAT ('0*** NOTE - NO ',A,' PARAMETER RECORD FOUND.')
790   FORMAT ('0*** NOTE - PARAMETER TYPE ',A,' IS NOT DEFINED.')
800   FORMAT ('0*** ERROR - PRINTING ',A,' ORDER PARAMETERS FOR ',
     *   ' IDENTIFIER ',A,'.')
805   FORMAT ('0*** ERROR - PRINTING ',A,' ORDER PARAMETERS.')
810   FORMAT ('0*** ERROR - PRINTING ',A,' PARAMETERS')
820   FORMAT ('0*** ERROR - READING PARAMETER TYPE ORDR. ',
     *   'STATUS CODE=',I2)
830   FORMAT ('0*** ERROR - ORDR PARAMETER RECORD NOT FOUND.')
840   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED BY ORDER ',
     *   'COMMAND.')
850   FORMAT (' *** EXIT SORDER')
C
      END
