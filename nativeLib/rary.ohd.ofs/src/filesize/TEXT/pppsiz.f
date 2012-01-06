C MODULE PPPSIZ
C-----------------------------------------------------------------------
C
      SUBROUTINE PPPSIZ (USER,XCMD,DCBDDN,DCBMBR,DSKUNT,MDSTRK,NDSTRK,
     *   LDEBUG,LPUNCH,ISTAT)
C
C  ROUTINE TO COMPUTE THE NUMBER OF TRACKS FOR EACH OF THE NWSRFS
C  FORECAST SYSTEM PARAMETRIC DATA BASE FILES.
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR,XCMD
      CHARACTER*4 XPARM
      CHARACTER*7 XFILE
      CHARACTER*8 XWORD
C
C jgg following added by jto to fix bug r25-12 12/05
      CHARACTER*8 CHAR
      CHARACTER*80 LINE
      CHARACTER DLIM
      DATA DLIM/' '/
      INTEGER CLEN
C end of additions
C
C  PARAMETER TYPES AND DEFAULT WORK LENGTHS
      PARAMETER (NPMARR=33)
      INTEGER IPMARR(2,NPMARR)/
     *   4HUSER,40,4HURRS,56,4HSTBN,1000,
     *   4HGENL,30,4HPCPN,88,4HTEMP,82,4HPE  ,59,4HRRS ,27,
     *   4HBASN,114,
     *   4HFFG ,92,
     *   4HASSM,70,
     *   4HMAPS,34,4HMAP ,84,4HMAT ,37,4HMAPE,48,
     *   4HMAPX,17,
     *   4HORDR,10,4HMPCO,10,4HMPFO,157,4HFMPO,105,4HXGRD,0,
     *   4HGBOX,17,4HGMDR,807,4HMARO,75,4HRFRO,16,
     *   4HNTWK,15,
     *   4HOP24,1005,4HOPVR,705,4HOT24,230,4HOE24,80,4HORRS,2005,
     *   4HGP24,2805,4HOG24,2005/
C
      DIMENSION NDSTRK(MDSTRK)
      PARAMETER (MTRKFL=5)
      DIMENSION TRKFL(MTRKFL)
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/pppsiz.f,v $
     . $',                                                             '
     .$Id: pppsiz.f,v 1.4 2006/05/09 13:13:20 jgofus Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      ICDBEG=1
      ICDEND=72
C
      NTTRKS=0
      CALL UMEMST (0.0,TRKFL,MTRKFL)
      NXTRKS=0
      MCHAR=0
      MTEMP=0
      NFILE=0
      IXPARM=0
      MXPARM=0
      NTPARM=0
      IFNAME=-1
      NUMEND=0
C
C  SET NUMBER OF WORDS IN PARAMETER RECORD HEADER
      LENHDR=5
C
C  SET NUMBER OF I*2 WORDS IN PARAMETER RECORD
      LRECP2=LRECPP*2
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
C  GET DCB FOR PARAMETER FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PPPPARM',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'UFLDCB',IERR
         GO TO 260
         ENDIF
C
C  GET NUMBER OF BLOCKS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'UDKBLK',IERR
         GO TO 260
         ENDIF
      NPRBLK=LBLOCK/LRECL
      TRKREC=NPRBLK*NBLKS
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ CARD
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'RPCARD',IERR
         GO TO 260
         ENDIF
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
C
20    IF (XWORD.EQ.'MAXPARMS') GO TO 210
C
C  CHECK IF PARAMETER TYPE
      IF (NCHAR.LE.4) THEN
         XFILE='PPPPARM'
         IF (XPARM.EQ.'MISC') GO TO 50
         CALL SUBSTR (XWORD,1,4,IPARM,1)
         DO 30 I=1,NMPTYP
            IF (IPARM.EQ.IPDTDR(1,I)) THEN
               NFILE=IPDTDR(2,I)
               IFNAME=0
               GO TO 50
               ENDIF
30          CONTINUE
         ENDIF
C
C  CHECK NUMBER OF FIELDS
      IF (NFIELD.NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,280) 'FILE NAME'
         GO TO 40
         ENDIF
C
C  GET FILE NAME
      IF (NCHAR.NE.8) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,300) 'IN FILE NAME'
         GO TO 40
         ENDIF
      XFILE=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XFILE,NCHAR)
      IFNAME=1
C
C  CHECK FOR VALID FILE NAME KEYWORD
      IF (XFILE.NE.'PPPPARM') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,320)
         ELSE
C        CHECK IF LAST CHARACTER IS AN INTERVER         
            NFLD=1
            CALL UINTFX (NFILE,IFSTOP(NFLD),IFSTOP(NFLD),IERR)
            IF (IERR.GT.0.OR.NFILE.LT.1.OR.NFILE.GT.NMPPPF) THEN
               CALL UEROR (LP,1,-1)
               WRITE (LP,320)
               ENDIF
         ENDIF
C
      IF (NUMEND.GT.0) NUMEND=NUMEND+1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ PARAMETER TYPE CARD
40    CALL RPCARD (IBUF,IERR)
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'RPCARD',IERR
         GO TO 260
         ENDIF
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 40
C
C  GET FIRST FIELD
      NFLD=1
      XWORD=' '
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 40
      IF (XWORD.EQ.'MAXPARMS') GO TO 210
C
C  CHECK NUMBER OF FIELDS ON CARD
50    IF (NFIELD.EQ.1) GO TO 200
      IF (NFIELD.GT.2) THEN
         NFLD=3
         XWORD=' '
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
         IF (LDEBUG.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,*) 'XWORD=',XWORD
            ENDIF
         IF (XWORD.EQ.'$') THEN
            NFIELD=NFLD-1
            GO TO 60
            ENDIF
         IF (NFIELD.GT.3) THEN
            NFLD=4
            XWORD=' '
            NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
            CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
            IF (LDEBUG.GT.0) THEN
               CALL ULINE (LP,1)
               WRITE (LP,*) 'XWORD=',XWORD
               ENDIF
            IF (XWORD.EQ.'$') THEN
               NFIELD=NFLD-1
               GO TO 60
               ENDIF
            CALL UEROR (LP,1,-1)
            WRITE (LP,280) 'PARAMETER TYPE'
            GO TO 40
            ENDIF
         ENDIF
C
C  SET PARAMETER TYPE
60    NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.LE.0.OR.NCHAR.GT.4) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,330) XPARM
         GO TO 40
         ENDIF
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 40
C
      XPARM=XWORD
C
      IF (XPARM.EQ.'CHAR') GO TO 80
      IF (XPARM.EQ.'MMMT') GO TO 90
      IF (XPARM.EQ.'MXCO') GO TO 100
      IF (XPARM.EQ.'XGRD') GO TO 110
      IF (XPARM.EQ.'MISC') GO TO 130
C
C  CHECK FOR VALID TYPE
      CALL SUBSTR (XPARM,1,4,IPARM,1)
      DO 70 I=1,NPMARR
         IF (IPARM.EQ.IPMARR(1,I)) GO TO 130
70       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,340) XPARM
      GO TO 130
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS STATIONS CHARACTERISTICS TYPE
C
80    NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IVALUE)
      IF (MCHAR.LT.IVALUE) MCHAR=IVALUE
C
C  CALCULATE NUMBER OF RECORDS
      NREC=IUNRCD(IVALUE,LRECP2)*12+1
      TRAKS=NREC/TRKREC
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
          WRITE (LP,*) 'XPARM=',XPARM,
     *      ' IVALUE=',IVALUE,
     *      ' LRECP2=',LRECP2,
     *      ' NREC=',NREC,
     *      ' TRKREC=',TRKREC,
     *      ' TRAKS=',TRAKS
         ENDIF
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS MAX/MIN TEMPERATURE TYPE

90    NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IVALUE)
      IF (MTEMP.LT.IVALUE) MTEMP=IVALUE
C
C  CALCULATE NUMBER OF RECORDS
      NREC=IUNRCD(IVALUE,LRECP2)*24+1
      TRAKS=NREC/TRKREC
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XPARM=',XPARM,
     *      ' IVALUE=',IVALUE,
     *      ' LRECP2=',LRECP2,
     *      ' NREC=',NREC,
     *      ' TRKREC=',TRKREC,
     *      ' TRAKS=',TRAKS
         ENDIF
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS MAPX COMPUATATIONAL ORDER TYPE

100   IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,280) 'PARAMETER TYPE'
         GO TO 40
         ENDIF
C
C  SET NUMBER OF MAPX AREAS IN CARRYOVER GROUP
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NXA)
C
C  CALCULATE NUMBER OF WORDS
      NWDS=LENHDR+4+NXA*3
C
C  CALCULATE NUMBER OF RECORDS
      NREC=IUNRCD(NWDS,LRECPP)
C
C  CALCULATE NUMBER OF TRACKS
      TRAKS=NREC/TRKREC
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XPARM=',XPARM,
     *     ' NWDS=',NWDS,
     *     ' LRECPP=',LRECPP,
     *     ' NREC=',NREC,
     *     ' TRKREC=',TRKREC,
     *     ' TRAKS=',TRAKS
         ENDIF
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS XRGD COMPUATATIONAL ORDER TYPE

110   IF (NFIELD.EQ.2.OR.NFIELD.EQ.3) THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,280) 'PARAMETER TYPE'
            GO TO 40
         ENDIF
C
C  SET NUMBER OF MAPX AREAS IN CARRYOVER GROUP
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NXA)
C
      NSEG=15
C
      IF (NFIELD.EQ.2) GO TO 120
C
C  SET NUMBER OF LINE SEGMENTS IN BASIN BOUNDARY DEFINITION
      NFLD=3
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NSEG)
C
C  CALCULATE NUMBER OF WORDS
120   NWDS=LENHDR+4+(NXA*(1+NSEG*3))/2
C
C  CALCULATE NUMBER OF RECORDS
      NREC=IUNRCD(NWDS,LRECPP)
C
C  CALCULATE NUMBER OF TRACKS
      TRAKS=NREC/TRKREC
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XPARM=',XPARM,
     *      ' NWDS=',NWDS,
     *      ' LRECPP=',LRECPP,
     *      ' NREC=',NREC,
     *      ' TRKREC=',TRKREC,
     *      ' TRAKS=',TRAKS
         ENDIF
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,350)
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),MXREC)
      NTPARM=NTPARM+MXREC
C
C  CHECK FOR OPTIONAL THIRD FIELD
      NWDS=0
      IF (NFIELD.GT.2) THEN
         NFLD=3
         IF (IFTYPE(NFLD).EQ.1) THEN
            CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NWDS)
            IF (XPARM.NE.'MISC') GO TO 160
            ENDIF
         ENDIF
C
C  CHECK IF PERCENT OF SPACE TO BE ADDED
      IF (XPARM.EQ.'MISC') THEN
         NNFILE=NFILE
         IF (NWDS.GT.0) NNFILE=NWDS
         TRAKS=TRKFL(NNFILE)*(MXREC/100.)
         IF (LDEBUG.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,*)
     *         ' NWDS=',NWDS,
     *         ' NNFILE=',NNFILE,
     *         ' TRAKS=',TRAKS,
     *         ' '
            ENDIF
         TRKFL(NNFILE)=TRKFL(NNFILE)+TRAKS
         GO TO 170
         ENDIF
C
C  GET AVERAGE WORDS
      CALL SUBSTR (XPARM,1,4,IPARM,1)
      DO 140 I=1,NPMARR
         IF (IPARM.EQ.IPMARR(1,I)) GO TO 150
140      CONTINUE
         CALL UEROR (LP,1,-1)
         WRITE (LP,360) XPARM
         GO TO 40
150   IF (IPMARR(2,I).GT.0) THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,370) XPARM
            GO TO 40
         ENDIF
      NWDS=LENHDR+IPMARR(2,I)
C
C  CALCULATE NUMBER OF TRACKS FOR THIS PARAMETER TYPE
160   NREC=MXREC*IUNRCD(NWDS,LRECPP)
      TRAKS=NREC/TRKREC
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XPARM=',XPARM,
     *      ' NWDS=',NWDS,
     *      ' LRECPP=',LRECPP,
     *      ' MXREC=',MXREC,
     *      ' NREC=',NREC,
     *      ' TRKREC=',TRKREC,
     *      ' TRAKS=',TRAKS
         GO TO 40
         ENDIF
C
170   CALL ULINE (LP,1)
      WRITE (LP,380) XPARM,TRAKS
C
C  CHECK IF FILE NAME SPECIFIED      
      IF (IFNAME.EQ.0) THEN
C     GET FILE NUMBER FOR PARAMETER TYPE      
         CALL SUBSTR (XPARM,1,4,IPARM,1)
         DO 180 I=1,NMPTYP
            IF (IPARM.EQ.IPDTDR(1,I)) THEN
               NFILE=IPDTDR(2,I)
               GO TO 190
               ENDIF
180         CONTINUE
         CALL UEROR (LP,1,-1)
         WRITE (LP,290) XPARM
         ENDIF
190   TRKFL(NFILE)=TRKFL(NFILE)+TRAKS
      GO TO 40
C
C  CHECK FOR END CARD
200   NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.NE.3) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,300) 'ON END CARD'
         GO TO 210
         ENDIF
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
      IF (XWORD.NE.'END') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,390)
         GO TO 20
         ENDIF
      NUMEND=NUMEND+1
C
      IF (IFNAME.EQ.1) THEN
C     CALCULATE NUMBER OF TRACKS FOR THIS FILE
         NTRKS=TRKFL(NFILE)+.99
         NTTRKS=NTTRKS+NTRKS
         NDSTRK(KPPRMU(NFILE))=NTRKS
         CALL ULINE (LP,2)
         WRITE (LP,400) XFILE,NFILE,KPPRMU(NFILE),NTRKS
         GO TO 10
         ENDIF
C
      IF (NUMEND.EQ.0) GO TO 10         
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     
C  CHECK IF FILE NAMES SPECIFIED
210   IF (IFNAME.EQ.0) THEN
C     CALCULATE NUMBER OF TRACKS FOR EACH FILE
         DO 220 NFILE=1,MTRKFL
            NTRKS=TRKFL(NFILE)+.99
            NTTRKS=NTTRKS+NTRKS
            NDSTRK(KPPRMU(NFILE))=NTRKS
            CALL ULINE (LP,2)
            WRITE (LP,400) XFILE,NFILE,KPPRMU(NFILE),NTRKS
220         CONTINUE
         ENDIF
C
      IF (NUMEND.EQ.1) GO TO 225         
C
C  PROCESS MAXIMUM NUMBER OF PARAMETERS RECORDS
      IXPARM=1
      IF (NFIELD.GT.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,280) 'MAXPARMS'
         ENDIF
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.NE.8) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,420) 'MAXPARMS'
         GO TO 260
         ENDIF
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
      IF (XWORD.NE.'MAXPARMS') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,430) 'MAXPARMS'
         GO TO 260
         ENDIF
C
      IF (NFIELD.GT.1) THEN
C     GET MAXIMUM NUMBER OF PARAMETER RECORDS
         NFLD=2
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,440) NFLD
            GO TO 260
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),MXPARM)
         ENDIF 
C
225   IF (MXPARM.EQ.0) THEN
C     ROUND MAXPARMS         
         CALL UROUND ('INT4',NTPARM,100,'HIGH',MXPARM,IERR)
         IF (IXPARM.EQ.1) THEN
            CALL UWARN (LP,1,-1)
            WRITE (LP,445) NFIELD,MXPARM
            ELSE
               CALL UWARN (LP,1,-1)
               WRITE (LP,447) MXPARM
            ENDIF
         ELSE
            IDIFF=NTPARM-MXPARM
            IPCT=IABS(IDIFF*100/MXPARM)
            ICHK=10
            IF (IPCT.GT.ICHK) THEN
               MXPARMO=MXPARM
               CALL UROUND ('INT4',NTPARM,100,'HIGH',MXPARM,IERR)   
               CALL UWARN (LP,1,-1)
               WRITE (LP,449) MXPARMO,NTPARM,ICHK,MXPARM            
               ENDIF
         ENDIF
C
      IF (NUMEND.EQ.1) GO TO 240 
C
C  READ 'END' CARD
230   CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'RPCARD',IERR
         GO TO 260
         ENDIF
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 230
C
C  GET FIRST FIELD
      NFLD=1
      XWORD=' '
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 230
C
      IF (NFIELD.EQ.1.AND.XWORD.EQ.'END'.AND.NCHAR.EQ.3) GO TO 240
         CALL UEROR (LP,1,-1)
         WRITE (LP,280) 'END'
         GO TO 260
C
C  GET DCB FOR INDEX FILE
240   CALL UFLDCB (DCBDDN,DCBMBR,'PPPINDEX',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'UFLDCB',IERR
         GO TO 260
         ENDIF
C
C  GET NUMBER OF BLOCKS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,480) 'UDKBLK',IERR
         GO TO 260
         ENDIF
      NPRBLK=LBLOCK/LRECL
      TRKREC=NPRBLK*NBLKS
C
C  CALCULATE NUMBER OF TRACKS FOR INDEX FILE
      RVALUE=MXPARM
      XTRKS=((2+MXPTYP*2)+(RVALUE*2))/TRKREC
      NXTRKS=XTRKS+.99
      NTTRKS=NTTRKS+NXTRKS
      NDSTRK(KPPIDX)=NXTRKS
      CALL ULINE (LP,2)
      WRITE (LP,450) 'PPPINDEX',KPPIDX,NXTRKS
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ALL UNITS HAVE AT LEAST 1 TRACK ALLOCATED
      DO 250 I=1,NMPPPF
         IF (NDSTRK(KPPRMU(I)).EQ.0) THEN
            NDSTRK(KPPRMU(I))=1
            NTTRKS=NTTRKS+1
            CALL ULINE (LP,2)
            WRITE (LP,460) KPPRMU(I),NDSTRK(KPPRMU(I))
            ENDIF
250      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,470) NTTRKS
      GO TO 270
C
260   ISTAT=1
C
270   IF (ISTAT.EQ.0) THEN
         CALL ULINE (LPUNCH,4)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,490) XCMD,'INDEX',NXTRKS,'FILES',NMPPPF
         WRITE (LPUNCH,500) ' '
         WRITE (LPUNCH,500) XCMD

         LINE=' INDEX'
         CALL KKI2AP(NXTRKS,CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
         WRITE (LPUNCH,500) LINE(1:LENSTR(LINE))
         LINE=' FILES'
         CALL KKI2AP(NMPPPF,CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
         WRITE (LPUNCH,500) LINE(1:LENSTR(LINE))
C end of changes

         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,510) 'TRACKS',(NDSTRK(KPPRMU(I)),I=1,NMPPPF)
         LINE=' TRACKS'
         DO 95 I=1,NMPPPF
           CALL KKI2AP(NDSTRK(KPPRMU(I)),CHAR,CLEN)
           CALL KKCONC(LINE,CHAR,DLIM)
   95      CONTINUE
         WRITE (LPUNCH,500) LINE(1:LENSTR(LINE))
C end of changes

         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,500) 'MAXCHAR',MCHAR
         LINE=' MAXCHAR'
         CALL KKI2AP(MCHAR,CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
         WRITE (LPUNCH,500) LINE(1:LENSTR(LINE))
C end of changes

         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,500) 'MAXTEMP',MTEMP
         LINE=' MAXTEMP'
         CALL KKI2AP(MTEMP,CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
         WRITE (LPUNCH,500) LINE(1:LENSTR(LINE))
C end of changes
         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,510) ' END'
         WRITE (LPUNCH,500) ' END'
C end of changes	 
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
280   FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON ',A,' CARD.')
290   FORMAT ('+*** ERROR - FILE NUMBER CANNOT BE DETERMINED FOR ',
     *  'PARAMETER TYPE ',A,'.')
300   FORMAT ('+*** ERROR - INVALID NUMBER OF CHARACTERS ',A,'.')
320   FORMAT ('+*** ERROR - INVALID FILE NAME.')
330   FORMAT ('+*** ERROR - INVALID NUMBER OF CHARACTERS FOR ',
     *   'PARAMETER TYPE ',A,'.')
340   FORMAT ('+*** ERROR - PARAMETER TYPE ',A,' NOT RECOGNIZED.')
350   FORMAT ('+*** ERROR - PARAMETER TYPE NOT FOLLOWED BY ',
     *   'AN INTEGER VALUE.')
360   FORMAT ('+*** ERROR - PARAMETER TYPE ',A,' IS NOT VALID.')
370   FORMAT ('+*** ERROR - NUMBER OF WORDS FOR PARAMETER TYPE ',A,
     *   ' IS NOT GREATER JHEN ZERO.')
380   FORMAT (' ',15X,'TRACKS FOR TYPE ',A,' = ',F7.2)
390   FORMAT ('+*** ERROR - INVALID END CARD.')
C jgg following line replaced by jto with below for bug r25-12
C 400   FORMAT ('0',15X,'TRACKS FOR FILE ',A,I1,' UNIT ',I2.2,' = ',I3)
400   FORMAT ('0',15X,'TRACKS FOR FILE ',A,I1,' UNIT ',I2.2,' = ',I5)
C end of changes
420   FORMAT ('+*** ERROR - INVALID NUMBER OF CHARACTERS IN KEYWORD ',
     *    'FIELD ON ',A,' CARD.')
430   FORMAT ('+*** ERROR - KEYWORD FIELD FOR ',A,' CARD ',
     *   'INVALID.')
440   FORMAT ('+*** ERROR - FIELD ',I2,' MUST BE AN INTEGER.')
445   FORMAT ('+*** WARNING - ONLY ',I2,' FIELD FOUND ON ',
     *   '''MAXPARMS'' CARD. ',
     *   'MAXPARMS SET TO ',I6,'.')
447   FORMAT ('+*** WARNING - ''MAXPARMS'' CARD NOT FOUND. ',
     *   'MAXPARMS SET TO ',I6,'.')
449   FORMAT ('+*** WARNING - MAXPARMS SPECIFIED ON ''MAXPARMS'' ',
     *      'CARD (',I6,') DIFFERS FROM THE TOTAL NUMBER OF' /
     *   15X,'PARAMETER RECORDS SPECIFED (',I6,') BY MORE THAN ',
     *      I2,' PERCENT. MAXPARMS SET TO ',I6,'.')
C jgg following lines replaced by jto with below for bug r25-12
C 450   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I3)
C 460   FORMAT ('0*** NOTE - TRACKS FOR UNIT ',I2.2,' SET TO ',I3,'.')
450   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I5)
460   FORMAT ('0*** NOTE - TRACKS FOR UNIT ',I2.2,' SET TO ',I5,'.')
C end of changes
470   FORMAT ('0TOTAL TRACKS = ',I5)
480   FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
C jgg following lines replaced by jto with below for bug r25-12
C 490   FORMAT (/ A / A,1X,I3 / A,1X,I3)
C 500   FORMAT (A,1X,I4)
C 510   FORMAT (A,1X,10(I3,1X))
500   FORMAT (A)
C end of changes
C
      END
