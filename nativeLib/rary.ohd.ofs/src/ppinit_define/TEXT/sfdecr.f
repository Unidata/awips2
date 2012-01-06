C MODULE SFDECR
C-----------------------------------------------------------------------
C
C ROUTINE TO CREATE OR CHANGE A STATION IN THE DATA ENTRY CONTROL FILES.
C
      SUBROUTINE SFDECR (DISP,STAID,
     *   DESCRP,STALOC,ICSTAN,NGPS,GPS,
     *   NSRCCD,SRCCD,SRCID,INWSRC,
     *   ITPPVR,ITTAVR,IPPROC,ITYOBS,
     *   RRSTYP,NRRSTP,IRTIME,IRTIMEA,
     *   NGOESN,GOESN,NCDASN,CDASN,
     *   NSTERR,NSTWRN,ISTAT)
C
C  STAN PARAMETER ARRAYS
      DIMENSION STALOC(2)
      DIMENSION INWSRC(NSRCCD),GOESN(NGOESN),CDASN(NCDASN)
C
C  RRS PARAMETER ARRAYS
      INCLUDE 'scommon/dimrrs'
      DIMENSION IRTIMEA(NRRSTP)
C
      CHARACTER*4 DISP,XTYPE,XPE,XSD,XSW
      CHARACTER*4 GPS(NGPS),SRCCD(NSRCCD),SRCID(2,1)
      PARAMETER (MDLYTP=25)
      CHARACTER*4 DLYTP(MDLYTP)
      CHARACTER*4 RRSTP(MRRSTP)
      PARAMETER (MADDTP=25,MDELTP=25)
      CHARACTER*4 ADDTP(MADDTP),DELTP(MDELTP)
      CHARACTER*7 ACTION
      CHARACTER*8 STAID,SAID,SMID,GOESID
      CHARACTER*8 TYPMSG
      CHARACTER*8 BLNK8/' '/
      CHARACTER*20 DESCRP
C
C  CONTROL FILE ARRAYS
      DIMENSION IDTIME(MDLYTP)
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfdecr.f,v $
     . $',                                                             '
     .$Id: sfdecr.f,v 1.4 2002/02/11 21:01:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFDECR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,450) STAID,NSRCCD
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,460) NGPS,(GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         DO 10 I=1,NSRCCD
            WRITE (IOSDBG,470) I,SRCCD(I),(SRCID(J,I),J=1,2)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         WRITE (IOSDBG,480) ITPPVR,ITTAVR,IPPROC,ITYOBS,MRRSTP,
     *      NRRSTP
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,490) NGOESN,(GOESN(I),I=1,NGOESN)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,500) NCDASN,(CDASN(I),I=1,NCDASN)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C      
      NUMERR=0
      NUMWRN=0
      INDERR=0
C    
      NDLYTP=0
      NADDTP=0
      NDELTP=0
C
C  SET TYPES OF DATA GROUPS
      IPCPN=0
      ITEMP=0
      IPE=0
      DO 20 I=1,NGPS
         IF (GPS(I).EQ.'PCPN') IPCPN=1
         IF (GPS(I).EQ.'TEMP') ITEMP=1
         IF (GPS(I).EQ.'PE') IPE=1
20       CONTINUE
C
C  ARRAY INWSRC CONTAINS INDICATORS IF TYPE IS NEW OR OLD
C      0 = OLD
C      1 = NEW
C
C  SET TYPES OF EXTERNAL SOURCES
      ISA=0
      ISM=0
      IGPLT=0
      IGHB5=0
      ICDAS=0
      DO 30 ISRCCD=1,NSRCCD
         ISASM=0
         IGOES=0
         IF (SRCCD(ISRCCD).EQ.'SA') THEN
            ISA=ISRCCD
            ISASM=1
            CALL SFDECK (STAID,SRCCD(ISRCCD),IGOES,ISASM,IFOUND,IERR)
            ENDIF
         IF (SRCCD(ISRCCD).EQ.'SM') THEN
            ISM=ISRCCD
            ISASM=1
            CALL SFDECK (STAID,SRCCD(ISRCCD),IGOES,ISASM,IFOUND,IERR)
            ENDIF
         IF (SRCCD(ISRCCD).EQ.'GPLT') THEN
            IGPLT=ISRCCD
            IGOES=1
            CALL SFDECK (STAID,SRCCD(ISRCCD),IGOES,ISASM,IFOUND,IERR)
            ENDIF
         IF (SRCCD(ISRCCD).EQ.'GHB5') THEN
            IGHB5=ISRCCD
            IGOES=1
            CALL SFDECK (STAID,SRCCD(ISRCCD),IGOES,ISASM,IFOUND,IERR)
            ENDIF
         IF (SRCCD(ISRCCD).EQ.'CDAS') THEN
            ICDAS=ISRCCD
            IGOES=1
            CALL SFDECK (STAID,SRCCD(ISRCCD),IGOES,ISASM,IFOUND,IERR)
            ENDIF
         IF (ISASM.EQ.0.AND.IGOES.EQ.0) THEN
            WRITE (LP,535) SRCCD(ISRCCD),STAID
            CALL SUERRS (LP,2,NUMERR)
            INDERR=1
            ENDIF
         IF (IFOUND.EQ.0) THEN
            INWSRC(ISRCCD)=1
            IF (DISP.EQ.'OLD') THEN
               WRITE (LP,533) DISP,SRCCD(ISRCCD),STAID,'NOT FOUND'
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            ENDIF
         IF (IFOUND.EQ.1) THEN
            INWSRC(ISRCCD)=0
            IF (DISP.EQ.'NEW') THEN
               WRITE (LP,533) DISP,SRCCD(ISRCCD),STAID,'FOUND'
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            ENDIF
30       CONTINUE
C
C  SET INDICATOR IF SOURCE CODES TO BE MARKED DELETED IF DATA BASE NOT
C  ALLOCATED
C      0 = NO
C      1 = YES
      INDDEL=0
C
C  CHECK IF SASM AND GOES DATA ENTRY DATA BASES ALLOCATED
      ISASM=0
      IGOES=0
      IDSASM=0
      IDGOES=0
      IF (ISA.GT.0.OR.ISM.GT.0) ISASM=1
      IF (IGPLT.GT.0.OR.IGHB5.GT.0.OR.ICDAS.GT.0) IGOES=1
      IF (ISA.GT.0.OR.ISM.GT.0) IDSASM=1
      IF (IGPLT.GT.0) IDGOES=1
      IF (IGHB5.GT.0) IDGOES=1
      IF (ICDAS.GT.0) IDGOES=1
      CALL SUDALC (0,0,0,0,0,0,0,IDSASM,IDGOES,0,NUMERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,540)
         CALL SULINE (LP,2)
         INDERR=1
         IF (ISASM.EQ.0) THEN
            IF (IDSASM.EQ.1) THEN
               IF (ISA.EQ.1) THEN
                  IF (INDDEL.EQ.1) SRCCD(ISA)='DELT'
                  ISA=0
                  ENDIF
               IF (ISM.EQ.1) THEN
                  IF (INDDEL.EQ.1) SRCCD(ISM)='DELT'
                  ISM=0
                  ENDIF
               ENDIF
            ENDIF
         IF (IGOES.EQ.0) THEN
            IF (IDGOES.EQ.1) THEN
               IF (INDDEL.EQ.1) THEN
                  IF (IGPLT.GT.0) SRCCD(IGPLT)='DELT'
                  IF (IGHB5.GT.0) SRCCD(IGHB5)='DELT'
                  IF (ICDAS.GT.0) SRCCD(ICDAS)='DELT'
                  ENDIF
               IGPLT=0
               IGHB5=0
               ICDAS=0
               ENDIF
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,510) ISA,ISM,ISASM,IGOES,IDSASM,IDGOES
         CALL SULINE (LP,1)
         WRITE (IOSDBG,520) NSRCCD,(SRCCD(I),I=1,NSRCCD)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF SASM STATION
      IF (IDSASM.EQ.1) THEN
         ISD=0
         ISW=0
C     CHECK IF SNOW ON GROUND OR SNOW WATER EQUIVALENT TO BE TRANSFERRED
         IF (NRRSTP.GT.0) THEN
            DO 70 I=1,NRRSTP
               IF (RRSTYP(I).EQ.'SNOG') ISD=1
               IF (RRSTYP(I).EQ.'SNWE') ISW=1
70             CONTINUE
             ENDIF
          ENDIF
C
C  CHECK IF ERRORS ENCOUNTERED
      IF (INDERR.EQ.1) GO TO 405
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CREATE OR CHANGE STATION IN CONTROL FILE
C
      DO 370 ISRC=1,NSRCCD
C
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,530) ISRC,SRCCD(ISRC),
     *         (SRCID(I,ISRC),I=1,2),INWSRC(ISRC)
            CALL SULINE (IOSDBG,1)
            ENDIF
C
         NDLYTP=0
         DO 80 I=1,MDLYTP
            DLYTP(I)='????'
            IDTIME(I)=0
80          CONTINUE
         NUMRRS=NRRSTP
         IF (NUMRRS.GT.0) THEN
            DO 90 I=1,NUMRRS
               RRSTP(I)=RRSTYP(I)
90             CONTINUE
            ENDIF
C
         IF (INWSRC(ISRC).EQ.-1) GO TO 370
C
         IF (SRCCD(ISRC).NE.'SA'.AND.SRCCD(ISRC).NE.'SM') GO TO 180
C
C  SASM CONTROL FILE
C
C     SET VARIABLES AND MAKE CHECKS
         ITPCPN=ITPPVR
         IF (IPCPN.EQ.1.AND.ITPCPN.EQ.0) ITPCPN=24
         INDERR=0
         IF (ITPPVR.EQ.1) THEN
            WRITE (LP,550) 'PCPN',STAID
            CALL SUERRS (LP,2,NUMERR)
            INDERR=1
            ENDIF
         IF (ITTAVR.EQ.1) THEN
            WRITE (LP,550) 'TEMP',STAID
            CALL SUERRS (LP,2,NUMERR)
            INDERR=1
            ENDIF
         IF (INDERR.GT.0) GO TO 370
         ITTEMP=ITTAVR
         IF (ITEMP.EQ.1.AND.ITTEMP.EQ.0) ITTEMP=24
C         
C     SET SA AND SM IDS
         SAID=' '
         IF (ISA.GT.0) THEN
            CALL SUBSTR (SRCID(1,ISA),1,LEN(SAID),SAID,1)
            IF (SAID.EQ.'SAME') THEN
               CALL SUBSTR (STAID,1,LEN(SAID),SAID,1)
               ENDIF
            ENDIF
         SMID=' '
         IF (ISM.GT.0) THEN
            CALL SUBSTR (SRCID(1,ISM),1,LEN(SMID),SMID,1)
            IF (SMID.EQ.'SAME') THEN
               CALL SUBSTR (STAID,1,LEN(SMID),SMID,1)
               ENDIF
            ENDIF
C            
C     OPEN DATA BASE
         CALL SUDOPN (1,'SASM',IERR)
         IF (IERR.NE.0) THEN
            ISTAT=1
            GO TO 370
            ENDIF
C            
         IF (INWSRC(ISRC).EQ.0) GO TO 160
C         
C     CREATE STATION IN CONTROL FILE
         ACTION='CREATED'
         CALL WDSCR (PUSRID,SAID,SMID,STAID,DESCRP,STALOC(1),
     *      ITPCPN,ITTEMP,IPE,ISD,ISW,IERR)
         IF (IERR.NE.0) THEN         
C        CHECK IF DEFINED IN CONTROL FILE AND STATION IS INCOMPLETE
            IF (IERR.EQ.2) THEN
               IF (ICSTAN.EQ.1) THEN
                  WRITE (LP,600) STAID,SAID,SMID,'SASM'
                  CALL SULINE (LP,2)
                  INWSRC(ISRC)=0
                  GO TO 160
                  ENDIF
               ENDIF
            CALL SWDEST ('WDSCR','SASM',PUSRID,STAID,
     *         IDTYPE,SAID,SMID,
     *         NDLYTP,DLYTP,NUMRRS,RRSTP,IDTIME,
     *         NUMWRN,NUMERR,IERR)
            WRITE (LP,620) 'SASM',STAID
            CALL SULINE (LP,2)
            IF (ISA.GT.0) SRCCD(ISA)='DELT'
            IF (ISM.GT.0) SRCCD(ISM)='DELT'
            GO TO 370
            ENDIF
         GO TO 170
C         
C     CHANGE STATION IN CONTROL FILE
160      ACTION='CHANGED'
         CALL WDSCH (PUSRID,SAID,SMID,STAID,DESCRP,STALOC(1),
     *      ITPCPN,ITTEMP,IPE,ISD,ISW,IERR)
         IF (IERR.NE.0) THEN
            CALL SWDEST ('WDSCH','SASM',PUSRID,STAID,
     *         IDTYPE,SAID,SMID,
     *         NDLYTP,DLYTP,NUMRRS,RRSTP,IDTIME,
     *         NUMWRN,NUMERR,IERR)
            WRITE (LP,620) 'SASM',STAID
            CALL SULINE (LP,2)
            GO TO 370
            ENDIF
C            
C     STATION SUCCESSFULLY CREATED OR CHANGED
170      WRITE (LP,740) STAID,ACTION,'SASM'
         CALL SULINE (LP,2)
         XPE='?'
         XSD='?'
         XSW='?'
         IF (IPE.EQ.0) XPE='NO'
         IF (IPE.EQ.1) XPE='YES'
         IF (ISD.EQ.0) XSD='NO'
         IF (ISD.EQ.1) XSD='YES'
         IF (ISW.EQ.0) XSW='NO'
         IF (ISW.EQ.1) XSW='YES'
         WRITE (LP,630) SAID,SMID,ITPCPN,ITTEMP,XPE,XSD,XSW
         CALL SULINE (LP,2)
         CALL SUDWRT (1,'SASM',IERR)
         IF (ISA.GT.0) INWSRC(ISA)=-1
         IF (ISM.GT.0) INWSRC(ISM)=-1
C
C      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180      IF (SRCCD(ISRC).NE.'GPLT'.AND.
     *       SRCCD(ISRC).NE.'GHB5'.AND.
     *       SRCCD(ISRC).NE.'CDAS') GO TO 370
C
C     GOES CONTROL FILE
C
         IDTYPE=-1
         IF (SRCCD(ISRC).EQ.'GHB5') IDTYPE=0
         IF (SRCCD(ISRC).EQ.'GPLT') IDTYPE=1
         IF (SRCCD(ISRC).EQ.'CDAS') IDTYPE=2
         IF (IDTYPE.EQ.-1) THEN
            WRITE (LP,560) SRCCD(ISRC),STAID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 370
            ENDIF
         XTYPE=' '
         IF (IDTYPE.EQ.0) XTYPE='GHB5'
         IF (IDTYPE.EQ.1) XTYPE='GPLT'
         IF (IDTYPE.EQ.2) XTYPE='CDAS'
         GOESID=' '
         CALL SUBSTR (SRCID(1,ISRC),1,LEN(GOESID),GOESID,1)
         IF (GOESID.EQ.'SAME') THEN
            CALL SUBSTR (STAID,1,LEN(GOESID),GOESID,1)
            ENDIF
C
C     CHECK SOURCE INFORMATION
         IF (IDTYPE.EQ.0.OR.IDTYPE.EQ.1)
     *      CALL SFGOES (STAID,NGPS,GPS,NGOESN,GOESN,
     *         ITPPVR,ITTAVR,IPPROC,ITYOBS,
     *         MDLYTP,DLYTP,NDLYTP,NUMRRS,RRSTP,
     *         NUMERR,NUMWRN,IERR)
         IF (IDTYPE.EQ.2)
     *      CALL SFGOES (STAID,NGPS,GPS,NCDASN,CDASN,
     *         ITPPVR,ITTAVR,IPPROC,ITYOBS,
     *         MDLYTP,DLYTP,NDLYTP,NUMRRS,RRSTP,
     *         NUMERR,NUMWRN,IERR)
         IF (IERR.NE.0) THEN
            IF (NDLYTP.EQ.0.AND.NUMRRS.EQ.0) SRCCD(ISRC)='DELT'
            GO TO 330
            ENDIF
C
C     OPEN DATA BASE
         CALL SUDOPN (1,'GOES',IERR)
         IF (IERR.NE.0) THEN
            ISTAT=1           
            GO TO 370
            ENDIF
C            
         IF (NUMRRS.GT.0) THEN
            DO 230 I=1,NUMRRS
               IRTIMEA(I)=IABS(IRTIME(I))
C           CHECK IF TYPE IS CDAS AND TIME INTERVAL IS LESS THAN 6 HRS
               IF (SRCCD(ISRC).EQ.'CDAS'.AND.IRTIMEA(I).LT.6) THEN
C              SET TIME INTERVAL TO 6 HRS
                  IVALUE=IRTIMEA(I)
                  IRTIMEA(I)=6
                  WRITE (LP,570) IVALUE,SRCCD(ISRC),GOESID,STAID,
     *               IRTIMEA(I)
                  CALL SUWRNS (LP,2,NUMWRN)
                  ENDIF
230            CONTINUE
            ENDIF
C
      IF (INWSRC(ISRC).EQ.0) GO TO 250
C
C     CREATE STATION IN CONTROL FILE
235      ACTION='CREATED'
         CALL WDGCR (PUSRID,GOESID,IDTYPE,STAID,DESCRP,
     *      NDLYTP,DLYTP,NUMRRS,RRSTP,IRTIMEA,IDTIME,
     *      IERR)
         IF (IERR.NE.0) THEN
C        CHECK IF DEFINED IN CONTROL FILE AND STATION IS INCOMPLETE
            IF (IERR.EQ.2) THEN
               IF (ICSTAN.EQ.1) THEN
                  WRITE (LP,610) STAID,SRCCD(ISRC),GOESID,'GOES'
                  CALL SULINE (LP,2)
                  INWSRC(ISRC)=0
                  GO TO 250
                  ENDIF
               ENDIF
            CALL SWDEST ('WDGCR',XTYPE,PUSRID,STAID,
     *         IDTYPE,GOESID,BLNK8,
     *         NDLYTP,DLYTP,NUMRRS,RRSTP,IDTIME,
     *         NUMWRN,NUMERR,IERR)
            SRCCD(ISRC)='DELT'
            GO TO 370
            ENDIF
         GO TO 260
C
C     CHANGE STATION IN CONTROL FILE
250      ACTION='CHANGED'
         CALL WDGCH (PUSRID,GOESID,IDTYPE,STAID,DESCRP,
     *      NDLYTP,DLYTP,NUMRRS,RRSTP,IRTIMEA,IDTIME,
     *      MADDTP,ADDTP,NADDTP,MDELTP,DELTP,NDELTP,
     *      IERR)
         IF (IERR.NE.0) THEN
            NUMWRNO=NUMWRN
            NUMERRO=NUMERR
            CALL SWDEST ('WDGCH',XTYPE,PUSRID,STAID,
     *         IDTYPE,GOESID,BLNK8,
     *         NDLYTP,DLYTP,NUMRRS,RRSTP,IDTIME,
     *         NUMWRN,NUMERR,IERR)
            TYPMSG=' '
            IF (NUMWRN.NE.NUMWRNO) TYPMSG='WARNING'
            IF (NUMERR.NE.NUMERRO) TYPMSG='ERROR'
            IF (TYPMSG.NE.' ') THEN
               WRITE (LP,590) TYPMSG(1:LENSTR(TYPMSG)),
     *            XTYPE,GOESID,STAID
               CALL SULINE (LP,2)
               IF (TYPMSG.EQ.'ERROR') SRCCD(ISRC)='DELT'
               IF (IERR.EQ.3) GO TO 235
               GO TO 370
               ENDIF               
            ENDIF
C
C     STATION SUCCESSFULLY CREATED OR CHANGED
260      WRITE (LP,740) STAID,ACTION,'GOES'
         CALL SULINE (LP,2)
         CALL SUDWRT (1,'GOES',IERR)
         WRITE (LP,640) GOESID,XTYPE
         CALL SULINE (LP,1)
         IF (NDLYTP.GT.0) THEN
            DO 300 I=1,NDLYTP
               IF (IDTIME(I).LE.0) GO TO 300
               IF (I.EQ.1) THEN
                  WRITE (LP,650) DLYTP(I),IDTIME(I)
                  CALL SULINE (LP,1)
                  ENDIF
               IF (I.GT.1) THEN
                  WRITE (LP,660) DLYTP(I),IDTIME(I)
                  CALL SULINE (LP,1)
                  ENDIF
300            CONTINUE
            ENDIF
         IF (NUMRRS.GT.0) THEN
            DO 320 I=1,NUMRRS
               N=NDLYTP+I
               IF (IDTIME(N).LE.0) GO TO 320
               IF (I.EQ.1) THEN
                  WRITE (LP,670) RRSTP(I),IDTIME(N)
                  CALL SULINE (LP,1)
                  ENDIF
               IF (I.GT.1) THEN
                  WRITE (LP,660) RRSTP(I),IDTIME(N)
                  CALL SULINE (LP,1)
                  ENDIF
               CALL SULINE (LP,1)
320            CONTINUE
            ENDIF
C
330      IF (INWSRC(ISRC).NE.0) GO TO 370
C
         IF (NADDTP.GT.0) THEN
C        PRINT TYPES ADDED
            NUM=NADDTP
            NPER=10
            IF (NUM.GT.NPER) NUM=NPER
            WRITE (LP,680) (ADDTP(I),I=1,NUM)
            CALL SULINE (LP,1)
            IF (NADDTP.GT.NPER) THEN
               NTIME=(NADDTP-NPER)/NPER
               IF (MOD(NADDTP,NPER).NE.0) NTIME=NTIME+1
               NUM1=NPER+1
               NUM2=NPER*2
               DO 340 J=1,NTIME
                  IF (NUM2.GT.NADDTP) NUM2=NADDTP
                  WRITE (LP,690) (ADDTP(I),I=NUM1,NUM2)
                  CALL SULINE (LP,1)
                  NUM1=NUM1+NPER
                  NUM2=NUM2+NPER
340               CONTINUE
               ENDIF
            ENDIF
C
         IF (NDELTP.GT.0) THEN
C        PRINT TYPES DELETED
            NUM=NDELTP
            NPER=10
            IF (NUM.GT.NPER) NUM=NPER
            WRITE (LP,700) (DELTP(I),I=1,NUM)
            CALL SULINE (LP,2)
            IF (NDELTP.GT.NPER) THEN
               NTIME=(NDELTP-NPER)/NPER
               IF (MOD(NDELTP,NPER).NE.0) NTIME=NTIME+1
               NUM1=NPER+1
               NUM2=NPER*2
               DO 360 J=1,NTIME
                  IF (NUM2.GT.NDELTP) NUM2=NDELTP
                  WRITE (LP,690) (DELTP(I),I=NUM1,NUM2)
                  CALL SULINE (LP,1)
                  NUM1=NUM1+NPER
                  NUM2=NUM2+NPER
360               CONTINUE
               ENDIF
            ENDIF
C
C      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
370      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR DELETED SOURCE CODES
      NDELT=0
      DO 400 I=1,NSRCCD
         IF (SRCCD(I).NE.'DELT') GO TO 400
            NDELT=NDELT+1
            IF (I.EQ.NSRCCD) GO TO 400
               ISTRT=I+1
               DO 390 J=ISTRT,NSRCCD
                  IF (SRCCD(J).EQ.'DELT') GO TO 390
                     IF (LDEBUG.GT.0) THEN
                        WRITE (IOSDBG,710) SRCCD(I),I
                        CALL SULINE (IOSDBG,1)
                        ENDIF
                     SRCCD(I)=SRCCD(J)
                     CALL SUBSTR (SRCID(1,I),1,8,SRCID(1,J),1)
                     GO TO 400
390               CONTINUE
400      CONTINUE
      NSRCCD=NSRCCD-NDELT
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,720) NDELT,NSRCCD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR ERRORS ENCOUNTERED WRITING
405   IF (NUMERR.GT.0) THEN
         WRITE (LP,730) NUMERR
         CALL SULINE (LP,2)
         ISTAT=1
         ENDIF
C
      NUMDLY=NDLYTP
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NUMDLY=',NUMDLY,
     *      ' NUMRRS=',NUMRRS,
     *      ' '
         CALL SULINE (IOSDBG,1)
         IF (NSRCCD.GT.0) THEN
            IF (ISA.GT.0.OR.ISA.GT.0) THEN
               IF (IDSASM.EQ.1) THEN
                  WRITE (IOSDBG,760) 'SASM'
                  CALL SULINE (IOSDBG,2)
                  CALL DSPRT (PUSRID,STAID,IERR)
                  ENDIF
               ENDIF
            ENDIF
         IF (IGOES.EQ.1) THEN
            IF (IDGOES.EQ.1) THEN
               WRITE (IOSDBG,760) 'GOES'
               CALL SULINE (IOSDBG,2)
               CALL DGPRT (PUSRID,STAID,IERR)
               ENDIF
            ENDIF
         ENDIF
C
      NSTERR=NSTERR+NUMERR
      NSTWRN=NSTWRN+NUMWRN
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFDECR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
450   FORMAT (' STAID=',A,3X,'NSRCCD=',I2)
460   FORMAT (' NGPS=',I2,3X,'GPS=',10(A,1X))
470   FORMAT (' SRCCD(',I2,')=',A,3X,'SRCID=',2A)
480   FORMAT (' ITPPVR=',I2,3X,'ITTAVR=',I2,3X,'IPPROC=',I2,3X,
     *   'ITYOBS=',I2,3X,'MRRSTP=',I2,3X,'NUMRRS=',I2)
490   FORMAT (' NGOESN=',I2,3X,'GOESN(I)=',10(A,1X))
500   FORMAT (' NCDASN=',I2,3X,'CDASN(I)=',10(A,1X))
510   FORMAT (' ISA=',I2,3X,'ISM=',I2,3X,'ISASM=',I2,3X,'IGOES=',I2,3X,
     *   'IDSASM=',I2,3X,'IDGOES=',I2)
520   FORMAT (' NSRCCD=',I2,3X,'SRCCD=',10(A,1X))
530   FORMAT (' ISRC=',I2,3X,'SRCCD=',A,3X,'SRCID=',2A,3X,
     *   'INWSRC=',I2)
533   FORMAT ('0*** WARNING - DISPOSITION IS ',A,' ',
     *   'FOR SOURCE CODE ',A,' ',
     *   'FOR STATION ',A,' ',
     *   'BUT IT WAS ',A,' IN THE CONTROL FILE.')
535   FORMAT ('0*** ERROR - SOURCE CODE ',A,' ',
     *   'IS INVALID FOR STATION ',A,'.')
540   FORMAT ('0*** NOTE - ONE OR MORE DATA ENTRY DATA BASES ',
     *   'NOT ALLOCATED. SOURCE CODES WILL BE DEFINED ONLY FOR THOSE ',
     *   'ALLOCATED.')
550   FORMAT ('0*** ERROR - SASM ',A,' DATA WITH A TIME INTERVAL ',
     *   'OF 1 HOUR CANNOT BE TRANSFRRED FOR STATION ',A,'.')
560   FORMAT ('0*** ERROR - GOES IDENTIFIER ''',A,''' ',
     *    'IS INVALID FOR STATION ',A,'.')
570   FORMAT ('0*** WARNING - TIME INTERVAL (',I2,') ',
     *   'FOR ',A,' IDENTIFIER ',A,' FOR STATION ',A,' ',
     *   'IS INVALID AND WILL BE SET TO ',I2,'.')
590   FORMAT ('0*** NOTE - THE ABOVE ',A,' OCCURRED PROCESSING ',A,
     *   ' IDENTIFIER ',A,' FOR STATION ',A,'.')
600   FORMAT ('0*** NOTE - STATION ',A,' (SAID=',A,1X,'SMID=',A,
     *   ') IS ALREADY DEFINED IN THE ',
     *   A,' CONTROL FILE AND WILL BE CHANGED.')
610   FORMAT ('0*** NOTE - STATION ',A,' (',A,1X,'ID=',A,
     *   ') IS ALREADY DEFINED IN THE ',
     *   A,' CONTROL FILE AND WILL BE CHANGED.')
620   FORMAT ('0*** NOTE - ',A,' CONTROL FILE NOT UPDATED FOR ',
     *   ' STATION ',A,' BECAUSE ERRORS ENCOUNTERED.')
630   FORMAT (T13,'SAID=',A,2X,'SMID=',A,2X,
     *   'PCPN INT=',I2,2X,
     *   'TEMP INT=',I2,2X,
     *   'TRANSFER PE=',A,2X,
     *   'TRANSFER SNOG=',A,2X,
     *   'TRANSFER SNWE=',A)
640   FORMAT (T13,'GOES STATION IDENTIFIER = ',A,5X,
     *   'TYPE = ',A)
650   FORMAT (T13,'DLY DATA TYPES : ',A,3X,
     *   'TIME INTERVAL=',I4,' MINUTES')
660   FORMAT (T30,A,3X,
     *   'TIME INTERVAL=',I4,' MINUTES')
670   FORMAT (T13,'RRS DATA TYPES : ',A,3X,
     *   'TIME INTERVAL=',I4,' MINUTES')
680   FORMAT (T13,'DATA TYPES ADDED   : ',10(A,2X))
690   FORMAT (T34,20(A,2X))
700   FORMAT (T13,'DATA TYPES DELETED : ',10(A,2X))
710   FORMAT (' SOURCE CODE ',A,' DELETED FROM POSITION ',I2)
720   FORMAT (' NDELT=',I2,3X,'NSRCCD=',I2)
730   FORMAT ('0*** NOTE - ',I2,' ERRORS ENCOUNTERED WHILE WRITING ',
     *   'TO DATA ENTRY CONTROL FILES.')
740   FORMAT ('0*** NOTE - STATION ',A,' SUCCESSFULLY ',A,' IN THE ',
     *   A,' CONTROL FILE.')
760   FORMAT ('0DUMP OF ',A,' CONTROL FILE')
C
      END
