C MEMBER SUGTFC
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/24/95.15:03:02 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  GET LIST OF FORECAST COMPONENT IDENTIFIERS FOR SPECIFIED TYPE.
C
C  DUPLICATE NAMES ARE REMOVED IF CARRYOVER GROUP, FORECAST GROUP
C  AND SEGMENT IDENTIFIERS ARE RETURNED.
C
C
      SUBROUTINE SUGTFC (DTYPE,MAXID,TSIDS,NUMID,IFILFC,XCGIDS,XFGIDS,
     *   XSGIDS,IPRINT,ISTAT)
C
C
      CHARACTER*4 DTYPE,XDTYPE,SDTYPE,TCODE
      CHARACTER*8 TSID,FMAPID,CGID
      CHARACTER*8 TSIDS(MAXID)
      CHARACTER*8 XSGIDS(MAXID),XFGIDS(MAXID),XCGIDS(MAXID)
C
cew increased mfgids from 50 to 200 for nwrfc
      PARAMETER (MFGIDS=200)
      DIMENSION FGIDS(2,MFGIDS)
      PARAMETER (MFCWRK=100)
      DIMENSION FCWRK(MFCWRK)
      DIMENSION IHEAD(22)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtfc.f,v $
     . $',                                                             '
     .$Id: sugtfc.f,v 1.4 2002/05/15 13:42:43 hank Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVELS
      LDEBUG=ISBUG('SYS ')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *     ' DTYPE=',DTYPE,
     *     ' MAXID=',MAXID,
     *     ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUMID=0
      NUMADD=0
      NUMERR=0
C
      XDTYPE=DTYPE
      IF (XDTYPE.EQ.'FMAP') XDTYPE='MAP'
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'FC  ',IERR)
      IF (IERR.GT.0) THEN
         NUMERR=NUMERR+1
         GO TO 130
         ENDIF
C
C  SET FORECAST COMPONENT READ INDICATORS

C
C  GET CARRYOVER GROUP INFORMATION
      CALL UREADT (KFCGD,1,NSLOTS,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' NCG=',NCG
         CALL SULINE(IOSDBG,1)
         ENDIF
      IF (NCG.EQ.0) THEN
         WRITE (LP,170)
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 150
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) ((CGIDS(I,J),I=1,2),J=1,NCG)
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      CALL UREADT (KFFGST,1,FGID,IERR)
      NFGREC=IDUMYG
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' NFGREC=',NFGREC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PROCESS EACH CARRYOVER GROUP
      DO 120 ICG=1,NCG
         CALL SUBSTR (CGIDS(1,ICG),1,8,CGID,1)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*)
     *         ' NCG=',NCG,
     *         ' ICG=',ICG,
     *         ' CGID=',CGID,
     *         ' '
            CALL SULINE (IOSDBG,1)
            ENDIF
C     GET NUMBER OF FORECAST GROUPS IN CARRYOVER GROUP
         CALL UREADT (KFCGD,ICOREC(ICG),CGIDC,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) ' NFG=',NFG
            CALL SULINE (IOSDBG,2)
            ENDIF
         NREC1=1
C     GET FORECAST GROUP NAMES
         DO 40 IFG=1,NFG
            IF (NREC1.GT.NFGREC) GO TO 20
            DO 10 NREC=NREC1,NFGREC
               CALL UREADT (KFFGST,NREC,FGID,IERR)
               IF (ISPEC.EQ.1) GO TO 10
               IF (CGIDS(1,ICG).EQ.CGIDF(1).AND.
     *             CGIDS(2,ICG).EQ.CGIDF(2)) THEN
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,200) FGID
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  IF (ICOSEQ.GT.MFGIDS) THEN
                     WRITE (LP,220) MFGIDS
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 130
                     ENDIF
                  FGIDS(1,ICOSEQ)=FGID(1)
                  FGIDS(2,ICOSEQ)=FGID(2)
                  GO TO 30
                  ENDIF
10             CONTINUE
20             WRITE (LP,180) CGIDS(1,ICG),ICOSEQ,NFG
               CALL SUERRS (LP,2,NUMERR)
               GO TO 120
30          NREC1=NREC+1
40          CONTINUE
C     PROCESS EACH FORECAST GROUP
         DO 110 IFG=1,NFG
C        GET LIST OF SEGMENTS IN ORDER FOR EACH FORECAST GROUP
            IDTYPE=2
            CALL FCORDR (IDTYPE,FGIDS(1,IFG),IERR,FCWRK,LFCWRK)
            IF (IERR.GT.0) THEN
               CALL SUERRS (LP,2,NUMERR)
               GO TO 130
               ENDIF
            IF (NSEGEX.EQ.0) THEN
               WRITE (LP,260) (FGIDS(J,IFG),J=1,2),NSEGEX
               CALL SUERRS (LP,2,NUMERR)
               GO TO 130
               ENDIF
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*)
     *            ' NSEGEX=',NSEGEX,
     *            ' IFG=',IFG,
     *            ' '
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,270) (IRSGEX(I),I=1,NSEGEX)
               CALL SULINE (IOSDBG,1)
               ENDIF
C        PROCESS EACH SEGMENT
            DO 100 NSG=1,NSEGEX
               IOPT=1
               NOPARM=0
               CALL FGETSG (FGID,IRSGEX(NSG),MP,P,MT,T,MTS,TS,IOPT,
     *            NOPARM,IERR)
               IF (IERR.GT.0) THEN
                  IF (IERR.EQ.1) THEN
                     WRITE (LP,240) IOPT,IRSGEX(NSG)
                     CALL SUERRS (LP,2,NUMERR)
                     ENDIF
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,250) MP,MT,MTS
                     CALL SUERRS (LP,2,NUMERR)
                     ENDIF
                  GO TO 130
                  ENDIF
               NTPOS=0
C           GET TIME SERIES TYPE
50             ITSTYP=TS(NTPOS+1)
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*) ' ITSTYP=',ITSTYP
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ITSTYP.EQ.0) GO TO 100
C           GET TYPE OF EXTERNAL DATA FILE
               CALL SUBSTR (TS(NTPOS+10),1,4,TCODE,1)
C           GET EXTERNAL IDENTIFIER
               CALL SUBSTR (TS(NTPOS+13),1,8,TSID,1)
C           GET EXTERNAL TIME SERIES DATA TYPE
               CALL SUBSTR (TS(NTPOS+15),1,4,SDTYPE,1)
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*)
     *               ' TSID=',TSID,
     *               ' SDTYPE=',SDTYPE,
     *               ' XDTYPE=',XDTYPE,
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ITSTYP.NE.1) GO TO 90
               IF (TCODE.NE.'FPDB') GO TO 90
               IF (SDTYPE.NE.XDTYPE) GO TO 90
               IF (DTYPE.EQ.'FMAP') THEN
                  CALL SUDOPN (1,'PRD ',IERR)
                  IF (IERR.GT.0) THEN
                     NUMERR=NUMERR+1
                     GO TO 130
                     ENDIF
                  MAXX=1
                  CALL RPRDH (TSID,'MAP ',MAXX,IHEAD,NUMX,XBUF,
     *               FMAPID,IERR)
                  IF (IERR.NE.0) THEN
                     IF (IERR.EQ.1) THEN
                        WRITE (LP,210) TSID
                        CALL SUERRS (LP,2,NUMERR)
                        ELSE
                           CALL SRPRST ('RPRDH   ',TSID,'MAP ',
     *                        MAXX,XBUF,IERR)
                        ENDIF
                     GO TO 90
                     ENDIF
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,*) ' FMAPID=',FMAPID
                     CALL SULINE (LP,1)
                     ENDIF
                  TSID=FMAPID
                  ENDIF
               IF (IFILFC.EQ.1) THEN
60                IF (NUMID.GT.1) THEN
                     DO 70 INUM=1,NUMID
                        IF (TSID.EQ.TSIDS(INUM)) GO TO 90
70                      CONTINUE
                     ENDIF
                  ENDIF
80             NUMID=NUMID+1
               IF (NUMID.GT.MAXID) THEN
                  NUMADD=NUMADD+1
                  IF (NUMADD.EQ.1) THEN
                     WRITE (LP,230) DTYPE,MAXID
                     CALL SUERRS (LP,2,NUMERR)
                     ENDIF
                  GO TO 90
                  ENDIF
               TSIDS(NUMID)=TSID
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*)
     *               ' NUMID=',NUMID,
     *               ' TSIDS(NUMID)=',TSIDS(NUMID),
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (IFILFC.EQ.1) THEN
                  CALL SUBSTR (IDSEGN,1,8,XSGIDS(NUMID),1)
                  CALL SUBSTR (IFGID,1,8,XFGIDS(NUMID),1)
                  CALL SUBSTR (ICGID,1,8,XCGIDS(NUMID),1)
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,*)
     *                  ' NUMID=',NUMID,
     *                  ' XSGIDS(NUMID)=',XSGIDS(NUMID),
     *                  ' XFGIDS(NUMID)=',XFGIDS(NUMID),
     *                  ' XCGIDS(NUMID)=',XCGIDS(NUMID),
     *                  ' '
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  ENDIF
90             NTPOS=TS(NTPOS+2)-1
               GO TO 50
100            CONTINUE
110         CONTINUE
120      CONTINUE
C
130   IF (NUMERR.GT.0) ISTAT=2
C
      IF (NUMADD.GT.0) THEN
         WRITE (LP,280) NUMADD,DTYPE
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' DTYPE=',DTYPE,
     *      ' NUMID=',NUMID,
     *      ' '
         CALL SULINE (IOSDBG,1)
         IF (NUMID.GT.0) THEN
            DO 140 I=1,NUMID
               IF (IFILFC.EQ.0) THEN
                  WRITE (IOSDBG,*)
     *               ' I=',I,
     *               ' TSIDS(I)=',TSIDS(I),
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (IFILFC.EQ.1) THEN
                  WRITE (IOSDBG,*)
     *               ' I=',I,
     *               ' TSIDS(I)=',TSIDS(I),
     *               ' XSGIDS(I)=',XSGIDS(I),
     *               ' XFGIDS(I)=',XFGIDS(I),
     *               ' XCGIDS(I)=',XCGIDS(I),
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
140            CONTINUE
            ENDIF
         ENDIF
C
150   IF (IPRMSG.EQ.1) THEN
         WRITE (LP,290) NUMID,DTYPE,MAXID
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,300)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT (' *** ENTER SUGTFC')
170   FORMAT ('0*** NOTE - NO CARRYOVER GROUPS FOUND IN FORECAST ',
     *   'COMPONENT DATA BASE.')
180   FORMAT ('0*** ERROR - IN SUGTFC - NOT ENOUGH FORECAST GROUPS ',
     *   'FOUND FOR ',
     *   'CARRYOVER GROUP ',2A4,'. ',
     *   I3,' FORECAST GROUPS FOUND. ',
     *   I3,' FORECAST GROUPS EXPECTED.')
190   FORMAT (' CARRYOVER GROUP IDS=' / (10(' ',2A4)/))
200   FORMAT (' FGID=',2A4)
210   FORMAT ('0*** ERROR - IN SUGTFC - TIME SERIES NOT FOUND FOR ',
     *   'MAP AREA ',A,'.')
220   FORMAT ('0*** ERROR - IN SUGTFC - MAXIMUM NUMBER OF FORECAST ',
     *   'GROUPS THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
230   FORMAT ('0*** ERROR - IN SUGTFC - MAXIMUM NUMBER OF ',A,
     *   ' TIME SERIES THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
240   FORMAT ('0*** ERROR - IN SUGTFC - INVALID ARGUMENTS PASSED ',
     *   'TO FGETSG : IOPT=',I2,3X,'IRSGEX(NSG)=',I5)
250   FORMAT ('0*** ERROR - IN SUGTFC - UNABLE TO DEFINE P, T, OR TS ',
     *   'ARRAY DUE TO LACK OF SPACE WHEN CALLING FGETSG : MP=',I5,3X,
     *   'MT=',I5,3X,'MTS=',I5)
260   FORMAT ('0*** ERROR - IN SUGTFC - FORECAST GROUP ',2A4,
     *   ' HAS NO SEGMENTS. FCORDR CALLED : NSEGEX=',I5)
270   FORMAT (' IRSEGX=',(10(I5,' ')/))
280   FORMAT ('0*** WARNING - IN SUGTFC - ',I3,' ',A,' IDENTIFIERS ',
     *   'NOT PROCESSED BECAUSE ARRAYS ARE TOO SMALL.')
290   FORMAT ('0*** NOTE - ',I4,' ',A,' IDENTIFIERS PROCESSED ',
     *   'FROM FORECAST COMPONENT DATA BASE. ',
     *   'A MAXIMUM OF ',I4,' CAN BE PROCESSED.')
300   FORMAT (' *** EXIT SUGTFC')
C
      END
