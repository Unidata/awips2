C MODULE SCMAP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PUNCH AREA MAP PARAMETER RECORD.
C
      SUBROUTINE SCMAP (IVMAP,IVMAPS,MAPID,ITMWT,POWER,NSTWT,
     *   ISTWT,NPCPN,NSETS,DESCRP,BASNID,FMAPID,CENTRD,TSTAID,
     *   TSTAWT,CSTAID,CSTAWT,IBASN,NBOX,CORMDR,ISTAT)
C
      CHARACTER*8 SEASN
      CHARACTER*8 CHAR/' '/
      CHARACTER*10 WORD
      CHARACTER*80 CARD/' '/
C
      PARAMETER (LARRAY=100)
      DIMENSION ARRAY(LARRAY)
C
      CHARACTER*8 MAPID,BASNID,FMAPID
      CHARACTER*8 TSTAID(1),CSTAID(1) 
      CHARACTER*20 DESCRP
      DIMENSION CENTRD(1)
      DIMENSION TSTAWT(1),CSTAWT(NPCPN,1)
      PARAMETER (NUMMDR=8)
      DIMENSION CORMDR(NUMMDR)
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scmap.f,v $
     . $',                                                             '
     .$Id: scmap.f,v 1.5 2002/02/11 21:09:57 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SCMAP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      MCHAR=LEN(CHAR)
      LCHAR=LEN(CHAR)/4
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MAP ')
C
      ISTAT=0
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IVMAP=',IVMAP
         CALL SULINE (IOSDBG,2)
         WRITE (IOSDBG,*) 'IVMAPS',IVMAPS
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH 'MAP' STARTING IN COLUMN 1
      NPOS=1
      NSPACE=2
      CALL UTOCRD (ICDPUN,NPOS,'MAP',3,NSPACE,CARD,0,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
C  PUNCH IDENTIFIER
      NSPACE=1
      CALL UTOCRD (ICDPUN,NPOS,MAPID,LEN(MAPID),NSPACE,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
C  PUNCH DESCRIPTION
      NSPACE=1
      CALL UTOCRD (ICDPUN,NPOS,DESCRP,LEN(DESCRP),NSPACE,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
C  CHECK IF BASIN BOUNDARY USED
      IF (BASNID.EQ.' ') GO TO 10
C
C  PUNCH BASIN BOUNDARY IDENTIFIER
      NSPACE=1
      CALL UTOCRD (ICDPUN,NPOS,BASNID,LEN(BASNID),NSPACE,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
      GO TO 20
C
C  PUNCH CENTROID COORDINATES (LAT/LON)
10    IBLLGD=0
      CALL SBLLGD (XLONG,XLAT,1,CENTRD(1),CENTRD(2),IBLLGD,IERR)
      IF (IERR.GT.0) GO TO 110
      IF (IUGFIL.EQ.0) THEN
         CALL SUGTUG (LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) GO TO 110
         ENDIF
      IF (XLAT.LE.ULLMTS(1).AND.XLAT.GE.ULLMTS(2)) THEN
         ELSE
            WRITE (LP,160) 'LATITUDE',XLAT,MAPID,
     *          ULLMTS(2),ULLMTS(1)
            CALL SUWRNS (LP,2,-1)
         ENDIF
      IF (XLONG.LE.ULLMTS(4).AND.XLONG.GE.ULLMTS(3)) THEN
         ELSE
            WRITE (LP,160) 'LONGITUDE',XLONG,MAPID,
     *          ULLMTS(3),ULLMTS(4)
            CALL SUWRNS (LP,2,-1)
         ENDIF
      NSPACE=0
      CALL UTOCRD (ICDPUN,NPOS,'(',1,NSPACE,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
      NUMDEC=2
      CALL URELCH (XLAT,MCHAR,CHAR,NUMDEC,NCHAR,IERR)
      IF (IERR.GT.0) GO TO 110
      NSPACE=1
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
      CALL URELCH (XLONG,MCHAR,CHAR,NUMDEC,NCHAR,IERR)
      IF (IERR.GT.0) GO TO 110
      NSPACE=0
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
      NSPACE=1
      CALL UTOCRD (ICDPUN,NPOS,')',1,NSPACE,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
C  PUNCH TYPE OF TIMING WEIGHTS
20    IF (ITMWT.EQ.1) THEN
         NSPACE=0
         WORD='PRE('
         LWORD=LENSTR(WORD)
         NCHAR=NPOS+LWORD+LENSTR(TSTAID(1))+1
         IF (NCHAR.GT.ICDSTP) THEN
            CALL UPNCRD (ICDPUN,CARD)
            NPOS=6
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,WORD,LWORD,NSPACE,CARD,0,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         NSPACE=1
         DO 30 I=1,NSTWT
            IF (I.EQ.NSTWT) NSPACE=0
            CALL UTOCRD (ICDPUN,NPOS,TSTAID(I),8,0,CARD,3,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
            CALL UTOCRD (ICDPUN,NPOS,',',1,0,CARD,0,0,LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
            NUMDEC=2
            CALL URELCH (TSTAWT(I),MCHAR,CHAR,NUMDEC,NCHAR,IERR)
            IF (IERR.GT.0) GO TO 110
            CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
30          CONTINUE
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 40
         ENDIF
      IF (ITMWT.EQ.2) THEN
         CALL UTOCRD (ICDPUN,NPOS,'D2',2,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 40
         ENDIF
      IF (ITMWT.EQ.3) THEN
         CALL UTOCRD (ICDPUN,NPOS,'DP(',3,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         NUMDEC=2
         CALL URELCH (POWER,MCHAR,CHAR,NUMDEC,NCHAR,IERR)
         IF (IERR.GT.0) GO TO 110
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 40
         ENDIF
C
      WRITE (LP,180) 'TYPE OF TIMIMG WEIGHTS',ITMWT
      CALL SUERRS (LP,2,-1)
C
C  PUNCH TYPE OF STATION WEIGHTS
40    IF (ISTWT.EQ.1) THEN
         DO 60 J=1,NSETS
            IF (J.EQ.1) SEASN='WNTR'
            IF (J.EQ.2) SEASN='SUMR'
            LSEASN=LENSTR(SEASN)
            NUMDEC=2
            NSPACE=1
            WORD='PRE('
            LWORD=LENSTR(WORD)
            NCHAR=NPOS+1+LWORD+LENSTR(CSTAID(1))+1
            IPNCRD=0
            IF (NCHAR.GT.ICDSTP) THEN
               CALL UPNCRD (ICDPUN,CARD)
               NPOS=6
               IPNCRD=1
               ENDIF
            IF (IPNCRD.EQ.0.AND.NPCPN.EQ.1) THEN
               CALL URELCH (CSTAWT(1,J),MCHAR,CHAR,NUMDEC,NCHAR2,IERR)
               IF (IERR.GT.0) GO TO 110
               NCHAR=NCHAR+NCHAR2+LSEASN
               IF (NCHAR.GT.ICDSTP) THEN
                  CALL UPNCRD (ICDPUN,CARD)
                  NPOS=6
                  ENDIF
               ENDIF
            CALL UTOCRD (ICDPUN,NPOS,WORD,LWORD,0,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
            LPOS=NPOS
            DO 50 I=1,NPCPN
C           CHECK IF STATION WEIGHT IS ZERO
               IF (CSTAWT(I,J).EQ.0.0) THEN
                  WRITE (LP,170) MAPID,CSTAID(I),SEASN(1:LSEASN)
                  CALL ULINE (LP,1)
                  NPOS=LPOS
                  IF (I.EQ.NPCPN) NPOS=NPOS-1
                  GO TO 50
                  ENDIF
            IF (I.EQ.NPCPN) THEN
               NCHAR=NPOS+1+LENSTR(CSTAID(I))+1+LSEASN+1
               IF (NCHAR.GT.ICDSTP) THEN
                  CALL UPNCRD (ICDPUN,CARD)
                  NPOS=6
                  ENDIF
               ENDIF
C           PUNCH STATION IDENTIFIER
               CALL UTOCRD (ICDPUN,NPOS,CSTAID(I),8,0,CARD,3,0,
     *            LNUM,IERR)
               IF (IERR.GT.0) GO TO 110
               CALL UTOCRD (ICDPUN,NPOS,',',1,0,CARD,0,0,LNUM,IERR)
               IF (IERR.GT.0) GO TO 110
C           PUNCH STATION WEIGHT
               CALL URELCH (CSTAWT(I,J),MCHAR,CHAR,NUMDEC,NCHAR,IERR)
               IF (IERR.GT.0) GO TO 110
               IF (NSETS.EQ.2.AND.I.EQ.NPCPN) THEN
                  IF (NPOS+NCHAR+LSEASN+1.GT.ICDSTP) THEN
                     CALL UPNCRD (ICDPUN,CARD)
                     NPOS=6
                     ENDIF
                  ENDIF
               IF (I.EQ.NPCPN) NSPACE=0
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*)
     *               ' I=',I,
     *               ' NPCPN=',NPCPN,
     *               ' MAPID=',MAPID,
     *               ' NPOS=',NPOS,
     *               ' NSPACE=',NSPACE,
     *               ' '
                  CALL ULINE (IOSDBG,1)
                  ENDIF
               CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *            LNUM,IERR)
               IF (IERR.GT.0) GO TO 110
               LPOS=NPOS
50             CONTINUE            
            IF (NSETS.EQ.1) THEN
               NSPACE=1
               CALL UTOCRD (ICDPUN,NPOS,')',1,NSPACE,CARD,0,0,
     *            LNUM,IERR)
               IF (IERR.GT.0) GO TO 110
               GO TO 60
               ENDIF
            NSPACE=0
            CALL UTOCRD (ICDPUN,NPOS,')',1,NSPACE,CARD,0,0,
     *         LNUM,IERR) 
            NCHAR=LSEASN
            NSPACE=1
            CALL UTOCRD (ICDPUN,NPOS,SEASN,NCHAR,NSPACE,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
60          CONTINUE
         GO TO 70
         ENDIF
      IF (ISTWT.EQ.2) THEN
         CALL UTOCRD (ICDPUN,NPOS,'GRID',4,1,CARD,0,0,
     *      LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 70
         ENDIF
      IF (ISTWT.EQ.3) THEN
         CALL UTOCRD (ICDPUN,NPOS,'THIE',4,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 70
         ENDIF
      IF (ISTWT.EQ.4) THEN
         CALL UTOCRD (ICDPUN,NPOS,'DP(',3,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         NUMDEC=2
         CALL URELCH (POWER,MCHAR,CHAR,NUMDEC,NCHAR,IERR)
         IF (IERR.GT.0) GO TO 110
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 70
         ENDIF
C
      WRITE (LP,180) 'TYPE OF STATION WEIGHTS',ISTWT
      CALL SUERRS (LP,2,-1)
C
C  PUNCH MDR USAGE OPTION AND LAT/LON PAIRS DEFINING SUBSET
C  OF MDR GRID
70    IF (NBOX.EQ.0) THEN
         CALL UTOCRD (ICDPUN,NPOS,'NMDR',4,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 100
         ENDIF
      IF (IBASN.EQ.1) GO TO 90
         CALL UTOCRD (ICDPUN,NPOS,'MDR(',4,0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         NSPACE=1
         DO 80 I=1,NUMMDR,2
            NUMDEC=2
            CALL URELCH (CORMDR(I),MCHAR,CHAR,NUMDEC,NCHAR,IERR)
            IF (IERR.GT.0) GO TO 110
            CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,0,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
            J=I+1
            CALL UTOCRD (ICDPUN,NPOS,',',1,0,CARD,0,0,LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
            CALL URELCH (CORMDR(J),MCHAR,CHAR,NUMDEC,NCHAR,IERR)
            IF (IERR.GT.0) GO TO 110
            IF (J.EQ.NUMMDR) NSPACE=0
            CALL UTOCRD (ICDPUN,NPOS,CHAR,MCHAR,NSPACE,CARD,0,0,
     *         LNUM,IERR)
            IF (IERR.GT.0) GO TO 110
80          CONTINUE
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 110
         GO TO 100
90    CALL UTOCRD (ICDPUN,NPOS,'MDR',3,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
C  PUNCH IDENTIFIER OF FUTURE MAP AREA ASSIGNED TO THIS AREA
100   CALL UTOCRD (ICDPUN,NPOS,FMAPID,LEN(FMAPID),1,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 110
C
      CALL UPNCRD (ICDPUN,CARD)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,105) MAPID
105   FORMAT (' MAP  PARAMETERS SUCCESSFULLY PUNCHED FOR AREA ',A)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      GO TO 120
C
110   ISTAT=1
C
120   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SCMAP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT ('0*** WARNING - ',A,' ',F11.1,' FOR AREA MAP ',A,' IS ',
     *   'NOT IN THE RANGE OF ',F11.1,' THROUGH ',F11.1,'.')
170   FORMAT (' *** NOTE - MAP AREA ',A,' WEIGHTING STATION ',A,
     *   ' HAS A PREDETERMINED ',A,' WEIGHT OF ZERO ',
     *   'AND WILL NOT BE PUNCHED.')
180   FORMAT ('0*** ERROR - ',A,' INDICATOR (',I3,' NOT RECOGNIZED.')
C
      END
