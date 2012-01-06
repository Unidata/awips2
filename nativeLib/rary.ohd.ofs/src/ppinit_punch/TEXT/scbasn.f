C MEMBER SCBASN
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/07/95.14:17:16 BY $WC21DT
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO PUNCH BASIN PARAMETER RECORD
C
      SUBROUTINE SCBASN (UNITS,BASNID,DESCRP,FLAT,FLON,NBPTS,AREA,ELEV,
     *   IVBASN,MXLDEC,PUFORM,ISTAT)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
      CHARACTER*(*) UNITS,PUFORM
      CHARACTER*8 CHAR
      CHARACTER*8 BASNID
      CHARACTER*20 DESCRP
      CHARACTER*80 CARD/' '/
C
      DIMENSION FLAT(NBPTS),FLON(NBPTS),ARRAY(100)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scbasn.f,v $
     . $',                                                             '
     .$Id: scbasn.f,v 1.1 1995/09/17 19:14:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('BASN')
C
      ISTAT=0
      LARRAY=100
      NUMWRN=0
C
C  PRINT VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,70) IVBASN
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH 'BASN' STARTING IN COLUMN 1
      NPOS=1
      NSPACE=0
      CALL UTOCRD (ICDPUN,NPOS,'BASN',4,NSPACE,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  PUNCH UNITS TYPE
      CALL UTOCRD (ICDPUN,NPOS,'(',1,0,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
      CALL UTOCRD (ICDPUN,NPOS,UNITS,4,0,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
      CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  PUNCH BASIN IDENTIFIER
      CALL UTOCRD (ICDPUN,NPOS,BASNID,LEN(BASNID),1,CARD,3,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  PUNCH DESCRIPTIVE INFORMATION
      CALL UTOCRD (ICDPUN,NPOS,DESCRP,LEN(DESCRP),1,CARD,3,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  PUNCH LAT/LON PAIRS
      CALL UTOCRD (ICDPUN,NPOS,'(',1,0,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
      IF (PUFORM.EQ.'FIXED') THEN
         CALL UPNCRD (ICDPUN,CARD)
         NPOS=6
         ENDIF
      IF (PUFORM.EQ.'ONEPERCARD') THEN
         CALL UPNCRD (ICDPUN,CARD)
         NPOS=6
         ENDIF
      MCHAR=LEN(CHAR)
      NCHECK=11
      NSPACE=1
      NSKIP=1
      ITYPE=2
      MAXDEC=MXLDEC
      IF (PUFORM.EQ.'FIXED') THEN
         MCHAR=-LEN(CHAR)
         NCHECK=2*(4+MAXDEC)+2
         NSPACE=2
         NSKIP=1
         ITYPE=4
         NPOS=6
         ENDIF
      IF (PUFORM.EQ.'ONEPERCARD') THEN
         MCHAR=-LEN(CHAR)
         NCHECK=2*(4+MAXDEC)+2
         NSKIP=3
         ITYPE=4
         NPOS=6
         ENDIF
      IF (IUGFIL.EQ.0) CALL SUGTUG(LARRAY,ARRAY,ICHECK)
      DO 10 I=1,NBPTS
         IF (FLAT(I).LE.ULLMTS(1).AND.FLAT(I).GE.ULLMTS(2)) THEN
            ELSE
               WRITE (LP,15) 'LATITUDE',FLAT(I),BASNID,
     *                        ULLMTS(2),ULLMTS(1)
               CALL SUWRNS(LP,2,NUMWRN)
            ENDIF
         CALL URELCH (FLAT(I),MCHAR,CHAR,MAXDEC,NFILL,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),0,CARD,ITYPE,NCHECK,
     *                LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         IF (PUFORM.EQ.'PACKED') THEN
            CALL UTOCRD (ICDPUN,NPOS,',',1,0,CARD,0,0,LNUM,IERR)
            ENDIF
         IF (PUFORM.EQ.'FIXED') THEN
            NPOS=NPOS+NSKIP
            ENDIF
         IF (PUFORM.EQ.'ONEPERCARD') THEN
            NPOS=NPOS+NSKIP
            ENDIF
         IF (FLON(I).LE.ULLMTS(4).AND.FLON(I).GE.ULLMTS(3)) THEN
            ELSE
               WRITE (LP,15) 'LONGITUDE',FLON(I),BASNID,
     *                        ULLMTS(3),ULLMTS(4)
               CALL SUWRNS(LP,2,NUMWRN)
            ENDIF
         CALL URELCH (FLON(I),MCHAR,CHAR,MAXDEC,NFILL,IERR)
         IF (IERR.GT.0) GO TO 40
         IF (PUFORM.EQ.'PACKED') THEN
            IF (I.EQ.NBPTS) NSPACE=0
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),NSPACE,CARD,ITYPE,0,
     *                LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         IF (PUFORM.EQ.'ONEPERCARD') THEN
            CALL UPNCRD (ICDPUN,CARD)
            NPOS=6
            ENDIF
10       CONTINUE
      IF (PUFORM.EQ.'FIXED') THEN
         CALL UPNCRD (ICDPUN,CARD)
         NPOS=6
         ENDIF
      CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  PUNCH USER SPECIFIED AREA
      IF (AREA.LE.-996.9) GO TO 20
         VALUE=AREA
         IF (UNITS.EQ.'ENGL') THEN
            CALL UDUCNV ('KM2 ','MI2 ',1,1,AREA,VALUE,IERR)
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,'AREA(',5,0,CARD,0,12,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         MAXDEC=2
         IPRERR=1
         CALL UFF2A (VALUE,CHAR,1,LEN(CHAR),MAXDEC,IPRERR,LP,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
C
C  PUNCH BASIN ELEVATION
20    IF (ELEV.LE.-996.9) GO TO 30
         VALUE=ELEV
         IF (UNITS.EQ.'ENGL') THEN
           CALL UDUCNV ('M   ','FT  ',1,1,ELEV,VALUE,IERR)
            ENDIF
         CALL UTOCRD (ICDPUN,NPOS,'ELEV(',5,0,CARD,0,12,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         MAXDEC=2
         IPRERR=1
         CALL UFF2A (VALUE,CHAR,1,LEN(CHAR),MAXDEC,IPRERR,LP,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),0,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
         CALL UTOCRD (ICDPUN,NPOS,')',1,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 40
C
30    CALL UPNCRD (ICDPUN,CARD)
      GO TO 50
C
40    ISTAT=1
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
15    FORMAT ('0*** WARNING - ',A,' ',F11.1,' FOR BASIN ',A8,' IS NOT ',
     *        'IN THE RANGE OF ',F11.1,' THROUGH ',F11.1,'.')
60    FORMAT (' *** ENTER SCBASN')
70    FORMAT (' IVBASN=',I2)
80    FORMAT (' *** EXIT SCBASN')
C
      END
