C MEMBER SCFMAP
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/07/95.14:17:33 BY $WC21DT
C
C @PROCESS LVL(77)
C
C DESC PUNCH FUTURE MAP TIME SERIES HEADER
C
      SUBROUTINE SCFMAP (FMAPID,ISTAT)
C
      DIMENSION FMAPID(2),IHEAD(22)
      DIMENSION CHAR(2),CARD(20),ARRAY(100)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scfmap.f,v $
     . $',                                                             '
     .$Id: scfmap.f,v 1.1 1995/09/17 19:14:30 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA MAXCHR/8/,CARD/20*4H    /
      DATA LARRAY/100/
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,40)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HFMAP)
C
      ISTAT=0
      NUMERR=0
      NUMWRN=0
C
C  READ TIME SERIES HEADER
      CALL SUDOPN (1,4HPRD ,IERR)
      IF (IERR.GT.0) GO TO 20
      MAXX=1
      CALL RPRDFH (FMAPID,4HMAP ,MAXX,IHEAD,NUMX,XBUF,IERR)
      IF (IERR.EQ.0) GO TO 10
         ISTAT=IERR
         CALL SRPRST (8HRPRDFH  ,FMAPID,4HMAP ,MAXX,NUMERR,IERR)
         WRITE (IOSDBG,50) FMAPID
         CALL SULINE (IOSDBG,2)
         GO TO 20
C
C  PUNCH 'FMAP' STARTING IN COLUMN 1
10    NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,4HFMAP,4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  PUNCH IDENTIFIER
      CALL UTOCRD (ICDPUN,NPOS,IHEAD(8),8,1,CARD,3,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  PUNCH DESCRIPTION
      CALL UTOCRD (ICDPUN,NPOS,IHEAD(18),20,1,CARD,3,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  PUNCH LATITUDE AND LONGITUDE
      CALL SUBSTR (IHEAD(12),1,4,XLAT,1)
      CALL SUBSTR (IHEAD(13),1,4,XLON,1)
      IF (IUGFIL.EQ.0) CALL SUGTUG (LARRAY,ARRAY,ICHECK)
      IF (XLAT.LE.ULLMTS(1).AND.XLAT.GE.ULLMTS(2)) THEN
         ELSE
            WRITE (LP,15) 'LATITUDE',XLAT,IHEAD(8),
     *                     ULLMTS(2),ULLMTS(1)
            CALL SUWRNS(LP,2,NUMWRN)
         ENDIF
      IF (XLON.LE.ULLMTS(4).AND.XLON.GE.ULLMTS(3)) THEN
         ELSE
            WRITE (LP,15) 'LONGTITUDE',XLON,IHEAD(8),
     *                     ULLMTS(3),ULLMTS(4)
            CALL SUWRNS(LP,2,NUMWRN)
         ENDIF
      CALL UTOCRD (ICDPUN,NPOS,1H(,1,0,CARD,0,0,LNUM,IERR)
      CALL URELCH (XLAT,MAXCHR,CHAR,1,NFILL,IST)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
      CALL URELCH (XLON,MAXCHR,CHAR,1,NFILL,IST)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,0,CARD,0,0,LNUM,IERR)
      CALL UTOCRD (ICDPUN,NPOS,1H),1,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
      CALL UPNCRD (ICDPUN,CARD)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,60)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      GO TO 30
C
20    ISTAT=1
30    IF (ISTRCE.GT.0) WRITE (IOSDBG,70)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
15    FORMAT ('0*** WARNING - ',A,' ',F11.1,' FOR AREA FMAP ',A8,' IS ',
     *        'NOT IN THE RANGE OF ',F11.1,' THROUGH ',F11.1,'.')
40    FORMAT (' *** ENTER SCFMAP')
50    FORMAT ('0*** ERROR - FMAP TIME SERIES HEADER NOT SUCCESSFULLY ',
     *   'READ FOR AREA ',2A4,'.')
60    FORMAT (' *** NOTE - FUTURE MAP TIME SERIES SUCCESSFULLY ',
     *     'PUNCHED.')
70    FORMAT(' *** EXIT SCFMAP')
C
      END
