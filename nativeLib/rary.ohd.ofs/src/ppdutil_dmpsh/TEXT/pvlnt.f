C MODULE PVLNT
C-----------------------------------------------------------------------
C
C  PGM: PVLNT(NXTYP,NOFTY,TYPS,IDTYP,ICOND) .. LOOP TO GET NEXT TYPE
C
C  I/O: NXTYP ...... LOOP COUNTER FOR NEXT DATA TYPE IN TYPS ARRAY - INT
C   IN: NOFTY ...... NUM OF DATA TYPES IN DATA TYPE ARRAY (MAX 64) - INT
C   IN: TYPS(64) ... DATA TYPE ARRAY (4 CHR/TYP) TO BE LOOPED THRU - INT
C  OUT: IDTYP ...... DATA TYPE OBTAINED (4 CHARS), ELSE BLANK - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1 = ERR, NO STA - INT
C
C  HIS: WRITTEN BY D. STREET IN MAY 1988
C  =====================================================================
C
      SUBROUTINE PVLNT (NXTYP,NOFTY,ITYPS,IDTYP,ICOND)
C
      CHARACTER*8 RTNNAM

      DIMENSION ITYPS(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvlnt.f,v $
     . $',                                                             '
     .$Id: pvlnt.f,v 1.2 2000/07/21 19:59:10 page Exp $
     . $' /
C    ===================================================================
C
      DATA  IBLNK/4h    /
C
C
      RTNNAM='PVLNT'
      CALL PVSUBB (RTNNAM,ICOND)
C
      IF (ICOND.NE.0) GO TO 10
C
      IDTYP=IBLNK
C
C  CHECK IF FIRST DATA TYPE
      IF (NXTYP.LE.0) NXTYP=1
C
C  CHECK IF LAST DATA TYPE
      IF (NXTYP.GT.NOFTY) THEN
         ICOND=1
         NXTYP=0
	 ENDIF
      IF (ICOND.NE.0) GO TO 10
C
C  SET DATA TYPE
      IDTYP=ITYPS(NXTYP)
      NXTYP=NXTYP+1
C
10    CALL PVSUBE (RTNNAM,ICOND)
C
      RETURN
C
      END
