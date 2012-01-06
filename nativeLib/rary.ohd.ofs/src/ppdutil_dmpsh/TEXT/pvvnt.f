C MODULE PVVNT
C-----------------------------------------------------------------------
C
C  PGM: PVVNT(IDTYP,LSTYP,LSMTF,NOFT,JMF,ICOND) .. VERFY NXT TYP ON LIST
C
C   IN: IDTYP ...... DATA TYPE (AS 4 CHARS) FOR CURRENT OPERATION - INT
C   IN: LSTYP(1) ... LIST OF ALLOWABLE DATA TYPES (4 CHR/TYP) - INT
C   IN: LSMTF(1) ... INDICATORS TO INCLUDE MISSG DATA (0,2=NO  1,3=YES),
C   IN:              OR ESTIMATED DATA (0,1=NO  2,3=YES) - INT
C   IN:                'ALL ' .. FOR ALL TYPES
C   IN:                'BOTH' .. FOR ALL TYPES
C   IN:                'RRS ' .. FOR ALL RRS DATA TYPES
C   IN:                'DLY ' .. FOR ALL DAILY DATA TYPES (FROM 'DAILY')
C   IN: NOFT ....... NUMBER OF DATA TYPES IN LSTYP - INT
C  OUT: JMF ........ INCLUDE-MISSING-DATA FLAG, 0=NO, 1=YES - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1=ERR, NO TYP - INT
C
C  HIS: WRITTEN BY D. STREET IN MAY 1988
C  =====================================================================
C
      SUBROUTINE PVVNT (IDTYP,LSTYP,LSMTF,NOFT,JMF,ICOND)
C
      CHARACTER*8 RTNNAM
C
      DIMENSION LSTYP(*),LSMTF(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvvnt.f,v $
     . $',                                                             '
     .$Id: pvvnt.f,v 1.2 2000/07/21 19:58:50 page Exp $
     . $' /
C    ===================================================================
C
      DATA LALL/4hALL /
      DATA LBOT/4hBOTH/
      DATA LRRS/4hRRS /
      DATA LDLY/4hDLY /
C
C
      RTNNAM='PVVNT'
      CALL PVSUBB (RTNNAM,ICOND)
C
      IF (ICOND.NE.0) GO TO 40
C
      ICOND=1
      JMF =0
      IOFT=0
C
C  CHECK IF TYPE TO BE PROCESSED
10    IF (IOFT.GE.NOFT) GO TO 40
      IOFT=IOFT+1
      LSTY=LSTYP(IOFT)
      IF (IDTYP.EQ.LSTY) GO TO 30
      IF (LALL .EQ.LSTY) GO TO 20
      IF (LBOT .EQ.LSTY) GO TO 20
      IF (LRRS .EQ.LSTY .AND. IPDCKR(IDTYP).NE.0) GO TO 20
      IF (LDLY .NE.LSTY .OR.  IPDCDW(IDTYP).EQ.0) GO TO 10
C
20    ICOND=0
      JMF=LSMTF(IOFT)
      GO TO 10
C
30    ICOND=0
      JMF=LSMTF(IOFT)
C
40    CALL PVSUBE (RTNNAM,ICOND)
C
      RETURN
C
      END
