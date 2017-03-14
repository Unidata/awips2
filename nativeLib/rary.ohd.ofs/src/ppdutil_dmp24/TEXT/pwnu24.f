C MEMBER PWNU24
C  (from old member PDBDMP24)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/10/95.13:51:53 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  PGM: PWNU24(STA,IDTYP,JULDB,JULDE,IUNFLG,DATA,LDATA,LDSUM) .. PPT SUM
C
C
C   IN: STA(2) ....... STATION ID AS 8 PACKED CHARS (2A4) - INT
C   IN: IDTYP ........ DATA TYPE AS 4 PACKED CHARS (A4) - INT
C   IN: JULDB ........ BEGINNING ORDINAL DAY NUM (JAN 1 1900 IS 1) - INT
C   IN: JULDE ........ ENDING ORDINAL DAY NUMBER (JAN 1 1900 IS 1) - INT
C   IN: IUNFLG ....... UNITS FLAG, 0 = ENGLISH, 1 = METRIC - INT
C  OTH: DATA(LDATA) .. DUMMY ARRAY - INT*2
C   IN: LDATA ........ DIMENSION OF DUMMY ARRAYS - INT
C  OUT: LDSUM ........ SUM OF PCPN DATA - INT
C
C
C  RQD: SUBPGM: RPD1S,PVE1S,PDGTPP
C  RQD: COMMON: PDBDTA
C
C
C  HIS: WRITTEN BY D. STREET IN JAN 1989
C  =====================================================================
      SUBROUTINE PWNU24 (STA,IDTYP,JULDB,JULDE,IUNFLG,DATA,LDATA,LDSUM)
C
C
C
      INCLUDE 'pdbcommon/pdbdta'
C
      INTEGER   IDELT(1),IRTYPE(1),NVPDT(1),STA(2),IDTYP,LDSUM,LDATA
      INTEGER   IHR,JUL1,JUL2,ISTAF1,NOTYP,IRLEN,KEYE,MTYPES,LDFILL
      INTEGER   ISTAT,NUMDTA,NCOU,L,JULDB,JULDE,IUNFLG,IPP
      INTEGER*2 DATA(LDATA),MSNG(1)
      REAL      RPP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwnu24.f,v $
     . $',                                                             '
     .$Id: pwnu24.f,v 1.1 1995/09/17 19:09:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA    NOTYP,IRLEN,KEYE /  1, 1, 1 /
C
C
C
C                  SET FLAG FOR STA TO BE NAME, NOT NUMBER, SET DATES
C
C
      LDSUM = -19999
      ISTAF1 = 0
      JUL1 = JULDB
      JUL2 = JULDE
C
C
C                  GET DATA FROM PPDB DATA BASE FOR GIVEN STA AND TYPE
C                  OUTPUT ERRORS IF ANY, EXIT RTN IF ERROR OR NO DATA
C                  GET TOTAL NUMBER OF DATA ITEMS, SET MISSG DATA VALUES
C
C
      CALL RPD1S(STA,ISTAF1,NOTYP,IDTYP,JUL1,JUL2,IRLEN,IRTYPE,
     1            MTYPES,LDATA,DATA,LDFILL,IDELT,NVPDT,MSNG,ISTAT)
        IF ( ISTAT .NE.0 ) CALL PVE1S(KEYE,STA,IDTYP,ISTAT)
        IF ( ISTAT .NE.0 ) GO TO 20
        IF ( LDFILL.LT.1 ) GO TO 20
      NUMDTA = NVPDT(1)*24/IDELT(1) * (JUL2-JUL1+1)
C
C
C                  LOOP TO GET SUM OF PP24 DATA AS INTEGER IN 100THS
C
C
      NCOU  = 0
      LDSUM = 0
      DO 10 L=1,NUMDTA
        CALL PDGTPP(DATA(L),RPP,IHR,IPP)
        IF ( IPP.NE.MISSPP ) LDSUM = LDSUM+IPP
        IF ( IPP.NE.MISSPP ) NCOU  = NCOU+1
10      CONTINUE
C
      IF ( NCOU.LE.0 ) LDSUM = -999
C
C
20    RETURN
C
      END
