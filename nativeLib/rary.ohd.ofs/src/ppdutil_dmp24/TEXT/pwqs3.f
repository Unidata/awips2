C MODULE PWQS3
C-----------------------------------------------------------------------
C
C  PGM: PWQS3(MMSA,MMS1,MMS2,MMLD,NS,MMNO) .. QUICK SORT ON STATE-STATN
C
C   IN: MMSA(MMNO) ..... STATE CODE (IN 3RD AND 4TH BYTES) - INT
C   IN: MMS1(MMNO) ..... STATION ID (FIRST 4 CHARACTERS) - INT
C   IN: MMS2(MMNO) ..... STATION ID (SECOND 4 CHARACTERS) - INT
C   IN: MMLD(MMNO) ..... STATION PCPN SUM - INT
C   IN: NS ............. NS=0 SORT BY STATE FIRST, NS=1 SKIP STATE - INT
C   IN: MMNO ........... NUMBER OF STATIONS - INT
C
C  =====================================================================
C
      SUBROUTINE PWQS3 (MMSA,MMS1,MMS2,MMLD,NS,MMNO)
C
      INCLUDE 'uiox'
C
      CHARACTER*4 MMSA(1),MMS1(1),MMS2(1),CTEMP
      INTEGER    MMLD(1),NS,MMNO
      INTEGER    ISX,ISLM,ISTAK1(200),ISTAK2(200),II,IJ,IL,IR,IX
      INTEGER    ICON,ISXMX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwqs3.f,v $
     . $',                                                             '
     .$Id: pwqs3.f,v 1.2 2002/02/11 20:48:25 dws Exp $
     . $' /
C    ===================================================================
C
C
        ISX   = 0
        ISLM  = 200
        ISXMX = 0
        IX    = MMNO+1
C
        IL = 1
        IR = MMNO
C
10      IF ( IR.LT.IL+1 ) GO TO 100
          II = IL
          IJ = IR
          MMSA(IX) = MMSA(IL)
          MMS1(IX) = MMS1(IL)
          MMS2(IX) = MMS2(IL)
          MMLD(IX) = MMLD(IL)
C
20        CALL PW30BC (MMSA,MMS1,MMS2,MMLD,IX,IJ,NS,ICON)
          IF ( ICON.GE.0 ) GO TO 30
            IJ = IJ-1
            GO TO 20
30        CONTINUE
C
          IF ( IJ.LE.II ) GO TO 70
            CTEMP    = MMSA(II)
            MMSA(II) = MMSA(IJ)
            MMSA(IJ) = CTEMP
            CTEMP    = MMS1(II)
            MMS1(II) = MMS1(IJ)
            MMS1(IJ) = CTEMP
            CTEMP    = MMS2(II)
            MMS2(II) = MMS2(IJ)
            MMS2(IJ) = CTEMP
            ITEMP    = MMLD(II)
            MMLD(II) = MMLD(IJ)
            MMLD(IJ) = ITEMP
            II = II+1
C
40          CALL PW30BC (MMSA,MMS1,MMS2,MMLD,IX,II,NS,ICON)
            IF ( ICON.LE.0 ) GO TO 50
               II = II+1
               GO TO 40
50          CONTINUE
C
            IF ( II.GE.IJ ) GO TO 60
              CTEMP    = MMSA(IJ)
              MMSA(IJ) = MMSA(II)
              MMSA(II) = CTEMP
              CTEMP    = MMS1(IJ)
              MMS1(IJ) = MMS1(II)
              MMS1(II) = CTEMP
              CTEMP    = MMS2(IJ)
              MMS2(IJ) = MMS2(II)
              MMS2(II) = CTEMP
              ITEMP    = MMLD(IJ)
              MMLD(IJ) = MMLD(II)
              MMLD(II) = ITEMP
              IJ = IJ-1
              GO TO 20
60        II = IJ
C
70        IF ( IR-1.LT.II-IL ) GO TO 80
            IF ( ISX.GE.ISLM ) GO TO 110
              ISX = ISX+1
              ISTAK1(ISX) = II+1
              ISTAK2(ISX) = IR
              IR = II-1
              GO TO 90
80          IF ( ISX.GE.ISLM ) GO TO 110
              ISX = ISX+1
              ISTAK1(ISX) = IL
              ISTAK2(ISX) = II-1
              IL = II+1
90          CONTINUE
            IF ( ISX.GT.ISXMX ) ISXMX = ISX
            GO TO 10
C
100     IF ( ISX.LE.0 ) GO TO 130
          IL = ISTAK1(ISX)
          IR = ISTAK2(ISX)
          ISX = ISX-1
          GO TO 10
C
110     WRITE (LP,120)
120     FORMAT (' **ERROR** STACK OVERFLOW IN PWQS1')
C
130     CONTINUE
C
      RETURN
C
      END
