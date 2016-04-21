C MODULE PWQS2
C-----------------------------------------------------------------------
C
C  PGM: PWQS2(MMSA,MMS1,MMS2,MMD1,MMD2,MMD3,MMD4,MMD5,NS,MMNO) .. SORT
C
C   IN: MMSA(MMNO) ..... STATE CODE (IN 3RD AND 4TH BYTES) - INT
C   IN: MMS1(MMNO) ..... STATION ID (FIRST 4 CHARACTERS) - INT
C   IN: MMS2(MMNO) ..... STATION ID (SECOND 4 CHARACTERS) - INT
C   IN: MMD1(MMNO) ..... STATION DESCRIPTION (CHARS 01-04) - INT
C   IN: MMD2(MMNO) ..... STATION DESCRIPTION (CHARS 05-08) - INT
C   IN: MMD3(MMNO) ..... STATION DESCRIPTION (CHARS 09-12) - INT
C   IN: MMD4(MMNO) ..... STATION DESCRIPTION (CHARS 13-16) - INT
C   IN: MMD5(MMNO) ..... STATION DESCRIPTION (CHARS 17-20) - INT
C   IN: NS ............. NS=0 SORT BY STATE FIRST, NS=1 SKIP STATE - INT
C   IN: MMNO ........... NUMBER OF STATIONS - INT
C
C  =====================================================================
C
      SUBROUTINE PWQS2 (MMSA,MMS1,MMS2,MMD1,MMD2,MMD3,MMD4,MMD5,NS,MMNO)
C
      INCLUDE 'uiox'
C
      INTEGER    MMSA(1),MMS1(1),MMS2(1),CTEMP
      INTEGER    MMD1(1),MMD2(1),MMD3(1)
      INTEGER    MMD4(1),MMD5(1),NS,MMNO
      INTEGER    ISX,ISLM,ISTAK1(200),ISTAK2(200),II,IJ,IL,IR,IX
      INTEGER    ICON,ISXMX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwqs2.f,v $
     . $',                                                             '
     .$Id: pwqs2.f,v 1.2 2002/02/11 20:47:57 dws Exp $
     . $' /
C    ===================================================================
C
C
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
          MMD1(IX) = MMD1(IL)
          MMD2(IX) = MMD2(IL)
          MMD3(IX) = MMD3(IL)
          MMD4(IX) = MMD4(IL)
          MMD5(IX) = MMD5(IL)
C
20        CALL PW20BC(MMSA,MMD1,MMD2,MMD3,MMD4,MMD5,IX,IJ,NS,ICON)
          IF ( ICON.GE.0 ) GO TO 30
            IJ = IJ-1
              GO TO 20
30          CONTINUE
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
            ITEMP    = MMD1(II)
            MMD1(II) = MMD1(IJ)
            MMD1(IJ) = CTEMP
            ITEMP    = MMD2(II)
            MMD2(II) = MMD2(IJ)
            MMD2(IJ) = CTEMP
            ITEMP    = MMD3(II)
            MMD3(II) = MMD3(IJ)
            MMD3(IJ) = CTEMP
            ITEMP    = MMD4(II)
            MMD4(II) = MMD4(IJ)
            MMD4(IJ) = CTEMP
            ITEMP    = MMD5(II)
            MMD5(II) = MMD5(IJ)
            MMD5(IJ) = CTEMP
            II = II+1
C
40          CALL PW20BC(MMSA,MMD1,MMD2,MMD3,MMD4,MMD5,IX,II,NS,ICON)
            IF ( ICON.LE.0 ) GO TO 50
              II = II+1
                GO TO 40
50            CONTINUE
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
              ITEMP    = MMD1(IJ)
              MMD1(IJ) = MMD1(II)
              MMD1(II) = ITEMP
              ITEMP    = MMD2(IJ)
              MMD2(IJ) = MMD2(II)
              MMD2(II) = ITEMP
              ITEMP    = MMD3(IJ)
              MMD3(IJ) = MMD3(II)
              MMD3(II) = ITEMP
              ITEMP    = MMD4(IJ)
              MMD4(IJ) = MMD4(II)
              MMD4(II) = ITEMP
              ITEMP    = MMD5(IJ)
              MMD5(IJ) = MMD5(II)
              MMD5(II) = ITEMP
              IJ = IJ-1
                GO TO 20
60            II = IJ
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
120     FORMAT (' **ERROR** STACK OVERFLOW IN PWQS2')
C
130     CONTINUE
C
C
      RETURN
C
      END
