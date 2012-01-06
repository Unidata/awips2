C MODULE PW40BC
C-----------------------------------------------------------------------
C
C  PGM: PW40BC(MMSA,MMD1,MMD2,MMD3,MMD4,MMD5,MMLD,I,J,NS,ICON) .. CMP
C
C   IN: MMSA(*) ........ STATE CODE (IN 3RD AND 4TH BYTES) - INT
C   IN: MMD1(MMNO) ..... STATION DESCRIPTION (CHARS 01-04) - INT
C   IN: MMD2(MMNO) ..... STATION DESCRIPTION (CHARS 05-08) - INT
C   IN: MMD3(MMNO) ..... STATION DESCRIPTION (CHARS 09-12) - INT
C   IN: MMD4(MMNO) ..... STATION DESCRIPTION (CHARS 13-16) - INT
C   IN: MMD5(MMNO) ..... STATION DESCRIPTION (CHARS 17-20) - INT
C   IN: MMLD(MMNO) ..... STATION PCPN SUM - INT
C   IN: I .............. FIRST INDEX OF MMSA-MMS1-MMS2 FOR COMPARE - INT
C   IN: J .............. SECOND INDX OF MMSA-MMS1-MMS2 FOR COMPARE - INT
C   IN: NS ............. NS=0 SORT BY STATE FIRST, NS=1 SKIP STATE - INT
C  OUT: ICON ........... IF   I.LT.J   THEN   ICON = -1
C  OUT:                  IF   I.EQ.J   THEN   ICON =  0
C  OUT:                  IF   I.GT.J   THEN   ICON =  1
C
C  RQD: SUBPGM: PWLW,PWRW
C  =====================================================================
C
      SUBROUTINE PW40BC (MMSA,MMD1,MMD2,MMD3,MMD4,MMD5,MMLD,I,J,NS,ICON)
C
      CHARACTER*4 MMSA(*),MMD1(*),MMD2(*),MMD3(*),MMD4(*),MMD5(*),CI,CJ
      INTEGER     MMLD(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pw40bc.f,v $
     . $',                                                             '
     .$Id: pw40bc.f,v 1.2 2002/02/11 20:47:32 dws Exp $
     . $' /
C    ===================================================================
C
C
        IF ( NS.NE.0 ) GO TO 10
        CALL PWRW (MMSA(I),CI)
        CALL PWRW (MMSA(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
10      CONTINUE
          IF ( MMLD(I).GT.MMLD(J) ) GO TO 20
          IF ( MMLD(I).LT.MMLD(J) ) GO TO 30
        CALL PWLW (MMD1(I),CI)
        CALL PWLW (MMD1(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMD1(I),CI)
        CALL PWRW (MMD1(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWLW (MMD2(I),CI)
        CALL PWLW (MMD2(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMD2(I),CI)
        CALL PWRW (MMD2(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWLW (MMD3(I),CI)
        CALL PWLW (MMD3(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMD3(I),CI)
        CALL PWRW (MMD3(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWLW (MMD4(I),CI)
        CALL PWLW (MMD4(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMD4(I),CI)
        CALL PWRW (MMD4(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWLW (MMD5(I),CI)
        CALL PWLW (MMD5(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMD5(I),CI)
        CALL PWRW (MMD5(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
C
            ICON =  0
              GO TO 40
20          ICON = -1
              GO TO 40
30          ICON =  1
40            CONTINUE
C
      RETURN
C
      END
