C MODULE PW30BC
C-----------------------------------------------------------------------
C
C  PGM: PW30BC(MMSA,MMS1,MMS2,MMLD,I,J,NS,ICON) .. COMPARE I VS J TH
C
C   IN: MMSA(*) ........ STATE CODE (IN 3RD AND 4TH BYTES) - INT
C   IN: MMS1(*) ........ STATION ID (FIRST 4 CHARACTERS) - INT
C   IN: MMS2(*) ........ STATION ID (SECOND 4 CHARACTERS) - INT
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
      SUBROUTINE PW30BC (MMSA,MMS1,MMS2,MMLD,I,J,NS,ICON)
C
      CHARACTER*4 MMSA(*),MMS1(*),MMS2(*),CI,CJ
      INTEGER     MMLD(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pw30bc.f,v $
     . $',                                                             '
     .$Id: pw30bc.f,v 1.2 2002/02/11 20:47:25 dws Exp $
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
        CALL PWLW (MMS1(I),CI)
        CALL PWLW (MMS1(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMS1(I),CI)
        CALL PWRW (MMS1(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWLW (MMS2(I),CI)
        CALL PWLW (MMS2(J),CJ)
          IF ( CI.LT.CJ ) GO TO 20
          IF ( CI.GT.CJ ) GO TO 30
        CALL PWRW (MMS2(I),CI)
        CALL PWRW (MMS2(J),CJ)
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
