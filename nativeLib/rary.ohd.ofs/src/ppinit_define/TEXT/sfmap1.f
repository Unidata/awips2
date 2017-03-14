C MEMBER SFMAP1
C  (from old member SFMAP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:18:40 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SFMAP1 (WINSTA,NWNSTA,SUMSTA,NSMSTA,WINTER,SUMMER,
     *   ID5,ID6,IDIM,ISTAT)
C
C  THIS ROUTINE MOVES STATION WEIGHT IDENTIFIERS AND STATION WEIGHT
C  FROM A TEMPORARY ARRAY TO <SWORK> ARRAY AT POSITION ID5 AND ID6,
C  RESPECTIVELY.
C
      CHARACTER*4 WINSTA(*),SUMSTA(*)
      CHARACTER*4 TEMP(2)
      REAL WINTER(*),SUMMER(*)
C
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfmap1.f,v $
     . $',                                                             '
     .$Id: sfmap1.f,v 1.1 1995/09/17 19:11:08 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
      KID=1
C
      CALL SFMAP2 (WINSTA,NWNSTA,WINTER,SUMSTA,NSMSTA,SUMMER,IDIM,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      CALL SFMAP2 (SUMSTA,NSMSTA,SUMMER,WINSTA,NWNSTA,WINTER,IDIM,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
C CHECK IF TOTAL NUMBER OF STATION EXCEEDS DIMENSION ALLOWED
C
      IF (((NWNSTA+NSMSTA)*2).GT.IDIM*2) THEN
         ISTAT=1
         GO TO 50
      ENDIF
C NOTE THAT NWNSTA=NSMSTA IF ISTAT=0
C
C MAKE A ONE-TO-ONE CORRESPONDENCE
C
      MID=1
      DO 20 M=1,NWNSTA
         NID=1
         DO 10 N=1,NSMSTA
            IF ((WINSTA(MID).EQ.SUMSTA(NID)).AND.(WINSTA(MID+1).EQ.
     *           SUMSTA(NID+1))) THEN
               CALL SUBSTR (SUMSTA(NID),1,8,TEMP,1)
               CALL SUBSTR (SUMSTA(MID),1,8,SUMSTA(NID),1)
               CALL SUBSTR (TEMP,1,8,SUMSTA(MID),1)
               TVALUE=SUMMER(N)
               SUMMER(N)=SUMMER(M)
               SUMMER(M)=TVALUE
            ENDIF
            NID=NID+2
10       CONTINUE
         MID=MID+2
20    CONTINUE
C
C FIRST MOVE STATION IDENTIFIERS AND WINTER STATION WEIGHT TO
C SWORK ARRAY STARTING AT LOCATION ID5 AND ID6, RESPECTIVELY
C
      I=1
30    IF (I.LE.NWNSTA) THEN
         CALL SUBSTR (WINSTA(KID),1,8,SWORK(ID5+KID-1),1)
         SWORK(ID6+I-1)=WINTER(I)
         KID=KID+2
         I=I+1
         GO TO 30
      ENDIF
C
C SECOND MOVE SUMMER STATION WEIGHT TO SWORK ARRAY IMMEDIATELY
C RIGHT AFTER WINTER STATION WEIGHT AT LOCATION ID6
C
      J=1
      I=I-1
40    IF (J.LE.NSMSTA) THEN
         SWORK(ID6+I+J-1)=SUMMER(J)
         J=J+1
         GO TO 40
      ENDIF
50    CONTINUE
C
      RETURN
C
      END
