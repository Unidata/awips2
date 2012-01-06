C MEMBER SFMAP2
C  (from old member SFMAP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:18:40 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SFMAP2 (STA1,ISTA1,VALUE1,STA2,ISTA2,VALUE2,IDIM,
     *   ICHECK)
C
C THIS ROUTINE ADDS STATION IDENTIFIER THAT APPEARS ON ONE LIST BUT
C NOT ON ANOTHER LIST.  FOR EXAMPLE, OLD LIST A=(A,B,C,Y,X); OLD LIST
C B=(X,Y,Z,C,A).  ----> NEW LIST A=(A,B,C,Y,X,Z);
C NEW LIST B=(X,Y,Z,C,A,B).  HOWEVER, THE STATION WEIGHT FIELD
C (ID6(I) IN <SWORK> ARRAY WILL BE FILLED WITH 0.00000
C TO INDICATE STATION WEIGHT FOR ANY STATION IDENTIFIER THAT PREVIOUSLY
C DID NOT APPEAR ON THE LIST BUT NOW APPEAR ON THE NEW LIST.
C
      CHARACTER*4 STA1(*),STA2(*)
      REAL VALUE1(*),VALUE2(*)
      LOGICAL FOUND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfmap2.f,v $
     . $',                                                             '
     .$Id: sfmap2.f,v 1.1 1995/09/17 19:11:09 dws Exp $
     . $' /
C    ===================================================================
C
C
      ICHECK=0
      KID=1
      NEWSTA=ISTA2
C
      ID1=1
10    IF (ID1.LE.ISTA1) THEN
         FOUND=.FALSE.
         JID=1
         ID2=1
C
20       IF (ID2.LE.ISTA2) THEN
            IF ((STA1(KID).EQ.STA2(JID)).AND.(STA1(KID+1).EQ.
     *           STA2(JID+1))) THEN
               FOUND=.TRUE.
               ID2=ISTA2
            ENDIF
            JID=JID+2
            ID2=ID2+1
            GO TO 20
         ENDIF
C
         IF ((FOUND).AND.(ISTA2.GT.0)) GO TO 30
         IF (NEWSTA.GE.IDIM) THEN
            ICHECK=1
            GO TO 40
         ENDIF
         NEWLOC=NEWSTA*2
         STA2(NEWLOC+1)=STA1(KID)
         STA2(NEWLOC+2)=STA1(KID+1)
         VALUE2(NEWSTA+1)=0.00000
         NEWSTA=NEWSTA+1
30       KID=KID+2
         ID1=ID1+1
         GO TO 10
      ENDIF
C
      ISTA2=NEWSTA
40    CONTINUE
C
      RETURN
C
      END
