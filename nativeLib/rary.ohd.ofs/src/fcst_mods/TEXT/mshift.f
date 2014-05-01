C MEMBER MSHIFT
C  (from old member MCSHFT)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 07/06/95.15:13:41 BY $WC21DT
C
      SUBROUTINE MSHIFT(IPOS,IDATE,LDATE,ISTAT)
C
      INCLUDE 'common/modrcs'      
      DIMENSION IDEL(5),IJ(5),LJ(5),IS(5),IHOUR(5),LHOUR(5)
      DIMENSION HN(5),QN(5),HLL(5),HUU(5)
      LOGICAL SORT,SPLIT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mshift.f,v $
     . $',                                                             '
     .$Id: mshift.f,v 1.1 1995/11/14 21:27:43 erb Exp $
     . $' /
C    ===================================================================
C
      ISTAT=0
      INEW=0
      MAX=5
      DO 10 I=1,MAX
         IDEL(I)=0
10    CONTINUE
      IF (NSHIFT(IPOS).LT.2) GOTO 40
      SORT=.TRUE.
20    IF (SORT) THEN
         SORT=.FALSE.
            DO 30 I=1,NSHIFT(IPOS)-1
               IF (IJHSHF(I,IPOS).GT.IJHSHF(I+1,IPOS)) THEN
                  CALL MWITCH(I,I+1,IPOS,0)
                  SORT=.TRUE.
                  ENDIF
30          CONTINUE
         GOTO 20
         ENDIF
40    CONTINUE
      SPLIT=.FALSE.
      NUM=NSHIFT(IPOS)
      CALL UMEMOV(IJHSHF(1,IPOS),IHOUR(1),NUM)
      CALL UMEMOV(LJHSHF(1,IPOS),LHOUR(1),NUM)
      DO 50 I=1,NUM
         IF (IDATE.LT.IHOUR(I).AND.LDATE.GT.LHOUR(I)) THEN
            IDEL(I)=1
            GOTO 50
            ENDIF
         IF (IDATE.EQ.IHOUR(I).AND.LDATE.EQ.LHOUR(I)) THEN
            IDEL(I)=1
            GOTO 50
            ENDIF
         IF (IDATE.GT.IHOUR(I).AND.LDATE.LT.LHOUR(I)) THEN
             SPLIT=.TRUE.
             KEY=I
             GOTO 60
             ENDIF
         IF (IDATE.GT.LHOUR(I)) GOTO 50
         IF (LDATE.LT.IHOUR(I)) GOTO 50
         IF (IDATE.LT.IHOUR(I).AND.LDATE.EQ.IHOUR(I)) THEN
            IHOUR(I)=LDATE+1
            GOTO 50
            ENDIF
         IF (IDATE.LT.IHOUR(I).AND.LDATE.EQ.LHOUR(I)) THEN
            IDEL(I)=1
            GOTO 50
            ENDIF
         IF (IDATE.LT.IHOUR(I).AND.LDATE.LT.LHOUR(I)) THEN
            IHOUR(I)=LDATE+1
            GOTO 50
            ENDIF
         IF (IDATE.EQ.IHOUR(I).AND.LDATE.LT.LHOUR(I)) THEN
            IHOUR(I)=IDATE+1
            GOTO 50
            ENDIF
         IF (IDATE.EQ.IHOUR(I).AND.LDATE.GT.LHOUR(I)) THEN
            IDEL(I)=1
            GOTO 50
            ENDIF
         IF (IDATE.GT.IHOUR(I).AND.LDATE.GT.LHOUR(I)) THEN
            LHOUR(I)=IDATE-1
            GOTO 50
            ENDIF
         IF (IDATE.GT.IHOUR(I).AND.LDATE.EQ.LHOUR(I)) THEN
            LHOUR(I)=IDATE-1
            GOTO 50
            ENDIF
50    CONTINUE
60    CONTINUE
      IF (SPLIT) GOTO 85
      NUMBER=0
      DO 65 I=1,NUM
         IF (IDEL(I).EQ.0) NUMBER=NUMBER+1
65    CONTINUE
      IF (NUMBER.GT.MAX-1) THEN
         ISTAT=1
         GOTO 999
         ENDIF
      CALL UMEMOV(IHOUR(1),IJHSHF(1,IPOS),NUM)
      CALL UMEMOV(LHOUR(1),LJHSHF(1,IPOS),NUM)
      DO 70 I=1,NUM
         IF (IDEL(I).EQ.0) THEN
            CALL UMEMOV(IJHSHF(I,IPOS),IJ(INEW+1),1)
            CALL UMEMOV(LJHSHF(I,IPOS),LJ(INEW+1),1)
            CALL UMEMOV(ISTYPE(I,IPOS),IS(INEW+1),1)
            CALL UMEMOV(HNEW(I,IPOS),HN(INEW+1),1)
            CALL UMEMOV(QNEW(I,IPOS),QN(INEW+1),1)
            CALL UMEMOV(HL(I,IPOS),HLL(INEW+1),1)
            CALL UMEMOV(HU(I,IPOS),HUU(INEW+1),1)
            INEW=INEW+1
            ENDIF
70    CONTINUE
      DO 80 I=1,INEW
         CALL UMEMOV(IJ(I),IJHSHF(I,IPOS),1)
         CALL UMEMOV(LJ(I),LJHSHF(I,IPOS),1)
         CALL UMEMOV(IS(I),ISTYPE(I,IPOS),1)
         CALL UMEMOV(HN(I),HNEW(I,IPOS),1)
         CALL UMEMOV(QN(I),QNEW(I,IPOS),1)
         CALL UMEMOV(HLL(I),HL(I,IPOS),1)
         CALL UMEMOV(HUU(I),HU(I,IPOS),1)
80    CONTINUE
      NUM=INEW
85    IF (.NOT.SPLIT) GOTO 90
      IF (NUM.GT.MAX-2) THEN
         ISTAT=1
         GOTO 999
         ENDIF
      ITEMP=LJHSHF(KEY,IPOS)
      LJHSHF(KEY,IPOS)=IDATE-1
      NUM=NUM+1
      CALL MWITCH(NUM,KEY,IPOS,1)
      IJHSHF(NUM,IPOS)=LDATE+1
      LJHSHF(NUM,IPOS)=ITEMP
90    CONTINUE
      IF (NUM.GT.MAX-1) THEN
         ISTAT=1
         GOTO 999
         ENDIF
      NSHIFT(IPOS)=NUM
999   CONTINUE
      RETURN
      END
