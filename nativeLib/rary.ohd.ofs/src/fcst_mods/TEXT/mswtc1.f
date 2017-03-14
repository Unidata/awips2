C MEMBER MSWTC1
C  (from old member MSWTCH)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 07/06/95.14:56:49 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE MSWTC1(OPNAME,KWRDS,IDATE,LDATE,ISTAT)
      CHARACTER*8 TEMPOP
      DIMENSION TEMPOP(10),KEYTMP(10),IDTTMP(10),LDTTMP(10)
      CHARACTER*8 OPNAME,OPNME
      CHARACTER*4 KWRDS,KEYWRD
      LOGICAL SORT,ADD
      CHARACTER*8 OPN30
      REAL*4 KEY30
      COMMON/MOD130/NDT30,KEY30(10),OPN30(10),IDT30(10),LDT30(10)
      DIMENSION IFLAG(10),ITEMP(10),LTEMP(10),IDEL(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mswtc1.f,v $
     . $',                                                             '
     .$Id: mswtc1.f,v 1.1 1995/11/14 21:28:25 erb Exp $
     . $' /
C    ===================================================================
C
      ISTAT=0
      CALL UMEMOV(OPN30,TEMPOP,20)
      CALL UMEMOV(KEY30,KEYTMP,10)
      CALL UMEMOV(IDT30,IDTTMP,10)
      CALL UMEMOV(LDT30,LDTTMP,10)
      DO 10 I=1,NDT30
         CALL UMEMOV(OPN30(I),OPNME,2)
         CALL UMEMOV(KEY30(I),KEYWRD,1)
         IFLAG(I)=0
         IDEL(I)=0
         IF (OPNME.EQ.OPNAME.AND.KEYWRD.EQ.KWRDS) IFLAG(I)=1
10    CONTINUE
      NUM=0
      DO 20 J=1,NDT30
         IF (IFLAG(J).NE.0) THEN
            NUM=NUM+1
            CALL UMEMOV(IDT30(J),ITEMP(NUM),1)
            CALL UMEMOV(LDT30(J),LTEMP(NUM),1)
            ENDIF
20    CONTINUE
      IF (NUM-1.LE.0) GOTO 50
      SORT=.TRUE.
30    IF (SORT) THEN
         SORT=.FALSE.
         DO 40 K=1,NUM-1
            IF (ITEMP(K).GT.ITEMP(K+1)) THEN
               ISTORE=ITEMP(K)
               LSTORE=LTEMP(K)
               ITEMP(K)=ITEMP(K+1)
               LTEMP(K)=LTEMP(K+1)
               ITEMP(K+1)=ISTORE
               LTEMP(K+1)=LSTORE
               SORT=.TRUE.
               ENDIF
40       CONTINUE
         GOTO 30
         ENDIF
50    CONTINUE
      DO 60 L=1,NUM
         IF (IDATE.GE.ITEMP(L).AND.LDATE.LE.LTEMP(L)) THEN
            ISTAT=1
            GOTO 500
            ENDIF
         IF (IDATE.LT.ITEMP(L).AND.LDATE.GT.LTEMP(L)) IDEL(L)=1
         IF (IDATE.LE.ITEMP(L).AND.LDATE.GE.ITEMP(L).AND.LDATE.LE.
     *                         LTEMP(L)) THEN
            LDATE=LTEMP(L)
            IDEL(L)=1
            GOTO 60
            ENDIF
         IF (IDATE.GE.ITEMP(L).AND.IDATE.LE.LTEMP(L).AND.LDATE.GE.
     *                          LTEMP(L)) THEN
            IDATE=ITEMP(L)
            IDEL(L)=1
            GOTO 60
            ENDIF
60    CONTINUE
500   IF (ISTAT.EQ.1) GOTO 999
      ADD=.TRUE.
      DO 70 I=1,NUM
         IF (IDEL(I).NE.0) ADD=.FALSE.
70    CONTINUE
      IF (ADD) THEN
         ISTAT=2
         GOTO 999
         ENDIF
      NEWNDT=0
      DO 80 M=1,NDT30
         IF (IFLAG(M).EQ.0) THEN
            NEWNDT=NEWNDT+1
            CALL UMEMOV(TEMPOP(M),OPN30(NEWNDT),2)
            CALL UMEMOV(KEYTMP(M),KEY30(NEWNDT),1)
            CALL UMEMOV(IDTTMP(M),IDT30(NEWNDT),1)
            CALL UMEMOV(LDTTMP(M),LDT30(NEWNDT),1)
            ENDIF
80    CONTINUE
      NDT30=NEWNDT
      DO 90 J=1,NUM
         IF (IDEL(J).EQ.0) THEN
            NDT30=NDT30+1
            CALL UMEMOV(OPNAME,OPN30(NDT30),2)
            CALL UMEMOV(KWRDS,KEY30(NDT30),1)
            CALL UMEMOV(ITEMP(J),IDT30(NDT30),1)
            CALL UMEMOV(LTEMP(J),LDT30(NDT30),1)
            ENDIF
         ISTAT=3
90    CONTINUE
999   RETURN
      END
