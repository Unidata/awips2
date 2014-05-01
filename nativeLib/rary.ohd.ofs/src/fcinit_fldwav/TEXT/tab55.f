C MODULE TAB55
C-----------------------------------------------------------------------
C
C THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR FLDWAV
C
      SUBROUTINE TAB55(T,LEFT,IUSET,NXT,LP,P,LC,TS,MTS,KD,NWORK,NDD,
     .                 LWORK,IDT,MP)

      INCLUDE 'updaio'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'

      INTEGER T(*)
      DIMENSION P(*),TS(MTS)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/tab55.f,v $
     . $',                                                             '
     .$Id: tab55.f,v 1.6 2004/02/02 20:43:06 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/'TAB55   '/

      CALL FPRBUG(SNAME, 1, 55, IBUG)

C   PASSED ARGUMENTS
C    NAME  I/O
C   ------ ---
C   T       O  INT  ARRAY USED TO STORE OPERATIONS TABLE ENTRIES
C   LEFT    I  INT  INDICATING HOW MUCH SPACE IS LEFT IN T()
C   IUSET   O  INT  HOW MUCH SPACE IN THE T() IS USED BY THIS OPER
C   NXT     I  INT  STARTING LOCATION OF THE TO() FOR THIS OPERATION
C   LP      I  INT  STARTING LOCATION OF THE P() FOR THIS OPERATION
C   P       I  REAL ARRAY TO STORE ALL PARAMETERS FOR THIS OPERATION
C   LC      I  INT  STARTING LOCATION OF THE C() FOR THIS OPERATION
C   TS      I  REAL ARRAY OF INFORMATION ON ALL TIME SERIES IN SEGMENT
C   MTS     I  INT  MAXIMUM DIMENSION OF THE TS()
C   KD      I  INT  D/S BOUNDARY SWITCH (KD=0 ==> TIDE BOUNDARY)
C   LWORK   O  INT  HOW MUCH WORKING SPACE IS NEEDED FOR THIS OPERATION
C   IDT     O  INT  DELTA T, MINIMUM TIME INTERVAL THIS OPER CAN BE RUN
C
C
C   LOCAL VARIABLES
C    NAME
C   ------
C   IBUG    DEBUG SWITCH
C
C.......................................................................
C  COMPUTE NUMBER OF OBSERVED  (NUO)  AND COMPUTED (NUC) VALUES NEEDED
C  FOR DUMMY SPACE PASSED TO EX55
C    JN     --  NUM. OF RIVERS
C.......................................................................


      CALL FPRBUG(SNAME, 1, 55, IBUG)
      LBUG=0

      JN=P(17)

      DTHYD=P(15)
      NDHF=DTHYD
      NUO=NDD*24/NDHF

      DTOUT=P(23)
      NDHFO=DTOUT
      NUC=NDD*24/NDHFO

      DHFC=P(52)
      NUT=NDD*24/DHFC+0.5

      DTI=P(133)
      NUP=2*NDD*24/DTI
C.......................................................................
C  EXTRACT TOTAL NUMBER OF VARIOUS TYPES OF TIME SERIES
C    NFGRF  --  PARAMETER INDICATING USE OF FLDGRF (=0)
C    NTQL   --  TOTAL NUM. OF LATERAL FLOWS
C    NLOCK  --  TOTAL NUM. OF LOCK/DAM
C    NTGAG  --  TOTAL NUM. OF GAGE STATIONS FOR PLOTTING
C    NTOUT  --  TOTAL NUM. OF OUTPUT T.S.
C.......................................................................

      NFGRF=P(35)
      NTQL =P(313)
      NLOCK=P(321)
      NTGAG=P(323)
      NTOUT=P(331)

C.......................................................................
C    ONE TIME SERIES EACH OF WORK SPACE NEEDED FOR (a)
C          PLTIM(NUP)
C          FFS(NUO)
C          FS(NUT)
C          QA(NUT)
C          YA(NUT)
C          TII(NUT)
C          TO(NUO)
C          IRF(NUT)
C.......................................................................
C    ONE TIME SERIES PER RIVER EACH OF WORK SPACE NEEDED FOR  (b)
C    (ACTUALLY NEED SPACE FOR JN-1 TIME SERIES)
C          QLJ(NUO,JN)
C          QUSJ(NUO,JN)
C          T1(NUO,JN)
C.......................................................................
C    WORK SPACE NEEDED (c)
C    ?     QLT(NUO,NTQL)
C          QTC,(NUT,NTGAG)
C          STC(NUT,NTGAG)
C          POOLT(NUC,NLOCK)
C          ITWT(NUC,NLOCK)
C.......................................................................
C    'FILLED' PORTIONS OF THE D ARRAY PROVIDED FOR  (d)
C          STN(NUO)
C          ST1(NUO,JN)
C          ITWTS(NUO,NLOCK)
C          POOLTS(NUO,NLOCK)
C          QL(NUO,NTQL)
C          STT(NUO,NTGAG)
C          STQ(NUO,NTGAG)
C          QSTR(NUO,NTOUT)
C          XNOS(NUT)
C          TIDE(NUT)
C          STE(NUO,NTGAG)
C.......................................................................
C    THE LOCATION OF THE BEGINNING OF ARRAY NB IS IN LOCATION P(68)

      LONB=P(68)
      LONB=P(126)
C.......................................................................
C    COMPUTE TOTAL  SPACE NEEDED IN T ARRAY

C      NEED FIVE LOCATIONS PLUS
C      16 EXTRA LOCATIONS FOR FUTURE USE PLUS
C      ONE FOR START OF NB ARRAY PLUS
C      ONE FOR EACH TIME SERIES USED
C---------------
C      NOW ADD TEN LOCATIONS
C
      NEEDT=22
C---------------
C       NOW ADD LOCATION FOR ARRAYS USING WORK SPACE

C                     (c)       (c)    (a)  (b)      (c)
      NEEDT=NEEDT  + 2*NLOCK + 0*NTQL + 8 + 3*JN + 2*NTGAG

      IF(NLOCK.EQ.0)  NEEDT=NEEDT+2
      IF(NTGAG.EQ.0)  NEEDT=NEEDT+2
C---------------
C       NOW ADD LOCATIONS FOR 'FILLED' ARRAYS (d)

      KPL=ABS(P(32))

      NEEDT=NEEDT + 3 + JN + 2*NLOCK + 1*NTQL + NTOUT

      IOBS=P(36)
      IF(IOBS.EQ.0.AND.KPL.EQ.0) GO TO 50
c  add stt
      NEEDT=NEEDT+NTGAG
c  add stq
      IF(KPL.EQ.3) NEEDT=NEEDT+NTGAG
c  add ste
      IF(IOBS.GT.1) NEEDT=NEEDT+NTGAG
c  no locks - add 2 spaces for poolts & itwts
 50   IF(NLOCK.EQ.0) NEEDT=NEEDT+2
c  no lateral flows - add 1 space for ql
      IF(NTQL.EQ.0)  NEEDT=NEEDT+1
c  no observed data - add 3 spaces for stt,stq,ste
      IF(NTGAG.EQ.0.OR.(IOBS.EQ.0.AND.KPL.EQ.0))  NEEDT=NEEDT+3
c  only 1 set of observed data - add 1 space for stq
      IF(NTGAG.GT.0.AND.KPL.NE.3)  NEEDT=NEEDT+1
c  no blending - add 1 space for ste
      IF(IOBS.EQ.1) NEEDT=NEEDT+1
c  no output t.s. - add 1 space for qstr
      IF(NTOUT.EQ.0) NEEDT=NEEDT+1



C---------------
C       NOW ADD ONE SPACE FOR LOCATION OF NB ARRAY IN P ARRAY

cc      NEEDT=NEEDT + 1

C       NOW ADD ONE SPACE FOR LOCATION OF RATING CURVE IDS IN P ARRAY

      NEEDT=NEEDT + 1

C---------------
C       NOW ADD EIGHTEEN SPACES FOR POINTERS IN T ARRAY TO OTHER
C       LOCATION IN THE T ARRAY WHICH ARE THE START OF THE SETS OF
C       LOCATIONS IN THE D ARRAY FOR THE FOLLOWING TIME SERIES:
C            T1
C            QLJ
C            QUSJ
C  ?         QLT
C            QTC
C            STC
C            POOLT
C            ITWT
C            STN
C            ST1
C            ITWTS
C            POOLTS
C            QL
C            STT
C            STQ
C            QSTR
C            XNOS
C            TIDE
C            STE
C
      NEEDT=NEEDT + 18

C        NOW ADD ONE SPACE TO STORE FIRST LOCATION IN D ARRAY OF WORKING
C        SPACE AFTER ALL FLDWAV TIME SERIES HAVE BEEN ALLOCATED

      NEEDT=NEEDT + 1

C---------------
      IF(IBUG.EQ.1) WRITE(IODBUG,100) JN,DTHYD,NDHF,NUO,DTOUT,NDHFO,NUC,
     .   NUT,NUP,NLOCK,NTGAG,NTQL,NTOUT,LONB,NEEDT,LEFT,NXT,KD
  100 FORMAT(11X,'*** IN SUBROUTINE TAB55 ***'/
     .11X,'     JN  DTHYD   NDHF    NUO  DTOUT  NDHFO    NUC    NUT    N
     .UP='/14X,I4,F7.1,2I7,F7.1,I7,3I7/
     . 11X,'  NLOCK  NTGAG   NTQL  NTOUT   LONB  NEEDT   LEFT    NXT     
     . KD='/11X,10I7)

C---------------
C       CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T ARRAY

      CALL CHECKT(NEEDT,LEFT,IERR)

C---------------
C       NOT ENOUGH SPACE IN T ARRAY - SET IUSET TO ZERO AND RETURN

      IF(IERR.NE.0) THEN
        IUSET=0
        LWORK=0
        GO TO 1000
      ENDIF

C.......................................................................
C        ENOUGH SPACE - BEGIN FILL T ARRAY

C---------------
C       ******  CONTENTS OF T ARRAY FOR FLDWAV OPERATION  ******

C   THE TO ARRAY ENTRIES ARE AS FOLLOWS:
C   T(5)  - T(16)        STARTING POINT IN T ARRAY OF LOCATORS
C   T(17) - T(IUSET)     T.S. LOCATORS IN D ARRAY
C   ---
C  POSITION   CONTENTS
C  --------   --------
C      1      OPERATION NUMBER (55)
C      2      LOCATION IN T ARRAY OF NEXT OPERATION
C      3      LOCATION IN P ARRAY OF PARAMETERS FOR THIS OPERATION (LP)
C      4      LOCATION IN C ARRAY OF CARRYOVER FOR THIS OPERATION (LC)
C      5      LOCATION IN P ARRAY OF 1ST RATING CURVE ID (0 IF NONE)
C             --NOTE-- THE ABOVE THREE LOCATIONS ARE RELATIVE TO
C                      THE BEGINNING OF THE ENTIRE P AND C ARRAYS
C      6      LOCATION IN P ARRAY OF ARRAY NB FOR THIS OPERATION
C             --NOTE-- THE LOCATION IS RELATIVE TO START OF THE
C                      'SECOND PORTION OF THE P ARRAY' FOR THIS
C                      OPERATION
C      7      POINTER IN T FOR LOCATION IN D OF POOLT (WORK SPACE)
C      8      POINTER IN T FOR LOCATION IN D OF ITWT (WORK SPACE)
C      9      LOCATION IN D ARRAY OF FLDWAV ARRAY PLTIM (WORK SPACE)
C     10      LOCATION IN D ARRAY OF FLDWAV ARRAY FFS (WORK SPACE)
C     11      LOCATION IN D ARRAY OF FLDWAV ARRAY FS (WORK SPACE)
C     12      LOCATION IN D ARRAY OF FLDWAV ARRAY QA (WORK SPACE)
C     13      LOCATION IN D ARRAY OF FLDWAV ARRAY YA (WORK SPACE)
C     14      LOCATION IN D ARRAY OF FLDWAV ARRAY TII (WORK SPACE)
C     15      LOCATION IN D ARRAY OF FLDWAV ARRAY T1 (WORK SPACE)
C     16      LOCATION IN D ARRAY OF FLDWAV ARRAY TO (WORK SPACE)
C
C     17      POINTER TO LOCATION IN **** ENTIRE T ARRAY **** FOR
C               LOCATION IN D ARRAY OF FLDWAV ARRAY QLJ (WORK SPACE)
C     18      POINTER IN T FOR LOCATION IN D OF QUSJ (WORK SPACE)
C     19      POINTER IN T FOR LOCATION IN D OF QLT (WORK SPACE) <not needed>
C     20      POINTER IN T FOR LOCATION IN D OF QTC (WORK SPACE)
C     21      POINTER IN T FOR LOCATION IN D OF STC (WORK SPACE)
C     22      POINTER IN T FOR LOCATION IN D OF STN (INPUT TS)
C              (only stn on main stem is stored... in p array space
C               stored for JN rivers in prepearation of new option)
C     23      POINTER IN T FOR LOCATION IN D OF ST1 (INPUT TS)
C     24      POINTER IN T FOR LOCATION IN D OF ITWTS (INPUT TS)
C     25      POINTER IN T FOR LOCATION IN D OF POOLTS (INPUT TS)
C     26      POINTER IN T FOR LOCATION IN D OF QL (INPUT TS)
C     27      POINTER IN T FOR LOCATION IN D OF STT (INPUT TS)
C     28      POINTER IN T FOR LOCATION IN D OF STQ (INPUT TS)
C     29      POINTER IN T FOR LOCATION IN D OF QSTR (OUTPUT TS)
C     30      TOTAL NUMBER OF RATING CURVES IN THE OPERATION
C     31      POINTER IN T FOR LOCATION IN D OF XNOS (INPUT TS)
C     32      POINTER IN T FOR LOCATION IN D OF TIDE (OUTPUT TS)
C     33      POINTER IN T FOR LOCATION IN D OF STE (OUTPUT TS)
C     34      POINTER IN T FOR LOCATION IN D OF IRF (WORK SPACE)
C  35-49      FUTURE USE
C     50      LOCATION OF START OF WORKING SPACE IN D ARRAY AFTER
C                 ALL FLDWAV TIME SERIES HAVE BEEN ALLOCATED
C  51-END     LOCATIONS IN D FOR VARIOUS FLDWAV ARRAYS
C             -- NOTE -- THERE MAY BE MORE THAN ONE TIME SERIES
C                 FOR MOST ARRAYS
C.......................................................................
      T(1)=55
      T(2)=NXT+NEEDT
      T(3)=LP
      T(4)=LC
      LRC=P(366)
      T(5)=0
      IF(LRC.GT.0) T(5)=LP+LRC-1
      T(30)=P(27)
C
C---------------
ccC     NOW STORE LOCATION OF START OF NB ARRAY WHICH IS IN P(68),
C     NOW STORE LOCATION OF START OF NB ARRAY WHICH IS IN P(126),
C     EX55 EXPECTS TO BE PASSED THIS ARRAY SEPARATELY FROM REST OF
C     P ARRAY TO GIVE IT AN INTEGER NAME.
C
      T(6)=LONB
C
C     FILL SPACE RESERVED FOR FUTURE USE WITH ZEROES
C
C jgg following change made as per jls to fix hsd bug r23-48 12/03
C jgg      DO 110 I=31,49
      DO 110 I=35,49
C jgg      
        T(I)=0
  110 CONTINUE
C.......................................................................
C         SET START OF WORKING SPACE
C         START FILLING LOCATIONS OF TIME SERIES
C         VARIABLE LOCDUM IS START OF AVAILABLE WORK SPACE IN D ARRAY
C         WILL BE UPDATED AS EACH TIME SERIES IS ENTERED
C
      LOCDUM=NWORK
C
C         COMPUTE NEEDED WORK SPACE AS DETERMINE WHICH ARRAYS ARE USED
C
      NEEDW=0
C
C---------------
C         ARRAY PLTIM
cc      IF(NFGRF.EQ.1) THEN
cc        T(9)=0
cc      ELSE
        T(9)=LOCDUM
        LOCDUM=LOCDUM+NUP+1
        NEEDW=NEEDW+NUP+1
cc      ENDIF
C---------------
C         ARRAY FFS
      T(10)=LOCDUM
      LOCDUM=LOCDUM+NUO+1
      NEEDW=NEEDW+NUO+1
C---------------
C         ARRAY FS
      T(11)=LOCDUM
      LOCDUM=LOCDUM+NUT+1
      NEEDW=NEEDW+NUT+1
C---------------
C         ARRAY QA
      T(12)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
C---------------
C         ARRAY YA
      T(13)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
C---------------
C         ARRAY TII
      T(14)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
C---------------
ccC         ARRAY T1
cc      T(15)=LOCDUM
cc      LOCDUM=LOCDUM+NUO
cc      NEEDW=NEEDW+NUO
C---------------
C         ARRAY TO
      T(16)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
C---------------
C         ARRAY IRF
      T(34)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
C---------------

C
C.......................................................................
C     ******** SET FIRST VARIABLE LOCATION IN T ARRAY *********
C
      LOCT=51
C
C---------------
C
C         ARRAY POOLT - NEED NUMLAD BY NUO LOCATIONS IN WORK SPACE
C-------- POINTER STORED IN T(7) ---------------------------------------
      T(7)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NLOCK.EQ.0) GO TO 120
      LOCT=LOCT-1
      DO 115 I=1,NLOCK
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
      LOCT=LOCT+1
  115 CONTINUE
C         ARRAY ITWT - NEED NLOCK BY NUO LOCATIONS IN WORK SPACE
C-------- POINTER STORED IN T(8) ---------------------------------------
  120 T(8)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NLOCK.EQ.0) GO TO 126
      LOCT=LOCT-1
      DO 125 I=1,NLOCK
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
      LOCT=LOCT+1
  125 CONTINUE
C.......................................................................
C         ARRAY T1 - NEED JN BY NUO LOCATIONS IN WORK SPACE
C-------- POINTER STORED IN T(15) --------------------------------------
  126 T(15)=NXT+LOCT-1
cc      T(LOCT)=0
cc      LOCT=LOCT+1
      DO 128 J=1,JN
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
      LOCT=LOCT+1
  128 CONTINUE
C.......................................................................
C         ARRAY QLJ - NEED JN-1 BY NUO LOCATIONS IN WORK SPACE
C         SET FIRST T ARRAY LOCATION (FOR J=1) TO ZERO
C             IT IS NEVER USED
C-------- POINTER STORED IN T(17) --------------------------------------
  130 T(17)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(JN.EQ.1) GO TO 140
      DO 135 J=2,JN
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
      LOCT=LOCT+1
  135 CONTINUE
C.......................................................................
C         ARRAY QUSJ - NEED JN BY NUO LOCATIONS IN WORK SPACE
C-------- POINTER STORED IN T(18) --------------------------------------
  140 T(18)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(JN.EQ.1) GO TO 150
      LOCT=LOCT-1
      DO 145 J=1,JN
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUO
      NEEDW=NEEDW+NUO
      LOCT=LOCT+1
  145 CONTINUE
C.......................................................................
C         ARRAY QLT - NEED NTQL BY NUO LOCATIONS IN WORK SPACE
C-------- POINTER STORED IN T(19) --------------------------------------
  150 T(19)=0
cc  150 T(19)=NXT+LOCT-1
cc      T(LOCT)=0
cc      LOCT=LOCT+1
cc      IF(NTQL.EQ.0) GO TO 160
cc      LOCT=LOCT-1
cc      DO 155 I=1,NTQL
cc      T(LOCT)=LOCDUM
cc      LOCDUM=LOCDUM+NUO
cc      NEEDW=NEEDW+NUO
cc      LOCT=LOCT+1
cc  155 CONTINUE
C.......................................................................
C         ARRAYS QTC, AND STC - NEED NTGAG BY NUT LOCATIONS IN WORK SPACE
C-------- POINTERS STORED IN T(20) AND T(21) RESPECTIVELY -----------
  160 T(20)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NTGAG.EQ.0) GO TO 170
      LOCT=LOCT-1
      DO 165 I=1,NTGAG
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
      LOCT=LOCT+1
  165 CONTINUE

C---------------
  170 T(21)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NTGAG.EQ.0) GO TO 180
      LOCT=LOCT-1
      DO 175 I=1,NTGAG
      T(LOCT)=LOCDUM
      LOCDUM=LOCDUM+NUT
      NEEDW=NEEDW+NUT
      LOCT=LOCT+1
  175 CONTINUE

C---------------
C.......................................................................
C
C         NOW OBTAIN LOCATIONS IN 'FILLED' PORTION OF D ARRAY FOR REST
C             OF TIME SERIES --
C         IF THE LOCATION OF ID FOR ANY TIME SERIES IS ZERO IT IS NOT
C             USED
C
C---------------
C         ARRAY STN - ID IN P(P(362)), DATA TYPE IN P(365), DT IS NDHF
C-------- POINTER IN T(22) ---------------------------------------------
  180 T(22)=NXT+LOCT-1
      LID=P(362)
cc      LTY=P(365)
      IF(LID.GT.0) GO TO 185
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 190
  185 CALL CKINPT(P(LID),P(LID+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
C---------------
C         ARRAYS ST1 - IDS START IN P(P(361)), DATA TYPE IN P(362),
C                      DT IS NDHF
C-------- POINTER IN T(23) ---------------------------------------------
  190 T(23)=NXT+LOCT-1
      LID=P(361)
cc      LTY=P(362)
      IF(LID.GT.0) GO TO 195
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 200
  195 IDADD=0
cc      ITYADD=0
      DO 197 J=1,JN
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+3
  197 CONTINUE
C---------------
C         ARRAYS ITWTS - IDS START IN P(P(368)), DATA TYPES IN P(370),
C                      DT IS NDHF
C-------- POINTER IN T(24) ---------------------------------------------
  200 T(24)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NLOCK.EQ.0) GO TO 210
      LOCT=LOCT-1

      LID=P(368)
      IF(LID.GT.0) GO TO 205
cc      T(LOCT)=0
cc      LOCT=LOCT+1
cc      GO TO 210
  205 IDADD=0
cc      ITYADD=0
      DO 207 I=1,NLOCK
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+3
  207 CONTINUE
C---------------
C         ARRAYS POOLTS -  IDS START IN P(P(367)), DATA TYPES IN P(368),
C           NDHF
C-------- POINTER IN T(25) ---------------------------------------------
  210 T(25)=NXT+LOCT-1
      T(LOCT)=0
      LOCT=LOCT+1
      IF(NLOCK.EQ.0) GO TO 220
      LOCT=LOCT-1
      LID=P(367)
cc      LTY=P(368)
      IF(LID.GT.0) GO TO 215
cc      T(LOCT)=0
cc      LOCT=LOCT+1
cc      GO TO 220
  215 IDADD=0
cc      ITYADD=0
      DO 217 I=1,NLOCK
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+3
  217 CONTINUE
C---------------
C         ARRAYS QL - IDS START IN P(P(350)), TYPES IN P(351), NDHF
C-------- POINTER IN T(26) ---------------------------------------------
  220 T(26)=NXT+LOCT-1
      LID=P(350)
cc      LTY=P(351)
      IF(LID.GT.0.AND.NTQL.GT.0) GO TO 225
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 230
  225 IDADD=0
cc      ITYADD=0
      ICKVAL=MP
      IF (LID.GT.ICKVAL) THEN
         WRITE (IPR,998) 'LID',LID,ICKVAL
998   FORMAT ('0**ERROR** IN TAB55 - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS GREATER THAN ',I6,'.')
         CALL ERROR
         GO TO 230
         ENDIF
      DO 227 I=1,NTQL
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+3
  227 CONTINUE
C---------------
C         ARRAYS STT - ID IN P(P(352)), TYPE IN P(353), NDHF
C-------- POINTER IN T(27) ---------------------------------------------
  230 T(27)=NXT+LOCT-1
      LID=P(352)
cc      LTY=P(353)
      IF(LID.GT.0.AND.IOBS.NE.0.AND.KPL.NE.0.AND.NTGAG.GT.0) GO TO 235
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 237
  235 IDADD=0
cc      ITYADD=0
      DO 236 I=1,NTGAG
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+5
  236 CONTINUE
C---------------
C         ARRAYS STQ - ID IN P(P(354)), TYPE IN P(355), NDHF
C-------- POINTER IN T(28) ---------------------------------------------
  237 T(28)=NXT+LOCT-1
      LID=P(354)
cc      LTY=P(355)
      IF(LID.GT.0.AND.IOBS.NE.0.AND.KPL.EQ.3.AND.NTGAG.GT.0) GO TO 238
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 240
  238 IDADD=0
cc      ITYADD=0
      DO 239 I=1,NTGAG
      CALL CKINPT(P(LID+IDADD),P(LID+IDADD+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
      IDADD=IDADD+3
  239 CONTINUE
C---------------
C         ARRAY QSTR - ID IN P(P(356)), DATA TYPE IN P(P(356)), NDHFO
C-------- POINTER IN T(29) ---------------------------------------------
  240 T(29)=NXT+LOCT-1
      LID=P(356)
cc      LTY=P(357)
C jgg following change made as per jls to fix hsd bug r23-48 12/03
C jgg      IF(LID.GT.0) GO TO 245
      IF(LID.GT.0.AND.NTOUT.GT.0) GO TO 245
C jgg      
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 250
  245 ICKVAL=MP
      IF (LID.GT.ICKVAL) THEN
         WRITE (IPR,998) 'LID',LID,ICKVAL
         CALL ERROR
         GO TO 250
         ENDIF
      IDADD=0
cc      ITYADD=0
      IF (LBUG.EQ.1) WRITE (UE,*) 'IN TAB55 - NTOUT=',NTOUT 
      DO 247 I=1,NTOUT
      CALL FINDTS (P(LID+IDADD),P(LID+IDADD+2),NDHFO,T(LOCT),LOCTS,DIMS)
      LOCT=LOCT+1
      IDADD=IDADD+3
C
C     SET VALUE IN TS ARRAY INDICATING THAT AN OUTPUT TS IS FILLED
C
      IF(LOCTS.GT.0) TS(LOCTS+8)=1.01
  247 CONTINUE
C---------------
C         ARRAY STE - ID IN P(P(353)), DATA TYPE IN P(P(353)+2), NDHFO
C-------- POINTER IN T(33) ---------------------------------------------
  250 T(33)=NXT+LOCT-1
      LID=P(353)
      IF(LID.GT.0.AND.IOBS.GT.1.AND.KPL.NE.0.AND.NTGAG.GT.0) GO TO 255
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 260
  255 IDADD=0
cc      ITYADD=0
      DO 257 I=1,NTGAG
      CALL FINDTS(P(LID+IDADD),P(LID+IDADD+2),NDHFO,T(LOCT),LOCTS,DIMS)
      LOCT=LOCT+1
      IDADD=IDADD+3
C
C     SET VALUE IN TS ARRAY INDICATING THAT AN OUTPUT TS IS FILLED
C
      IF(LOCTS.GT.0) TS(LOCTS+8)=1.01
  257 CONTINUE

C---------------
C         ARRAY XNOS - ID IN P(P(348)), DATA TYPE IN P(P(348)+2)), DT IS NDHF
C-------- POINTER IN T(31) ---------------------------------------------
  260 T(31)=NXT+LOCT-1
      LID=P(348)
      IF(LID.GT.0.AND.KD.EQ.0) GO TO 265
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 270
  265 CALL CKINPT(P(LID),P(LID+2),NDHF,T(LOCT),TS,MTS,IER)
      LOCT=LOCT+1
C---------------
C         ARRAY TIDE - ID IN P(P(351)), DATA TYPE IN P(P(351)+2), NDHF
C-------- POINTER IN T(32) ---------------------------------------------
  270 T(32)=NXT+LOCT-1
      LID=P(351)
      IF(LID.GT.0.AND.KD.EQ.0) GO TO 275
      T(LOCT)=0
      LOCT=LOCT+1
      GO TO 280
  275 CALL FINDTS(P(LID),P(LID+2),NDHF,T(LOCT),LOCTS,DIMS)
      LOCT=LOCT+1
C
C     SET VALUE IN TS ARRAY INDICATING THAT AN OUTPUT TS IS FILLED
C
      IF(LOCTS.GT.0) TS(LOCTS+8)=1.01

C.......................................................................
C
C     THIS COMPLETES ASSIGNMENT OF LOCATIONS FOR ALL TIME SERIES
C     NOW SET FIRST LOCATION AVAILABLE IN D ARRAY AFTER ALL FLDWAV
C         TIME SERIES HAVE BEEN ALLOCATED
C
C---------------
C
  280 T(50)=LOCDUM
C
      LOCT=LOCT-1
      IDUMPO=P(3)
C
C     CHECK NEEDT VS LOCT TO SEE IF SPACE USED IN T ARRAY
C     MATCHES WITH SPACE COMPUTED AS NEEDED AT BEGINNING OF SUBROUTINE
C
      IF(NEEDT.EQ.LOCT) GO TO 300
C
      WRITE(IPR,600) NEEDT,LOCT
  600 FORMAT ('0**ERROR** IN TAB55 - SPACE COMPUTED AS NEEDED IN THE ',
     1 'T ARRAY (',I5,') DIFFERS FROM SPACE ACTUALLY FILLED  (',I5,')')
      CALL ERROR
      IUSET=0
      LWORK=0
      GO TO 320
C
C---------------
C     SET VARIABLE FOR AMOUNT OF WORKING SPACE NEEDED FOR EX55
C     THIS INCLUDES WORKING SPACE FOR TIME SERIES (NEEDW) AND
C     WORKING SPACE TO HOLD PARAMETERS DURING FLDWAV EXECUTION (IDUMPO)
C
  300 LWORK=NEEDW + IDUMPO
C
C     SET VARIABLE FOR AMOUNT OF SPACE USED IN T ARRAY
C
      IUSET=LOCT
C
C     SET VARIABLE FOR MINIMUM TIME INTERVAL FOR THIS OPERATION
C
  320 IDT=NDHF
C
C     CHECK FOR DEBUG
C
      IF(IBUG.EQ.0) GO TO 1000
C
      WRITE(IODBUG,900)IDT,NWORK,IDUMPO,LWORK,IUSET,(T(I),I=1,LOCT)
  900 FORMAT(/' IN SUBROUTINE TAB55 - FLDWAV DEBUG -- ',
     1  'COMPUTATIONAL DT = ',I3/'  NWORK = ',I6,', IDUMPO = ',I6/
     2  '  WORKING SPACE USED = ',I5,', SPACE NEEDED IN T ARRAY = ',I5//
     3  '  CONTENTS OF THE T ARRAY IS'/20(1X,20I6/))
C

 1000 RETURN
      END

