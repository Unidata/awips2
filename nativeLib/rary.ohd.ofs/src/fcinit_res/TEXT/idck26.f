C MEMBER IDCK26
C  (from old member FCIDCK26)
C
C DESC   TO CHECK THE RESTRICTIONS FOR A 'SET' VARIABLE NAME
C        - NO MORE THAN 3 WORDS
C        - FIRST CHARACTER CAN NOT BE NUMERICAL
C        - NAME CAN NOT BE ALL BLANKS
C        - NAME CAN NOT START WITH CHARACTERS --'RULE' OR 'MAXQ'
C        - NAME IS NEITHER STYPE NOR UTYPE
C
C-----------------------------------------------------------------------
C
C   INPUT VARIABLE
C      ID     -  THE IDENTIFIER TO BE CHECKED
C      NWORDS -  THE NUMBER OF WORDS IN ID
C   OUTPUT VALUES
C      0      -  PASS THE ERROR CHECK
C      1      -  ERROR
C
C
C-----------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C-----------------------------------------------------------------------
C
      FUNCTION IDCK26(ID,NWORDS)
      INCLUDE 'common/err26'
C
      DIMENSION SUTYPE(3,19),ID(3),DIGIT(10),UNPKID(12),BADSYM(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/idck26.f,v $
     . $',                                                             '
     .$Id: idck26.f,v 1.1 1995/09/17 18:51:46 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUTYPE/
     .   4HQO  ,4H    ,4H    ,4HQI  ,4H    ,4H    ,4HPOOL,4H    ,4H    ,
     .   4HSTOR,4HAGE ,4H    ,4HDAY ,4H    ,4H    ,4HFLOO,4HD   ,4H    ,
     .   4HSURC,4HHARG,4HE   ,4HFORE,4HCAST,4H    ,4HGOFL,4HASH ,4H    ,
     .   4HNFLO,4HOD  ,4H    ,4HNSUR,4HCHAR,4HGE  ,4HOBSE,4HRVED,4H    ,
     .   4HNGOF,4HLASH,4H    ,4HRISI,4HNG  ,4H    ,4HFALL,4HING ,4H    ,
     .   4HRULE,4H    ,4H    ,4HMAXQ,4H    ,4H    ,4HQOM ,4H    ,4H    ,
     .   4HQIM ,4H    ,4H    /
C
      DATA DIGIT/1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H0/
C
      DATA IBLNK/4H    /,RULE/4hRULE/,MAXQ/4hMAXQ/
C
      DATA BADSYM/1H+,1H-,1H*,1H/,1H.,1H(,1H),1H@/
C
      IST = 0
C
C  A 'SET' VARIABLE NAME CAN'T BE MORE THAN 3 WORDS
C
      IF (NWORDS .LE. 3) GO TO 10
           CALL STER26(75,1)
           IST = 1
           NWORDS = 3
   10      MAXWDS = 4 * NWORDS
C
C  A 'SET' VARIABLE NAME CAN'T BE ALL BLANK
C
      DO 20 I=1,NWORDS
           IF (ID(I) .NE. IBLNK) GO TO 30
   20 CONTINUE
           CALL STER26(76,1)
           IST = 1
           GO TO 200
C
C  UNPACK THE VARIABLE NAME FOR COMPARISION
C
   30 CALL UNPAKS(ID,UNPKID,NWORDS,MAXWDS,IERR)
      IF (IERR .GT. 0) IST = IERR
C
C  FIRST CHAR CAN'T BE NUMERICAL
C
      DO 40 I=1,10
           IF(UNPKID(1) .NE. DIGIT(I)) GO TO 40
                CALL  STER26(77,1)
                IST = 1
                GO TO 200
   40 CONTINUE
C
C  A 'SET' VARIABLE NAME CAN'T START WITH 'RULE' OR 'MAXQ'
C
      IF(ID(1) .NE. RULE) GO TO 50
           CALL STER26(78,1)
           IST = 1
           GO TO 200
C
   50 IF(ID(1) .NE. MAXQ) GO TO 55
           CALL STER26(79,1)
           IST = 1
           GO TO 200
C
C  NO +,-,*,/,.,(,) AND @ ALLOWED IN 'SET' VARIABLE NAME
C
   55 DO 70 I=1,12
           CHAR = UNPKID(I)
           IF (CHAR .EQ. IBLNK) GO TO 75
C
           DO 60 J=1,8
                IF (CHAR .NE. BADSYM(J) ) GO TO 60
                     CALL STER26(80,1)
                     IST=1
                     GO TO 200
   60      CONTINUE
   70 CONTINUE
C
C  A 'SET' VARIABLE NAME IS NEITHER STYPE NOT UTYPE
C
   75 DO 80 I=1,17
           IF(IUSAME(ID,SUTYPE(1,I),3) .EQ. 0) GO TO 80
                CALL STER26(81,1)
                IST=1
                GO TO 200
   80 CONTINUE
C
  200 IDCK26 = IST
C
      RETURN
      END
