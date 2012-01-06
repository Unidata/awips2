C MODULE PWCDC
C-----------------------------------------------------------------------
C
      SUBROUTINE PWCDC (JULDB,JULDE,PDTYPE,MDAYS,ICOND)
C
C   IN: JULDB ...... BEGINNING ORDINAL DAY-OF-CENTURY (STRTS 1900) - INT
C   IN: JULDE ...... ENDING ORDINAL DAY-OF-CENTURY (STARTS 1900) - INT
C   IN: PDTYPE ..... DATA TYPE AS 4-CHAR CODE (SUCH AS 'PP24') - CHAR
C   IN: MDAYS ...... MAXIMUM NUMBER OF DAYS ALLOWED IN RANGE - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP RTN, SET TO 1 FOR ERR - INT
C   IN: LP ........ (FROM COMMON) UNIT NUM FOR OUTPT ERR MESSAGES - INT
C
      CHARACTER*4 PDTYPE,TZCODE
C
      INCLUDE 'uiox'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwcdc.f,v $
     . $',                                                             '
     .$Id: pwcdc.f,v 1.3 2002/02/11 20:51:30 dws Exp $
     . $' /
C    ===================================================================
C
C
      CALL PVSUBB ('PWCDC   ',ICOND)
C
C  CHECK DATES
      NDAYS=JULDE-JULDB+1
      IF (NDAYS.LT.0) THEN
         WRITE (LP,10)
10    FORMAT ('0**ERROR** ENDING DATE IS BEFORE BEGINNING DATE.')
         CALL UEROR (LP,0,-1)
         ICOND=1
	 ENDIF
C
C  CHECK NUMBER OF DAYS OF DATA
      IF (NDAYS.GT.MDAYS) THEN
         WRITE (LP,20) NDAYS,MDAYS,MDAYS
20    FORMAT ('0**WARNING** NUMBER OF DAYS TO BE PROCESSED (',I2,
     *   ') EXCEEDS MAXIMUM (',I2,') AND WILL BE SET TO ',I2,'.')
         CALL UWARN (LP,0,-1)
         IDIFF=NDAYS-MDAYS
         NDAYS=MDAYS
         JULDE=JULDE-IDIFF
	 ENDIF
C
C  GET INDEX FOR DATA TYPE
      IX=IPDCKD(PDTYPE)
      IF (IX.LE.0) THEN
         WRITE (LP,25)
25    FORMAT ('0**ERROR** DATA TYPE ',A,'NOT DEFINED IN ',
     *   'THE PREPROCESSOR DATA BASE.')
         CALL UEROR (LP,0,-1)
         ICOND=1
         GO TO 50
	 ENDIF
C
C  GET FIRST AND LAST DAY OF DATA
      CALL UMEMOV (IDDTDR(8,IX),IDATE,1)
      CALL UMEMOV (IDDTDR(11,IX),LDATE,1)
      CALL UMEMOV (TIME(3),TZCODE,1)
      CALL MDYH2 (IDATE,0,IMO,IDA,IYR,IHR,ITZ,IDSAV,TIME(3))
CCC      IEYR=MOD(IEYR,100)
      CALL MDYH2 (LDATE,0,LMO,LDA,LYR,LHR,ITZ,IDSAV,TIME(3))
CCC      ILYR=MOD(ILYR,100)
      CALL MDYH2 (JULDB,0,JMO,JDA,JYR,JHR,ITZ,IDSAV,TIME(3))
CCC      IEYR=MOD(IEYR,100)
      CALL MDYH2 (JULDE,0,KMO,KDA,KYR,KHR,ITZ,IDSAV,TIME(3))
CCC      ILYR=MOD(ILYR,100)
      IF (JULDB.LT.IDATE) THEN
         WRITE (LP,30) 'BEGINNING',
     *      JMO,JDA,JYR,JHR,TZCODE(1:LENSTR(TZCODE)),
     *      IMO,IDA,IYR,IHR,TZCODE(1:LENSTR(TZCODE))
30    FORMAT ('0**ERROR** ',A,' DATE (',
     *   I2.2,'/',I2.2,'/',I4,'-',I2.2,A,') ',
     *   'IS BEFORE FIRST DAY OF ',
     *   'DATA IN THE PREPROCESSOR DATA BASE ('
     *   I2.2,'/',I2.2,'/',I4,'-',I2.2,A,').')
         CALL UEROR (LP,0,-1)
         ICOND=1
	 ENDIF
      IF (JULDE.LT.IDATE) THEN
         WRITE (LP,30) 'ENDING',
     *      KMO,KDA,KYR,KHR,TZCODE(1:LENSTR(TZCODE)),
     *      LMO,LDA,LYR,LHR,TZCODE(1:LENSTR(TZCODE))
         ICOND=1
	 ENDIF
      IF (JULDB.GT.LDATE) THEN
         WRITE (LP,40) 'BEGINNING',
     *      JMO,JDA,JYR,JHR,TZCODE(1:LENSTR(TZCODE)),
     *      IMO,IDA,IYR,IHR,TZCODE(1:LENSTR(TZCODE))
40    FORMAT ('0**ERROR** ',A,' (',
     *   I2.2,'/',I2.2,'/',I4,'-',I2.2,A,') ',
     *   'DATE IS AFTER LAST DAY OF ',
     *   'DATA IN THE PREPROCESSOR DATA BASE ('
     *   I2.2,'/',I2.2,'/',I4,'-',I2.2,A,').')
         CALL UEROR (LP,0,-1)
         ICOND=1
	 ENDIF
      IF (JULDE.GT.LDATE) THEN
         WRITE (LP,40) 'ENDING',
     *      KMO,KDA,KYR,KHR,TZCODE(1:LENSTR(TZCODE)),
     *      LMO,LDA,LYR,LHR,TZCODE(1:LENSTR(TZCODE))
         ICOND=1
	 ENDIF
C
50    CALL PVSUBE('PWCDC   ',ICOND)
C
      RETURN
C
      END
