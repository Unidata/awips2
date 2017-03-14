C MEMBER PDDTYP
C  (from old member PDBDDTYP)
C***********************************************************************
C                                                                      *
C         MEMBER PDBDDTYP                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDDTYP(IDLTYP,IANS,MAXSTA,MAXDAY,IFLAG,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDDTYP                                         *
C                                                                      *
C             VERSION: 1.0.1 JF ADDED DATES FOR NEW TYPE DEFINED ON
C                               PPDB.  3-15-85
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  7-26-83                                        *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE IS CALLED BY PDEFTP DRIVER WHEN THE DEFTYPE       *
C    COMMAND IS INPUT.  IT DEFINES DAILY DATA TYPES ON THE PPDB.       *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       IDLTYP     A4    I     1     DAILY DATA TYPE                   *
C       IANS       I     I     1     FILE # OF DAILY FILE              *
C       MAXSTA     I     I     1     MAX # OF STATIONS FOR TYPE        *
C       MAXDAY     I     I     1     MAX # OF DAYS FOR TYPE            *
C       IFLAG      I     I/0    1    REWRITE CONTROL FLAG
C                                      0 = NO RE-WRITE
C                                      1 = RE-WRITE
C       ISTAT      I     O     1     STATUS INDICATOR                  *
C                                      0 = NORMAL RETURN               *
C                                      OTHER = ERROR                   *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pddtyp.f,v $
     . $',                                                             '
     .$Id: pddtyp.f,v 1.1 1995/09/17 19:09:20 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA ITF24/4hTF24/
C                                                                      *
C***********************************************************************
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,100)
  100 FORMAT(' ENTER PDDTYP')
C
C  SET PROGRAM VARIABLES
C
      MXDP = 0
      MXDT = 0
      IFLP = 0
      IFLT = 0
      NWORDP = 0
      NWORDT = 0
      IERFLG = 0
      LRCP = LRCPDD * 2
C
C  GET INDEX OF TYPE
C
      IX = IPDCKD(IDLTYP)
      IF(IX.NE.0) GO TO 150
      WRITE(LPE,125) IDLTYP
  125 FORMAT(' ***ERROR*** INVALID DATA TYPE ',A4)
      IERFLG = 1
      GO TO 990
  150 CONTINUE
C
C  STORE DATA INTO DIRECTORY RECORD
C
      IDDTDR(4,IX) = IANS
      IDDTDR(7,IX) = MAXDAY
      IDDTDR(16,IX) = MAXSTA
  175 CONTINUE
C
C  CALCULATE POINTERS FOR NEW DATA
C
      IF(IDLTYP.NE.ITF24) GO TO 200
C
C  CALCULATE POINTERS FOR FUTURE DATA
C
      NWPTFT = IDDTDR(5,IX) * IDDTDR(16,IX)
      NWDFT = (IDDTDR(7,IX) * 4) + 2
      NPTREC = IUNRCD(NWPTFT,LRCP) + IUNRCD(NWDFT,LRCP)
      GO TO 250
  200 CONTINUE
C
C  SEE IF DAILY TYPE IS PRECIP OR TEMP
C
      IF(IDDTDR(6,IX).NE.-1) GO TO 225
      CALL PDSPTP(LDTYPE,MAXSTA,IX,MXDP,MXDT,IFLP,IFLT,NWORDP,NWORDT,
     1            IERFLG)
      IF(IERFLG.NE.0) GO TO 990
      GO TO 275
  225 CONTINUE
C
C  CALCULATE # OF POINTER RECORDS FOR THIS TYPE
C
      NWPTRC = IDDTDR(16,IX) * IDDTDR(5,IX)
      NPTREC = IUNRCD(NWPTRC,LRCP)
  250 CONTINUE
C
C  CALCULATE # OF DATA RECORDS FOR THIS TYPE
C
      NWDDRC = IDDTDR(16,IX) * IDDTDR(6,IX)
      NDLYRC = IUNRCD(NWDDRC,LRCP)
C
C  CALCULATE TOTAL DATA RECORDS FOR THIS TYPE
C
      NTDDRC = NDLYRC * IDDTDR(7,IX)
      IDDTDR(14,IX) = IPDDFC(2,IANS) + 1
      IDDTDR(15,IX) = IDDTDR(14,IX) + NPTREC
      IDDTDR(21,IX) = NDLYRC
C
C  UPDATE LAST USED RECORD IN FILE
C
      IPDDFC(2,IANS) = IDDTDR(15,IX) + NTDDRC - 1
C
C  SET POINTERS FOR EARLIEST AND LATEST RECORD NUMBERS
C
      IDDTDR(10,IX) = IDDTDR(15,IX)
      IDDTDR(13,IX) = (IPDDFC(2,IANS) + 1) - IDDTDR(21,IX)
      IF(IPDDFC(2,IANS).LT.IPDDFC(1,IANS)) GO TO 400
      WRITE(LPE,260) KPDDDF(IANS)
  260 FORMAT(' ***ERROR*** EXCEEDED MAXIMUM # OF RECORDS ALLOCATED ',
     1       'IN DAILY DATA FILE ',I4)
      GO TO 325
  275 CONTINUE
C
C  CALCULATE # OF TOTAL RECORDS FOR PRECIP TYPE
C
      IF(MXDP.EQ.0) GO TO 300
      NRECP = IUNRCD(NWORDP,LRCP)
C
C  CALCULATE POINTER RECORDS FOR PRECIP TYPE
C
      NWPTRC = IDDTDR(16,IX) * IDDTDR(5,IX)
      NPTREC = IUNRCD(NWPTRC,LRCP)
      IDDTDR(21,IX) = NRECP
      NTDDRC= NRECP * IDDTDR(7,IX)
      IDDTDR(14,IX) = IPDDFC(2,IANS) + 1
      IDDTDR(15,IX) = IDDTDR(14,IX) + NPTREC
C
C  UPDATE LAST USED RECORD IN FILE
C
      IPDDFC(2,IANS) = IDDTDR(15,IX) + NTDDRC - 1
C
C  SET POINTERS FOR EARLIEST AND LATEST RECORD NUMBERS
C
      IDDTDR(10,IX) = IDDTDR(15,IX)
      IDDTDR(13,IX) = (IPDDFC(2,IANS) + 1) - IDDTDR(21,IX)
      IF(IPDDFC(2,IANS).LT.IPDDFC(1,IANS)) GO TO 300
      WRITE(LPE,260) KPDDDF(IANS)
      GO TO 325
  300 IF(MXDT.EQ.0) GO TO 400
      NRECT = IUNRCD(NWORDT,LRCP)
C
C  CALCULATE POINTER RECORDS FOR TEMP TYPE
C
      NWPTRC = IDDTDR(16,IX) * IDDTDR(5,IX)
      NPTREC = IUNRCD(NWPTRC,LRCP)
      IDDTDR(21,IX) = NRECT
C
C  CALCULATE # OF TOTAL DATA RECORDS FOR DAILY TEMP TYPE
C
      NTDDRC = NRECT * IDDTDR(7,IX)
      IDDTDR(14,IX) = IPDDFC(2,IANS) + 1
      IDDTDR(15,IX) = IDDTDR(14,IX) + NPTREC
C
C  UPDATE LAST USED RECORD IN FILE
C
      IPDDFC(2,IANS) = IDDTDR(15,IX) + NTDDRC - 1
C
C  SET POINTERS FOR EARLIEST AND LATEST RECORD NUMBERS
C
      IDDTDR(10,IX) = IDDTDR(15,IX)
      IDDTDR(13,IX) = (IPDDFC(2,IANS) + 1) - IDDTDR(21,IX)
      IF(IPDDFC(2,IANS).LT.IPDDFC(1,IANS)) GO TO 400
      WRITE(LPE,260) KPDDDF(IANS)
  325 CONTINUE
C
C  RESET WORDS BACK TO ZERO
C
      IDDTDR(7,IX) = 0
      IDDTDR(14,IX) = 0
      IDDTDR(15,IX) = 0
      IDDTDR(16,IX) = 0
      IDDTDR(21,IX) = 0
      IERFLG = 1
  400 CONTINUE
C
C  SET DATES FOR TYPES
C
      DO 425  K = 1,NMDTYP
       CALL UMEMOV(IDDTDR(8,K),IEDATE,1)
       IF(IEDATE.NE.0) GO TO 450
  425 CONTINUE
      WRITE(LPE,430)
  430 FORMAT(' **ERROR** NO DATA TYPES DEFINED ON PPDB.')
      IERLFG = 1
      GO TO 999
  450 CONTINUE
C
C  MOVE IN DATES FOR THIS TYPE
C
      CALL UMEMOV(IDDTDR(8,K),IDDTDR(8,IX),1)
      CALL UMEMOV(IDDTDR(11,K),IDDTDR(11,IX),1)
      IF(IDDTDR(7,IX).EQ.IDDTDR(7,K)) GO TO 475
C
C  DAYS ARE NOT THE SAME ADJUST EARLIEST DAY OF DATA
C
      CALL UMEMOV(IDDTDR(11,IX),LDAY,1)
      IEDAY = LDAY - IDDTDR(7,IX) + 1
      CALL UMEMOV(IEDAY,IDDTDR(8,IX),1)
  475 CONTINUE
C
C  CHECK ERROR FLAG
C
      IF(IERFLG.EQ.1) GO TO 990
  950 CONTINUE
C
C  SET FLAG TO RE-WRITE DIRECTORY RECORDS
C
      WRITE(LP,975) IDLTYP
  975 FORMAT(' DATA TYPE ',A4,' DEFINED ON PPDB')
      IFLAG = 1
      GO TO 999
  990 CONTINUE
C
C  SYSTEM ERROR
C
      WRITE(LPE,995) IERFLG
  995 FORMAT(' ***ERROR*** IN PDDTYP. STATUS = ',I2,' NOT ENOUGH ROOM.')
  999 CONTINUE
      RETURN
      END
