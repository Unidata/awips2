C MEMBER PDLYFL
C  (from old member PDBCHGFL)
C***********************************************************************
C                                                                      *
C         MEMBER PDBCHGFL                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDLYFL(IFLAG,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDLYFL                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  7-5-83                                         *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE CHANGES THE FILE ON WHICH A SPECIFIED DAILY DATA  *
C    TYPE IS HELD.  IT CHECKS  FOR THE APPROPRIATE AMOUNT OF SPACE     *
C    AND MOVES THE POINTER RECORDS AND DATA RECORDS TO A NEW DAILY     *
C    FILE.                                                             *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       ISTAT      I     O     1    STATUS CODE                        *
C                                    0 = NORMAL RETURN                 *
C                                    OTHER = ERROR                     *
C       IFLAG      I     O     1    WRITE FLAG FOR CONTROL RECORDS     *
C                                    0 = DO NOT REWRITE CONTROLS       *
C                                    1 = RE-WRITE CONTROLS             *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdddfc'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION IBUFF(32),IFILE(2),IDLY(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdlyfl.f,v $
     . $',                                                             '
     .$Id: pdlyfl.f,v 1.1 1995/09/17 19:09:28 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA IDLY/4hPDBD,4hLY  /
C                                                                      *
C***********************************************************************
C
C
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,100)
  100 FORMAT(' ***ENTER SUBROUTINE PDLYFL')
C
      ISTAT = 0
      IFLAG = 0
C
C  GET DAILY DATA TYPE
C
      IF = 2
      NUM = IFSTOP(IF) - IFSTRT(IF) + 1
      IF(NUM.GT.4) NUM = 4
      CALL UPACK1(IBUF(IFSTRT(IF)),IDTYP,NUM)
C
C  GET NEW DATA FILE NAME
C
      IF = IF + 1
      NUM = IFSTOP(IF) - IFSTRT(IF)
      IF(NUM.EQ.6) GO TO 150
      WRITE(LPE,125)
  125 FORMAT(' ***ERROR*** INVALID # OF CHARACTERS IN FILE NAME')
      GO TO 999
  150 IFILE(2) = IBLNK
      CALL UPACK1(IBUF(IFSTRT(IF)),IFILE,NUM)
      CALL UNAMCP(IDLY,IFILE,IERR)
      IF(IERR.NE.0) GO TO 175
      CALL UINTFX(IANS,IFSTOP(IF),IFSTOP(IF),JERR)
      IF(IANS.GE.1.AND.IANS.LE.5) GO TO 200
  175 WRITE(LPE,180)
  180 FORMAT(' ***ERROR*** INVALID FILE NAME.')
      GO TO 999
  200 CONTINUE
C
C  SEE IF VALID DATA TYPE
C
      IDX = IPDCKD(IDTYP)
      IF(IDX.NE.0) GO TO 250
      WRITE(LPE,225) IDTYP
  225 FORMAT(' ***ERROR*** INVALID DAILY DATA TYPE ',A4)
      GO TO 999
  250 CONTINUE
C
C  GET THE FILE THAT THIS TYPE IS PRESENTLY ON
C
      IDF = IDDTDR(4,IDX)
C
C  CHECK THAT IT'S NOT THE SAME FILE
C
      IF(IDF.NE.IANS) GO TO 300
      WRITE(LPE,275)
  275 FORMAT(' ***ERROR*** DATA TYPE ALREADY EXISTS ON THIS',
     1       ' FILE. NO FILE CHANGE WILL BE MADE.')
      GO TO 999
  300 CONTINUE
C
C  SEE HOW MUCH SPACE IS AVAILABLE ON NEW FILE
C
      NEWSP = IPDDFC(1,IANS) - IPDDFC(2,IANS)
C
C  SEE HOW MANY RECORDS WILL BE MOVED
C
      MOVREC = IDDTDR(21,IDF) * IDDTDR(7,IDF) + IDDTDR(15,IDF) -
     1         IDDTDR(14,IDF)
C
C  SEE IF ROOM IN NEW FILE TO MOVE RECORDS
C
      IF(MOVREC.GT.NEWSP) GO TO 950
C
C  MOVE DATA TYPE TO NEW FILE
C
      IREC = IDDTDR(14,IDX)
      NREC = IPDDFC(2,IANS) + 1
C
C  MOVE DATA TO NEW FILE
C
      DO 350 I = 1,MOVREC
      CALL RVLRCD(KPDDDF(IDF),IREC,1,IBUFF,LRCP,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      CALL WVLRCD(KPDDDF(IANS),NREC,1,IBUFF,LRCP,ISTAT)
      IF(ISTAT.NE.0) GO TO 990
      IREC = IREC + 1
      NREC = NREC + 1
  350 CONTINUE
C
C  CALCULATE OFFSETS TO RESET POINTERS
C
      IOFF = IDDTDR(15,IDX) - IDDTDR(14,IDX)
      IEOFF = IDDTDR(10,IDX) - IDDTDR(15,IDX)
      ILOFF = IDDTDR(13,IDX) - IDDTDR(10,IDX)
C
C  RESET POINTERS FOR THE NEW FILE
C  LAST USED RECORD
C
      IPDDFC(2,IANS) = IPDDFC(2,IANS) + MOVREC
C
C  RESET THE LOGICAL UNIT FOR THIS TYPE
C
      IDDTDR(4,IDX) = IANS
C
C  RESET THE REST OF THE POINTERS - POINTERS, DATA, EARLIEST AND LATEST
C
      IDDTDR(14,IDX) = NREC
      IDDTDR(15,IDX) = NREC + IOFF
      IDDTDR(10,IDX) = IDDTDR(15,IDX) + IEOFF
      IDDTDR(13,IDX) = IDDTDR(10,IDX) + ILOFF
  900 CONTINUE
C
C  DEBUG AND RETURN
C
      IF(IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE(IOGDB,925)
  925 FORMAT(' SUBROUTINE PDLYFL EXECUTED.')
      WRITE(LP,935) IDTYP,IANS
  935 FORMAT(' DAILY DATA TYPE ',A4,' MOVED TO FILE ',I2)
      IFLAG = 1
      GO TO 999
  950 CONTINUE
C
C  FILE TOO SMALL
C
      WRITE(LPE,975) IANS,IDTYP
  975 FORMAT(' ***ERROR*** NOT ENOUGH ROOM ON FILE ',I2,' TO MOVE',
     1       ' DATA TYPE ',A4)
      GO TO 999
  990 CONTINUE
C
C  SYSTEM ERROR
C
      WRITE(LPE,995) ISTAT
  995 FORMAT(' ***ERROR*** IN PDLYFL, STATUS = ',I2)
  999 CONTINUE
      RETURN
      END
