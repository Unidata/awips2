C MEMBER PDEFTP
C  (from old member PDBDEFTP)
C-----------------------------------------------------------------------
C
       SUBROUTINE PDEFTP (IFLAG)
C
C          SUBROUTINE:  PDEFTP
C
C             VERSION:  1.0.0
C
C                DATE:  7-21-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C     THIS SUBROUTINE PARSES THE INPUT CARDS FOR THE COMMAND 'DEFTYPE'
C     WHEN CALLED BY THE MAIN DRIVER FOR THE PPDB UTILITY PROGRAM.
C     THIS ROUTINE CHECKS FOR A VALID TYPE NOT PREVIOUSLY DEFINED.  IT
C     THEN CALLS THE MAIN ROUTINE PDDTYP TO DEFINE THE INPUT DATA TYPE
C     ON THE PPDB.  WHEN AN END CARD IS REACHED CONTROL IS RETURNED
C     TO THE MAIN DRIVER.
C
C***********************************************************************
C
C
C        ARGUMENT LIST :
C
C     NAME      TYPE      I/0     DIM     DESCRIPTION
C
C    IFLAG       I       I/O      1        REWRITE CONTROL FLAG
C                                            0=NO REWRITE
C                                            1=REWRITE CONTROLS
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'pdbcommon/pddtdr'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IFILE(2),LFILE(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdeftp.f,v $
     . $',                                                             '
     .$Id: pdeftp.f,v 1.1 1995/09/17 19:09:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LFILE/4hPDBD,4hLY  /,LEND/4hEND /
C
C***********************************************************************
C
C  DEBUG
C
      IF (IPDTR.EQ.1) WRITE (IOGDB,50)
   50 FORMAT (' ENTER PDEFTP')
  100 CONTINUE
C
      CALL RWCARD(ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (NFIELD.EQ.1.OR.NFIELD.EQ.4) GO TO 150
      WRITE (LP,125)
  125 FORMAT (' INVALID # OF FIELDS ON INPUT CARD')
      GO TO 100
  150 IF=1
      NUM=IFSTOP(IF)-IFSTRT(IF)+1
      IF (NUM.GT.4) NUM=4
      CALL UPACK1(IBUF(IFSTRT(IF)),IDLTYP,NUM)
      IF (IDLTYP.EQ.LEND) GO TO 950
  175 CONTINUE
C
C  FIND DATA TYPE IN DIRECTORY
C
      DO 200 I=1,8
         CALL UMEMOV (IDDTDR(2,I),ITYPE,1)
         IF (ITYPE.EQ.IDLTYP) GO TO 250
  200    CONTINUE
      WRITE (LPE,225) IDLTYP
  225 FORMAT (' ***ERROR*** INVALID DATA TYPE. ',A4,' NOT DEFINED ON ',
     *  'PPDB.')
      GO TO 100
  250 CONTINUE
C
C  SEE IF ANY STATIONS DEFINED
C
      IF (IDDTDR(16,I).EQ.0) GO TO 300
      WRITE (LPE,275) IDLTYP
  275 FORMAT (' ***ERROR*** DATA TYPE ALREADY DEFINED ON PPDB.',
     1       ' TYPE ',A4,' WILL NOT BE ADDED.')
      GO TO 100
  300 CONTINUE
C
C  READ REST OF DAILY DATA
C
      IF=IF+1
      NUM=IFSTOP(IF)-IFSTRT(IF)
      IF (NUM.EQ.6) GO TO 350
      WRITE (LPE,325) IF
  325 FORMAT (' ***ERROR*** INVALID # OF CHARACTERS IN FILE NAME ',
     1       ' IN FIELD ',I2)
      GO TO 999
  350 IFILE(2)=IBLNK
      CALL UPACK1(IBUF(IFSTRT(IF)),IFILE,NUM)
      CALL UNAMCP(IFILE,LFILE,IERR)
      IF (IERR.NE.0) GO TO 375
      CALL UINTFX(IANS,IFSTOP(IF),IFSTOP(IF),JERR)
      IF (IANS.GE.1.AND.IANS.LE.5) GO TO 425
  375 WRITE (LPE,400)
  400 FORMAT (' ***ERROR*** INVALID FILE NAME.')
      GO TO 100
  425 CONTINUE
C
C  GET MAX # OF STATIONS ALLOWABLE FOR THIS TYPE
C
      IF=IF+1
      IF (IFTYPE(IF).NE.1) GO TO 450
      CALL UNUMIC(IBUF,IFSTRT(IF),IFSTOP(IF),MAXSTA)
      GO TO 500
  450 WRITE (LPE,475) IF
  475 FORMAT (' ***ERROR*** FIELD ',I2,' IS NOT AN INTEGER')
      GO TO 100
  500 CONTINUE
C
C  GET MAX # OF DAYS ON FILE
C
      IF=IF+1
      IF (IFTYPE(IF).EQ.1) GO TO 525
      WRITE (LPE,475) IF
      GO TO 100
  525 CALL UNUMIC(IBUF,IFSTRT(IF),IFSTOP(IF),MAXDAY)
  550 CONTINUE
C
C  DEFINE NEW DATA TYPE ON PPDB
C
      CALL PDDTYP(IDLTYP,IANS,MAXSTA,MAXDAY,IFLAG,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      GO TO 100
  900 CONTINUE
C
C  SYSTEM ERROR
C
      WRITE (LPE,925) ISTAT
  925 FORMAT (' ***ERROR*** IN PDEFTP. STATUS=',I2)
      GO TO 999
  950 CONTINUE
C
C  DEBUG AND RETURN
C
      IF (IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE (IOGDB,975)
  975 FORMAT (' PDEFTP EXECUTED')
  999 CONTINUE
C
      RETURN
C
      END
