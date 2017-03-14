C MODULE PDELTP
C-----------------------------------------------------------------------
C
C
       SUBROUTINE PDELTP
C
C          ROUTINE:  PDELTP
C
C             VERSION:  1.0.0
C
C                DATE:  7-29-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE IS INITIATED BY THE DELTYPE COMMAND.  IT
C    DELETES DAILY DATA TYPES FROM THE PPDB.  THE DATA TYPE CANNOT
C    BE DELETED IF THERE ARE ANY STATIONS DEFINED FOR THIS TYPE ON
C    THE DATA BASE.
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ITPARR(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdeltp.f,v $
     . $',                                                             '
     .$Id: pdeltp.f,v 1.2 2000/12/18 22:18:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
C  DEBUG
C
      IF (IPDTR.EQ.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDELTP')
C
C  READ CARD CONTAINING DATA TYPES FOR DELETION
C
20    CONTINUE
C
      CALL RDLYTP(ITPARR,NTYP,LASTCD,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
30    CONTINUE
C
C  CHECK FOR LEGAL TYPE
C
      DO 90 I=1,NTYP
      IX=IPDCKD(ITPARR(I))
      IF (IX.EQ.0) GO TO 70
      IF (IDDTDR(18,IX).NE.0) GO TO 50
C
C  DELETE THIS DATA TYPE FROM THE PPDB
C
      CALL UMEMST(0,IDDTDR(7,IX),8)
      WRITE (LP,40) ITPARR(I)
40    FORMAT (' DAILY DATA TYPE ',A4,' DELETED FROM THE PPDB')
      GO TO 90
50    CONTINUE
C
C  STATIONS DEFINED CANNOT DELETE TYPE
C
      WRITE (LPE,60) ITPARR(I)
60    FORMAT (' **ERROR** STATIONS DEFINED FOR DATA TYPE ','.',A4,
     *   ' NO DELETE OCCURED.')
      GO TO 90
70    CONTINUE
C
C  ILLEGAL TYPE
C
      WRITE (LPE,80)
80    FORMAT (' **ERROR** DATA TYPE NOT FOUND ON PPDB.',
     *   ' NO DELETE OCCURRED.')
C
C  SEE IF MORE CARDS TO READ
C
90    IF (LASTCD.EQ.0) GO TO 100
      GO TO 20
100   CONTINUE
C
C DEBUG AND RETURN
C
      IF (IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE (IOGDB,110)
110   FORMAT (' *** EXIT PDELTP')
      GO TO 140
120   CONTINUE
C
C  SYSTEM ERROR
C
      WRITE (LPE,130) ISTAT
130   FORMAT (' **ERROR** IN PDELTP - STATUS=',I2)
140   CONTINUE
C
      RETURN
C
      END
