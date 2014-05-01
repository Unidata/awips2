C MEMBER PIN42
C  (from old member FCPIN42)
C
      SUBROUTINE PIN42(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
C.......................................
C   THIS IS THE INPUT SUBROUTINE FOR THE 'RSNWELEV' OPERATION
C.......................................
C   INITIALLY WRITTEN BY ERIC ANDERSON - HRL  DEC 1991
C.......................................
      DIMENSION PO(1),CO(1),SNAME(2)
      DIMENSION TAID(2),RSID(2),ZLID(2)
C   COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin42.f,v $
     . $',                                                             '
     .$Id: pin42.f,v 1.1 1995/09/17 18:48:40 dws Exp $
     . $' /
C    ===================================================================
C
C   DATA STATEMENTS
      DATA SNAME,BLANK/4HPIN4,4H2   ,4H    /
      DATA TEMP,DL/4HTEMP,4HL   /
C.......................................
C   TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,42,IBUG)
C.......................................
C    CONTROL VARIABLES
      IVER=1
      LPO=20
      LCO=1
      IUSEP=0
      IUSEC=0
      NOFILL=0
C.......................................
C    READ INPUT CARDS AND MAKE CHECKS
C.......................................
C    CARD NUMBER 1
      READ(IN,900) RSID,RSTYPE,IDT,PXTEMP,TALR,TAID,TATYPE,TAELEV,
     -ZLID,ZLTYPE,IRDCO
  900 FORMAT(2X,2A4,1X,A4,3X,I2,2F5.0,7X,2A4,1X,A4,F5.0,
     -  2X,2A4,1X,A4,1X,I1)
      CALL CHEKTS(TAID,TATYPE,IDT,1,TEMP,0,1,IERR)
      IF (TALR.NE.0.0) GO TO 101
      WRITE (IPR,901)
  901 FORMAT(1H0,10X,'**ERROR** THE LAPSE RATE CANNOT BE ZERO.')
      CALL ERROR
  101 IF (TALR.GT.0.0) GO TO 105
      WRITE(IPR,902)
  902 FORMAT(1H0,10X,'**WARNING** THE LAPSE RATE IS NOT POSITIVE.  CHECK
     - THAT VALUE IS CORRECT.')
      CALL WARN
  105 CALL CHEKTS(RSID,RSTYPE,IDT,1,DL,0,1,IERR)
      IF (ZLID(1).EQ.BLANK) GO TO 106
      CALL CHEKTS(ZLID,ZLTYPE,IDT,1,DL,1,1,IERR)
      IZIN=1
      GO TO 110
  106 IZIN=0
      ZLID(2)=BLANK
      ZLTYPE=BLANK
  110 IF (IRDCO.NE.0) IRDCO=1
C.......................................
C   CARD NUMBER 2
      IF (IZIN.EQ.0) GO TO 120
      IF (IRDCO.EQ.1) GO TO 115
      PRSEL=-999.0
      GO TO 120
  115 READ(IN,903) PRSEL
  903 FORMAT(F10.0)
      IF (PRSEL.GE.-100.0) GO TO 120
      PRSEL=-999.0
C.......................................
C   CHECK SPACE IN P AND C ARRAYS.
  120 CALL CHECKP(LPO,LEFTP,IERR)
      IF (IERR.EQ.1) NOFILL=1
      IF (IZIN.EQ.0) GO TO 130
      CALL CHECKC(LCO,LEFTC,IERR)
      IF (IERR.EQ.1) NOFILL=1
  130 IF (NOFILL.EQ.1) RETURN
C.......................................
C    SPACE AVAILABLE -- STORE VALUES.
C    PARAMETERS
      PO(1)=IVER+0.01
      PO(2)=IDT+0.01
      PO(3)=TAID(1)
      PO(4)=TAID(2)
      PO(5)=TATYPE
      PO(6)=TAELEV
      PO(7)=TALR
      PO(8)=PXTEMP
      PO(9)=RSID(1)
      PO(10)=RSID(2)
      PO(11)=RSTYPE
      PO(12)=IZIN+0.01
      PO(13)=ZLID(1)
      PO(14)=ZLID(2)
      PO(15)=ZLTYPE
      PO(16)=0.01
      PO(17)=0.01
      PO(18)=0.01
      PO(19)=0.01
      PO(20)=0.01
      IUSEP=LPO
C.......................................
C    CARRYOVER
      IF (IZIN.EQ.0) GO TO 140
      CO(1)=PRSEL
      IUSEC=LCO
C.......................................
C    CHECK FOR DEBUG OUTPUT
  140 IF (IBUG.EQ.0) GO TO 199
C.......................................
C    DEBUG OUTPUT
      WRITE(IODBUG,904)
  904 FORMAT(1H0,'RSNWELEV DEBUG--CONTENTS OF PO ARRAY.')
      WRITE(IODBUG,905) (PO(I),I=1,IUSEP)
  905 FORMAT(1H0,15F8.2)
      WRITE(IODBUG,906) (PO(I),I=1,IUSEP)
  906 FORMAT(1H0,15(4X,A4))
      IF (IZIN.EQ.0) GO TO 199
      WRITE(IODBUG,908) CO(1)
  908 FORMAT(1H0,'CARRYOVER=',F10.2)
C.......................................
  199 IF (ITRACE.GE.1) WRITE(IODBUG,907)
  907 FORMAT(1H0,'** EXIT PIN42')
      RETURN
      END
