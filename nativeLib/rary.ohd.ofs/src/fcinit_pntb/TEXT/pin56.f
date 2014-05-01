C MODULE PIN56
C
      SUBROUTINE PIN56(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
C......................................................
C  THIS IS THE INPUT SUBROUTINE FOR THE GLACIER STORAGE
C  MODEL.
C.......................................................
      DIMENSION PO(*),CO(*)
      DIMENSION GOID(2),RMID(2),AFID(2)
      CHARACTER*8  SNAME
      REAL KG1,KG2
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin56.f,v $
     . $',                                                             '
     .$Id: pin56.f,v 1.5 2003/08/15 13:16:19 gzhou Exp $
     . $' /
C    ===================================================================
C
C
      DATA  SNAME / 'PIN56   ' /
      DATA  BLANK /4H    /
C
C  TRACE LEVEL FOR SUBROUTINE =1, DEBUG SWITCH = IBUG
      CALL FPRBUG(SNAME,1,56,IBUG)

C  CONTROL VARIABLES
      IVER=2
      IUSEP=0
      IUSEC=0
      LPO=16
      LCO=2
      NOFILL=0
C........................................................
C     READ INPUT CARDS
      IERR=0
C     CARD 1
      READ(IN,500)RMID,RMTYPE,IDT,GOID,GOTYPE,AFID,AFTYPE
 500  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,2X,2A4,1X,A4)
      CALL CHEKTS(RMID,RMTYPE,IDT,1,'L   ',0,1,IERR)
      IF (IERR.EQ.1) GOTO 900
      CALL CHEKTS(GOID,GOTYPE,IDT,1,'L   ',0,1,IERR)
      IF (IERR.EQ.1) GOTO 900
      IF((AFID(1).EQ.BLANK).AND.(AFID(2).EQ.BLANK))IVER=1
C     CARD2
      READ(IN,800)CG1,CG2,CG3,KG1,KG2
 800  FORMAT(2X,F6.1,2X,F4.2,2X,F4.2,2X,F4.2,2X,F4.2)
C     CARD3
      READ(IN,850)STORAGE,AFI
 850  FORMAT(2X,F6.1,2X,F6.1)
C.........................................................
C     CHECK INITIAL VALUES
      IF (CG3.LT.0.0.OR.CG3.GT.1.0) THEN
      WRITE(IPR,852)
 852  FORMAT(1H0,10X,'THE AFI DECAY PARAMETER MUST BE GREATER THAN OR
     &EQUAL TO ZERO AND LESS THAN OR EQUAL TO ONE.')
      CALL ERROR
      IERR=1.0
      ENDIF
      IF((KG1.LT.0.OR.KG1.GT.1.0).OR.(KG2.LT.0.0.OR.KG2.GT.1.0)) THEN
      WRITE(IPR,854)
 854  FORMAT(1H0,10X,'OUTFLOW COEFS MUST BE GREATER THAN OR EQUAL TO
     &ZERO AND LESS THAN OR EQUAL TO ONE.')
      IERR=1.0
      CALL ERROR
      ENDIF
      IF(KG1.GT.KG2) THEN
      WRITE(IPR,856)
 856  FORMAT(1HO,10X,'MAX OUTFLOW COEF MUST BE GREATER THAN OR EQUAL TO
     &MIN COEF')
      CALL ERROR
      IERR=1.0
      ENDIF
      IF(AFI.LT.0.0) THEN
      WRITE(IPR,858)
 858  FORMAT(1HO,10X,'ANTECEDENT FLOW INDEX CAN NOT BE LESS THAN ZERO')
      CALL ERROR
      IERR=1.0
      ENDIF
      IF(STORAGE.LT.0.0) THEN
      WRITE(IPR,860)
 860  FORMAT(1HO,10X,'STORAGE CAN NOT BE LESS THAN ZERO')
      CALL ERROR
      IERR=1.0
      ENDIF
C.........................................................
C     CHECK IF OKAY TO LOAD VALUES IN PO().
      CALL CHECKP(LPO,LEFTP,IERR)
C     CHECK IF OKAY TO LOAD VALUES IN CO().
      CALL CHECKC(LCO,LEFTP,IERR)
 900  IF (IERR.EQ.1) NOFILL=1
      IF (NOFILL.EQ.1) THEN
      WRITE(IPR,902)
 902  FORMAT(1H0,10X,'**GLACIER OPERATION WILL BE IGNORED',
     1' BECAUSE OF THE PRECEEDING ERRORS.')
      CALL WARN
      RETURN
      ENDIF
C.........................................................
C     LOAD INFORMATION INTO PO().
      PO(1)=IVER+0.01
      PO(2)=RMID(1)
      PO(3)=RMID(2)
      PO(4)=RMTYPE
      PO(5)=GOID(1)
      PO(6)=GOID(2)
      PO(7)=GOTYPE
      PO(8)=IDT+0.01
      PO(9)=CG1
      PO(10)=CG2
      PO(11)=CG3
      PO(12)=KG1
      PO(13)=KG2
C     ONLY LOAD AFI IF IVER = 2
      IF (IVER.EQ.2) THEN
      PO(14)=AFID(1)
      PO(15)=AFID(2)
      PO(16)=AFTYPE
      ELSE
      LPO=13
      ENDIF
      CO(1)=STORAGE
      CO(2)=AFI
      IUSEP=LPO
      IUSEC=LCO

C................................................
C     CHECK FOR DEBUG OUTPUT
      IF (IBUG.EQ.0) GOTO 908
C................................................
C     DEBUG OUTPUT
      WRITE(IODBUG,904)
 904  FORMAT(1H0,'GLACIER DEBUG--CONTENTS OF PO ARRAY.')
      WRITE(IODBUG,905)  (PO(I),I=1,IUSEP)
 905  FORMAT(1H0,13(1X,F8.2))
      WRITE(IODBUG,906) (PO(I),I=1,IUSEP)
 906  FORMAT(1H0,13(4X,A4))
      WRITE(IODBUG,907) (CO(I),I=1,IUSEC)
 907  FORMAT(1H0,'CARRYOVER=',2(1X,F8.2))
C.................................................
 908  IF (ITRACE.GE.1) WRITE(IODBUG,909)
 909  FORMAT(1H0,'** EXIT PIN56')
      RETURN
      END
