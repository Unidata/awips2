      SUBROUTINE COX56(POLD,COLD,PONEW,CONEW)
C............................................
C     CARRYOVER TRANSFER ROUTINE
C............................................
      DIMENSION POLD(*),COLD(*),PONEW(*),CONEW(*)
      CHARACTER*8  SNAME
      REAL KG1,KG2,KGNEW,KGOLD
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox56.f,v $
     . $',                                                             '
     .$Id: cox56.f,v 1.2 2000/12/19 14:47:10 jgofus Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA  SNAME / 'COX56   ' /
C.............................................
C     TRACEL LEVEL =1, DEBUG SWITCH = IBUG
      CALL FPRBUG(SNAME,1,56,IBUG)
C.............................................
C     IF DEBUG ON, PRINT OLD CORRYOVER IF ANY EXISTS
      IF (IBUG.EQ.0) GOTO 100
      WRITE(IODBUG,900)COLD(1),COLD(2)
 900  FORMAT(1H0,'GLACIER OPER -- OLD CARRYOVER=',F8.2,2x
     &,f8.2)
C.............................................
C     CONTROL STATEMENTS
 100  CG1OLD=POLD(9)
      CG2OLD=POLD(10)
      CG1NEW=PONEW(9)
      CG2NEW=PONEW(10)
      KG1=POLD(12)
      KG2=POLD(13)
      AFINEW=CONEW(2)
      AFIOLD=COLD(2)
C.............................................
C     TRANSFER CARRYOVER -- ADJUST STORAGE - CO(1)
C     CALCULATE KGOLD
      NUM=EXP(CG1OLD+CG2OLD*AFIOLD)
      FAFI=NUM/(1+NUM)
      KGOLD=KG1+(KG2-KG1)*FAFI
C     CALCULATE KGNEW
      NUM=EXP(CG1NEW+CG2NEW*AFIOLD)
      FAFI=NUM/(1+NUM)
      KGNEW=KG1+(KG2-KG1)*FAFI
C     CALCULATE ADJUSTED CONEW(1)
      CONEW(1)=(KGOLD/KGNEW)*COLD(1)
C
C     TRANSFER CARRYOVER -- NO ADJUSTMENT - CO(2)
      CONEW(2)=COLD(2)
C     IF DEBUG IS ON PRINT NEW CARRYOVER
      IF (IBUG.EQ.0) GOTO 190
      WRITE(IODBUG,901)CONEW(1),CONEW(2)
 901  FORMAT(1H0,'GLACIER OPER -- NEW CARRYOVER',F8.2,2X,
     &F8.2)
C...............................................
 190  IF(ITRACE.GE.1) WRITE (IODBUG,902)
 902  FORMAT(1H0,'**EXIT COX56')
      END

