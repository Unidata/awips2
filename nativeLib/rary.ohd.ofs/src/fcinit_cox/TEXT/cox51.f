C MEMBER COX51
C--------------------------------------------------------     
C
C@PROCESS LVL(77)
C
C DESC CARRYOVER TRANSFER FOR OPERATION 51 - SSARRESV.
C
      SUBROUTINE COX51(POLD,COLD,PNEW,CNEW)
C
C  CARRYOVER TRANSFER ROUTINE FOR OPERATION 51 - SSARRESV.
C
C--------------------------------------------------
C  WRITTEN BY -  KSHSU - HRL - OCTOBER 1994
C---------------------------------------------------
C
C      COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      REAL*8 SUBNAM
      LOGICAL CONV3
      DIMENSION POLD(*),COLD(*),PNEW(*),CNEW(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox51.f,v $
     . $',                                                             '
     .$Id: cox51.f,v 1.2 2006/03/16 18:55:14 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/8HCOX51   /
C
C----------------------------
C  SET DEBUG AND TRACE LEVELS
C
      IBUG = 0
      CALL FPRBUG(SUBNAM,1,51,IPBUG)
      IF (ITRACE .GT. 0) IBUG=1
      IF (IPBUG .GT. 0) IBUG=2
      IUNTO=POLD(8)
      IUNTN=PNEW(8)
      NCTO=POLD(15)
      NCTN=PNEW(15)

C dws    The following 4 statements were replace below to avoid
C dws     compiler warnings ... 2006-01-23

C     IRUO=POLD(POLD(11))
C     IRUN=PNEW(PNEW(11))
C     IRDO=POLD(POLD(10))
C     IRDN=PNEW(PNEW(10))

        IDXO10=POLD(10)
        IDXO11=POLD(11)
        IDXN10=PNEW(10)
        IDXN11=PNEW(11)
      IRUO=POLD(IDXO11)
      IRUN=PNEW(IDXN11)
      IRDO=POLD(IDXO10)
      IRDN=PNEW(IDXN10)

      IF(IBUG.EQ.0) GO TO 410
      WRITE(IODBUG,902) (COLD(I),I=1,NCTO)
  902 FORMAT(5X,'OLD CARRYOVER ARRAY:',
     & /5X,F10.0,2(F10.0,F11.2,F10.0,F10.0))
      WRITE(IODBUG,904) (CNEW(I),I=1,NCTN)
 904  FORMAT(5X,'CARRYOVER ARRAY FROM SEGMENT REDEFITION:',
     & /5X,F10.0,2(F10.0,F11.2,F10.0,F10.0))
  410 CONTINUE
      IF(IUNTO.EQ.IUNTN .AND. NCTN.EQ.NCTO
     & .AND. IRUO.EQ.IRUN .AND. IRDO.EQ.IRDN) GO TO 44
      WRITE(IPR,800) (PNEW(I),I=2,6)
 800  FORMAT(
     & /5X,'**WARNING** FOR SSARRESV OPERATION',2X,'NAME= ',5A4)
      IF(IUNTN.NE.IUNTO) WRITE(IPR,805)
 805  FORMAT(10X,'INPUT UNITS HAS BEEN CHANGED')
      IF(NCTN.NE.NCTO) WRITE(IPR,806)
 806  FORMAT(10X,'NO OF RESERVOIRS/STATIONS HAS BEEN CHANGED')
      IF(IRUN.NE.IRUO) WRITE(IPR,807)
 807  FORMAT(10X,'UPSTREAM RESERVOIRS/STATIONS TYPE HAS BEEN CHANGED')
      IF(IRDN.NE.IRDO) WRITE(IPR,808)
 808  FORMAT(10X,'UPSTREAM RESERVOIRS/STATIONS TYPE HAS BEEN CHANGED')
      WRITE(IPR,810)
 810  FORMAT(
     3  10X,'CARRYOVER VALUES INPUT DURING THE SEGMENT REDEFINITION ',
     4  'WILL BE USED.')
      GO TO 510
C
C-----------------------------------------
C  TRANSFER INFLOW CARRYOVER
C
   44 CONTINUE
      DO 50 I=1,NCTN
 50   CNEW(I) = COLD(I)
C
C
 500  CONTINUE
      IF(IBUG.EQ.0) GO TO 510
      WRITE(IODBUG,906) (CNEW(I),I=1,NCTN)
 906  FORMAT(5X,'FINAL CARRYOVER ARRAY AFTER CARRYOVER TRANSFER:',
     & /5X,F11.1,2(F11.1,F11.2,F11.0))
  510 CONTINUE
      RETURN
      END
