C MEMBER PIN16
C  (from old member MCPIN16)
C
      SUBROUTINE PIN16(PO,LEFTP,IUSEP,W,LEFTW,Q)
C.......................................................................
C     THIS IS THE INPUT SUBROUTINE FOR THE QME STATISTICS OPERATION FOR
C     THE MANUAL CALIBRATION PROGRAM.  THIS SUBROUTINE INPUTS ALL CARDS
C     FOR THE OPERATION, FILLS THE PO ARRAY, INITIALIZES THE
C     MULTIYEAR CARRYOVER VALUES, AND STORES THE INITIAL CARRYOVER
C     VALUES ON THE SCRATCH DISK.
C.......................................................................
C    SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
C
      DIMENSION PO(1),QSID(2),QOID(2),QNAME(5),W(1),Q(1),FI(6)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/calbrt'
      INCLUDE 'common/fwyds'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/pin16.f,v $
     . $',                                                             '
     .$Id: pin16.f,v 1.2 1996/07/11 19:33:25 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA DL3,DYR,DQR,DFR/4HL3  ,4HYEAR,4HQUAR,4HFREQ/
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PIN16 ENTERED)
C.......................................................................
C
C     CHECK TO SEE IF DEBUG OUTPUT IS NEEDED FOR THIS OPERATION.
      IBUG=0
      IF(IDBALL.GT.0) GO TO 11
      IF(NDEBUG.EQ.0) GO TO 100
      DO 10 I=1, NDEBUG
      IF(IDEBUG(I).EQ.16) GO TO 11
   10 CONTINUE
      GO TO 100
   11 IBUG=1
  100 CONTINUE
C
C.......................................................................
C
C     INITIALIZE VARIABLES
      IVER=1
      NOWY=0
      NOFILL=0
      IUSEP=0
      IPO=28
      IYRFL=0
      IQR=0
      IFR=0
      INEXT=0
      EXCED=0.0
      DO 101 J=1,6
  101 FI(J)=0.0
C
C.......................................................................
C
C     READ INPUT AND MAKE CHECKS
C
C     INPUT CARD NO.1
      READ(IN,901) QNAME,AREA,QSID,QSTYPE,IDTQS,QOID,QOTYPE,IDTQO,INEXT
  901 FORMAT(5A4,F10.0,2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,1X,I1)
      IVALUE=1
      IMISS=1
      IDIMEN=1
      CALL CHEKTS(QSID,QSTYPE,IDTQS,IDIMEN,DL3,IMISS,IVALUE,IERR)
      CALL CHEKTS(QOID,QOTYPE,IDTQO,IDIMEN,DL3,IMISS,IVALUE,IERR)
      IF(INEXT.EQ.0) GO TO 110
C
C     INPUT CARD NO.2 IF NEEDED
C
      READ(IN,902) YEAR,QUAR,FREQ,EXCED,FI
  902 FORMAT(1X,A4,1X,A4,1X,A4,F10.0,6F5.0)
C
C     CHECK STAT OPTION SWITCHES
C
      IF(YEAR.EQ.DYR) IYRFL=1
      IF(QUAR.EQ.DQR) IQR=1
      IF(FREQ.EQ.DFR) IFR=1
      IF(EXCED.EQ.-1.) EXCED=0.3* AREA
  110 CONTINUE
C
C     COMPUTE STAT FLOW INTERVALS
C
      TQ=-.01
      DO 220 I=1,6
      IF(FI(I).GT.TQ) GO TO 221
      GO TO 225
  221 TQ=FI(I)
  220 CONTINUE
      GO TO 230
  225 TQ=0.011574*AREA
C     INTERVALS OF .032,.1,.32,1.0,3.2,AND 10 MM.
      FI(1)=0.032*TQ
      FI(2)=0.1*TQ
      FI(3)=0.32*TQ
      FI(4)=1.0*TQ
      FI(5)=3.2*TQ
      FI(6)=10.0*TQ
C
C.......................................................................
C
C     CHECK SPACE IN P AND C ARRAYS
  230 IF(LEFTP.GE.IPO) GO TO 111
      NOFILL=1
      WRITE(IPR,910)
  910 FORMAT(1H0,10X,75H**ERROR** THIS OPERATION NEEDS MORE SPACE THAN I
     1S AVAILABLE IN THE P ARRAY.)
      CALL ERROR
      GO TO 800
C
C     COMPUTE AND CHECK WORKING SPACE
C.......................................................................
C
C     COMPUTE AND CHECK WORKING SPACE
C
C
  111 IWSW=381
      IF(IQR.EQ.0) GO TO 140
      INUMYR=LYR-IYR
      IF(IMO.LE.9) INUMYR=INUMYR+1
      IF(LMO.GT.9) INUMYR=INUMYR+1
      IXB=INUMYR*8
      IWSW=382+IXB
  140 CONTINUE
      IF(IFR.EQ.0) GO TO 142
      IWSW=IWSW+51
  142 CONTINUE
      IF(EXCED.LT.0.01) GO TO 144
      IWSW=IWSW+60
  144 IF(LEFTW.GE.IWSW) GO TO 112
      NOWY=1
      WRITE(IPR,914)
  914 FORMAT(1H0,10X,83H**ERROR** THIS OPERATION NEEDS MORE WORKING SPAC
     1E THAN IS AVAILABLE IN THE D ARRAY.)
      CALL ERROR
      GO TO 800
C.......................................................................
C
C      INITIALIZE WORKING SPACE NEEDED BY STAT OPTIONS
C
C
  112 CALL INA16(W(1),W(25),W(61),W(219),W(226),W(257),W(332))
      IWS=381
      IF(IQR.EQ.0) GO TO 120
      IW=IWS+1
      J=IW+IXB
      CALL INB16(W(IW),IXB,W(J))
      IWS=IWS+IXB+1
  120 CONTINUE
      IF(IFR.EQ.0) GO TO 122
      IW=IWS+1
      CALL INC16(W(IW))
      IWS=IWS+51
  122 CONTINUE
      IF(EXCED.LT.0.01) GO TO 130
      IW=IWS+1
      IX=IWS+21
      IY=IWS+41
      CALL IND16(W(IW),W(IX),W(IY),EXCED)
      IWS=IWS+60
C
C.......................................................................
C
C     COMPUTE NUMBER OF CARRYOVER RECORDS NEEDED AND UPDATE NXWY
  130 IREC=IWS/372
      IF(IWS/372*372.NE.IWS)IREC=IREC+1
      IXWY=NXWY
      NXWY=NXWY+IREC+2
      IF((NXWY-1).LE.NTWY) GO TO 124
      NOWY=1
      WRITE(IPR,912)
  912 FORMAT(1H0,10X,80H**ERROR** THIS OPERATION NEEDS MORE SPACE THAN I
     1S AVAILABLE ON THE SCRATCH FILE.)
      CALL ERROR
C
C.......................................................................
C
C     STORE INFORMATION IN PO ARRAY
  124 CONTINUE
      IUSEP=IPO
      PO(1)=IVER+0.01
      DO 132 I=1,5
  132 PO(I+1)=QNAME(I)
      PO(7)=AREA
      PO(8)=QSID(1)
      PO(9)=QSID(2)
      PO(10)=QSTYPE
      PO(11)=IDTQS+0.01
      PO(12)=QOID(1)
      PO(13)=QOID(2)
      PO(14)=QOTYPE
      PO(15)=IDTQO+0.01
      PO(16)=IYRFL+0.01
      PO(17)=IQR+0.01
      PO(18)=IFR+0.01
      PO(19)=EXCED
      PO(20)=IWS+0.01
      PO(21)=IXWY+0.01
      PO(22)=FI(1)
      PO(23)=FI(2)
      PO(24)=FI(3)
      PO(25)=FI(4)
      PO(26)=FI(5)
      PO(27)=FI(6)
      PO(28)=IUSEP+0.01
C
C     INITIALIZE DISCHARGE DATA AND STORE ON SCRATCH FILE
C
      IF(NOWY.EQ.1) GO TO 800
      DO 400 I=1,372
  400 Q(I)=-999.
      WRITE(IRWY,REC=IXWY) (Q(I),I=1,372)
      IXWY=IXWY+1
      WRITE(IRWY,REC=IXWY) (Q(I),I=1,372)
C
C     STORE INITIAL CARRYOVER ON SCRATCH FILE.
C
      JW=1
      IW=372
      DO 500 I=1,IREC
      IX=IXWY+I
      IF(I.EQ.IREC) IW=IWS
      WRITE(IRWY,REC=IX) (W(L),L=JW,IW)
      JW=JW+372
      IW=IW+372
  500 CONTINUE
C
C.......................................................................
C
C     DEBUG OUTPUT - PRINT PO().
  800 IF(IBUG.EQ.0) GO TO 810
      WRITE(IODBUG,940) IUSEP,NOFILL,NOWY
  940 FORMAT(1H0,42HCONTENTS OF PO ARRAY. NUMBER OF PO VALUES=,I3,5X,7HN
     1OFILL=,I2,6H NOWY=,I2)
      WRITE(IODBUG,942) (PO(I),I=1,IUSEP)
  942 FORMAT(1H0,15F8.3)
C
C.......................................................................
C
C     CONTENTS OF THE PO ARRAY - STATISTICS OPERATION
C
C     POSITION           CONTENTS
C        1        VERSION NUMBER OF THE OPERATION
C        2-6      AREA NAME
C        7        DRAINAGE AREA
C        8        SIMULATED Q TIME SERIES I.D.
C        9-10     SIMULATED Q TIME SERIES TYPE
C       11        SIMULATED Q TIME SERIES TIME INTERVAL
C       12-13     OBSERVED Q TIME SERIES I.D.
C       14        OBSERVED Q TIME SERIES TYPE
C       15        OBSERVED Q TIME SERIES TIME INTERVAL
C       16        YEARLY STATISTICS OPTION SWITCH
C       17        QUARTERLY ACCUMULATION TABLE SWITCH
C       18        CUMMULATIVE FREQUENCY TABLE SWITCH
C       19        Q-EXCEEDENCE TABLE SWITCH
C       20        NUMBER OF W() SPACES NEEDED BY THIS OPERATION
C       21        FIRST AVAILABLE RECORD FOR THIS OPERATION ON THE
C                 SCRATCH FILE
C       22-27     FLOW INTERVALS
C       28        NUMBER OF PO() VALUES
C.......................................................................
  810 RETURN
      END
