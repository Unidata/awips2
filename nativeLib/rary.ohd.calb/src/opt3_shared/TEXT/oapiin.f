C  MEMBER OAPIIN
C
      SUBROUTINE OAPIIN(OPID,NUMOP,OPNEW,PARM,DELTA,CHECKL,CHECKU,
     *       NOTHER,OA,MOA,P,MP,A,MA,NPARM,LOA,LEFTOA,IERO)
C***********************************************************
C  THIS SUBROUTINE FILLS THE A AND OA ARRAYS WITH INFORMATION
C           NEEDED BY OPT3 FOR THE API-CONT OPERATION.
C***********************************************************
C  INITIALLY WRITTEN BY - ERIC ANDERSON, HRL - JULY 1990
C***********************************************************
      DIMENSION P(MP),A(MA),OA(MOA),PARM(2),OPID(2),OPNEW(2)
      DIMENSION PMNAM(2,13),ABSL(13),ABSU(13),IRD(13),LOCPO(13)
      DIMENSION DIFF(3),RATIO(3),OLDOPN(2),OPIDD(2),OPNEWW(2),RDIFF(3)
C
C  COMMON BLOCKS
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'ocommon/opschm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/oapiin.f,v $
     . $',                                                             '
     .$Id: oapiin.f,v 1.2 1996/07/11 20:44:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C  DATA STATEMENTS
C
      DATA PMNAM/4HAIXW,4H    ,4HAIXD,4H    ,4HCW  ,4H    ,
     *           4HCD  ,4H    ,4HCS  ,4H    ,4HSMIX,4H    ,
     *           4HFRSX,4H    ,4HBFIM,4H    ,4HAICR,4H    ,
     *           4HCG  ,4H    ,4HCP  ,4H    ,4HCT  ,4H    ,
     *           4HCSOI,4HL   /
      DATA ABSL/0.0,0.0,0.1,0.1,0.5,0.0,0.0,0.0,0.0,0.001,0.01,0.0001,
     *  0.01/
      DATA ABSU/25.0,50.0,0.99,0.99,10.0,20.0,1.0,100.0,25.0,0.99,
     *  10.0,1.0,10.0/
      DATA IRD/0,0,1,1,1,0,1,1,1,1,0,1,1/
      DATA LOCPO/3,4,5,6,8,7,9,3,4,5,11,15,12/
      DATA DIFF/4H DIF,4HFERE,4HNCE /
      DATA RATIO/4H   R,4HATIO,4H    /
      DATA IBUG/4HAPIC/
C***********************************************************
C  TRACE LEVEL FOR THIS SUBROUTINE = 1
C***********************************************************
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
 900  FORMAT (1H0,'** ENTER OAPIIN')
C
      CALL FSTWHR('OAPIIN ',0,OLDOPN,IOLDOP)
C***********************************************************
C  INITIAL VARIABLES
C***********************************************************
      LOCP=0
      IRGPM = 8
      IFGPM = 11
C***********************************************************
C  CHECK SPACE IN OA ARRAY
C***********************************************************
      IPLUS = 0
      IF (NOTHER.GT.0) IPLUS = 2
      IUSE = 10 + (NOTHER*4) + IPLUS
      IF (LEFTOA.GE.IUSE) GO TO 100
      WRITE (IPR,901)
 901  FORMAT (1H0,10X,'**ERROR** THIS PARAMETER REQUIRES MORE SPACE THAN
     * IS AVAILABLE IN THE OA ARRAY.')
      CALL ERROR
      IERO = 1
      GO TO 195
C***********************************************************
C  SPACE IS AVAILABLE--STORE VALUES IN OA AND A.
C     CHECK IF PARAMETER CAN BE OPTIMIZED.
C***********************************************************
 100  IPNUM = 0
      DO 110 I = 1,14
      IPNUM = IPNUM + 1
      IF ((PARM(1).EQ.PMNAM(1,I)).AND.(PARM(2).EQ.PMNAM(2,I))) GO TO 120
 110  CONTINUE
C
C  CANNOT OPTIMIZE THIS PARAMETER
C
  112 WRITE (IPR,902) PARM
 902  FORMAT(1H0,10X,'**WARNING**',1X,2A4,1X,
     *  'IS NOT A PARAMETER THAT CAN BE OPTIMIZED.')
      CALL WARN
 111  NPARM = NPARM - 1
      IF (NOTHER.EQ.0) GO TO 195
      DO 115 I = 1, NOTHER
      READ (IN,903) OPIDD, OPNEWW
 903  FORMAT (2A4,2X,2A4)
 115  CONTINUE
      GO TO 195
C
C  THIS PARAMETER CAN BE OPTIMIZED.
C     CHECK IF OPERATION EXISTS
C
 120  CALL FSERCH(NUMOP,OPNEW,LOCP,P,MP)
      IF (LOCP.NE.0) GO TO 125
C
C  OPERATION NOT DEFINED
C
      WRITE (IPR,904) OPID, OPNEW
 904  FORMAT(1H0,10X,'**WARNING** OPERATION(ID=',2A4,1X,
     *      'NAME=',2A4,') HAS NOT BEEN DEFINED.')
      CALL WARN
      GO TO 111
C
C   CHECK IF FROZEN GROUND SELECTED
C
  125 IF (IPNUM.LT.IFGPM) GO TO 130
      LFRZE=P(LOCP+23)
      IF (LFRZE.GT.0) GO TO 130
      WRITE(IPR,905) OPID,OPNEW
  905 FORMAT(1H0,10X,'**WARNING**  THE FROZEN GROUND OPTION HAS NOT BEEN
     * SELECTED FOR',1X,2A4,1X,2A4)
      GO TO 112
C
C  BEGIN TO FILL ARRAY SPACE
C
 130  OA(LOA+1) = LOCP + 0.01
      OA(LOA+2) = NUMOP + 0.01
      OA(LOA+3) = PARM(1)
      OA(LOA+4) = PARM(2)
      IF (IPNUM.GE.IFGPM) GO TO 132
      IF (IPNUM.GE.IRGPM) GO TO 131
      LRSPM = P(LOCP+25)
      IPLOC = LOCP+LRSPM+LOCPO(IPNUM)-2
      GO TO 133
 131  LRGPM = P(LOCP+26)
      IPLOC = LOCP+LRGPM+LOCPO(IPNUM)-2
      GO TO 133
  132 IPLOC=LOCP+LFRZE+LOCPO(IPNUM)-2
  133 OA(LOA+5) = IPLOC + 0.01
      OA(LOA+6) = DELTA
      OA(LOA+10) = 1.01
      A(NPARM) = P(IPLOC)
C***********************************************************
C  CHECK UPPER AND LOWER BOUNDS
C***********************************************************
      IF (CHECKL.LT.CHECKU) GO TO 135
      WRITE(IPR,906)PARM,CHECKL,CHECKU,ABSL(IPNUM),ABSU(IPNUM)
 906  FORMAT(1H0,10X,'**WARNING** FOR PARAMETER(',2A4,
     * ') LOWER LIMIT(',F7.4,
     * ') IS GREATER OR EQUAL TO THE UPPER LIMIT(',F7.3,
     * ').',/16X,'THE USER SPECIFIED BOUNDS HAVE BEEN REPLACED WITH THE 
     *ABSOLUTE LIMITS(',F7.4,',',F7.3,').')
      CALL WARN
      PML = ABSL(IPNUM)
      PMU = ABSU(IPNUM)
      GO TO 145
 135  IF (CHECKL.GE.ABSL(IPNUM)) GO TO 139
      WRITE(IPR,907) CHECKL,PARM,ABSL(IPNUM)
 907  FORMAT(1H0,10X,'**WARNING** THE SPECIFIED LOWER LIMIT(',
     *   F7.4,') FOR PARAMETER(',2A4,')',/16X,
     *   'HAS BEEN REPLACED BY THE ABSOLUTE LOWER LIMIT(',F7.4,').')
      CALL WARN
      PML = ABSL(IPNUM)
      GO TO 140
 139  PML = CHECKL
 140  IF (CHECKU.LE.ABSU(IPNUM)) GO TO 144
      WRITE (IPR,908) CHECKU,PARM,ABSU(IPNUM)
 908  FORMAT(1H0,10X,'**WARNING** THE SPECIFIED UPPER LIMIT(',
     * F7.3,') FOR PARAMETER(',2A4,')',/16X,
     * 'HAS BEEN REPLACED BY THE ABSOLUTE UPPER LIMIT(',F7.3,').')
      CALL WARN
      PMU = ABSU(IPNUM)
      GO TO 145
 144  PMU = CHECKU
  145 OA(LOA+7) = PML
      OA(LOA+8) = PMU
C***********************************************************
C  CHECK THAT INITIAL VALUE IS NOT TOO CLOSE TO BOUNDS
C***********************************************************
      ICLOSE = 0
      CALL OBNDCK(PARM,OPID,OPNEW,A(NPARM),DELTA,
     *      OA(LOA+7),OA(LOA+8),NPER,ICLOSE)
      IF(ICLOSE.NE.1) GO TO 155
      GO TO 111
C***********************************************************
C  PRINT PARAMETER INFORMATION
C***********************************************************
 155  WRITE(IPR,909)NPARM,OPID,OPNEW,PARM,A(NPARM),PML,PMU,OA(LOA+6)
 909  FORMAT(1H0,I2,'.',1X,2A4,2X,2A4,3X,2A4,2X,
     *      F9.3,1X,F9.3,1X,F9.3,5X,F9.3)
C***********************************************************
C  CHECK TO SEE IF OTHER AREAS ARE AFFECTED.
C***********************************************************
      N = NOTHER
      IF (NOTHER.EQ.0) GO TO 180
      OA(LOA+11) = ABSL(IPNUM)
      OA(LOA+12) = ABSU(IPNUM)
      NOTH = 0
      DO 170 I = 1,NOTHER
      READ(IN,903) OPIDD,OPNEWW
C
C  CHECK OPERATION ID AND GET NUMBER
C
      CALL FOPCDE(OPIDD,NUMOPP)
      IF(NUMOPP.EQ.24) GO TO 171
      WRITE(IPR,910) OPIDD
 910  FORMAT(1H0,10X,'**WARNING** THE IDENTIFIER(',2A4,
     *  ') IS NOT API-CONT.  THIS OTHER PARAMETER IS NOT OPTIMIZED.')
      CALL WARN
      N = N - 1
      GO TO 170
 171  LOCPP = 0
      CALL FSERCH(NUMOPP,OPNEWW,LOCPP,P,MP)
      IF(LOCPP.NE.0) GO TO 160
      WRITE(IPR,904)OPIDD,OPNEWW
      CALL WARN
      N = N - 1
      GO TO 170
C
C   CHECK IF FROZEN GROUND SELECTED
C
  160 IF (IPNUM.LT.IFGPM) GO TO 172
      LFRZE=P(LOCPP+23)
      IF (LFRZE.GT.0) GO TO 172
      WRITE(IPR,905) OPIDD,OPNEWW
      CALL WARN
      N=N-1
      GO TO 170
C
C  STORE NEEDED INFORMATION
C
 172  NOTH = NOTH+1
      LN = 12+(NOTH-1)*4
      OA(LOA+LN+1) = IRD(IPNUM)+0.01
      OA(LOA+LN+2) = LOCPP+0.01
      IF (IPNUM.GE.IFGPM) GO TO 165
      IF(IPNUM.GE.IRGPM) GO TO 173
      LRSPM = P(LOCPP + 25)
      IPLOC = LOCPP+LRSPM+LOCPO(IPNUM)-2
      GO TO 174
 173  LRGPM = P(LOCPP+26)
      IPLOC = LOCPP+LRGPM+LOCPO(IPNUM)-2
      GO TO 174
 165  IPLOC=LOCPP+LFRZE+LOCPO(IPNUM)-2
  174 OA(LOA+LN+3) = IPLOC+0.01
      OA(LOA+LN+4) = 1.01
C***********************************************************
C  PRINT INFORMATION FOR OTHER OPERATIONS
C***********************************************************
      IF (IRD(IPNUM).EQ.1) GO TO 176
      DO 175 J= 1,3
 175  RDIFF(J) = DIFF(J)
      GO TO 178
 176  DO 177 J=1,3
 177  RDIFF(J) = RATIO(J)
 178  IF (I.NE.1) GO TO 179
      WRITE(IPR,911) OPNEWW,RDIFF
 911  FORMAT(1H+,88X,2A4,15X,3A4)
      GO TO 170
 179  WRITE(IPR,912)OPNEWW,RDIFF
 912  FORMAT(1H ,88X,2A4,15X,3A4)
 170  CONTINUE
 180  OA(LOA+9) = N+0.01
      IUSE = 10+(N*4) + IPLUS
C***********************************************************
C  CHECK IF DEBUG REQUESTED
C***********************************************************
      IF(IFBUG(IBUG).NE.1) GO TO 190
      WRITE(IODBUG,913) OA(LOA+3),OA(LOA+4)
 913  FORMAT(1H0,'OAPIIN DEBUG--PARAMETER=',2A4)
      I = LOA+1
      L = LOA+IUSE
      WRITE(IODBUG,914)(OA(J),J=I,L)
 914  FORMAT(1H0,10F10.3)
C***********************************************************
C  UPDATE TOTAL SPACE USED IN THE OA ARRAY
C***********************************************************
 190  LOA = LOA + IUSE
      LEFTOA=LEFTOA-IUSE
C***********************************************************
C    EXIT  --  CHECK TRACE LEVEL
C***********************************************************
 195  CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE(IODBUG,915)
 915  FORMAT(1H0,'** EXIT OAPIIN')
C
      RETURN
      END
