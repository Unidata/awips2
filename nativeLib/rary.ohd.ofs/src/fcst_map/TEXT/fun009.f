C MODULE FUN009
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/15/94.15:10:11 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FUN009
C.......................................
C     THIS IS THE MAIN ROUTINE FOR THE MAP FUNCTION
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- JUNE 1983
C.......................................
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON/WHERE/IA(2),IND,SUB(2)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/ERRDAT/IOERR,NWARN,NERRS
      COMMON/KILLCD/KLCODE,NKILLS,KLSTOP
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/FUN5CB/IFIRST,ICPUF5
      COMMON/XCPU/ICKCPU,ICPU,KCPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/fun009.f,v $
     . $',                                                             '
     .$Id: fun009.f,v 1.1 1995/09/17 18:59:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HFUN0,4H09  /
      DATA XCPU/4HXCPU/
C.......................................
C     SET WHERE COMMON BLOCK
      IND=-1
      SUB(1)=SNAME(1)
      SUB(2)=SNAME(2)
C.......................................
C     INITIAL VALUES AND CLOSE OPEN FILES
      NWARN=0
      NERRS=0
      IKILLS=NKILLS
C.......................................
C     GET LOGICAL UNIT NUMBERS AND DEBUG CONTROL.
      CALL XGTSYS
C     RESET WHERE COMMON BLOCK
      IND=-1
      SUB(1)=SNAME(1)
      SUB(2)=SNAME(2)
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.1) WRITE(IOPDBG,904)
 904  FORMAT(1H0,23H** MAP FUNCTION ENTERED)
C.......................................
C     INITIALIZE CPU CHECK IF ON.
      ICKCPU=0
      IF (IPBUG(XCPU).EQ.1) ICKCPU=1
      IF (ICKCPU.EQ.0) GO TO 100
      IF (IFIRST.EQ.-45678) GO TO 105
      ICPU=0
      KCPU=0
      CALL URTIMR(LAPSE,KCPU)
      WRITE(IOPDBG,900)
  900 FORMAT(1H0,37HTOTAL CPU SET TO ZERO AT START OF MAP)
      GO TO 100
  105 CALL URTIMR(LAPSE,ICPUF5)
      KCPU=ICPUF5
      ICPU=KCPU
      ELAPSE=LAPSE/100.
      TCPU=KCPU/100.
      WRITE(IOPDBG,901) ELAPSE,TCPU
  901 FORMAT(1H0,41HAT START OF MAP--CPU CHECK--ELAPSED TIME=,F6.2,
     1  1X,4HSEC.,4X,11HTOTAL TIME=,F7.2,1X,4HSEC.)
C.......................................
C     GET VALUES OF MAP CONTROL AND DISPLAY VARIALBLES FROM HCL.
  100 CALL XGTOPT(IERR)
C     RESET WHERE COMMON BLOCK
      IND=-1
      SUB(1)=SNAME(1)
      SUB(2)=SNAME(2)
      IF(IERR.EQ.1) GO TO 199
C.......................................
C     INITIALIZE PPDB R/W CONTROL VALUES
      CALL RPPDCO(ISTAT)
      IF(ISTAT.EQ.0) GO TO 115
      WRITE(IPR,905)
 905  FORMAT(1H0,51H**FATAL ERROR** CANNOT INITAILIZE PPDB R/W ROUTINES)
      CALL KILLFN(8HMAP     )
      GO TO 199
C.......................................
C     INITIALIZE PPPDB R/W CONTROL VALUES
  115 CALL RPPPCO(ISTAT)
      IF(ISTAT.EQ.0) GO TO 120
      WRITE(IPR,906)
 906  FORMAT(1H0,52H**FATAL ERROR** CANNOT INITIALIZE PPPDB R/W ROUTINES
     1)
      CALL KILLFN(8HMAP     )
      GO TO 199
C.......................................
C     PERFORM MAP COMPUTATIONS
120   CALL XCNTRL
C.......................................
C     RESET WHERE COMMON BLOCK
      IND=-1
      SUB(1)=SNAME(1)
      SUB(2)=SNAME(2)
C.......................................
C     CHECK TRACE LEVEL AND CALL STOP
  199 IF (ICKCPU.EQ.0) GO TO 198
      CALL XCKCPU(8HEND OF  ,8HMAP     )
      IF (IFIRST.EQ.-45678) ICPUF5=KCPU
      TCPU=(KCPU-ICPU)/100.
      WRITE(IOPDBG,902) TCPU
  902 FORMAT(1H0,36HTOTAL CPU TIME FOR THE MAP FUNCTION=,F7.2,1X,4HSEC.)
  198 IF(IPTRCE.GE.1) WRITE(IOPDBG,907)
 907  FORMAT(1H0,20H** EXIT MAP FUNCTION)
      IF (NKILLS.GT.IKILLS) RETURN
      CALL STOPFN(8HMAP     )
C.......................................
      RETURN
      END
