C MEMBER FGETFG
C  (from old member FCFGETFG)
C ---------------------------------------------------------------------
C
C                             LAST UPDATE: 09/22/95.15:37:05 BY $WC21DT
C
      SUBROUTINE FGETFG(FGNUM,FGID0,FLAG)
C
C ----------------------------------------------------------------------
C
C    SUBROUTINE FGETFG USED TO FILL /FCFGS/, THE INPUT COULD BE EITHER
C FG IDENTIFIER OR THE SEQENTIAL NUMBER IN FILE FCFGSTAT.
C
C    ARGUMENT LIST:
C
C       FGNUM  I/O   SEQUENTIAL NUMBER IN FILE FCFGSTAT
C       FGID0  I/O   ID OF A FORECAST GROUP
C       FLAG    I    A FLAG INDICATES WHICH BE THE INPUT
C                    =0, FGNUM INPUT
C                    =1, FGID0 INPUT
C
C ----------------------------------------------------------------------
C
      DIMENSION FGID0(2)
      INTEGER  FGNUM,FLAG
C.......................................................................
C..
C..   COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcfgid'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgetfg.f,v $
     . $',                                                             '
     .$Id: fgetfg.f,v 1.2 1996/01/17 18:53:24 page Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
C  TRACE LEVEL=2
C
      IF(ITRACE.GE.2) WRITE(IODBUG,905)
  905 FORMAT('   *** ENTER FGETFG ***')
C
C  CHECK IF FCFGID ALREADY IN CORE, READ FROM DISC IF NEED.
C
      IF(IHASRD.EQ.1.OR.FLAG.EQ.0) GOTO 20
        I=1
  10    CALL UREADT(KFFGST,I,FGID,IERR)
        FCFGID(1,I)=FGID(1)
        FCFGID(2,I)=FGID(2)
        IF(I.EQ.1) NFGREC=IDUMYG
        I=I+1
        IF(I.LE.NFGREC) GOTO 10
        IHASRD=1
C
C
   20 IF (FLAG.NE.0) GOTO 30
      IF (FGNUM.GT.0.AND.FGNUM.LE.NFGREC) GOTO 100
C
C  FORECAST GROUP NO. IS INVALID
      WRITE(IPR,910) FGNUM,NFGREC
  910 FORMAT('0**ERROR** FROM FGETFG, THE FG NO. ',I3,' IS BEYOND TH
     .E RANGE 1 TO ',I3,' .')
      FGNUM=-1
      GOTO 900

   30 CONTINUE
      IF(FLAG.EQ.1) GOTO 50
C  FLAG NOT EQUAL TO 0 NOR 1
      WRITE(IPR,930) FLAG
  930 FORMAT('0**ERROR** FROM FGETFG, FLAG=',I2,' IS NOT A VALID VAL
     .UE!')
      FGNUM=-1
      GOTO 900
C
C  CHECK TO SEE IF THIS FG IN CORE
   50 CONTINUE
      IED=MIN0(100,NFGREC)
      DO 40 I=1,IED
      IF(FCFGID(1,I).EQ.FGID0(1).AND.FCFGID(2,I).EQ.FGID0(2)) GOTO 60
   40 CONTINUE
C
C  CHECK IF MORE FGS IN FILE
      IF(IED.GE.NFGREC) GOTO 80
        IST=IED+1
        DO 70 I=IST,NFGREC
        CALL UREADT(KFFGST,I,FGID,IERR)
        IF(FGID(1).NE.FGID0(1).OR.FGID(2).NE.FGID0(2)) GOTO 70
        FGNUM=I
        GOTO 900
   70   CONTINUE
C
C  FGID0 NOT FOUND IN THE FORECAST GROUP TABLE
   80 WRITE(IPR,920) FGID0
  920 FORMAT('0**ERROR** FROM FGETFG, THE FG ID ',2A4,' IS NOT VALID
     .')
      FGNUM=0
      GOTO 900

   60 FGNUM=I
      IF(FGID0(1).EQ.FGID(1).AND.FGID0(2).EQ.FGID(2)) GOTO 900
  100 CALL UREADT(KFFGST,FGNUM,FGID,IERR)
C
  900 IF(ITRACE.GE.2) WRITE(IODBUG,906)
  906 FORMAT('   *** EXIT FGGETFG ***')
      RETURN
      END
