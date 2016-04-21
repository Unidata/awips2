C$PRAGMA C (get_apps_defaults)
C MODULE OPTMN
C-----------------------------------------------------------------------
C
C  THIS IS THE MAIN CONTROLLING ROUTINE FOR PROGRAM OPT3.
C
      SUBROUTINE OPTMN (TS,MTS,P,MP,C,MC,T,MT,D,MD,
     *   OA,MOA,POLD,MOLDP,COLD,MOLDC)
C
      integer, parameter::MaxNopt=100
	  integer, parameter::MaxNpg=2*MaxNopt+1,MaxNpt=MaxNopt*MaxNpg
C
      DIMENSION TS(MTS),P(MP),C(MC),D(MD)
      DIMENSION OA(MOA),POLD(MOLDP),COLD(MOLDC)
C      DIMENSION NDAYS(12),A(16),ILOCOA(16)
      DIMENSION NDAYS(12),A(MaxNopt),ILOCOA(MaxNopt)
      DIMENSION OLDOPN(2)
      INTEGER T(MT)
      CHARACTER*80 INFILE,OFILE
      CHARACTER*128 rmresjFile    ! File name of RES-J fs5file
ckwz      CHARACTER*128 user   ! user name
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
ckwz change user to UserHome for the TEMP.RESJ file
C
      INCLUDE 'uiox'
      INCLUDE 'updaio'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/errdat'
      INCLUDE 'common/calbrt'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fpltab'
      INCLUDE 'common/fwyds'
      INCLUDE 'clbcommon/cprces'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/fsacpr'
      INCLUDE 'common/modscb'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/oplist'
      INCLUDE 'ocommon/optts'
      INCLUDE 'ocommon/opschm'
      INCLUDE 'ocommon/odrop'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/optmn.f,v $
     . $',                                                             '
     .$Id: optmn.f,v 1.12 2004/05/05 15:24:14 hank Exp $
     . $' /
C    ===================================================================
C
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA IBUG/4HOPT /
      DATA NUGPRT/0/
C
C
      IOPNUM=0
      CALL FSTWHR ('OPTMN   ',IOPNUM,OLDOPN,IOLDOP)
C
C  SET I/O UNIT NUMBERS.
      IN=5
      IPR=6
      IPU=7
      ITUNIT=74
      ITREC=0
C
      NOEX=0
      NOWT=0
      NOPROT=1
      NOSNOW=0
      IPRSNW=0
      IPRSAC=0
      IPLHY=0
      IPRHY=0
      NOLIST=0
      IFPR=-1
C      MA=16
C      MILOC=16
      MA=MaxNopt
      MILOC=MaxNopt
      LEFTOA=MOA
      IERROR=0
      NCARDS=0
C
C  INITIALIZE PROCES COMMON BLOCK VALUES
      IPROC=1
      MYSEQ=1
C
C  INITIALIZE WATER YEAR SCRATCH FILE VARIABLE
      IDEFWY=0
C
C  SET PROG COMMON BLOCK VALUES
      MAINUM=4
      NDD=31
C
C  Parse the command line arguments
      INFILE = ' '
      OFILE  = ' '
      CALL MCPARSE ( INFILE, OFILE )
      IF (INFILE.EQ.' ') THEN
         WRITE (IPR,*) 'Enter input file name'
         READ (*,*) INFILE
         ENDIF
      IF (OFILE.EQ.' ') THEN
         WRITE (IPR,*) 'Enter output file name'
         READ (*,*) OFILE
         ENDIF
C
C  Set platform-dependent values
      CALL SETPLAT ( IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to get platform'
         CALL STOP
         ENDIF
C
C  Open the I/O files.
C  Open the output file as soon as possible so that it can hold
C  debug and error messages.
      CALL RSFILE ( 'STDIN ', ISTDIN, IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to reserve unit ',
     *      'number for STDIN'
         STOP
         ENDIF
      CALL RSFILE ( 'STDOUT ', ISTDOUT, IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to reserve unit ',
     *     'number for STDOUT'
         STOP
         ENDIF
      CALL RSFILE ( 'STDERR ', ISTDERR, IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to reserve unit ',
     *      'number for STDERR'
         STOP
         ENDIF
      CALL OPFILE ( OFILE, 'OUTPUT ', 'SEQUENTIAL', 'UNKNOWN',
     +              'FORMATTED', 0, IPR, IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to open output file'
         STOP
         ENDIF
      IODBUG = IPR
      IF ( INFILE .eq. ' ' ) THEN
         WRITE(ISTDERR,*) '**ERROR** No input file specified.'
         STOP
         ENDIF
      CALL CANRD ( INFILE, ISTAT )
      IF ( ISTAT .eq. 0 ) THEN
         WRITE(UE,*) '**ERROR** Cannot read input file.'
         WRITE(ISTDERR,*) '**ERROR** Cannot read input file.'
         STOP
         ENDIF
      CALL OPFILE ( INFILE, 'INPUT ', 'SEQUENTIAL', 'OLD', 'FORMATTED',
     +              0, IN, IERR )
      IF (IERR.GT.0) THEN
         WRITE(ISTDERR,*) '**ERROR** Unable to open input file'
         STOP
         ENDIF
      IF ( OFILE .eq. ' ' ) THEN
         WRITE(ISTDERR,*) '**ERROR** No output file specified.'
         STOP
         ENDIF
C
C  PRINT PAGE HEADER
      LP=IPR
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (IPR)
      CALL USETO1 ('NOPAGHDR',IERR)
C      
      INDERR=0
C
C  READ ALL INPUT CARDS AND SET UP MAIN ARRAYS.
      CALL CARDS(TS,MTS,P,MP,C,MC,T,MT,D,MD,NWORK)
C
C  READ OPT CARDS AND SET UP OA ARRAY.
      CALL OPCARD(P,MP,OA,MOA,LEFTOA,NPARM,ILOCOA,MILOC,A,MA)
C
C  CHECK IF ERRORS HAVE OCCURRED READING INPUT CARDS
      IF (NERRS.NE.0) THEN
         WRITE (IPR,10)
10    FORMAT ('0**ERROR** ERRORS OCCURRED READING INPUT CARDS.')
         CALL ERROR
         INDERR=1
         ENDIF
C
C  CHECK ARRAY SIZES
      INDERR=0
      IF (MOLDP.LT.MP) THEN
         WRITE (IPR,20) 'OLDP',MOLDP,'P',MP
20    FORMAT ('0**ERROR** NUMBER OF WORDS IN ARRAY ',A,' (',I5,') ',
     *   'IS LESS THAN THE NUMBER OF WORDS IN ARRAY ',A,' (',I5,').')
         CALL ERROR
         INDERR=1
         ENDIF
      IF (MOLDC.LT.MC) THEN
         WRITE (IPR,20) 'OLDC',MOLDC,'C',MC
         CALL ERROR
         INDERR=1
         ENDIF
C         
      IF (INDERR.EQ.1) CALL STOP
C
C  STORE INITIAL P AND C VALUES FOR USE IN CARRYOVER TRANSFER
      DO 30 I=1,MP
         POLD(I)=P(I)
30       CONTINUE
      DO 40 I=1,MC
         COLD(I)=C(I)
40       CONTINUE
C
C  COMPUTE NUMBER OF MONTHS IN SIMULATION PERIOD
      NOMO=(LMO+LYR*12)-(IMO+IYR*12)+1-NBUFMO
C
C  INITIALIZE STARTING DATE AND TIME
      IFINSH=0
      IPASS1=1
C      
50    MONTH=IMO
      IYEAR=IYR
      IDA=IDARUN
      IHZERO=IHRRUN
      IEND=0
      MONTH1=1
      IF (NDRP.GT.0) IDRP=1
      IF (MAINUM.EQ.3) NOPROT=0
C
C.......................................
C
C  BEGIN MAIN MONTHLY LOOP
C
C  FIND LAST DAY AND HOUR - ALSO INITIAL DAY OF DATA
60    IF ((IYEAR.EQ.LYR).AND.(MONTH.EQ.LMO)) GO TO 70
         LAST=NDAYS(MONTH)
         IF ((MONTH.EQ.2).AND.((IYEAR/4)*4.EQ.IYEAR)) LAST=LAST+1
         CALL FCTZC (100,0,TZCODE)
         CALL JULDA (LDA,LHR,MONTH,LAST,IYEAR,24,100,0,TZCODE)
         GO TO 80
70    LDA=LDARUN
      LHR=LHRRUN
      IEND=1
80    CALL FCTZC (100,0,TZCODE)
      CALL JULDA (IDADAT,I,MONTH,1,IYEAR,1,100,0,TZCODE)
      IF (IFBUG(IBUG).EQ.1) THEN
         WRITE (IODBUG,90) MONTH,IYEAR,IDA,IHZERO,
     *      LDA,LHR,IDADAT,NOEX,IEND
90    FORMAT (' MONTH=',I2,' YEAR=',I4,' IDA=',I6,' IHZERO=',I6,
     *   ' LDA=',I6,' LHR=',I6,' IDADAT=',I6,' NOEX=',I1,'IEND=',I1)
         ENDIF
C
C  READ INPUT TIME SERIES FOR THE MONTH.
      IF (IPASS1.EQ.0) GO TO 110
C
      CALL FCTSRD (TS,MTS,D,MD,IHZERO,IERR,JDAMIS,JHRMIS,NWORK)
C
C  IF ERROR OCCURRED IN FCTSRD - EXECUTION SHOULD STOP,
C  BUT READING OF TIME SERIES SHOULD CONTINUE.
      IF (NOEX.EQ.1) GO TO 160
      IF (IERR.EQ.0) GO TO 110
         WRITE (IPR,100)
100   FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS IN READING DATA. ',
     1 'EXECUTION WILL STOP BUT READING OF TIME SERIES WILL CONTINUE.')
         CALL ERROR
         NOEX=1
         GO TO 160
C
C  IF IPASS1=1, WRITE TIME SERIES DATA TO TEMPORARY FILE
C  IF IPASS1=0, READ TIME SERIES DATA FROM TEMPORARY FILE
110   CALL OPREAD (D,MD,IPASS1,ITUNIT,ITREC)
C
C  EXECUTE THE OPERATIONS TABLE FOR THE MONTH
      NERROR=NERRS
      CALL FDRIVE (P,MP,C,MC,T,MT,TS,MTS,D,MD,IHZERO)
      IF (NERRS.EQ.NERROR) GO TO 130
C     ERROR OCCURRED IN FDRIVE - EXECUTION SHOULD STOP, BUT READING
C     OF TIME SERIES SHOULD CONTINUE.
         WRITE (IPR,120)
120   FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS IN AN OPERATION ',
     1 'EXECUTION WILL STOP BUT READING OF TIME SERIES WILL CONTINUE.')
         CALL ERROR
         NOEX=1
         GO TO 160
130   IF (NOWT.EQ.1) GO TO 150
      IF (MAINUM.EQ.4) GO TO 150
C
C  WRITE OUTPUT TIME SERIES FOR THE MONTH
      CALL FCTSWT (TS,MTS,D,MD,IHZERO,NWORK,IERR)
      IF (IERR.EQ.0) GO TO 160
C     ERROR OCCURRED IN FCTSWT -- WRITING OF TIME SERIES SHOULD STOP,
C     BUT EXECUTION CAN CONTINUE.
         WRITE (IPR,140)
140   FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS WRITING DATA ',
     1 'WILL STOP BUT READING DATA AND EXECUTION WILL CONTINUE.')
         CALL ERROR
         NOWT=1
C
150   IF (MAINUM.EQ.3) GO TO 160
C
C  CALL MONTHLY STAT ROUTINE TO COMPUTE OBJECTIVE FUNCTION.
      CALL OPSTAT (D(LDDS),D(LDDO),NOMO,MONTH1)
C
160   MONTH1=0
C     CHECK FOR END OF RUN.
      IF (IEND.EQ.1) GO TO 180
C
C  INCREMENT TO THE NEXT MONTH
      MONTH=MONTH+1
      IF (MONTH.LE.12) GO TO 170
         MONTH=1
         IYEAR=IYEAR+1
170   CALL FCTZC (100,0,TZCODE)
      CALL JULDA (IDA,I,MONTH,1,IYEAR,1,100,0,TZCODE)
      IHZERO=0
      GO TO 60
C
C  END OF MAIN MONTHLY LOOP
C
C.......................................
C

180   IF (NOEX.EQ.1) GO TO 210
      IF (MAINUM.EQ.3) GO TO 200
C     CALL OPTIMIZATION SCHEME ROUTINE.
      CALL OPTYPE (A,MA,OA,MOA,ILOCOA,MILOC,IZY,IPASS1,IFINSH,NPARM,
     * NCOUN,NN,IPMOVE)
      IPASS1=0
C
C     RESET PARAMETERS VALUES IN P ARRAY
      CALL ORESET (P,MP,OA,MOA,LEFTOA,A,MA,IZY,ILOCOA,MILOC,NPARM,
     * NCOUN,NN,IFINSH,POLD,IPMOVE,IERROR)
      IF (IERROR.GT.0) GO TO 210
C
C  SET NEW C ARRAY EQUAL TO OLD C ARRAY PRIOR TO COX
      DO 190 I=1,MC
         C(I)=COLD(I)
190      CONTINUE
C
C  PERFORM CARRYOVER TRANSFER.
      CALL COXDRV (P,MP,C,MC,POLD,MP,COLD,MC,D,MD)
C
      IF (IFINSH.NE.1) GO TO 50
C
C  PRINT OPTIMIZED PARAMETER VALUES
      CALL OPRINT (P,MP,C,MC,POLD,COLD,ILOCOA,MILOC,OA,MOA,NPARM,T,MT,
     * A,MA,NUGPRT)
C
C  PERFORM FINAL MCP RUN IF REQUESTED
      IF (MCP.NE.1) GO TO 200
      MAINUM=3
      GO TO 50
C
C  PUNCH OPERATIONS TABLE IF REQUESTED
200   IF (IPUNCH.NE.1) GO TO 210
      CALL PUOPTB( P,MP,C,MC,T,MT,TS,MTS)
C
C  END OF RUN
210   CALL FSTWHR( OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
C  Delete the temporary file
      IPASS1=-1
      CALL OPREAD (D,MD,IPASS1,ITUNIT,ITREC)
C
CHDH  Commented out remove stuff, because the call to stop will call
CHDH  ofscln, which should remove the temp files.
ckwz      call get_user( user,len_user )
ckwz      rmresjFile = 'rm ' // user(1:len_user) // '.TEMP.RESJ'    
CHDH     TmpNum=4;
CHDH     call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
CHDH     rmresjFile = 'rm ' // UserHome(1:HomeDirLength) // '/TEMP.RESJ'
C      call system( "echo delete temporary file $LOGNAME.TEMP.RESJ" )
CHDH   call system( rmresjFile )
C
C  CLOSE ALL DATACARD FILES
      CALL CCLOSL
C
      CALL STOP
C
      RETURN
C
      END
