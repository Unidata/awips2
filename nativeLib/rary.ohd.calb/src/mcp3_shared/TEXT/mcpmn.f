C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (GETPID2)
C$PRAGMA C (datim3)
C MODULE MCPMN
C-----------------------------------------------------------------------
C
C  THIS IS THE MAIN CONTROLLING ROUTINE FOR PROGRAM MCP3.
C
      SUBROUTINE MCPMN (TS,MTS,P,MP,C,MC,T,MT,D,MD)
C
      DIMENSION TS(MTS),P(MP),C(MC),D(MD)
      INTEGER T(MT)
      DIMENSION NDAYS(12)
      CHARACTER*100 INFILE,OFILE
      CHARACTER*128 rmresjFile    ! File name of RES-J fs5file
ckwz      CHARACTER*128 user          ! character string to hold name of user
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
      
ckwz change user to UserHome for the TEMP.RESJ file
C
C  CANNOT USE INCLUDES FOR FOLLOWING BECAUSE VARIABLE LP USED IN COMMONS
C  UIOX AND FLARYS
CCC      INCLUDE 'uio'
CCC      INCLUDE 'uiox'
      COMMON /UIO/ LPU,ICDU,LPDU,LPEU,ICDPUNU
      COMMON /UIOX/ LPX,ICDX,LPDX,LPEX,ICDPUNX,LSYSX
      INCLUDE 'updaio'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/errdat'
      INCLUDE 'common/calbrt'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fpltab'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/fsacpr'
      INCLUDE 'common/oplist'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/flarys'
      INCLUDE 'common/modscb'
      INCLUDE 'common/rdwtco'
      INCLUDE 'common/sionum'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fcsegn'
      INCLUDE 'clbcommon/cprces'
      INCLUDE 'common/cdate'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/mcpmn.f,v $
     . $',                                                             '
     .$Id: mcpmn.f,v 1.14 2004/05/05 15:23:41 hank Exp $
     . $' /
C    ===================================================================
C
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA IBUG/4HCALI/
      DATA IBLANK/4H    /
C
C
      NOEX=0
      NOWT=0
      IEND=0
      NOPROT=0
      NOSNOW=0
      IPRSNW=0
      NOFRZE=0
      IPRSAC=0
      IPLHY=0
      IPRHY=0
      NOLIST=0
      IFPR=-1
      NCARDS=0
C
C  INITIALZE CO I/O VARIABLES
      JRDCO = 0
      JWTCO = 0
C
C  SET I/O DATA SET UNIT NUMBERS
      IN=5
      IPR=6
      IPU=7
      ISTDERR=0
      ISTDIN=5
      ISTDOUT=6
C
C  SET DEFAULT AND INITIAL VALUES FOR DEBUG AND ERROR CONTROL
      ITRACE=0
      IDBALL=0
      NDEBUG=0
      NDEBGS=0
      IALL=0
      DO 10 I=1,20
         IDEBGS(I)=IBLANK
         IDEBUG(I)=0
10       CONTINUE
C
C  INITIALIZE PROCES COMMON BLOCK VALUES
      IPROC=1
      MYSEQ=1
C
C  INITIALIZE WATER YEAR SCRATCH FILE VARIABLE
      IDEFWY=0
C
C  SET PROG COMMON BLOCK VALUES
	call datim3( CDATE )
      MAINUM=3
      NDD=31

C  SET SEGID IN COMMON BLOCK FCSEGN TO BLANK
      IDSEGN(1)=IBLANK
      IDSEGN(2)=IBLANK
C
C  PARSE THE COMMAND LINE ARGUMENTS
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
C  set platform-dependent values
      CALL SETPLAT ( IERR )
      IF (IERR.GT.0) THEN
         WRITE (ISTDERR,*) '**ERROR** Unable to get platform'
         GO TO 170
         ENDIF
C
C  OPEN THE I/O FILES
      INDERR=0
      CALL RSFILE ( 'STDIN ', ISTDIN, IERR )
      IF (IERR.GT.0) THEN
         WRITE (ISTDERR,*) '**ERROR** Unable to get unit ',
     *         'number for STDIN.'
         INDERR=1
         ENDIF
      CALL RSFILE ( 'STDOUT ', ISTDOUT, IERR )
      IF (IERR.GT.0) THEN
         WRITE (ISTDERR,*) '**ERROR** Unable to get unit ',
     *      'number for STDOUT.'
         INDERR=1
         ENDIF
      CALL RSFILE ( 'STDERR ', ISTDERR, IERR )
      IF (IERR.GT.0) THEN
            WRITE (ISTDERR,*) '**ERROR** Unable to get unit ',
     *         'number for STDERR.'
         INDERR=1
         ENDIF
      CALL OPFILE ( OFILE, 'OUTPUT ', 'SEQUENTIAL', 'UNKNOWN',
     +              'FORMATTED', 0, IPR, IERR )
      IF (IERR.GT.0) THEN
         WRITE (ISTDERR,*) '**ERROR** Unable to open output file'
         INDERR=1
         ENDIF
      IODBUG = IPR
      IF (INFILE(1:1).EQ.' ') THEN
         WRITE (ISTDERR,*) '**ERROR** No input file specified.'
         CALL MCUSAGE
         INDERR=1
         ENDIF
      CALL CANRD ( INFILE, ISTAT )
      IF (ISTAT.EQ.0) THEN
         WRITE (UE,*) '**ERROR** Cannot read input file.'
         WRITE (ISTDERR,*) '**ERROR** Cannot read input file.'
         INDERR=1
         ENDIF
      CALL OPFILE ( INFILE, 'INPUT ', 'SEQUENTIAL', 'OLD', 'FORMATTED',
     +              0, IN, IERR )
      ICDU=IN
      ICDX=IN
      IF (IERR.GT.0) THEN
         WRITE (ISTDERR,*) '**ERROR** Unable to open input file.'
         INDERR=1
         ENDIF
      IF (OFILE(1:1).EQ.' ') THEN
         WRITE (ISTDERR,*) '**ERROR** No output file specified.'
         CALL MCUSAGE
         INDERR=1
         ENDIF
      IF (INDERR.EQ.1) GO TO 170
C
C  PRINT PAGE HEADER
      LPX=IPR
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (IPR)
      CALL USETO1 ('NOPAGHDR',IERR)
C
C  READ ALL INPUT CARDS AND SET UP MAIN ARRAYS
      CALL CARDS (TS,MTS,P,MP,C,MC,T,MT,D,MD,NWORK)
C
C  CHECK IF TO READ INPUT FROM CARRYOVER FILE
      IF (JRDCO.EQ.1) THEN
         CALL OPFILE ( RDCOFL, 'CO-INPUT ', ' ', 'OLD', ' ',
     +      0, LURDCO, IERR )
            IF (IERR.GT.0) THEN
               WRITE (IPR,*) '**ERROR** Unable to open CO input file.'
               GO TO 170
               ENDIF
            READ (LURDCO,*) LENCO
            IF (LENCO.GT.MC) THEN
               WRITE (IPR,*) '**ERROR** CO array in input file',
     +         ' too long (',LENCO,' > ',MC,')'
               GO TO 170
               ENDIF
            READ (LURDCO,*) (C(J),J=1,LENCO)
            LC=LENCO
            CALL CLFILE ( ' ', LURDCO, IERR )
            ENDIF
C
C  CHECK IF ERRORS OCCURRED READING THE INPUT CARDS
      IF (NERRS.EQ.0) GO TO 30
         WRITE (IPR,20)
20    FORMAT ('0**FATAL ERROR** PROGRAM IS TERMINATED BECAUSE ',
     1 'ONE OR MORE ERRORS OCCURRED WHILE READING THE INPUT CARDS.')
         CALL KILLPM ()
C
C  INITIALIZE STARTING DATE AND TIME
30    MONTH=IMO
      IYEAR=IYR
      IDA=IDARUN
      IHZERO=IHRRUN
C
C  BEGIN MAIN MONTHLY LOOP
C
C  FIND LAST DAY AND HOUR - ALSO INITIAL DAY OF DATA.
40    IF ((IYEAR.EQ.LYR).AND.(MONTH.EQ.LMO)) GO TO 50
      LAST=NDAYS(MONTH)
      IF ((MONTH.EQ.2).AND.((IYEAR/4)*4.EQ.IYEAR)) LAST=LAST+1
      CALL FCTZC (100,0,TZCODE)
      CALL JULDA (LDA,LHR,MONTH,LAST,IYEAR,24,100,0,TZCODE)
      GO TO 60
50    LDA=LDARUN
      LHR=LHRRUN
      IEND=1
60    CALL FCTZC (100,0,TZCODE)
      CALL JULDA (IDADAT,I,MONTH,1,IYEAR,1,100,0,TZCODE)
C
      IF (IFBUG(IBUG).NE.1) GO TO 80
      WRITE (IODBUG,70) MONTH,IYEAR,IDA,IHZERO,LDA,LHR,IDADAT,NOEX,IEND
70    FORMAT(1H0,22HMCP MAIN DEBUG--MONTH=,I2,3X,5HYEAR=,I4,3X,4HIDA=,
     1I6,3X,7HIHZERO=,I6,3X,4HLDA=,I6,3X,4HLHR=,I6,3X,7HIDADAT=,I6,3X,
     25HNOEX=,I1,3X,5HIEND=,I1)
C
C  READ INPUT TIME SERIES FOR THE MONTH
80    CALL FCTSRD (TS,MTS,D,MD,IHZERO,IERR,JDAMIS,JHRMIS,NWORK)
      IF (NOEX.EQ.1) GO TO 140
      IF (IERR.EQ.0) GO TO 100
         WRITE (IPR,90)
90    FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS IN READING DATA, ',
     1 'EXECUTION WILL STOP BUT READING OF TIME SERIES WILL CONTINUE.')
         CALL ERROR
         NOEX=1
         GO TO 140
C
C  EXECUTE THE OPERATIONS TABLE FOR THE MONTH
100   NERROR=NERRS
      CALL FDRIVE (P,MP,C,MC,T,MT,TS,MTS,D,MD,IHZERO)
      IF (NERRS.EQ.NERROR) GO TO 120
         WRITE (IPR,110)
110   FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS IN AN OPERATION, ',
     1 'EXECUTION WILL STOP BUT READING OF TIME SERIES WILL CONTINUE.')
         CALL ERROR
         NOEX=1
         GO TO 140
C
120   IF (NOWT.EQ.1) GO TO 140
C
C  WRITE OUTPUT TIME SERIES FOR THE MONTH
      CALL FCTSWT (TS,MTS,D,MD,IHZERO,NWORK,IERR)
      IF (IERR.EQ.0) GO TO 140
         WRITE (IPR,130)
130   FORMAT ('0**ERROR** DUE TO PRECEDING ERRORS WRITING DATA ',
     1 'WILL STOP BUT READING DATA AND EXECUTION WILL CONTINUE.')
         CALL ERROR
         NOWT=1
C
C  CHECK FOR END OF RUN
140   IF (IEND.EQ.1) GO TO 160
C
C  INCREMENT TO THE NEXT MONTH
      MONTH=MONTH+1
      IF (MONTH.LE.12) GO TO 150
         MONTH=1
         IYEAR=IYEAR+1
150   CALL FCTZC (100,0,TZCODE)
      CALL JULDA (IDA,I,MONTH,1,IYEAR,1,100,0,TZCODE)
      IHZERO=0
      GO TO 40
C
C  END OF THE MAIN MONTHLY LOOP
C
C  CHECK IF TO WRITE CARRYOVER TO OUTPUT FILE
160   IF (JWTCO.EQ.1) THEN
            CALL OPFILE ( WTCOFL, 'CO-OUTPUT ', ' ', ' ', ' ',
     +      0, LUWTCO, IERR )
            IF (IERR.GT.0) THEN
                  WRITE (IPR,*)
     +            '**ERROR** Unable to open CO output file'
                  GO TO 170
            ENDIF
            WRITE (LUWTCO,*) LC
            WRITE (LUWTCO,*) (C(J),J=1,LC)
            CALL CLFILE ( ' ', LUWTCO, IERR )
         ENDIF
C
C  CLOSE ALL CALIBRATION DATA FILES
      CALL CCLOSL
C
C  DELETE TEMPORARY RES-J FILE
cc170   call get_user ( user,len_user )
cc      rmresjFile = 'rm ' // user(1:len_user) // '.TEMP.RESJ'
ckwz get user home dir. instead of user name
170     continue


CHDH     This code has been commented out, because ofscln will clean up the
C        temp file this code used to clean.
CCC        TmpNum=4
CCC        call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
CCC        rmresjFile = 'rm ' // UserHome(1:HomeDirLength) // '/TEMP.RESJ'
CCC     +                     // CDATE // '.' // ZPROCID
CCC      call system ( "echo delete temporary file $LOGNAME.TEMP.RESJ" )


CCC      call system ( rmresjFile )

      Call OFSCLN
      CALL NEW_STOP()
C
      RETURN
C
      END
