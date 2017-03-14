C MODULE UPFNCU
C  =====================================================================
C  pgm: UPFNCU .. Get file name or unit number given the other, get file
C  pgm:           record length and blocksize
C
C  use:     CALL UPFNCU( cntl.key, fil.inp, unt.inp, fil.out,
C  use:                  unt.out, rec.len, blk.siz, cond )
C
C   in: cntl.key ...... key to control initialization: - CHAR*4
C   in:                   = 'NORM' to get file data from first number
C   in:                            after the filename in the input file
C   in:                            (this forces initialization of files)
C   in:                   = 'REOR' to get file data from second number
C   in:                            after the filename in the input file
C   in:                            (this forces initialization of files)
C   in:                   = "anything else" defaults to initialization
C   in:                            using the first number as with 'NORM'
C   in:                            only if the files have not been
C   in:                            previously initialized
C   in: fil.inp ....... 32-char file name, if given it is used to get
C   in:                 full file pathname and unit number - CHAR*32
C   in: unt.inp ....... file unit number, if given it is used to get
C   in:                 the full file pathname and unit number - INT
C  out: fil.out ....... 128-char file pathname obtained - CHAR*128
C  out: unt.out ....... file unit number obtained - INT
C  out: rec.len ....... record length in bytes of gvn fil else -1 - INT
C  out: blk.siz ....... block size of given file else -1 - INT
C  out: cond .......... status condition as follows: - INT
C  out:                   = -2 initialized, file obtained but no rec.len
C  out:                   = -1 initialized but no file output obtained
C  out:                   =  0 all output is obtained and rec.len good
C  out:                   =  1 cannot obtain file numbers (wrong file)
C  out:                   =  2 error in rtn UPFNLS (unit number file)
C  out:                   =  3 error in rtn UPFNLL (rec length file)
C  out:                   =  4 error in rtn UPPFIX (making full filenm)
C   in: (common) ...... block common UPDAIO contains unit numbers for
C   in:                 PRIME i/o routine messages (see subrtn UPRIMO)
C
C  rqd: subrtns: UPFNLS,UPFNLL,UPPFIX
C  rqd: common:  UPDAIO
C
C  cmt: The input variable, cntl.key, is only used during the first call
C  cmt:   to this routine.
C  cmt: Only one of the two parameters (fil.nam or fil.num) should be
C  cmt:   given as input ... fil.num is checked first.
C  cmt: It is critical to "SAVE" arrays NAM, NUREC, NUBLK, and
C  cmt:   variable KEYINZ; else they would have to be placed in COMMON.
C  =====================================================================
      SUBROUTINE UPFNCU(KEY,FILNAM,FILUN,NEWNAM,NEWUN,RCLN,BKSZ,IC)


      EXTERNAL      UPFNLS,UPFNLL,UPPFIX
      CHARACTER*4   KEY
      CHARACTER*128 AFIL,BFIL,NEWNAM
      INTEGER       FILUN,NEWUN,RCLN,BKSZ,IC,LIM,II,JC,NOC
      PARAMETER     ( LIM=99 )
	  CHARACTER*32  FILNAM,AFILNM,BFILNM,NAM(LIM),FIL
      INTEGER       NUREC(LIM),NUBLK(LIM),NTY(LIM),KEYINZ

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfncu.f,v $
     . $',                                                             '
     .$Id: upfncu.f,v 1.2 2001/06/12 19:12:06 dws Exp $
     . $' /
C    ===================================================================
C

C                  The following arrays are filled by obtaining data
C                  from files given by the pathnames AFIL and BFIL

      SAVE          KEYINZ,NAM,NUREC,NUBLK,NTY

      DATA          KEYINZ / -1 /


C                  Initialize output in case input is missing or bad

        RCLN = -1
        BKSZ = -1
        IC   =  1

C                  Check for key to force initialization of files, else
C                  check "KEYINZ" to see if saved files have been initzd

C                  Fill the "SAVE'd" arrays for filenames and related
C                  unit numbers and size information using subroutines
C                  UPFNLS and UPFNLL.

CCC        IF( KEY .EQ. 'NORM' ) GO TO 90
CCC        IF( KEY .EQ. 'REOR' ) GO TO 90
        IF( KEYINZ .NE. -1  ) GO TO 100

   90   AFILNM = 'FS5FFILE'
        BFILNM = 'FS5FLDCB'
        KEYINZ = 1
        IC = 4

                    CALL UPPFIX('SYST',AFILNM,AFIL,JC)
        IF(JC.NE.0) CALL UPPFIX('SYST',BFILNM,BFIL,JC)
        IF(JC.NE.0) CALL UPFNLS(KEY,AFIL,LIM,NAM,NUREC,NUBLK,NTY,IC)
        IF(IC.EQ.0) CALL UPFNLL(BFIL,LIM,NAM,NUREC,NUBLK,IC)
        IF(IC.NE.0 .AND. UE.GE.0) WRITE(UE,94)
   94   FORMAT(' upfncu      ** ERROR = cannot initialize UPFNCU files')
        IF(IC.NE.0) GO TO 400

        IC = -1

  100   CONTINUE

C                  First check for given file unit number to get rest of
C                  the file description such as name and record length

        IF( FILUN .LE.   0 ) GO TO 110
        IF( FILUN .GT. LIM ) GO TO 110
              FIL   = NAM(FILUN)
              NEWUN = FILUN
              RCLN  = NUREC(FILUN)
              BKSZ  = NUBLK(FILUN)
              IF( NTY(FILUN).EQ.1 ) CALL UPPFIX('OPER',FIL,NEWNAM,NOC)
              IF( NTY(FILUN).EQ.2 ) CALL UPPFIX('REOR',FIL,NEWNAM,NOC)
              IF( NOC.GT.0 .AND. RCLN.GE.0 ) IC =  0
              IF( NOC.GT.0 .AND. RCLN.LT.0 ) IC = -2
                GO TO 400
  110     CONTINUE

C                  If not given, check for input filename to get data

        IF( FILNAM .EQ. '  ' ) GO TO 150
          II = 0
  140     IF( II .GE. LIM ) GO TO 150
            II = II+1
            IF( FILNAM .NE. NAM(II) ) GO TO 140
              FIL   = FILNAM
              NEWUN = II
              RCLN  = NUREC(II)
              BKSZ  = NUBLK(II)
              IF( NTY(II).EQ.1 ) CALL UPPFIX('OPER',FIL,NEWNAM,NOC)
              IF( NTY(II).EQ.2 ) CALL UPPFIX('REOR',FIL,NEWNAM,NOC)
              IF( NOC.GT.0 .AND. RCLN.GE.0 ) IC =  0
              IF( NOC.GT.0 .AND. RCLN.LT.0 ) IC = -2
                GO TO 400
  150     CONTINUE


  400   CONTINUE


      RETURN
      END
