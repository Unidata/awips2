C MODULE UPFNLS
C  =====================================================================
C  pgm: UPFNLS .. Get all filenames and related numbers
C
C  use:     CALL UPFNLS( cntl.key, inpt.fil, arr.sz, fil.names,
C  use:                  rec.len,  block.siz, fil.typ, err.code  )
C
C   in: cntl.key ...... if set to "REOR" then use 2nd and 3rd numbers
C   in:                 after filename on input record for the file unit
C   in:                 number - CHAR*4
C   in: inpt.fil ...... file pathname of file containing a list of all
C   in:                 possible file names and their related unit
C   in:                 numbrs, record lngths, and blck sizes - CHAR*128
C   in: arr.sz ........ number of allowable files (max unit num) - INT
C  out: fil.names(1) .. array of filenames indexed by unt num - CHAR*32
C  out: rec.len(1) .... array of rec lens indx by unt num - INT
C  out: block.siz(1) .. array of blk sizes indx by unt num - INT
C  out: fil.typ(1) .... file type, 1 = normal, 2 = REORDER write - INT
C  out: err.code ...... error code, 0 = no error, 2 = error - INT
C   in: (common) ...... block common UPDAIO contains unit numbers for
C   in:                 i/o routine messages (see subrtn UPRIMO)
C
C  rqd: subrtns: KKNXPO,UPOPEN,UPCLOS,UPEXIS
C  rqd: common:  UPDAIO
C
C  cmt: This routine will get the record length and block size if they
C  cmt:   are included in the input file, otherwise they are set to -1.
C  =====================================================================
      SUBROUTINE UPFNLS(KEY,AFIL,LMOF,NAM,NUREC,NUBLK,NTY,IC)


      EXTERNAL      KKNXPO,UPOPEN,UPCLOS,UPEXIS

      CHARACTER*128 AFIL
      PARAMETER (LIM=99)
      CHARACTER*32  NAM(LIM),TMPNAM
      CHARACTER*4   KEY
      CHARACTER*80  LINE
      CHARACTER*1   BLANK,ASTRX,KHAR
      INTEGER       LMOF,NUREC(LIM),NUBLK(LIM),NTY(LIM),IC,LMLN,IUNIT
      INTEGER       II,JJ,NUM1,NUM2,NUM3,NUM4,NUM5,LRECL
      PARAMETER   ( LMLN=80, IUNIT=99, BLANK=' ', ASTRX='*' )

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfnls.f,v $
     . $',                                                             '
     .$Id: upfnls.f,v 1.2 2001/06/12 19:12:47 dws Exp $
     . $' /
C    ===================================================================
C

C                  Initialize all output file data arrays, these arrays
C                  are indexed by the primary file unit number up to
C                  a maximum file unit number of "LMOF"

        II = 0
  100   IF( II .GE. LMOF ) GO TO 110
          II = II+1
          NAM(II)   = '  '
          NUREC(II) = -1
          NUBLK(II) = -1
          NTY(II)   = -1
            GO TO 100
  110     CONTINUE

C                  Skip this routine if no input file is given

        IC = 0
        IF( AFIL .EQ. '  ' ) GO TO 900

C                  Routine to open the data file (a useful record
C                  looks like "HCLINDEX 12 17 16 15472" for file
C                  HCLINDEX to be opened on primary unit number 12 or
C                  secondary unit number 17 with a record length of 16
C                  bytes and a blocksize of 15472 bytes ... the record
C                  length and blocksize are optional)

        CALL UPEXIS(IUNIT,AFIL,IC)
          IF( IC .NE. 0 ) GO TO 810
        LRECL = 0
        CALL UPOPEN(IUNIT,AFIL,LRECL,'F',IC)
          IF( IC .GT. 0 ) GO TO 820

C                  Read line, interpret in free-format, skip "*" lines

  120   READ(IUNIT,122,ERR=830,END=200) LINE
  122   FORMAT(A)

C                  Get filename first

        TMPNAM = '  '
        II = 0
        JJ = 0
  130   IF( II .GE. LMLN ) GO TO 140
          II = II+1
          KHAR = LINE(II:II)
          IF( KHAR .EQ. BLANK ) GO TO 130
          IF( KHAR .EQ. ASTRX ) GO TO 120
  132       JJ = JJ+1
            IF( JJ .LE. 32 ) TMPNAM(JJ:JJ) = KHAR
            IF( II .GE. LMLN ) GO TO 140
              II = II+1
              KHAR = LINE(II:II)
              IF( KHAR .NE. BLANK ) GO TO 132
  140     CONTINUE
          IF( JJ .LE. 0 ) GO TO 120

  150     IF( JJ .GE. 32 ) GO TO 152
            JJ = JJ+1
            TMPNAM(JJ:JJ) = BLANK
              GO TO 150
  152       CONTINUE

C                  Second, get next 5 positive integers as unit
C                  numbers, record length, and block size

          CALL KKNXPO(LINE,II,LMLN,NUM1)
          CALL KKNXPO(LINE,II,LMLN,NUM2)
          CALL KKNXPO(LINE,II,LMLN,NUM3)
          CALL KKNXPO(LINE,II,LMLN,NUM4)
          CALL KKNXPO(LINE,II,LMLN,NUM5)

          IF( KEY .EQ. 'REOR' ) GO TO 160
            IF( NUM1.LT.1 .OR. NUM1.GT.LMOF ) GO TO 180
              NAM(NUM1)   = TMPNAM
              NUREC(NUM1) = NUM4
              NUBLK(NUM1) = NUM5
              NTY(NUM1)   = 1
                GO TO 180
  160       IF( NUM2.LT.1 .OR. NUM2.GT.LMOF ) GO TO 170
              NAM(NUM2)   = TMPNAM
              NUREC(NUM2) = NUM4
              NUBLK(NUM2) = NUM5
              NTY(NUM2)   = 1
  170       IF( NUM3.LT.1 .OR. NUM3.GT.LMOF ) GO TO 180
              NAM(NUM3)   = TMPNAM
              NUREC(NUM3) = NUM4
              NUBLK(NUM3) = NUM5
              NTY(NUM3)   = 2
  180       CONTINUE
              GO TO 120

  200   CONTINUE

C                  Close data file and return

        CALL UPCLOS(IUNIT,AFIL,IC)

        IC = 0
          GO TO 900

C                  Statements for errors

  810   IF( UE.GE.0 ) WRITE(UE,812)
  812   FORMAT(' upfnls      ** ERROR = error returned from UPEXIS')
        IC = 2
          GO TO 900

  820   IF( UE.GE.0 ) WRITE(UE,822)
  822   FORMAT(' upfnls      ** ERROR = error returned from UPOPEN')
        IC = 2
          GO TO 900

  830   IF( UE.GE.0 ) WRITE(UE,832) AFIL
  832   FORMAT(' upfnls      ** ERROR reading frm ',A)
        IC = 2
          GO TO 900

  900   CONTINUE


      RETURN
      END
