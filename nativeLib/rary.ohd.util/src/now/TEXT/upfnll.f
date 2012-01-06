C MODULE UPFNLL
C  =====================================================================
C  pgm: UPFNLL .. Get LRECL and BLOCKSIZE from second file
C
C  use:     CALL UPFNLL( inpt.fil, arr.sz, fil.names,
C  use:                  rec.len,  block.siz, err.code  )
C
C   in: inpt.fil ...... file pathname of file containing a list of
C   in:                 filenames and their related record length and
C   in:                 block size (rec length in bytes) - CHAR*128
C   in: arr.sz ........ number of allowable files (max unit num) - INT
C   in: fil.names(1) .. array of filenames indexed by unt num - CHAR*32
C  out: rec.len(1) .... array of rec lens indx by unt num - INT
C  out: block.siz(1) .. array of blk sizes indx by unt num - INT
C  out: err.code ...... error code, 0 = no error, 3 = error - INT
C   in: (common) ...... block common UPDAIO contains unit numbers for
C   in:                 i/o routine messages (see subrtn UPRIMO)
C
C  rqd: subrtns: KKNXPO,UPFNGN,UPEXIS,UPOPEN,UPCLOS
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UPFNLL(BFIL,LMOF,NAM,NUREC,NUBLK,IC)


      EXTERNAL      KKNXPO,UPFNGN,UPOPEN,UPCLOS,UPEXIS

      CHARACTER*128 BFIL
      CHARACTER*80  LINE
      PARAMETER     ( LIM=99 )
      CHARACTER*32  NAM(LIM),TMPNAM,NAMSUB
      CHARACTER*1   BLANK,ASTRX,KHAR
      INTEGER       LMOF,NUREC(LIM),NUBLK(LIM),IC,LMLN,IUNIT,LRECL
      INTEGER       II,JJ,KK,NULREC,NUBLCK
      PARAMETER   ( LMLN=80, IUNIT=99, BLANK=' ', ASTRX='*' )

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfnll.f,v $
     . $',                                                             '
     .$Id: upfnll.f,v 1.2 2001/06/12 19:12:21 dws Exp $
     . $' /
C    ===================================================================
C

C                  Skip this routine if no input file is given

        IC = 0
        IF( BFIL .EQ. '  ' ) GO TO 900

C                  Open second data file (a useful record looks
C                  like "HCLINDEX 16 15472" for file HCLINDEX to have
C                  a record length of 16 bytes and blksiz of 15472 bytes

        CALL UPEXIS(IUNIT,BFIL,IC)
          IF( IC .NE. 0 ) GO TO 810
        LRECL = 0
        CALL UPOPEN(IUNIT,BFIL,LRECL,'F',IC)
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

  142     IF( JJ .GE. 32 ) GO TO 144
            JJ = JJ+1
            TMPNAM(JJ:JJ) = BLANK
              GO TO 142
  144       CONTINUE

C                  Second, get next integer as record length, if none
C                  found go to read next line

          CALL KKNXPO(LINE,II,LMLN,NULREC)
          CALL KKNXPO(LINE,II,LMLN,NUBLCK)
          IF( NULREC .LT. 1 ) GO TO 120

C                  Locate filename in arrays to get index number, then
C                  put record length in its array using this index
C                  number (then get blocksize and put in its array)

          KK = 0
  150     IF( KK .GE. LMOF ) GO TO 180
            KK = KK+1
            NAMSUB = NAM(KK)
            IF( TMPNAM .EQ. 'PDBDLY'  ) CALL UPFNGN(NAMSUB)
            IF( TMPNAM .EQ. 'PRDTS'   ) CALL UPFNGN(NAMSUB)
            IF( TMPNAM .EQ. 'PPPPARM' ) CALL UPFNGN(NAMSUB)
            IF( TMPNAM .EQ. 'ESPTS'   ) CALL UPFNGN(NAMSUB)
            IF( TMPNAM .NE. NAMSUB ) GO TO 150
              IF( NULREC .GE. 0 ) NUREC(KK) = NULREC
              IF( NUBLCK .GE. 0 ) NUBLK(KK) = NUBLCK
                GO TO 150
  180       CONTINUE

            GO TO 120
  200   CONTINUE

C                  Close data file and return

        CALL UPCLOS(IUNIT,BFIL,IC)

        IC = 0
          GO TO 900

C                  Statements for errors

  810   IF( UE.GE.0 ) WRITE(UE,812)
  812   FORMAT(' upfnll      ** ERROR = error returned from UPEXIS')
        IC = 3
          GO TO 900

  820   IF( UE.GE.0 ) WRITE(UE,822)
  822   FORMAT(' upfnll      ** ERROR = error returned from UPOPEN')
        IC = 3
          GO TO 900

  830   IF( UE.GE.0 ) WRITE(UE,832) BFIL
  832   FORMAT(' upfnll      ** ERROR reading frm ',A)
        IC = 3
          GO TO 900

  900   CONTINUE


      RETURN
      END
