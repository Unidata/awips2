C  =====================================================================
C  pgm: UPFNIN .. Prompt the user for a file name or unit number
C
C  use:     CALL UPFNIN( in.un, out.un, fil.nam, p.num, key)
C
C   in: in.un ......... unit number for input from user - INT
C   in: out.un ........ unit number for prompt to user - INT
C  out: fil.nam ....... 32-char file name entered, - CHAR*32
C  out: p.num ......... or file unit number entered - INT
C  out: key ........... control key for initialization, set to
C  out:                 'NORM' for given normal files, else set to
C  out:                 'REOR' for given input for REORDER, else set to
C  out:                 '    ' for no control key given - CHAR*4
C
C  rqd: subrtns: KKNXWD,KKNXPO,KKCAPS
C
C  cmt: Only one of the output variables has a value, the other initzd.
C  =====================================================================
      SUBROUTINE UPFNIN(UNR,UNW,FILNAM,FRUN,KEY)


      EXTERNAL      KKNXWD,KKNXPO,KKCAPS
      CHARACTER*32  FILNAM,C1FIL,C2FIL
      CHARACTER*4   KEY
      CHARACTER*80  LINE
      CHARACTER*1   KHRR,KHNN,KHRRL,KHNNL,KHAR
      INTEGER       UNR,UNW,FRUN,II,JJ,NUM,NOCH,J,IERR
      PARAMETER  ( KHRR='R',KHNN='N',KHRRL='r',KHNNL='n' )
 
      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfnin.f,v $
     . $',                                                             '
     .$Id: upfnin.f,v 1.3 2001/06/13 14:50:00 dws Exp $
     . $' /
C    ===================================================================
C
 

        FILNAM = '  '
        FRUN   = 0

C                  Prompt user and get input line of packed chars

        IF( UNR .EQ. UTR ) WRITE(UNW,120)
  120   FORMAT(' Enter FS5F file name or unit number (N, R to initz):')
        READ(UNR,'(A)',IOSTAT=IERR) LINE
          IF (IERR .NE. 0) THEN
            WRITE(UNW,'('' ERROR: Bad read in upfnin:'',I8)') IERR
            CALL EXIT
          ENDIF

C                  Get first word from input line

        II = 0
        CALL KKNXWD(LINE,II,80,C1FIL,32,NOCH)
        CALL KKNXWD(LINE,II,80,C2FIL,32,J)

        IF( NOCH .GT. 32 ) WRITE(UNW,126)
  126   FORMAT(' WARNING: Filename is too long and has been truncated.')

        IF( NOCH .LE.  0 ) WRITE(UNW,128)
  128   FORMAT(' WARNING: No filename is given.')

C                  If second word starts with N then set key to 'NORM'
C                  type of initialization, if starts with R set to
C                  'REOR' for REORDER files for initialization, else
C                  set to '    '

        KEY = '    '
        KHAR = C2FIL(1:1)
        IF( KHAR.EQ.KHRR .OR. KHAR.EQ.KHRRL ) KEY = 'REOR'
        IF( KHAR.EQ.KHNN .OR. KHAR.EQ.KHNNL ) KEY = 'NORM'

C                  Check for number in input, if found then use it as
C                  the desired file unit number, else assume the input
C                  is the file name and capitalize all letters.

        IF( NOCH .LE. 0 ) GO TO 150
          JJ = 0
          CALL KKNXPO(C1FIL,JJ,NOCH,NUM)
          IF( NUM .LT. 0 ) GO TO 130
            FRUN = NUM
              GO TO 140
  130       CALL KKCAPS(C1FIL(1:NOCH))
            FILNAM = C1FIL
  140     CONTINUE
  150   CONTINUE


      RETURN
      END
