C  =====================================================================
C  pgm: KKNXR .. Get the next word or phrase, read file as needed, cmts
C
C  use:     CALL KKNXR(IUN,LINE,WORD,LWORD,KHDELM,ISTAT,DLMG,DLMF,DLMS)
C
C   in: IUN ....... unit number to read in lines - INT
C   in:               if IUN < 0 then reinitialize variables in this rtn
C   in:               if IUN = 0 then use var LINE as an input line
C   in:                          instead of reading a file
C   in:               if IUN > 0 then continue normal file input
C  i/o: LINE ...... internal string for reading the next line in the
C  i/o:             file (made an argument for sizing only) - CHAR(*)
C  i/o:             (NOTE** LINE must be a saved var in the calling rtn)
C  i/o:             if IUN is < 0, LINE becomes an input string
C  out: WORD ...... next word found, else blank - CHAR(*)
C  out:              (note, this string must be long enough to allow
C  out:               for groups of words inside delimiters spanning
C  out:               multiple lines)
C  out: LWORD ..... number of characters in output word, set to
C  out:             zero for null word, set to -1 for no word - INT
C  out: KHDELM .... grouping delimiter encountered - CHAR*1
C  out: ISTAT ..... output status of internal read - INT
C  out:               neg .. end of file
C  out:                 0 .. no error, file nog finished
C  out:               pos .. error reading file
C
C  cmt: Note, the following characters in DLMG or DLMF (if given)
C  cmt:       are ended by the correspinding character:
C  cmt:                        (  )
C  cmt:                        [  ]
C  cmt:                        {  }
C  cmt:                        <  >
C  cmt: Blanks are always a general delimiter.
C
C  cmt: The DLMF is a subset of DLMG that will take effect even
C  cmt:  when no blank or comma precedes it (i.e. up against a word).
C  =====================================================================
      SUBROUTINE KKNXR(IUN,LINE,WORD,LWORD,KHDELM,ISTAT,DLMG,DLMF,DLMS)

      INTRINSIC       LEN
      INTEGER         LEN

      CHARACTER*(*)   LINE,WORD,DLMG,DLMF,DLMS
      CHARACTER*1     KHDELM,KHAR,DELIM,FDELIM,SVDELM,KH
      INTEGER         IUN,LWORD,ISTAT,II,JJ,INEX,IFOU,MAXLIM,MAXW
      INTEGER         LIMIT,LEADDL,LNDLMG,LNDLMF,LNDLMS,IEOL
      INTEGER         LBEG,LAST,MAXL,LSTAT,IHAVC,LASW,LASF

      SAVE   LBEG,LAST,MAXL,LSTAT,IHAVC,LASW,LASF,DELIM,FDELIM,MAXLIM
      SAVE   SVDELM,IEOL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kknxr.f,v $
     . $',                                                             '
     .$Id: kknxr.f,v 1.1 2001/06/13 18:12:17 dws Exp $
     . $' /
C    ===================================================================
C

      DATA   LBEG   / -1 /
C                           Set LEADDL=0 to skip lead delim, else =1
      DATA   LEADDL /  1 /

        IF (IUN.LT.0 .OR. LBEG.EQ.-1) THEN
          LBEG  = 1
          MAXL  = LEN(LINE)
          LAST  = MAXL
          MAXLIM = MAXL
          IHAVC = LEADDL
          LSTAT = 0
          SVDELM = ' '
          FDELIM = ' '
          IEOL   = -1
        ENDIF

        WORD   = ' '
        LWORD  = -1
        KHDELM = ' '

        IF (IUN .GE. 0) THEN
          LNDLMG = LEN(DLMG)
          LNDLMF = LEN(DLMF)
          LNDLMS = LEN(DLMS)

          IF (DLMG(1:1) .EQ. ' ') LNDLMG = 0
          IF (DLMF(1:1) .EQ. ' ') LNDLMF = 0
          IF (DLMS(1:1) .EQ. ' ') LNDLMS = 0

          DELIM  = ' '
          MAXW   = LEN(WORD)
          LIMIT  = MAXLIM

          LASW   = 0
          LASF   = 0

          II = 0
          IF (FDELIM .NE. ' ') II = LAST+1
  110     IF (LSTAT.NE.0 .OR. LWORD.NE.-1) GOTO 160

            IF (LAST .GE. LIMIT) THEN
              LAST = 0
              IF (IUN .EQ. 0) THEN
                IF (IEOL .EQ. -1) THEN
                  IEOL  = 1
                ELSE
                  LSTAT = -1
                ENDIF
              ELSE

                KH = '@'
   20           IF (KH.NE.'@' .AND. KH.NE.'#' .AND. KH.NE.' ') GOTO 24
                  READ(IUN,'(A)',IOSTAT=LSTAT) LINE
                  IF (LSTAT.NE.0 .OR. DELIM.NE.' ' .OR.
     $                FDELIM.NE.' '                     ) THEN
                    KH = 'A'
                  ELSE
                    INEX = 1
                    IFOU = MAXL
   21               IF (INEX .GE. IFOU) GOTO 22
                      KH = LINE(INEX:INEX)
                      IF (KH .NE. ' ') IFOU = INEX
                      INEX = INEX+1
                      GOTO 21
   22               CONTINUE
                  ENDIF
                  GOTO 20
   24           CONTINUE

              ENDIF
              IF (LSTAT .EQ. 0) THEN
                INEX = MAXL
                IFOU = 0
  112           IF (INEX .LE. IFOU) GOTO 114
                  IF (LINE(INEX:INEX) .NE. ' ') IFOU = INEX
                  INEX = INEX-1
                  GOTO 112
  114           CONTINUE
                MAXLIM = IFOU
              ENDIF
              LIMIT = MAXLIM
              II    = 0
              IHAVC = LEADDL
              IF (DELIM .NE. ' ') THEN
                II = 1
                IF (LINE(1:1) .NE. DELIM) THEN
                  IF (LASW.GT.0 .AND. LASW.LT.MAXW) THEN
                    LASW = LASW+1
                    WORD(LASW:LASW) = ' '
                  ENDIF
                ENDIF
              ELSEIF (FDELIM .NE. ' ') THEN
                II = 1
                IF (LINE(1:1) .NE. FDELIM) THEN
                  IF (LASF.GT.0 .AND. LASF.LT.MAXW) THEN
                    LASF = LASF+1
                    WORD(LASF:LASF) = ' '
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            IF (LSTAT.EQ.0 .AND. LAST.LT.LIMIT) THEN
              JJ   = LAST
              LAST = LAST+1
              KHAR = LINE(LAST:LAST)
              IF (DELIM .NE. ' ') THEN
                IF (KHAR .EQ. DELIM) THEN
                  LIMIT = 0
                  DELIM = ' '
                ELSEIF (LAST .GE. LIMIT) THEN
                  JJ    = LAST
                  LIMIT = 0
                ENDIF
                IF (LIMIT .EQ. 0) THEN
                  IF (II.GT.0 .AND. II.LE.JJ) THEN
                    IF (LASW .LT. MAXW) THEN
                      LASW = LASW+1
                      WORD(LASW:) = LINE(II:JJ)
                      LASW = LASW+JJ-II
                      IF (LASW .GT. MAXW) LASW = MAXW
                    ENDIF
                    IF (DELIM .EQ. ' ') LWORD = LASW
                  ELSE
                    LWORD = LASW
                  ENDIF
                  IF (LWORD .NE. -1) KHDELM = KHAR
                ENDIF
              ELSEIF (FDELIM .NE. ' ') THEN
                IF (KHAR .EQ. FDELIM) THEN
                  LIMIT = 0
                  FDELIM = ' '
                  KHDELM = SVDELM
                  SVDELM = ' '
                ELSEIF (LAST .GE. LIMIT) THEN
                  JJ    = LAST
                  LIMIT = 0
                ENDIF
                IF (LIMIT .EQ. 0) THEN
                  IF (II.GT.0 .AND. II.LE.JJ) THEN
                    IF (LASF .LT. MAXW) THEN
                      LASF = LASF+1
                      WORD(LASF:) = LINE(II:JJ)
                      LASF = LASF+JJ-II
                      IF (LASF .GT. MAXW) LASF = MAXW
                    ENDIF
                    IF (FDELIM .EQ. ' ') LWORD = LASF
                  ELSE
                    LWORD = LASF
                  ENDIF
                  IF (LWORD .NE. -1) KHDELM = KHAR
                ENDIF
              ELSEIF (II .EQ. 0) THEN
                INEX = LNDLMG
                IFOU = 0
  120           IF (INEX .LE. IFOU) GOTO 130
                  IF (KHAR .EQ. DLMG(INEX:INEX)) IFOU = INEX
                  INEX = INEX-1
                  GOTO 120
  130           CONTINUE
                IF (IFOU .NE. 0) THEN
                  DELIM  = KHAR
                  IF (KHAR .EQ. '(') DELIM = ')'
                  IF (KHAR .EQ. '[') DELIM = ']'
                  IF (KHAR .EQ. '{') DELIM = '}'
                  IF (KHAR .EQ. '<') DELIM = '>'
                  II     = LAST+1
                  IHAVC  = 0
                ENDIF
                IF (DELIM.EQ.' ' .AND. KHAR.NE.' ') THEN
                  INEX = LNDLMS
                  IFOU = 0
  132             IF (INEX .LE. IFOU) GOTO 134
                    IF (KHAR .EQ. DLMS(INEX:INEX)) IFOU = INEX
                    INEX = INEX-1
                    GOTO 132
  134             CONTINUE
                  IF (IFOU .NE. 0) THEN
                    IF (IHAVC .EQ. 1) THEN
                      LIMIT  = 0
                      LWORD  = 0
                      KHDELM = KHAR
                      IF (II.GT.0 .AND. II.LE.JJ) THEN
                        WORD = LINE(II:JJ)
                        LWORD = JJ-II+1
                        IF (LWORD .GT. MAXW) LWORD = MAXW
                      ENDIF
                    ELSE
                      IHAVC = 1
                    ENDIF
                  ELSE
                    IHAVC = 0
                    II = LAST
                    IF (LAST .GE. LIMIT) THEN
                      WORD = LINE(II:II)
                      LWORD = 1
                      LIMIT = 0
                    ENDIF
                  ENDIF
                ENDIF
              ELSE
                IF (KHAR .EQ. ' ') THEN
                  LIMIT = 0
                ELSE
                  INEX = LNDLMF
                  IFOU = 0
  140             IF (INEX .LE. IFOU) GOTO 150
                    IF (KHAR .EQ. DLMF(INEX:INEX)) IFOU = INEX
                    INEX = INEX-1
                    GOTO 140
  150             CONTINUE
                  IF (IFOU .NE. 0) THEN
                    SVDELM = KHAR
                    KHDELM = KHAR
                    FDELIM = KHAR
                    IF (KHAR .EQ. '(') FDELIM = ')'
                    IF (KHAR .EQ. '[') FDELIM = ']'
                    IF (KHAR .EQ. '{') FDELIM = '}'
                    IF (KHAR .EQ. '<') FDELIM = '>'
                    IHAVC  = 0
                    LIMIT  = 0
                  ENDIF
                  IF (LIMIT .NE. 0) THEN
                    INEX = LNDLMS
                    IFOU = 0
  152               IF (INEX .LE. IFOU) GOTO 154
                      IF (KHAR .EQ. DLMS(INEX:INEX)) IFOU = INEX
                      INEX = INEX-1
                      GOTO 152
  154               CONTINUE
                    IF (IFOU .NE. 0) THEN
                      LIMIT = 0
                      KHDELM = KHAR
                    ENDIF
                  ENDIF
                  IF (LIMIT.NE.0 .AND. LAST .GE. LIMIT) THEN
                    JJ    = LIMIT
                    LIMIT = 0
                  ENDIF
                ENDIF
                IF (LIMIT .EQ. 0) THEN
                  IF (II .LE. JJ) THEN
                    WORD = LINE(II:JJ)
                    LWORD = JJ-II+1
                    IF (LWORD .GT. MAXW) LWORD = MAXW
                  ELSE
                    LWORD = 0
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            GOTO 110
  160     CONTINUE

          IF (LSTAT .LT. 0) THEN
            IF (DELIM .NE. ' ') THEN
              KHDELM = DELIM
              DELIM  = ' '
              LWORD  = LASW
            ELSEIF (FDELIM .NE. ' ') THEN
              KHDELM = FDELIM
              FDELIM = ' '
              LWORD  = LASF
            ENDIF
          ENDIF

        ENDIF

        ISTAT = LSTAT

      RETURN
      END
