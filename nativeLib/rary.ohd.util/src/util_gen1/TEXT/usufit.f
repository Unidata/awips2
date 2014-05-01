C  =====================================================================
C  pgm: USUFIT .. Add suffix before dots to pathname found from unit num
C
C  use:     CALL USUFIT(IUN,SUF,NEWNAM,ISTAT)
C
C   in: IUN ...... unit number of already opened file - INT
C   in: SUF ...... suffix to add to name of file identified by
C   in:             unit number IUN (suffix goes before dots) - CHAR*(*)
C  out: NEWNAM ... pathname with suffix created from IUN file - CHAR*(*)
C  out: ISTAT .... status: 0 for no error, non-zero for error - INT
C  =====================================================================      
      SUBROUTINE USUFIT(IUN,SUF,NEWNAM,ISTAT)

      CHARACTER*(*)  NEWNAM,SUF
      CHARACTER*128  CURNAM,MADNAM
      CHARACTER*32   SUFT
      CHARACTER*1    KHAR
      INTEGER        IUN,ISTAT,IB,IE,JE,KBLNK,KDOT1,KDOT2,KSLSH
      LOGICAL        LNAM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usufit.f,v $
     . $',                                                             '
     .$Id: usufit.f,v 1.1 1998/04/07 20:20:18 page Exp $
     . $' /
C    ===================================================================
C

C     Set default output values

        MADNAM = ' '
        ISTAT  = -1

C     For the given unit number, get the file's name

        CURNAM = ' '
        INQUIRE(UNIT=IUN,NAMED=LNAM,NAME=CURNAM,IOSTAT=ISTAT)

        IF (LNAM .AND. ISTAT.EQ.0) THEN
          SUFT = SUF

C     If the entered suffix has no characters, return cur pathname

          IF (SUFT .EQ. ' ') THEN
            MADNAM = CURNAM
          ELSE

C     Else find the location of the last slash (KSLSH), first dot
C      after the slash (KDOT1), ending blank (KBLNK)

            KBLNK = 129
            KDOT1 = 0
            KDOT2 = 0
            KSLSH = 0
            JE = 128
  110       IF (JE.LE.0 .OR. KSLSH.NE.0) GOTO 120
              KHAR = CURNAM(JE:JE)
              IF (KHAR .EQ. ' ') THEN
                KBLNK = JE
              ELSEIF (KHAR .EQ. '.') THEN
                IF (KDOT1 .GT. 0) KDOT2 = KDOT1
                KDOT1 = JE
              ELSEIF (KHAR .EQ. '/') THEN
                KSLSH = JE
              ENDIF
              JE = JE-1
               GOTO 110
  120       CONTINUE

C     If first dot started filename, use second dot

            IF (KDOT2.GT.0 .AND. KDOT1.EQ.(KSLSH+1)) KDOT1 = KDOT2

C     Make sure there is at least one character in the filename, else
C      return a status of -1

            IF (KBLNK .GT. (KSLSH+1)) THEN

C     Get beg/end chars in suffix; if no dot in filename or the
C      filename only has a leading dot then just append the suffix,
C      else insert the suffix before the first non-leading dot

              CALL KKTRIM(SUFT,IB,IE)
              IF (KDOT1.EQ.0 .OR. KDOT1.EQ.KSLSH+1) THEN
                MADNAM = CURNAM(1:KBLNK-1) // SUFT(IB:IE)
              ELSE
                MADNAM = CURNAM(1:KDOT1-1) // SUFT(IB:IE) //
     $                   CURNAM(KDOT1:KBLNK-1)
              ENDIF
            ELSE
              ISTAT = -1
            ENDIF

          ENDIF
        ENDIF

C     Put new pathname into output buffer (avoids checking its length)

        NEWNAM = MADNAM

      RETURN
      END
