C  =====================================================================
C  pgm: UDNXWD .. Get nxt wrd in character*80 line (read line if needed)
C
C  use:     CALL UDNXWD(CTL,LP,ICD,COMN,WORD)
C
C   in: CTL ......... routine control (use first char): - CHAR*(*)
C   in:                 'N' ... force new read (and prompt)
C   in:                 'Q' ... quit - do not read new buffer
C   in:                     ... anything else continues reading buffer
C   in: LP .......... unit num to output 'enter command' message - INT
C   in:               (if neg, do not write interactive commands)
C   in: ICD ......... unit number to input command line, LIN - INT
C   in: COMN ........ command prompt if given, else no prompt - CHAR*(*)
C  out: WORD ........ next word in buffer, else ' ' - CHAR*(*)
C  out:               else ' ' - CHAR*80
C  out:                 (WORD=' ' = end of line, callg rtn may set IX=0)
C
C  cmt: Note, a word cannot be split between two input lines.
C  =====================================================================
      SUBROUTINE UDNXWD(CTL,LP,ICD,COMN,WORD)

      INTEGER       LP,ICD,IX,IB,IE,IC,LINMXP,IXEND
      CHARACTER*(*) CTL,COMN,WORD
      CHARACTER*80  COM,LIN
      CHARACTER*1   CTLX,BLNK

      SAVE    IX,BLNK,LIN,LINMXP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/udnxwd.f,v $
     . $',                                                             '
     .$Id: udnxwd.f,v 1.2 1997/12/31 19:50:56 page Exp $
     . $' /
C    ===================================================================
C

      DATA    IX,BLNK,LIN,LINMXP / 81, ' ', ' ', 81 /

C           If control character is 'N', force start of new buffer

        CTLX = CTL
        IF (CTLX .EQ. 'N') IX = LINMXP

C           If IX is at end of line, print command prompt and enter line

        IF (IX.EQ.LINMXP .AND. CTLX.NE.'Q') THEN
          COM = COMN
          IF (LP.GE.0 .AND. COM.NE.' ') WRITE(LP,'(/A)',IOSTAT=IC) COM
          LIN = ' '
          IC = -1
          IF (ICD .GE. 0) READ(ICD,'(A)',IOSTAT=IC) LIN
          IF (IC .EQ. 0) IX = 1
          IF (LP.GE.0 .AND. COM.NE.' ') WRITE(LP,'(/)',IOSTAT=IC)
        ENDIF

C           Find the next character that starts a word

        IB = LINMXP
   20   IF (IX .GE. IB) GOTO 40
          IF (LIN(IX:IX) .NE. BLNK) IB = IX
          IX = IX+1
            GOTO 20
   40   CONTINUE

C           Now find the last character in that word

        IE = LINMXP
   60   IF (IX .GE. IE) GOTO 80
          IF (LIN(IX:IX) .EQ. BLNK) IE = IX
          IX = IX+1
            GOTO 60
   80   CONTINUE
        IE = IE-1

C           Move character index IX to next non-blank character

        IXEND = LINMXP
  100   IF (IX .GE. IXEND) GOTO 120
          IF (LIN(IX:IX) .NE. BLNK) IXEND = IX
          IX = IX+1
            GOTO 100
  120   CONTINUE
        IX = IXEND

C           Put next word into output string, else set it to blank

        IF (IB .LE. IE) THEN
          WORD = LIN(IB:IE)
        ELSE
          WORD = ' '
        ENDIF

      RETURN
      END
