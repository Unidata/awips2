C  =====================================================================
C  pgm: WLIN .. output the given text line with a possible prefix
C
C  use:     CALL WLIN(CMD,LIN)
C
C   in: CMD ....... one character code for type of output - CHAR*(*)
C   in:               'B' ... output blank line
C   in:               'E' ... output ERROR message prefix
C   in:               'W' ... output WARNG message prefix
C   in:               'C' ... output CAUTION message prefix
C   in:               'N' ... output NOTE message prefix
C   in:               'M' ... output line with no prefix
C   in:               'S' ... output blank message prefix
C   in: LIN ....... character string to be output - CHAR*(*)
C   in: (common) .. var LP in common 'uiox' is the output unit number
C  =====================================================================
      SUBROUTINE WLIN(CMD,LIN)

      EXTERNAL       KKLAST
      INTEGER        KKLAST

      INCLUDE 'uiox'

      CHARACTER*(*)   CMD,LIN
      CHARACTER*1     KCMD
      CHARACTER*13    MSGE,MSGW,MSGN,MSGC,MSGS
      INTEGER         IE,LLIN,IECOU,IEMAX

      SAVE            IECOU,IEMAX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/wlin.f,v $
     . $',                                                             '
     .$Id: wlin.f,v 1.1 2001/06/13 09:14:09 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA            IECOU,IEMAX / 0, 20 /

      DATA   MSGE / ' ** ERROR ** ' /
      DATA   MSGW / ' ** WARNG ** ' /
      DATA   MSGN / '    NOTE:    ' /
      DATA   MSGC / '    CAUTION: ' /
      DATA   MSGS / '             ' /

        IF (LP.LT.0 .OR. LP.GT.99) IECOU = IEMAX
        IF (IECOU .LT. IEMAX) THEN

          KCMD = CMD(1:1)
          IF (KCMD .EQ. 'B') THEN
            WRITE(LP,'()',IOSTAT=IE)
          ELSE
            LLIN = KKLAST(0,LIN)
            IF (LLIN .GT. 0) THEN
              IF (KCMD .EQ. 'E') THEN
                WRITE(LP,'(A,A)',IOSTAT=IE) MSGE,LIN(1:LLIN)
              ELSEIF (KCMD .EQ. 'W') THEN
                WRITE(LP,'(A,A)',IOSTAT=IE) MSGW,LIN(1:LLIN)
              ELSEIF (KCMD .EQ. 'N') THEN
                WRITE(LP,'(A,A)',IOSTAT=IE) MSGN,LIN(1:LLIN)
              ELSEIF (KCMD .EQ. 'C') THEN
                WRITE(LP,'(A,A)',IOSTAT=IE) MSGC,LIN(1:LLIN)
              ELSEIF (KCMD .EQ. 'S') THEN
                WRITE(LP,'(A,A)',IOSTAT=IE) MSGS,LIN(1:LLIN)
              ELSEIF (KCMD .EQ. 'M') THEN
                WRITE(LP,'(A)',IOSTAT=IE) LIN(1:LLIN)
              ENDIF
            ENDIF
          ENDIF

          IF (IE .NE. 0) IECOU = IECOU+1

        ENDIF

      RETURN
      END
