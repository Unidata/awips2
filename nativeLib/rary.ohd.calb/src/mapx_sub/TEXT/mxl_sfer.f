C  =====================================================================
C  pgm: MXL_SFER .. Output errors from routine droutp
C
C  rqd: WLIM
C  =====================================================================
      SUBROUTINE MXL_SFER(ID,ISTAT)

      EXTERNAL        WLIN

      CHARACTER*(*)   ID
      INTEGER         ISTAT,JE
      CHARACTER*200   LIN
      CHARACTER*5     FM2
      CHARACTER*39    MSG1
      CHARACTER*47    MSG2
      CHARACTER*28    MSG3
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_sfer.f,v $
     . $',                                                             '
     .$Id: mxl_sfer.f,v 1.1 2001/06/13 09:22:51 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  FM2  / '(A,A)' /
      DATA MSG1  / 'no data so no output to hrap file for: ' /
      DATA MSG2  / 'bad unit number so no output to hrap file for: ' /
      DATA MSG3  / 'bad write to hrap file for: ' /

        IF (ISTAT .NE. 0) THEN

          CALL WLIN('B',' ')

          LIN = ' '
          IF     (ISTAT .EQ. -1) THEN
            WRITE(LIN,FM2,IOSTAT=JE) MSG1,ID
            IF(JE.EQ.0) CALL WLIN('W',LIN)
          ELSEIF (ISTAT .EQ.  1) THEN
            WRITE(LIN,FM2,IOSTAT=JE) MSG2,ID
            IF(JE.EQ.0) CALL WLIN('E',LIN)
          ELSEIF (ISTAT .EQ.  2) THEN
            WRITE(LIN,FM2,IOSTAT=JE) MSG3,ID
            IF(JE.EQ.0) CALL WLIN('E',LIN)
          ENDIF

        ENDIF

      RETURN
      END
