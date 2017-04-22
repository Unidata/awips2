C MODULE MXX_RHE1
C  =====================================================================
C  pgm: MXX_RHE1 .. Output errors for routine rdm_bnd
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXX_RHE1(FLNAM,LNUM,ISTAT)

      EXTERNAL        WLIN

      CHARACTER*200   LIN
      CHARACTER*(*)   FLNAM
      CHARACTER*3     FM1
      CHARACTER*5     FM2
      CHARACTER*7     FM3
      CHARACTER*22    MSGL
      CHARACTER*33    MSG1
      CHARACTER*31    MSG1A
      CHARACTER*39    MSG2
      CHARACTER*41    MSG3
      CHARACTER*40    MSG4
      CHARACTER*26    MSG5
      INTEGER         LNUM,ISTAT,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_rhe1.f,v $
     . $',                                                             '
     .$Id: mxx_rhe1.f,v 1.1 2001/06/13 09:26:45 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  FM1  / '(A)' /
      DATA  FM2  / '(A,A)' /
      DATA  FM3  / '(A,I10)' /
      DATA MSGL  / ' error at line number:' /
      DATA MSG1  / 'no basins found in segment file: '/
      DATA MSG1A / ' check control and .hrap files.' /
      DATA MSG2  / 'bad read of data line in segment file: ' /
      DATA MSG3  / 'bad read of no-of-areas in segment file: ' /
      DATA MSG4  / 'bad read of basin name in segment file: ' /
      DATA MSG5  / 'cannot open segment file: ' /

        IF (ISTAT .GT. 0) THEN

          CALL WLIN('B',' ')

          LIN = ' '
          IF     (ISTAT .EQ.  5) THEN
            WRITE(LIN,FM3,IOSTAT=JE) MSG5,FLNAM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
          ELSEIF (ISTAT .EQ.  4) THEN
            WRITE(LIN,FM3,IOSTAT=JE) MSG4,FLNAM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSGL,LNUM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
          ELSEIF (ISTAT .EQ.  3) THEN
            WRITE(LIN,FM3,IOSTAT=JE) MSG3,FLNAM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSGL,LNUM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
          ELSEIF (ISTAT .EQ.  2) THEN
            WRITE(LIN,FM2,IOSTAT=JE) MSG2,FLNAM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSGL,LNUM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
          ELSEIF (ISTAT .EQ.  1) THEN
            WRITE(LIN,FM2,IOSTAT=JE) MSG1,FLNAM
            IF (JE.EQ.0) CALL WLIN('E',LIN)
            LIN = ' '
            WRITE(LIN,FM1,IOSTAT=JE) MSG1A
            IF (JE.EQ.0) CALL WLIN('S',LIN)
          ENDIF

          CALL WLIN('B',' ')

        ENDIF

      RETURN
      END
