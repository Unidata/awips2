C MODULE MXL_ERRS
C  =====================================================================
C  pgm: MXL_ERRS .. Output errors or notes from processing the hrap name
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXL_ERRS(LLRUN,NAMLLG,ISTAT)

      EXTERNAL        WLIN

      CHARACTER*(*)   NAMLLG
      CHARACTER*200   LIN
      CHARACTER*5     FM2
      CHARACTER*3     FM3
      CHARACTER*46    MSG1
      CHARACTER*45    MSG2
      CHARACTER*35    MSG3
      CHARACTER*47    MSG4
      CHARACTER*41    MSG5
      CHARACTER*43    MSG6
      CHARACTER*34    MSG7
      CHARACTER*34    MSG8
      CHARACTER*22    MSG8A
      CHARACTER*42    MSG9
      CHARACTER*44    MSG10
      CHARACTER*46    MSG11
      CHARACTER*10    MSG12
      INTEGER         LLRUN,ISTAT,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxl_errs.f,v $
     . $',                                                             '
     .$Id: mxl_errs.f,v 1.2 2002/02/12 17:21:22 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  FM2,FM3 / '(A,A)', '(A)' /
      DATA MSG1 / 'bad unit number in hrap filename routine' /
      DATA MSG2 / 'bad input filename in hrap filename routine: ' /
      DATA MSG3 / 'hrap filename begins with a blank: ' /
      DATA MSG4 / 'pathname is too long in hrap filename routine: ' /
      DATA MSG5 / 'no lat/lon or hrap file exists for name: ' /
      DATA MSG6 / 'only the input lat/lon file was found for: ' /
      DATA MSG7 / 'only the hrap file was found for: ' /
      DATA MSG8 / 'the hrap file already exists for: ' /
      DATA MSG8A/ 'it will be overwritten' /
      DATA MSG9 / 'the hrap file was only found locally for: ' /
      DATA MSG10/ 'the hrap file will not be generated since it' /
      DATA MSG11/ 'already exists, but the input lat/lon file was' /
      DATA MSG12/ 'not found!' /

        CALL WLIN('B',' ')

        LIN = ' '
        IF     (ISTAT .EQ.  5) THEN
          WRITE(LIN,FM3,IOSTAT=JE) MSG1
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (ISTAT .EQ.  4) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG2,NAMLLG
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (ISTAT .EQ.  3) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG3,NAMLLG
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (ISTAT .EQ.  2) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG4,NAMLLG
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (ISTAT .EQ.  1) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG5,NAMLLG
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (ISTAT .EQ.  0) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG6,NAMLLG
          IF(JE.EQ.0) CALL WLIN('N',LIN)
        ELSEIF (ISTAT .EQ. -1) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG7,NAMLLG
          IF(JE.EQ.0) CALL WLIN('N',LIN)
        ELSEIF (ISTAT .EQ. -2) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG8,NAMLLG
          IF(JE.EQ.0) CALL WLIN('N',LIN)
          LIN = ' '
          WRITE(LIN,FM3,IOSTAT=JE) MSG8A
          IF(JE.EQ.0) CALL WLIN('S',LIN)
        ELSEIF (ISTAT .EQ. -3) THEN
          WRITE(LIN,FM2,IOSTAT=JE) MSG9,NAMLLG
          IF(JE.EQ.0) CALL WLIN('N',LIN)
        ENDIF

        CALL WLIN('B',' ')

        IF ( (LLRUN.EQ. 1 .OR. LLRUN.EQ. 2) .AND.
     $       (ISTAT.EQ.-1 .OR. ISTAT.EQ.-3)       ) THEN
          LLRUN = 3
          LIN = ' '
          WRITE(LIN,FM3,IOSTAT=JE) MSG10
          IF(JE.EQ.0) CALL WLIN('W',LIN)
          LIN = ' '
          WRITE(LIN,FM3,IOSTAT=JE) MSG11
          IF(JE.EQ.0) CALL WLIN('S',LIN)
          LIN = ' '
          WRITE(LIN,FM3,IOSTAT=JE) MSG12
          IF(JE.EQ.0) CALL WLIN('S',LIN)
          CALL WLIN('B',' ')
        ENDIF

      RETURN
      END
