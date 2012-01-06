C MODULE MXC_CHEK
C  =====================================================================
C  pgm: MXC_CHEK .. Check values of control file input
C
C  rqd: WLIN,MXC_WBKH
C
C  cmt: Some input args may not be used; later add check for NAREA
C  cmt:  less than MAXBSN, if NAREA is 0 set BASINS all to blanks.
C  =====================================================================
      SUBROUTINE MXC_CHEK(ISTART,IEND,LLRUN,NAMLLG,DIRGRD,
     $                    PREF,NYY,DT,ICV,RMISS,UNIT,NCOL,
     $                    DIRMAP,MAXBSN,NAREA,BASINS,
     $                    JSTAT)

      EXTERNAL        WLIN,MXC_WBKH

      CHARACTER*(*)   NAMLLG,DIRGRD,DIRMAP
      CHARACTER*(*)   PREF,UNIT
      CHARACTER*8     BASINS(*)
      INTEGER         ISTART,IEND,LLRUN,NYY,DT,ICV
      INTEGER         RMISS,NCOL,MAXBSN,NAREA
      INTEGER         JSTAT
      INTEGER         JE,IDEFLT,LLRUNS
      CHARACTER*200   LIN
      CHARACTER*7     FM1
      CHARACTER*11    FM2
      CHARACTER*5     FM3
      CHARACTER*26    MSG1
      CHARACTER*24    MSG2
      CHARACTER*30    MSG3
      CHARACTER*34    MSG4
      CHARACTER*19    MSG5
      CHARACTER*30    MSG6
      CHARACTER*38    MSG7
      CHARACTER*41    MSG8
      CHARACTER*39    MSG9
      CHARACTER*28    MSG10
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_chek.f,v $
     . $',                                                             '
     .$Id: mxc_chek.f,v 1.1 2001/06/13 09:19:37 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA FM1   / '(A,I10)' /
      DATA FM2   / '(A,I10,I10)' /
      DATA FM3   / '(A,A)' /
      DATA MSG1  / 'start date out of bounds: ' /
      DATA MSG2  / 'end date out of bounds: ' /
      DATA MSG3  / 'start date is after end date: ' /
      DATA MSG4  / 'bad code for obtaining hrap file: ' /
      DATA MSG5  / 'bad time interval: ' /
      DATA MSG6  / 'bad number of output columns: ' /
      DATA MSG7  / 'number of digits in year is not 2 or 4' /
      DATA MSG8  / 'option to generate time series not 0 or 1' /
      DATA MSG9  / 'missing data option should be 0 or -999' /
      DATA MSG10 / 'unit code is not MM nor IN: ' /

        JSTAT  = 0
        IDEFLT = -9090

C  Check for ISTART and IEND

        IF (ISTART .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'start date')
        ELSEIF (ISTART.LE.0 .AND. (LLRUN.EQ.2 .OR.
     $          LLRUN.EQ.3 .OR. LLRUN.EQ.6 .OR. LLRUN.EQ.8)) THEN
          CALL WLIN('W','no start date given for MAPX operation')
          LLRUNS = LLRUN
          IF (LLRUN .EQ. 2) THEN
            LLRUN = 1
          ELSEIF(LLRUN .EQ. 3) THEN
            LLRUN = 0
          ELSEIF(LLRUN .EQ. 6) THEN
            LLRUN = 5
          ELSEIF(LLRUN .EQ. 8) THEN
            LLRUN = 7
          ENDIF
          LIN = ' '
          WRITE(LIN,'(''run-code (LLRUN) reset from'',I3,'' to'',I3)',
     $          IOSTAT=JE) LLRUNS,LLRUN
          IF (JE.EQ.0) CALL WLIN('S',LIN)
        ELSEIF ( (ISTART.GT.0 .AND. ISTART.LT.010100) .OR.
     $        ISTART.GT.12312100                       ) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG1,ISTART
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

        IF (IEND .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'end date')
        ELSEIF ((IEND.LE.0 .AND. ISTART.GT.0) .AND. (LLRUN.EQ.2 .OR.
     $          LLRUN.EQ.3 .OR. LLRUN.EQ.6 .OR. LLRUN.EQ.8)) THEN
          CALL WLIN('W','no end date given for MAPX operation')
          IEND = ISTART
          CALL WLIN('S','end date set reset to start date')
        ELSEIF ( (IEND.GT.0 .AND. IEND.LT.010100) .OR.
     $        IEND.GT.12312100                     ) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG2,ISTART
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

        IF (ISTART.GT.0 .AND. IEND.GT.0 .AND.
     $      ISTART.GT.IEND                    ) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM2,IOSTAT=JE) MSG3,ISTART,IEND
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for LLRUN

        IF (LLRUN .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'hrap source flag')
        ELSEIF (LLRUN.LT.0 .OR. LLRUN.GT.8) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG4,LLRUN
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for NYY

        IF (NYY .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'number of digits in year')
        ELSEIF (NYY.NE.2 .AND. NYY.NE.4) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG7,NYY
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for DT

        IF (DT .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'time interval')
        ELSEIF (DT.LT.1 .OR. DT.GT.24) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG5,DT
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for ICV

        IF (ICV .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'time series option')
        ELSEIF (ICV.LT.0 .OR. ICV.GT.1) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG8,ICV
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for RMISS

        IF (RMISS .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'missing data value')
        ELSEIF (RMISS.NE.0 .AND. RMISS.NE.-999) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG9,RMISS
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF
 
C  Check UNIT

        IF (UNIT.NE.'MM' .AND. UNIT.NE.'IN') THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM3,IOSTAT=JE) MSG10,UNIT
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  Check for NCOL

        IF (NCOL .EQ. IDEFLT) THEN
          CALL MXC_WBKH(JSTAT,LIN,'number of columns')
        ELSEIF (NCOL.LT.1 .OR. NCOL.GT.100) THEN
          JSTAT = JSTAT+1
          LIN = ' '
          WRITE(LIN,FM1,IOSTAT=JE) MSG6,NCOL
          IF(JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

C  If there was an error, output a blank line

        CALL WLIN('B',' ')

      RETURN
      END
