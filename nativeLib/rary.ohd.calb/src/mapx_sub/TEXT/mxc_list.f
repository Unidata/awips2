C MODULE MXC_LIST
C  =====================================================================
C  pgm: MXC_LIST .. Output the values obtained from the control file
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXC_LIST(ISTART,IEND,LLRUN,NAMLLG,DIRGRD,
     $                    PREF,NYY,DT,ICV,RMISS,UNIT,NCOL,
     $                    DIRMAP,MAXBSN,NAREA,BASINS,XTIME)

      EXTERNAL        WLIN

      CHARACTER*(*)   NAMLLG,DIRGRD,DIRMAP
      CHARACTER*(*)   PREF,UNIT,XTIME
      CHARACTER*8     BASINS(*)
      INTEGER         ISTART,IEND,LLRUN,NYY,DT,ICV
      INTEGER         RMISS,NCOL,MAXBSN,NAREA
      INTEGER         JE,NEX
      CHARACTER*200   LIN
      CHARACTER*6     FM1
      CHARACTER*5     FM2
      CHARACTER*8     FM3
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_list.f,v $
     . $',                                                             '
     .$Id: mxc_list.f,v 1.1 2001/06/13 09:21:28 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   FM1,FM2,FM3 / '(A,I8)', '(A,A)', '(A,I8.8)' /

        CALL WLIN('B',' ')

        LIN = ' '
        JE = 0
        WRITE(LIN,FM2,IOSTAT=JE) '  ========>   ',
     $                           'MAPX CONTROL FILE INPUT VALUES'
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        CALL WLIN('B',' ')

        LIN = ' '
        WRITE(LIN,FM3,IOSTAT=JE) ' @A: ISTART is:    ',ISTART
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM3,IOSTAT=JE) '     IEND is:      ',IEND
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) ' @B: LLRUN is:     ',LLRUN
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) '     NAMLLG is:    ',NAMLLG
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) ' @C: DIRGRD is:    ',DIRGRD
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) ' @D: PREF is:      ',PREF
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) '     NYY is:       ',NYY
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) ' @E: DT is:        ',DT
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) '     ICV is:       ',ICV
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) '     RMISS is:     ',RMISS
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) ' @F: UNIT is:      ',UNIT
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) '     NCOL is:      ',NCOL
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) ' @G: DIRMAP is:    ',DIRMAP
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        WRITE(LIN,FM1,IOSTAT=JE) ' @I: NAREA is:     ',NAREA
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        IF (NAREA.GT.0 .AND. NAREA.LE.MAXBSN .AND. JE.EQ.0) THEN
          DO 20 NEX=1,NAREA
            LIN = ' '
            WRITE(LIN,'(''     BASINS('',I3,''):  '',A)',IOSTAT=JE)
     $            NEX,BASINS(NEX)
            IF(JE.EQ.0) CALL WLIN('M',LIN)
   20     CONTINUE
        ENDIF

        LIN = ' '
        WRITE(LIN,FM2,IOSTAT=JE) ' @J: XTIME is:     ',XTIME
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        CALL WLIN('B',' ')

      RETURN
      END
