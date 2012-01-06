C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE HCLDBG
C  =====================================================================
C  pgm: HCLDBG .. HCL debug message output routine
C
C  use:     CALL HCLDBG(CMD,LVL,MESSG)
C
C   in: CMD ..... control char for command to be performed - CHAR*1
C   in:             'I' ... Initialize number of warngs/errs to 0
C   in:                     set (or reset) unit number IU2
C   in:             'D' ... output general debug message on IU2 if
C   in:                     IHCLDB is set to input value LVL or greater
C   in:             'T' ... output general trace message on IU2 if
C   in:                     IHCLTR is set to input value LVL or greater
C   in:             '+' ... turn output messages on (default)
C   in:             '-' ... turn output messages off (IERR still set)
C   in: LVL ..... message level (0 no message usually, 1 least likely,
C   in:           2+ increasing likelyhood of being output) - INT
C   in:            For example, for higher IHCLDB values such as 3, only
C   in:            LVL=3 will be output, but for IHCLDB=1, all LVL=1,2,
C   in:            or 3 will be output
C   in: MESSG ... message string to be output - CHAR*(*)
C   in: (common)  'udebug' var IOGDB ... debug output unit number,
C   in:                    var IHCLDB .. priority level for debug messg
C   in:                    var IHCLTR .. priority level for enter/exit
C   in:                                  trace messages
C
C  rqd: KKEND,GET_APPS_DEFAULTS
C  =====================================================================
      SUBROUTINE HCLDBG(CMD,LVL,MESSG)

cc AV pgf90 port 7/3/01      EXTERNAL    KKEND,GET_APPS_DEFAULTS
      EXTERNAL    KKEND

      INCLUDE 'udebug'

      CHARACTER*(*)  CMD,MESSG
      CHARACTER      CMDT*1,REP*128
      INTEGER        LVL,INITZ,IU2,IOFF,LVLDB,LVLTR,JER,IEND
      INTEGER        LENREP,ITEMP

      SAVE    INITZ,IU2,IOFF,LVLDB,LVLTR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_hckdat/RCS/hcldbg.f,v $
     . $',                                                             '
     .$Id: hcldbg.f,v 1.2 2002/02/11 19:22:23 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    INITZ,IU2,IOFF,LVLDB,LVLTR / 0,-1,0,99,99 /

        CMDT = CMD

        IF (CMDT.EQ.'I' .OR. INITZ.EQ.0) THEN
          INITZ = 1
          IOFF  = 0

C  Can use get_apps_defaults to get debug/trace level info

          CALL GET_APPS_DEFAULTS('IHCLDB',6,REP,LENREP)
            JER = 1
            IF (LENREP .EQ. 1) READ(REP,'(I1)',IOSTAT=JER) ITEMP
            IF (LENREP .EQ. 2) READ(REP,'(I2)',IOSTAT=JER) ITEMP
            IF (JER .EQ. 0) IHCLDB = ITEMP
          CALL GET_APPS_DEFAULTS('IHCLTR',6,REP,LENREP)
            JER = 1
            IF (LENREP .EQ. 1) READ(REP,'(I1)',IOSTAT=JER) ITEMP
            IF (LENREP .EQ. 2) READ(REP,'(I2)',IOSTAT=JER) ITEMP
            IF (JER .EQ. 0) IHCLTR = ITEMP

C  Set HCL debug and trace levels (may be set by blockdata or apps)

          LVLDB = 99
          LVLTR = 99
          IF (IHCLDB.GT.0 .AND. IHCLDB.LT.99) LVLDB = IHCLDB
          IF (IHCLTR.GT.0 .AND. IHCLTR.LT.99) LVLTR = IHCLTR

C  Unit number for output is: IU2 ... for messages

          IF (IOGDB.GE.0 .AND. IOGDB.LE.99) IU2 = IOGDB
        ENDIF

        IF (CMDT .EQ. 'D') THEN
          IF (IU2.GE.0 .AND. IOFF.EQ.0 .AND. LVL.GE.LVLDB) THEN
            CALL KKEND(MESSG,IEND)
            WRITE(IU2,'(A)',IOSTAT=JER) MESSG(1:IEND)
          ENDIF

        ELSEIF (CMDT .EQ. 'T') THEN
          IF (IU2.GE.0 .AND. IOFF.EQ.0 .AND. LVL.GE.LVLTR) THEN
            CALL KKEND(MESSG,IEND)
            WRITE(IU2,'(A)',IOSTAT=JER) MESSG(1:IEND)
          ENDIF

        ELSEIF (CMD .EQ. '+') THEN
          IOFF = 0

        ELSEIF (CMD .EQ. '-') THEN
          IOFF = 1

        ENDIF

      RETURN
      END
