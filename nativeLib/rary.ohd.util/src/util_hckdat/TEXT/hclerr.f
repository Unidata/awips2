C MODULE HCLERR
C  =====================================================================
C  pgm: HCLERR .. HCL Error/Warning message output routine
C
C  use:     CALL HCLERR(CMD,RTN,NUM,IERR)
C
C   in: CMD ..... control char for command to be performed - CHAR*(*)
C   in:             'I' ... Initialize number of warngs/errs to 0,
C   in:                     set (or reset) unit numbers IU1, IU2, IU3
C   in:             'W' ... output warning message on IU1
C   in:             'E' ... output error message on IU1
C   in:             'M' ... output general message on IU2
C   in:             'X' ... output warning on IU3 if IWHERE.LE.1
C   in:                      and MODWRN.NE.0
C   in:             '+' ... turn all output messages on (default)
C   in:             '-' ... turn all output messgs off (IERR still set)
C   in: RTN ..... for CMD='W','E','X'; calling routine's name - CHAR*(*)
C   in:           for CMD='M'; output statement string
C   in: NUM ..... for CMD='W' or 'E', number of warng/err messg - INT
C   in:            (number is 1 to variable "LIMIT")
C  out: IERR .... error status: set to 1 for error, -1 for warning - INT
C  out:           (currently status is 1 for warnings also)
C   in: (common)  'uiox' contains var LP (output unit for messages)
C   in: (common)  'common/modctl' contains var IWHERE (for debug output)
C   in: (common)  'common/fpwarn' contains var MODCTL (for debug output)
C
C  rqd: KKEND,WARN,ERROR
C  =====================================================================
      SUBROUTINE HCLERR(CMD,RTN,NUM,IERR)

      EXTERNAL    KKEND,WARN,ERROR

      INCLUDE 'uiox'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/modctl'

      CHARACTER*(*)  CMD,RTN
      CHARACTER      CMDT*1,RTNT*16,MESW*16,MESE*14
      CHARACTER*62   MSG(45)
      CHARACTER*62   MM1(15),MM2(15),MM3(15)
      INTEGER        NUM,IERR,INITZ,IU1,IU2,IU3,IOFF,LIMIT,JER
      INTEGER        IENDM,IENDR

      EQUIVALENCE (MSG(1),MM1),(MSG(16),MM2),(MSG(31),MM3)

      SAVE    INITZ,IU1,IU2,IU3,IOFF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_hckdat/RCS/hclerr.f,v $
     . $',                                                             '
     .$Id: hclerr.f,v 1.2 2005/03/18 14:56:39 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    INITZ,IU1,IU2,IU3,IOFF,LIMIT / 0,-1,-1,-1,0,45 /
      DATA    MESW / '0**WARNING** in ' /
      DATA    MESE / '0**ERROR** in '   /

      DATA MM1
     $/'                                                              '    001
     $,'Number of characters in date field is incorrect.              '    002
     $,'Reading preprocessor data base control records.               '    003
     $,'Data type PP24 not found in preprocessor data base.           '    004
     $,'Expected "+" or "-" not found in date field.                  '    005
     $,'"HOUR" specified in date field is not in 0 to 24 range.       '    006
     $,'Specified hour does NOT divide evenly into 24.                '    007
     $,'Date field starts with a bad character.                       '    008
     $,'Not enough numbers in date field.                             '    009
     $,'"MONTH" specified in date field is not in 1 to 12 range.      '    010
     $,'"DAY" specified in date field is not in 1 to 31 range.        '    011
     $,'Year field is too small (need 4 digits).                      '    012
     $,'Bad character conversion in hour field after year number.     '    013
     $,'"YEAR" specified in date field is out of range.               '    014
     $,'Number of characters in time zone code exceeds 4.             '/   015
      DATA MM2
     $/'Given time zone code is invalid.                              '    016
     $,'Unexpected end of characters in date field.                   '    017
     $,'Bad characters in the day field of the * format.              '    018
     $,'Bad characters in the time interval of the # format.          '    019
     $,'Bad character conversion in month field.                      '    020
     $,'Bad character conversion in day field.                        '    021
     $,'Bad character conversion in slash year field.                 '    022
     $,'Bad character conversion in hour or year field after mmdd.    '    023
     $,'Could not get year for given month-day in routine DDECML.     '    024
     $,'Negative number in date field for hour or year number.        '    025
     $,'C A U T I O N - date format mmddyy is obsolete (now mmddhh)!  '    026
     $,'"HOUR" specified in # date field is not in 1 to 24 range.     '    027
     $,'Extra characters found at end of "#" format.                  '    028
     $,'Only one character given for hour number.                     '    029
     $,'                                                              '/   030
      DATA MM3
     $/'                                                              '    031
     $,'                                                              '    032
     $,'                                                              '    033
     $,'                                                              '    034
     $,'                                                              '    035
     $,'                                                              '    036
     $,'                                                              '    037
     $,'                                                              '    038
     $,'                                                              '    039
     $,'                                                              '    040
     $,'                                                              '    041
     $,'                                                              '    042
     $,'                                                              '    043
     $,'================== debug error =====================          '    044
     $,'Unknown error number given                                    '/   045

        IERR = 0
        CMDT = CMD

        IF (CMDT.EQ.'I' .OR. INITZ.EQ.0) THEN
          INITZ = 1
          IOFF  = 0

C  Unit numbers for output are: IU1 ... for warnings and errors
C                               IU2 ... for messages
C                               IU3 ... for warnings (limited)

          IF (LP.GE.0 .AND. LP.LE.99) IU1 = LP
          IF (LP.GE.0 .AND. LP.LE.99) IU2 = LP
          IF (IWHERE.LE.1 .AND. MODWRN.NE.0) THEN
            IF (LP.GE.0 .AND. LP.LE.99) IU3 = LP
          ENDIF
        ENDIF

        IF (CMDT .EQ. 'W') THEN
          IERR = 1
          IF (IU1.GE.0 .AND. IOFF.EQ.0) THEN
            IF (NUM.LT.1 .OR. NUM.GT.LIMIT) NUM = LIMIT
            RTNT = RTN
            CALL KKEND(RTNT,IENDR)
            CALL KKEND(MSG(NUM),IENDM)
            WRITE(IU1,'(A,A,'' ('',I2,'') '',A)',IOSTAT=JER)
     $       MESW,RTNT(1:IENDR),NUM,MSG(NUM)(1:IENDM)
            CALL WARN()
          ENDIF

        ELSEIF (CMDT .EQ. 'E') THEN
          IERR = 1
          IF (IU1.GE.0 .AND. IOFF.EQ.0) THEN
            IF (NUM.LT.1 .OR. NUM.GT.LIMIT) NUM = LIMIT
            RTNT = RTN
            CALL KKEND(RTNT,IENDR)
            CALL KKEND(MSG(NUM),IENDM)
            WRITE(IU1,'(A,A,''('',I2,'') '',A)',IOSTAT=JER)
     $       MESE,RTNT(1:IENDR),NUM,MSG(NUM)(1:IENDM)
            CALL ERROR()
          ENDIF

        ELSEIF (CMDT .EQ. 'M') THEN
          IERR = 0
          IF (IU2.GE.0 .AND. IOFF.EQ.0) THEN
            CALL KKEND(RTN,IENDR)
            WRITE(IU2,'(A)',IOSTAT=JER) RTN(1:IENDR)
          ENDIF

        ELSEIF (CMDT .EQ. 'X') THEN
          IERR = 1
          IF (IU3.GE.0 .AND. IOFF.EQ.0) THEN
            IF (NUM.LT.1 .OR. NUM.GT.LIMIT) NUM = LIMIT
            RTNT = RTN
            CALL KKEND(RTNT,IENDR)
            CALL KKEND(MSG(NUM),IENDM)
            WRITE(IU3,'(A,A,'' ('',I2,'') '',A)',IOSTAT=JER)
     $       MESW,RTNT(1:IENDR),NUM,MSG(NUM)(1:IENDM)
            CALL WARN()
          ENDIF

        ELSEIF (CMDT .EQ. '+') THEN
          IOFF = 0

        ELSEIF (CMDT .EQ. '-') THEN
          IOFF = 1

        ENDIF

      RETURN
      END
