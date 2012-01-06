C  =====================================================================
C  pgm: SHERRM .. Output err or warng message for given error number
C
C  use:     CALL SHERRM(CMD,NUM)
C
C   in: CMD ....... control char for command to be performed - CHAR*1
C   in:               'I' .. initialize vars (automatic for first entry)
C   in:               'W' .. output warning message
C   in:               'E' .. output error message
C   in:               'M' .. output message without warng or error stmt
C   in: NUM ....... number of the error message - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFERROR',<number>)
C
C  rqd: SHSAVU
C  =====================================================================
      SUBROUTINE SHERRM(CMD,NUM)

      EXTERNAL       SHSAVU

      INTEGER        NUM,JJ,NM,INITZ,LUNE,LSTAT
      CHARACTER*1    CMD
      CHARACTER*62   MSG(90)
      CHARACTER*62   MM1(15),MM2(15),MM3(15),MM4(15),MM5(15),MM6(15)

      EQUIVALENCE (MSG(1),MM1),(MSG(16),MM2),(MSG(31),MM3),(MSG(46),MM4)
      EQUIVALENCE (MSG(61),MM5),(MSG(76),MM6)

      SAVE           INITZ,LUNE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/shefpars_driv/RCS/sherrm.f,v $
     . $',                                                             '
     .$Id: sherrm.f,v 1.10 2000/03/14 14:21:25 page Exp jgofus $
     . $' /
C    ===================================================================
C

      DATA           INITZ / 0 /

      DATA MM1
     $/'                                                              '    001
     $,'Two digits are required in date or time group                 '    002
     $,'An expected parameter code is missing                         '    003
     $,'File read error while accessing data file                     '    004
     $,'No dot in column 1 when looking for new message               '    005
     $,'Dot found but not in column 1 of new message                  '    006
     $,'Unknown message type, looking for .A, .B, or .E               '    007
     $,'Bad char in message type format (or missing blank delimiter)  '    008
     $,'Last message format was different from this continuation messg'    009
     $,'Last message was NOT a revision unlike this continuation messg'    010
     $,'Last message had an error so cannot continue                  '    011
     $,'No positional data or no blank before it                      '    012
     $,'Bad character in station id                                   '    013
     $,'Station id has more than 8 characters                         '    014
     $,'Bad number in positional data date group                      '/   015
      DATA MM2
     $/'Incorrect number in date group                                '    016
     $,'Incorrect number in time group                                '    017
     $,'Missing blank char in positional data                         '    018
     $,'Bad creation date                                             '    019
     $,'Bad date code letter after the character "D"                  '    020
     $,'Unknown data qualifier, data value is lost                    '    021
     $,'Unknown data units code (need S or E)                         '    022
     $,'Unknown duration code                                         '    023
     $,'Bad 2-digit number following duration code                    '    024
     $,'Unknown time interval code (need Y,M,D,H,N,S,E)               '    025
     $,'Bad 2-digit number following time interval code               '    026
     $,'Bad character after "DR" (relative date code)                 '    027
     $,'Bad 1- or 2-digit number in relative date code                '    028
     $,'Bad character in parameter code                               '    029
     $,'Bad parameter code calls for send code                        '/   030
      DATA MM3
     $/'Trace for code other than PP, PC, PY, SD, SF, SW              '    031
     $,'Variable duration not defined                                 '    032
     $,'Bad character where delimiter is expected                     '    033
     $,'Non-existent value for given type and source parameter code   '    034
     $,'ZULU, DR, or DI has send code QY, PY, or HY                   '    035
     $,'Forecast data given without creation date                     '    036
     $,'No value given after parameter code and before slash or eol   '    037
     $,'Explicit date for codes DRE or DIE is not the end-of-month    '    038
     $,'Year not in good range (1753-2199)                            '    039
     $,'Exceeded limit of data items                                  '    040
     $,'Too many data items for given .B format                       '    041
     $,'Not enough data items for given .B format                     '    042
     $,'Cannot adjust forecast date to Zulu time                      '    043
     $,'Time between 0201 & 0259 on day changing from stnd to daylight'    044
     $,'No time increment specified (use DI code)                     '/   045
      DATA MM4
     $/'No ".END" message for previous ".B" format                    '    046
     $,'ID requires 3 to 8 characters                                 '    047
     $,'For DST, check Apr/Mar or Oct/Nov for 1976 thru 2040 only     '    048
     $,'Bad character in the message                                  '    049
     $,'Missing parameter code                                        '    050
     $,'Bad value chars (or missing delimiter), data may be lost      '    051
     $,'Bad character in data field                                   '    052
     $,'"?" not accepted for missing, use "M" or "+"                  '    053
     $,'Parameter code is too long or too short                       '    054
     $,'Missing delimiter between data type fields                    '    055
     $,'Missing delimiter after data type field                       '    056
     $,'Should use "/" after date, time, or other D-code; before data '    057
     $,'Parm codes PP and PC require decimal value                    '    058
     $,'Abort, cannot read "shefparm" file correctly                  '    059
     $,'Non-existent value for given duration parameter code          '/   060
      DATA MM5
     $/'Non-existent value for given extremum parameter code          '    061
     $,'Non-existent value for given conversion factor parameter code '    062
     $,'Non-existent value for given probability parameter code       '    063
     $,'Parameter code too short or field misinterpreted as param-code'    064
     $,'Comma not allowed in data field, data value is lost           '    065
     $,'Date check for yr-mo-da shows bad date                        '    066
     $,'No data on line identified with a message type format         '    067
     $,'An unexpected ".END" message was encountered                  '    068
     $,' BUMMER!!!  Maximum number of errors reached, abort message   '    069
     $,'Cannot output to binary shefpars file                         '    070
     $,'Cannot access "PE conversion factors" from the "shefparm" file'    071
     $,'Cannot access "send codes" from the "shefparm" file           '    072
     $,'Cannot access "duration codes" from the "shefparm" file       '    073
     $,'Cannot access "type/source codes" from the "shefparm" file    '    074
     $,'Cannot access "extremum codes" from the "shefparm" file       '/   075
      DATA MM6
     $/'Cannot access "probability codes" from the "shefparm" file    '    076
     $,'Cannot read "SHEFPARM" file!!!!!                              '    077
     $,'Bad character in data value, data value is lost               '    078
     $,'Julian day should be written with 3 digits                    '    079
     $,'Too many digits in date group!                                '    080
     $,'Too many characters in quotes                                 '    081
     $,'Data line found before completing .B format line(s)           '    082
     $,'Missing slash delimiter or bad time zone code                 '    083
     $,'Too many chars in qualifier code, data value is lost          '    084
     $,'Bad data qualifier, rest of format is lost                    '    085
     $,'Retained comment found without a data value, comment is lost  '    086
     $,'Unexpected slash found after parameter code, before data value'    087
     $,'Cannot access "qualifier codes" from the "shefparm" file      '    088
     $,'                                                              '    089
     $,'Unknown error number given                                    '/   090

        IF (CMD.EQ.'I' .OR. INITZ.EQ.0) THEN
          INITZ = 1
          CALL SHSAVU('G_SHEFERROR ',LUNE)
        ENDIF

        IF (CMD.EQ.'W' .OR. CMD.EQ.'E' .OR. CMD.EQ.'M') THEN
         IF (LUNE .GE. 0) THEN
          NM = NUM
          IF (NM.LT.1 .OR. NM.GT.90) NM = 90

          JJ = 62
   20     IF (MSG(NM)(JJ:JJ).NE.' ' .OR. JJ.LE.1) GOTO 30
            JJ = JJ-1
            GOTO 20
   30     CONTINUE

          IF (CMD .EQ. 'W') THEN
           WRITE(LUNE,'(''  ** WARNG'',I3,'' ** '',A)',IOSTAT=LSTAT)
     $           NM,MSG(NM)(1:JJ)
          ELSEIF (CMD .EQ. 'E') THEN
           WRITE(LUNE,'(''  ** ERROR'',I3,'' ** '',A)',IOSTAT=LSTAT)
     $           NM,MSG(NM)(1:JJ)
          ELSEIF (CMD .EQ. 'M') THEN
            WRITE(LUNE,'(1X,A)',IOSTAT=LSTAT) MSG(NM)(1:JJ)
          ENDIF
         ENDIF

        ENDIF

      RETURN
      END
