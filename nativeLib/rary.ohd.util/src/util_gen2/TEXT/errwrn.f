C MODULE ERRWRN
C-----------------------------------------------------------------------
C
      SUBROUTINE ERRWRN (CALLER)
C
C  THIS ROUTINE KEEPS TRACK OF THE NUMBER OF ERROR AND WARNING
C  MESSAGES GENERATED WITHIN VARIOUS NWSRFS PROGRAMS AND
C  PRINTS MESSAGES AS TO WHERE THE ERROR OR WARNING OCCURRED.
C
      CHARACTER*(*) CALLER
      CHARACTER*8 EWTYPE,OPID,OPNAM
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/killcd'
      INCLUDE 'common/toterz'
      INCLUDE 'common/errdat'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/errwrn.f,v $
     . $',                                                             '
     .$Id: errwrn.f,v 1.4 2001/06/19 12:05:00 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA IBLNK/4H    /
      DATA BLNK/4H    /
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ERRWRN :',
     *   ' CALLER=',CALLER,
     *   ' MAINUM=',MAINUM,
     *   ' IOPNUM=',IOPNUM,
     *   ' '
C
      CALL UMEMOV (OPNAME,OPNAM,2)
      IF (OPNAM.EQ.' '.OR.OPNAM.EQ.'**NONE**') GO TO 20
C
      IF (CALLER.EQ.'ERROR') THEN
C     COUNT THE ERROR
         NERRS=NERRS+1
         NERRST=NERRST+1
         IF (KLCODE.LT.8) KLCODE=8
         EWTYPE='ERROR'
         ENDIF
C
      IF (CALLER.EQ.'WARN') THEN
C     COUNT THE WARNING
         NWARN=NWARN+1
         NWARNT=NWARNT+1
         IF (KLCODE.LT.4) KLCODE=4
         EWTYPE='WARNING'
         ENDIF
C
      IF (IOPNUM.LE.0) GO TO 10
C
C  ERROR OR WARNING ASSOCIATED WITH AN OPERATION
C
C  GET THE OPERATION IDENTIFIER
      CALL FOPCDX (OPID,IOPNUM)
      IF (MAINUM.LE.2) THEN
C     OPERATIONAL FORECAST SYSTEM PROGRAM
         WRITE (IPR,30) EWTYPE(1:LENSTR(EWTYPE)),
     *      'SEGMENT',ISEG,'OPERATION',OPID,OPNAME
         IF (IOERR.NE.IPR) THEN
            WRITE (IOERR,40) EWTYPE(1:LENSTR(EWTYPE)),
     *         'SEGMENT',ISEG,'OPERATION',OPID,OPNAME
            ENDIF
         GO TO 20
         ELSE
C        CALIBRATION SYSTEM PROGRAM
            WRITE (IPR,50) EWTYPE(1:LENSTR(EWTYPE)),
     *         'OPERATION',OPID,OPNAME
            IF (IOERR.NE.IPR) THEN
               WRITE (IOERR,60) EWTYPE(1:LENSTR(EWTYPE)),
     *            'OPERATION',OPID,OPNAME
               ENDIF
            GO TO 20
         ENDIF
C
C  ERROR OR WARNING ASSOCIATED WITH A ROUTINE AND WITHIN A SEGMENT
10    IF (IOPNUM.EQ.0) THEN
         IF (MAINUM.LE.2) THEN
C        OPERATIONAL FORECAST SYSTEM PROGRAM
            IF (ISEG(1).EQ.IBLNK.AND.ISEG(2).EQ.IBLNK.AND.
     *          OPNAME(1).EQ.BLNK.AND.OPNAME(2).EQ.BLNK) THEN
               ELSE
                  WRITE (IPR,70) EWTYPE(1:LENSTR(EWTYPE)),
     *               'SEGMENT',ISEG,'ROUTINE',OPNAME
                  IF (IOERR.NE.IPR) THEN
                     WRITE (IOERR,80) EWTYPE(1:LENSTR(EWTYPE)),
     *                   'SEGMENT',ISEG,'ROUTINE',OPNAME
                     ENDIF
               ENDIF
            GO TO 20
            ELSE
C           CALIBRATION SYSTEM PROGRAM
               WRITE (IPR,90) EWTYPE(1:LENSTR(EWTYPE)),
     *            'ROUTINE',OPNAME
               IF (IOERR.NE.IPR) THEN
                  WRITE (IOERR,100) EWTYPE(1:LENSTR(EWTYPE)),
     *               'ROUTINE',OPNAME
                  ENDIF
               GO TO 20
            ENDIF
         ENDIF
C
C ERROR OR WARNING ASSOCIATED WITH A ROUTINE AND NOT WITHIN A SEGMENT
      IF (IOPNUM.EQ.-1) THEN
         WRITE (IPR,90) EWTYPE(1:LENSTR(EWTYPE)),
     *      'ROUTINE',OPNAME
         IF (IOERR.NE.IPR) THEN
            WRITE (IOERR,100) EWTYPE(1:LENSTR(EWTYPE)),
     *         'ROUTINE',OPNAME
            ENDIF
         GO TO 20
         ENDIF
C
C  ROUTINE ASSOCIATED WITH AN AREA
      IF (IOPNUM.EQ.-2) THEN
         WRITE (IPR,70) EWTYPE(1:LENSTR(EWTYPE)),
     *      'AREA',ISEG,'ROUTINE',OPNAME
         IF (IOERR.NE.IPR) THEN
            WRITE (IOERR,80) EWTYPE(1:LENSTR(EWTYPE)),
     *          'AREA',ISEG,'ROUTINE',OPNAME
            ENDIF
         GO TO 20
         ENDIF
C
C  ROUTINE ASSOCIATED WITH A STATION
      IF (IOPNUM.EQ.-3) THEN
         WRITE (IPR,70) EWTYPE(1:LENSTR(EWTYPE)),
     *      'STATION',ISEG,'ROUTINE',OPNAME
         IF (IOERR.NE.IPR) THEN
            WRITE (IOERR,80) EWTYPE(1:LENSTR(EWTYPE)),
     *          'STATION',ISEG,'ROUTINE',OPNAME
            ENDIF
         GO TO 20
         ENDIF
C
20    IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ERRWRN'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' **NOTE** THE ABOVE ',A,' OCCURRED IN ',A,
     *   ' ',2A4,' FOR ',A,' ',A,' AND OPERATION NAME ',2A4,'.')
40    FORMAT (' ',A,' OCCURRED IN ',A,
     *   ' ',2A4,' FOR ',A,' ',A,' AND OPERATION NAME ',2A4,'.')
50    FORMAT (' **NOTE** THE ABOVE ',A,' OCCURRED IN ',A,
     *   ' ',A,' FOR ',2A4,'.')
60    FORMAT (' ',A,' OCCURRED IN ',A,
     *   ' ',A,' FOR ',2A4,'.')
70    FORMAT (' **NOTE** THE ABOVE ',A,' OCCURRED IN ',A,
     *   ' ',2A4,' FOR ',A,' ',2A4,'.')
80    FORMAT (' ',A,' OCCURRED IN ',A,
     *   ' ',2A4,' FOR ',A,' ',2A4,'.')
90    FORMAT (' **NOTE** THE ABOVE ',A,' OCCURRED IN ',A,
     *   ' ',2A4,'.')
100   FORMAT (' ',A,' OCCURRED IN ',A,
     *   ' ',2A4,'.')
C
      END
