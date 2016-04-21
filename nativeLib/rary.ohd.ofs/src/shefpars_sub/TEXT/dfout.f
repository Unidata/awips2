C  =====================================================================
C  pgm: DFOUT .. Display contents of "shefout" file in wide "ofs" formt
C
C  use:      CALL DFOUT(LUOUT,LUDIS)
C
C   in: LUOUT ..... logical unit number of opened "shefout" file - INT
C   in: LUDIS ..... logical unit number of opened display file - INT
C  =====================================================================
      SUBROUTINE DFOUT(LUOUT,LUDIS)

      REAL               CODP
      DOUBLE PRECISION   VALU
      INTEGER            LUOUT,LUDIS,IDATE(12),ISTAT,IDUR,IREV,ITIME
      CHARACTER*8        KHID,JID,KHEND,PARCOD,BLNK8
      CHARACTER*4        PA1,PA2,PA4,PA5,PA6,IQUAL

C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_sub/RCS/dfout.f,v $
     . $',                                                             '
     .$Id: dfout.f,v 1.3 2000/12/18 21:10:56 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    KHEND / 'ZCZC    ' /

      IF (LUOUT.GT.0 .AND. LUDIS.GE.0) THEN

C                   Make sure shefout file is at beginning

      REWIND LUOUT

C                   Output report headers

      WRITE(LUDIS,'(//4(''*** LISTING OF SHEFOUT FILE'',3X),''***''/)')

      WRITE (LUDIS,50)
   50 FORMAT (/2X,'STATION',8X,'OBSERVATION',11X,'CREATION',5X,'PE',
     1    3X,'DUR',2X,'TYPE',1X,'SRCE',1X,'EXTRM',2X,'PROB',6X,'DATA',
     2    8X,'DATA',3X,'REV',3X,'DATA',2X,'TIME SERIES'/5X,'ID',13X,
     3    'DATE',18X,'DATE',7X,5('CODE',1X),2X,'CODE',
     4    6X,'VALUE',7X,'QUALF',2X,'CODE',2X,'SRCE',2X,'INDICATOR'/)

C                   Read each line in the shefout file and display

   60   READ(LUOUT,IOSTAT=ISTAT) KHID,IDATE,PA1,PA2,IDUR,PA4,PA5,PA6,
     1     CODP,VALU,IQUAL,IREV,JID,ITIME,PARCOD,BLNK8

        IF (ISTAT .NE. 0) GOTO 100
         IF (KHID .EQ. KHEND) THEN
          WRITE(LUDIS,'(5X,A4)',IOSTAT=ISTAT) KHEND(1:4)
         ELSE
          WRITE(LUDIS,70,IOSTAT=ISTAT)
     1       KHID,IDATE,PA1,PA2,IDUR,PA4,PA5,PA6,
     2       CODP,VALU,IQUAL,IREV,JID,ITIME
         ENDIF

         GOTO 60
  100   CONTINUE

   70 FORMAT (3X,A8,2X,2(I4.4,'/',I2.2,'/',I2.2,'-',I2.2,':',I2.2,':',
     $        I2.2,1X),1X,A1,A1,2X,I4,2X,3(A1,4X),F6.3,1X,F15.2,2X,
     $        A4,3X,I1,3X,A8,3X,I1)

      ENDIF

      RETURN
      END
