C MEMBER HSETPM
C  (from old member HCLSETPM)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/06/95.15:21:53 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HSETPM
C
C          ROUTINE:  HSETPM
C
C             VERSION:  1.0.0
C
C                DATE:  3-14-83
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C  DESCRIPTION:
C
C    THIS ROUTINE CHANGES VALUES IN THE USER PARAMETER FILE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NONE
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*8 KEYWD
      CHARACTER*8 RTNOLD
      CHARACTER*8 KEYOP(6)
     *            /'TZC     ','USERNAME','ZOFF    ',
     *             'DISPLAY ','CLKZONE ','INTERVAL'/
      CHARACTER*8 CZONES(7)
     *            /'Z',
     *             'EASTERN',
     *             'CENTRAL',
     *             'MOUNTAIN',
     *             'PACIFIC',
     *             'ALASKA',
     *             '????????'/
      CHARACTER*72 CHAR72
C
      DIMENSION IDFLTS(60)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'uunits'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'hclcommon/hcomnd'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsetpm.f,v $
     . $',                                                             '
     .$Id: hsetpm.f,v 1.1 1995/09/17 18:43:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      NKEYOP=6
      NZONES=6
      LDFLTS=60
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HSETPM'
C
C
      IDSPLY=0
      ICHNGE=0
C
C  READ FIRST CARD
      NNCARD=1
      CALL HCARDR (NNCARD,IERR)
      IF (IERR.NE.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,190)
         GO TO 180
         ENDIF
      NFLD=1
C
C  INCREMENT FIELD NUMBER
10    NFLD=NFLD+1
C
C  CHECK IF NEED TO READ NEXT CARD
      IF (NFLD.GT.NFIELD) THEN
         NNCARD=NNCARD+1
         IF (NNCARD.GT.NCARD) GO TO 140
         CALL HCARDR (NNCARD,IERR)
         IF (IERR.NE.0) THEN
            CALL ULINE (LP,2)
            WRITE (LP,190)
            GO TO 180
            ENDIF
         NFLD=1
         ENDIF
C
C  GET KEYWORD
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(KEYWD)) NCHAR=LEN(KEYWD)
      KEYWD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),KEYWD,NCHAR)
C
C  CHECK IF VALID KEYWORD
      DO 20 IKEYOP=1,NKEYOP
         IF (KEYWD.EQ.KEYOP(IKEYOP)) GO TO 40
20       CONTINUE
C
C  INVALID KEYWORD
30    CALL ULINE (LP,2)
      WRITE (LP,200) KEYWD
      GO TO 10
C
40    GO TO (60,70,80,90,100,130),IKEYOP
      GO TO 30
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  TZC KEYWORD
C
60    NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) THEN
         CALL ULINE (LP,2)
         WRITE (LP,210) KEYWD,NFLD
         GO TO 10
         ENDIF
C
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=4
      IF (NCHAR.GT.MCHAR) THEN
         CALL ULINE (LP,2)
         WRITE (LP,240) NFLD,MCHAR
         GO TO 10
         ENDIF
      IF (NCHAR.GT.LEN(CHAR72)) NCHAR=LEN(CHAR72)
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR72,NCHAR)
C
C  CHECK FOR VALID TIME ZONE CODE
      CALL HCKDTC (CHAR72,TIME(2),IERR)
      IF (IERR.GT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,220) CHAR72(1:LENSTR(CHAR72))
         GO TO 10
         ENDIF
C
      CALL SUBSTR (CHAR72,1,4,TIME(3),1)
      CALL ULINE (LP,2)
      WRITE (LP,230) TIME(3)
      IDSPLY=1
      ICHNGE=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  USER NAME KEYWORD
C
70    NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) THEN
         CALL ULINE (LP,2)
         WRITE (LP,210) KEYWD,NFLD
         GO TO 10
         ENDIF
C
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=8
      IF (NCHAR.GT.MCHAR) THEN
         CALL ULINE (LP,2)
         WRITE (LP,240) NFLD,MCHAR
         GO TO 10
         ENDIF
C
      HNAMRF(2)=IBLNK
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),HNAMRF,NCHAR)
      CALL ULINE (LP,2)
      WRITE (LP,250) HNAMRF
      IDSPLY=1
      ICHNGE=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ZOFF KEYWORD - OFFSET FROM 0Z THAT THE USER WANTS THE CALENDAR
C                 DATE REFERENCED BY THE '*' DATE CODE TO ADVANCE
C
80    NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) THEN
         CALL ULINE (LP,2)
         WRITE (LP,210) KEYWD,NFLD
         GO TO 10
         ENDIF
C
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(CHAR72)) NCHAR=LEN(CHAR72)
      CHAR72=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR72,NCHAR)
C
C  CHECK IF INTEGER VALUE
      CALL UCKINT (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.GT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,290) KEYOP(IKEYOP),CHAR72(1:LENSTR(CHAR72))
         GO TO 10
         ENDIF
C
C  DETERMINE NUMBER OF DIGITS IN VALUE
C  IF 1 OR 2, ONLY THE HOUR ASSUMED SPECIFIED
C  IF 3 OR 4, BOTH THE HOUR AND MINUTE ASSUMED SPECIFIED
      ISTART=IFSTRT(NFLD)
      IF (CHAR72(1:1).EQ.'-') ISTART=ISTART+1
      NDIGIT=IFSTOP(NFLD)-ISTART+1
      MCHAR=4
      IF (NDIGIT.GT.MCHAR) THEN
         CALL ULINE (LP,2)
         WRITE (LP,240) NFLD,MCHAR
         GO TO 10
         ENDIF
C
C  CHECK IF INTEGER VALUE
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
C
      IPOWER=4-(NDIGIT+1)/2*2
      INTEGR=INTEGR*10**IPOWER
C
      MINVAL=-200
      MAXVAL=1400
      IF (INTEGR.LT.MINVAL.OR.INTEGR.GT.MAXVAL) THEN
         CALL ULINE (LP,2)
         WRITE (LP,300) NFLD,INTEGR,MINVAL,MAXVAL
         GO TO 10
         ENDIF
C
      ZOFF=INTEGR
      CALL ULINE (LP,2)
      WRITE (LP,260) ZOFF
      IDSPLY=1
      ICHNGE=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DISPLAY KEYWORD
C
90    IDSPLY=-1
      GO TO 150
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CLKZONE KEYWORD - TIME ZONE OF THE MACHINE CLOCK
C
100   NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) THEN
         CALL ULINE (LP,2)
         WRITE (LP,210) KEYWD,NFLD
         GO TO 10
         ENDIF
C
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      MCHAR=LEN(CZONES(1))
      IF (NCHAR.GT.MCHAR) THEN
         CALL ULINE (LP,2)
         WRITE (LP,240) NFLD,MCHAR
         GO TO 10
         ENDIF
      IF (NCHAR.GT.LEN(CHAR72)) NCHAR=LEN(CHAR72)
      CHAR72=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR72,NCHAR)
C
      CALL ULENTH (CHAR72,LEN(CHAR72),LENGTH)
      DO 110 IZONES=1,NZONES
         IF (CHAR72(1:LENGTH).EQ.CZONES(IZONES)(1:LENGTH)) GO TO 120
110      CONTINUE
C
C  INVALID TIME ZONE CODE
      CALL ULINE (LP,2)
      WRITE (LP,270) CHAR72(1:LENSTR(CHAR72))
      GO TO 10
C
120   CALL SUBSTR (CZONES(IZONES),1,1,CLKZON,1)
      CALL ULINE (LP,2)
      WRITE (LP,280)
     *   CZONES(IZONES)(1:1),CZONES(IZONES)(1:LENSTR(CZONES(IZONES)))
      IDSPLY=1
      ICHNGE=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INTERVAL KEYWORD
C
130   NFLD=NFLD+1
      IF (NFLD.GT.NFIELD) THEN
         CALL ULINE (LP,2)
         WRITE (LP,210) KEYWD,NFLD
         GO TO 10
         ENDIF
C
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.LEN(CHAR72)) NCHAR=LEN(CHAR72)
      CHAR72=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),CHAR72,NCHAR)
C
C  CHECK IF INTEGER VALUE
      CALL UCKINT (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.GT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,290) KEYOP(IKEYOP),CHAR72(1:LENSTR(CHAR72))
         GO TO 10
         ENDIF
C
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
      MINVAL=1
      MAXVAL=24
      IF (INTEGR.LT.MINVAL.OR.INTEGR.GT.MAXVAL) THEN
         CALL ULINE (LP,2)
         WRITE (LP,300) NFLD,INTEGR,MINVAL,MAXVAL
         GO TO 10
         ENDIF
      NMOD=24
      IF (MOD(NMOD,INTEGR).GT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,310) NFLD,INTEGR,NMOD
         GO TO 10
         ENDIF
C
      INTDFL=INTEGR
      CALL ULINE (LP,2)
      WRITE (LP,320) INTDFL
      IDSPLY=1
      ICHNGE=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  WRITE THE RECORD
140   IF (ICHNGE.EQ.1) THEN
         CALL UMEMST (IBLNK,IDFLTS,LDFLTS)
         CALL UMEMOV (TIME(1),IDFLTS,25)
         IUNIT=KUPARM
         CALL UWRITT (IUNIT,1,IDFLTS,IERR)
         IF (IERR.EQ.0) THEN
            CALL ULINE (LP,2)
            WRITE (LP,330)
            ELSE
               CALL ULINE (LP,2)
               WRITE (LP,340) IUNIT
            ENDIF
         ENDIF
C
150   IF (IDSPLY.EQ.0) GO TO 180
C
C  DISPLAY PARAMETERS
C
      CHAR72=' '
      CALL SUBSTR (CLKZON,1,1,CHAR72,1)
      DO 160 IZONES=1,NZONES
         IF (CHAR72.EQ.CZONES(IZONES)(1:1)) GO TO 170
160      CONTINUE
C
C  INVALID TIME ZONE CODE
      CALL ULINE (LP,2)
      WRITE (LP,350) CLKZON
      CALL WARN
      IZONES=NZONES+1
C
170   CALL ULINE (LP,2)
      WRITE (LP,360)
      CALL ULINE (LP,2)
      WRITE (LP,370) HNAMRF
      CALL ULINE (LP,2)
      WRITE (LP,380) TIME(3)
      CALL ULINE (LP,2)
      WRITE (LP,390) ZOFF
      CALL ULINE (LP,2)
      WRITE (LP,400)
     *   CZONES(IZONES)(1:1),CZONES(IZONES)(1:LENSTR(CZONES(IZONES)))
      CALL ULINE (LP,2)
      WRITE (LP,410) INTDFL
      IF (IDSPLY.EQ.-1) THEN
         IDSPLY=0
         GO TO 10
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HSETPM'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT ('0**ERROR** READING INPUT CARD.')
200   FORMAT ('0**ERROR** INVALID KEYWORD : ',A)
210   FORMAT ('0**ERROR** VALUE FOR KEYWORD ',A,' EXPECTED IN FIELD ',
     *   I2,'.')
220   FORMAT ('0**ERROR** ',A,' IS NOT A VALID TIME ZONE CODE.')
230   FORMAT ('0**NOTE** DEFAULT TIME ZONE CODE SET TO ',A4,'.')
240   FORMAT ('0**ERROR** VALUE IN FIELD ',I2,
     *   ' IS MORE THAN ',I2,' CHARACTERS.')
250   FORMAT ('0**NOTE** USER NAME SET TO ',2A4,'.')
260   FORMAT ('0**NOTE** ZOFF HAS BEEN SET. ',
     *   'THE CALENDAR DAY REFERENCED BY THE ''*'' WILL ',
     *   'INCREMENT AT ',I5.4,'Z.')
270   FORMAT ('0**ERROR** ',A,' IS NOT A VALID TIME ZONE FOR ',
     *   'SYSTEM CLOCK.')
280   FORMAT ('0**NOTE** SYSTEM CLOCK TIME ZONE SET TO ',A,
     *   ' (',A,').')
290   FORMAT ('0**ERROR** SPECIFIED VALUE FOR ',A,
     *   ' IS NOT AN INTEGER : ',A)
300   FORMAT ('0**ERROR** VALUE IN FIELD ',I2,
     *   ' (',I5,') IS INVALID. ',
     *   'MINIMUM VALUE IS ',I5,' AND MAXIMUM VALUE IS ',I5,'.')
310   FORMAT ('0**ERROR** VALUE IN FIELD ',I2,' (',I5,') ',
     *   ' DOES NOT DIVIDE EVENLY INTO ',I2,'.')
320   FORMAT ('0**NOTE** DEFAULT INTERVAL FOR THE ''#'' DATE CODE ',
     *   'SET TO ',I2,'.')
330   FORMAT ('0**NOTE** USER PARAMETERS SUCCESSFULLY UPDATED.')
340   FORMAT ('0**ERROR** WRITING TO UNIT ',I3,'.')
350   FORMAT ('0**WARNING** ',A4,' IS NOT A VALID TIME ZONE FOR ',
     *   'SYSTEM CLOCK.')
360   FORMAT ('0- USER PARAMETERS -')
370   FORMAT ('0USER NAME = ',2A4)
380   FORMAT ('0DEFAULT TIME ZONE CODE = ',A4)
390   FORMAT ('0TIME FOR CALENDAR DAY INCREMENT = ',I5.4,'Z')
400   FORMAT ('0TIME ZONE FOR SYSTEM CLOCK = ',A,' (',A,')')
410   FORMAT ('0DEFAULT INTERVAL FOR ''#'' DATE = ',I2)
C
      END
