#include "smpdbk_pragma.inc"
C
      SUBROUTINE SDBMENU(I100,I400,DATFIL,ICAT,ICAT2)
      CHARACTER DATFIL*12,DATFIL2*12,FO*3,ANSW,DAMID*5,TYPE*4
      COMMON /DATFL/DATFIL2
      COMMON /FOFF/FO, TYPE
      COMMON/RECNO/IREC

      I400=0
      IF(I100.EQ.1) GO TO 100
      PRINT 40
   40 FORMAT(///1X,79('='),21X,' NOTE:    CAPS LOCK MUST BE TURNED ON!!'
     ./1X,79('=')//)

      PRINT 45
   45 FORMAT(22X,' SELECT (1) FOR WSFO OR (2) FOR WFO: '/)
      READ '(A)', TYPE
      IF (TYPE(1:1) .EQ. '1') THEN
        TYPE = 'WSFO'
      ELSE
        TYPE = 'WFO '
      ENDIF
      PRINT 50, TYPE
   50 FORMAT(/32X,' ENTER 'A4,': '/)
      READ '(A3)', FO

      PRINT 60
   60 FORMAT(///1X,79('=')/20X,' SPECIAL NOTICE TO DAM CATALOG USERS '
     .//5X,'IF YOU EXITED THE PROGRAM ABNORMALLY (E.G., HITTING <Ctrl> C
     .), THEN YOU '/5X,'MUST GENERATE THE INDEX POINTERS USING THE "DAMC
     .AT" OPTION IN THE MENU'/1X,79('=')/)
      CALL WAIT

  100 IREC=0
      ICAT=0
CC	ICAT2=0

C		       M A I N	 M E N U
C			(1) Run DAMCAT
C			(2) Run SMPDBK
C			(3) Update a Dam
C			(4) EXIT the program
C
  105 CALL SCREEN(7,ISCRN)
      CALL CLSCR
      GO TO (110,500,200,130), ISCRN
      GO TO 105

C	    RUN DAMCAT
  110 ICAT2=0
      CALL DAMCAT(ICAT2)
      ICAT2=ICAT2+1
      GO TO 100

  130 CALL EXIT

C	    UPDATE A DAM
  200 IF(ICAT2.EQ.0) THEN
	ICAT2=ICAT2+1
	CALL DAMCAT(ICAT2)
      END IF
      DATFIL='DAMCAT.'//FO
      CLOSE(5)
      OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1     STATUS='OLD',ERR=240)

C	       D A M   U P D A T I N G	 M E N U
C	     (1) Search for Dam to be Updated
C	     (2) Manually Update Dam Using DAMCAT
C	     (3) Update Dam Using SMPDBK
C	     (4) MAIN MENU
C	     (5) EXIT
C
  210 CALL SCREEN(8,ISCRN)
      GO TO (220,230,300,100,130), ISCRN
      GO TO 200

C           SEARCH FOR A PARTICULAR DAM
  220 CALL SRCH(1)
CC	PRINT 225, IREC
CC  225 FORMAT(5X,'REC NO = ',I5)
CC	PAUSE ' '
      GO TO 210

C           MANUALLY UPDATE A DAM USING DAMCAT
  230 DAMID='DAMID'
      CALL EDITDC(DAMID,0)
      GO TO 210

  240 PRINT 250, FO, TYPE
  250 FORMAT(//15X,'DAM CATALOG FOR ',A3,1X,A4,' NOT FOUND IN DIRECTORY!
     .'/30X,'PROGRAM TERMINATED.'/)
      CALL EXIT

C           UPDATE DAM USING SMPDBK MODEL
  300 CALL CLSCR
      CLOSE(5)
      ICAT=1
      IHER=0
      PRINT 310
  310 FORMAT(/' DO YOU WANT TO UPDATE DAM USING AN EXISTING SMPDBK INPUT
     . FILE? '/)
      READ '(A)', ANSW
      IF(ANSW.EQ.'Y') IHER=1
      IF(ANSW.NE.'Y') PRINT 320
  320 FORMAT(/' A SMPDBK INPUT FILE WILL BE GENERATED USING THE DAM CATA
     .LOG '/' INFORMATION TO UPDATE THE DAM')
      PRINT 325
  325 FORMAT(/' ENTER THE NAME OF SMPDBK FILE TO BE USED FOR UPDATING: '
     ./)
      READ 330, DATFIL2
  330 FORMAT(A12)

      OPEN(8,FILE=DATFIL2,STATUS='OLD',ERR=350)
      READ(8,335,END=350,ERR=340) DUMM
  335 FORMAT(A4)
      CLOSE(8)
      ICAT=2
      GO TO 500

  340 PRINT *, ' UNABLE TO READ FILE!!!!'
      GO TO 352
  350 IF(IHER.EQ.0) GO TO 356
      PRINT *, ' THIS FILE IS EMPTY!!!!!'
  352 PRINT 354
  354 FORMAT(/' DO YOU WANT TO GENERATE THE DATA TO FILL THIS FILE? '/)
      READ '(A)', ANSW
      IF(ANSW.NE.'Y') CALL EXIT
  356 PRINT 358
  358 FORMAT(/' DO YOU KNOW THE DAM RECORD NO.? '/)
      READ '(A)', ANSW
      IF(ANSW.EQ.'Y') THEN
        PRINT 360
  360	FORMAT(/' ENTER THE DAM RECORD NO. '/)
        READ*, IREC
      ELSE
        CLOSE(5)
        CALL GETREC(DATFIL)
      END IF

CC	PRINT 365, IREC,ICAT,DATFIL
CC  365 FORMAT(5X,'IREC=',I5,5X,'ICAT=',I5,5X,'FILE=',A12)
      CLOSE(8)
      OPEN(8,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     .   STATUS='OLD')
      ICAT=1
      I400=1
  500 RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE GETID(DAMID,DAMID2)
C           THIS PROGRAM CONVERTS A 5-DIGIT NUMBER FROM CHARACTER FORM
C           TO INTEGER FORM, INCREMENTS IT BY ONE AND CONVERTS IT BACK
C           TO CHARACTER FORM.

      CHARACTER IDC(5)
      CHARACTER*5 DAMID,DAMID2
      DIMENSION ID(5)

C         CONVERT THE DAMID TO A NUMBER
C             DAMID = 5-DIGIT DAM I.D. IN CHARACTER FORMAT
C             IDD   = RUNNING SUM OF INTEGER FORM OF DAMID
C             IDD2  = DAMID CONVERTED TO AN INTEGER AND ICREASED BY 1

      IDD=0
      DO 100 I=1,5
      IDAM=ICHAR(DAMID(I:I))
      CALL GETNUM(IDAM,IDM)
      IDD=IDD+IDM*(10**(5-I))
  100 CONTINUE
      IDD2=IDD+1

C           CONVERT IDD2 BACK TO A CHARACTER
C             ISUM   = RUNNING SUM OF IDD2 (EACH INCREMENTS DROP A DIGIT
C                      FROM THE LEFT
C             IPW    = POWER 10 IS RAISED TO IN ORDER TO DROP DIGIT ON LEFT
C             ID(I)  = DIGIT IN INTEGER FORMAT
C             IDC(I) = DIGIT IN CHARACTER FORMAT
C             DAMID2 = DAMID INCREMENTED BY 1 IN CHARACTER FORMAT


      ISUM=IDD2
      IPW=5
      DO 200 I=1,5
      IPW=IPW-1
      IF(I.EQ.1) GO TO 5
      ISUM=ISUM-ID(I-1)*(10**(IPW+1))
    5 ID(I)=ISUM/(10**IPW)
      CALL GETCHR(ID(I),IDC(I))
  200 CONTINUE

      DAMID2=IDC(1)//IDC(2)//IDC(3)//IDC(4)//IDC(5)

CC    PRINT 10, IDD, DAMID, IDD2, DAMID2
CC 10 FORMAT(2X,'IDD=',I5,2X,'DAMID=',A5,2X,'IDD2=',I5,2X,'DAMID2=',A5)
      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE GETNUM(IDAM,IDM)
C
C           THIS SUBROUTINE CONVERTS NUMBER CHARACTERS TO INTEGERS
C
      SELECT CASE(IDAM)
      CASE (48)
      IDM=0
      CASE (49)
      IDM=1
      CASE (50)
      IDM=2
      CASE (51)
      IDM=3
      CASE (52)
      IDM=4
      CASE (53)
      IDM=5
      CASE (54)
      IDM=6
      CASE (55)
      IDM=7
      CASE (56)
      IDM=8
      CASE (57)
      IDM=9
      CASE DEFAULT
        STOP 'THIS IS NOT A NUMBER!!!'
      END SELECT
      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE GETCHR(ID,IDC)
C
C           THIS SUBROUTINE CONVERTS NUMBER TO ASCII CHARACTERS
C
      CHARACTER IDC
      SELECT CASE(ID)
      CASE (0)
      IDC=CHAR(48)
      CASE (1)
      IDC=CHAR(49)
      CASE (2)
      IDC=CHAR(50)
      CASE (3)
      IDC=CHAR(51)
      CASE (4)
      IDC=CHAR(52)
      CASE (5)
      IDC=CHAR(53)
      CASE (6)
      IDC=CHAR(54)
      CASE (7)
      IDC=CHAR(55)
      CASE (8)
      IDC=CHAR(56)
      CASE (9)
      IDC=CHAR(57)
      CASE DEFAULT
        STOP 'THIS IS NOT A NUMBER!!!'
      END SELECT
      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DAMCAT(ICAT2)
c     PROGRAM $DAMCAT.FOR by Janice M. Lewis & Raul R. Edwards   7/15/90
c
c  This program is for screen editing the Dambrk Catalog
c
c  Main Program
c  INPUT:  In the event of a dam break, the hydrologist inputs
c          data known about a dam or the vicinity of the dam.
c
c  OUTPUT:  Dambrk Catalog listing of the dam's location, peakflow, peakdepth,
c           time to arrive at nearest town, and distance from the city.
c
c
      INTEGER DECIDE, MAXV, NDX, SZ
      CHARACTER SAVE, DATFIL*10, DATFIL2*10, DATFIL3*12, BCKUP, UPDT, WH
      PARAMETER(SZ=5031)
      CHARACTER*272 INFO
      CHARACTER DAMNAM*37, VOLUME*8, COUNTY*26, EMPTY*34
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO,WSFO, FO
      CHARACTER*4 HEIGHT, TIMEFL, TYPE
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN,RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/UPDATE/ DATE, UPDT
      COMMON/FOFF/FO, TYPE
      COMMON/RECNO/IREC

      DATA EMPTY/'                                  '/

      DATFIL='DAMCAT.'//FO
      IREC=0

      PRINT 10
   10 FORMAT(///15X,'DO YOU WANT TO BACK UP THE ORIGINAL DATA SET? '/)
      READ '(A)', BCKUP
      IF(BCKUP.EQ.'Y') THEN
        DATFIL2='DAM'//FO//'.BAK'
        PRINT 20, DATFIL2
   20   FORMAT(//15X,' **** ORIGINAL DATA STORED IN FILE ',A12)
CC        PRINT 30
   30   FORMAT(//)
      END IF

      DATFIL3='DMNAM'//FO//'.NDX'
      PRINT 40
   40 FORMAT(//15X,' **** CATALOG IS INITIALLY SORTED BY DAM NAME ')
      PRINT 30
      CALL WAIT

c
c  /////////////////////////////////////////////////////////////////////
c  / (1)  Search for Dam information.                                  /
c  / (2)  Review previously selected dams.                             /
c  / (3)  Retrieve information for experimentation or specific regions /
c  /       in WSFO.                                                    /
c  / (4)  Edit Dam Catalog file.                                       /
c  / (5)  Add More Dams to the file.                                   /
c  / (6)  Generate pointer files for sorting                           /
c  / (7)  Quit.                                                        /
c  /////////////////////////////////////////////////////////////////////
c

      OPEN(UNIT=7,FILE=DATFIL3,ACCESS='DIRECT',RECL=4)
      READ(7,END=100) MAXV
      CLOSE(7)
      GO TO 200
  100 CLOSE(7)
      CLOSE(5)
      OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1     STATUS='OLD',ERR=110)
      CALL INDX
      CLOSE(5)
      GO TO 200
  110 PRINT 120, FO, TYPE
  120 FORMAT(//15X,'DAM CATALOG FOR ',A3,1X,A4' NOT FOUND IN DIRECTORY'
     ./30X,'PROGRAM TERMINATED.'/)
      CALL EXIT

  200 DO 210 INIT = 1,8
      INIT1=34*(INIT-1)+1
      INIT2=34*INIT
      INFO(INIT1:INIT2) = EMPTY
  210 CONTINUE

      IF(BCKUP.EQ.'Y') THEN
        OPEN(UNIT=10,FILE=DATFIL,STATUS='OLD',ERR=250)
        OPEN(UNIT=11, FILE=DATFIL2,STATUS='UNKNOWN')
        DO 240 NDX=1,MAXV
        READ(10,220,END=260,ERR=250) INFO
        WRITE(11,220) INFO
  220	FORMAT(A272)
        PRINT 230, NDX
  230   FORMAT(1H+,15X,'R E A D I N G  R E C O R D  N O ....  ',I5)
  240   CONTINUE
        GO TO 260
c
  250   PRINT*,' RESTART AGAIN, FILE MIGHT NOT BE IN DIRECTORY!'
        CLOSE (UNIT=10)
        GO TO 5000
  260   PRINT*,' READING FINISHED.'
        CLOSE (UNIT=10)
        CLOSE (UNIT=11)
      END IF

      UPDT='N'
      IF(ICAT2.EQ.1) GO TO 5000
c
300   CLOSE(5)
      OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1     STATUS='OLD',ERR=250)
c
  310 CALL SCREEN(1,DECIDE)
      GO TO (320,330,340,350,360,370,400) DECIDE
      GO TO 310

  320 CALL SRCH(0)
      GO TO 300

  330 IF(NVAL.GT.0) THEN
         CALL DISPLA(0,WH)
      ELSE
          PRINT 335
  335     FORMAT(/10X,'****  NO DAMS HAVE BEEN PREVIOUSLY SELECTED '
     .     //20X,'HIT <ENTER> KEY TO CONTINUE')
          PAUSE ' '

      ENDIF
      GO TO 300

  340 CALL RETRVE
      GO TO 300

  350 DAMID='DAMID'
      CALL EDITDC(DAMID,0)
      GO TO 300

  360 IADD=0
      CALL ADDC(0)
      CLOSE(5)
      IF(UPDT.EQ.'Y') THEN
	OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1   STATUS='OLD',ERR=250)
	CALL INDX
        IADD=1
      END IF
      GO TO 300

  370 CALL INDX
      GO TO 300
c
c  When files are updated, the user decides on keeping the changes on disk.
c
  400 CLOSE(10)
      CALL CLSCR
      IF(BCKUP.NE.'Y'.AND.UPDT.EQ.'Y') THEN
        PRINT 410
  410 FORMAT(//15X,'***************  W A R N I N G  ***************'//
     * 25X,'DAM CATALOG HAS BEEN UPDATED.'//6X,'SINCE  NO BACKUP WAS MAD
     *E, THE ORIGINAL CATALOG HAS BEEN DESTROYED'///)
        CALL WAIT
      END IF
      IF(BCKUP.NE.'Y') GO TO 5000
      IF(UPDT.NE.'Y') GO TO 5000
      PRINT 420
  420 FORMAT(//25X,'SAVE ALL CHANGES (Y or N) ? '/)
      READ '(A)', SAVE
      IF (SAVE .EQ. 'Y' .OR. SAVE .EQ. 'y') THEN
        GO TO 5000
      ELSE
        CLOSE(5)
        OPEN(UNIT=10, FILE=DATFIL,STATUS='UNKNOWN')
        OPEN(UNIT=11, FILE=DATFIL2,STATUS='UNKNOWN')
        PRINT 430
  430   FORMAT(//20X,'ORIGINAL DATA SET IS BEING RESTORED.'////)
        DO 440 I =1, MAXV
         READ(11,220,END=450) INFO
         WRITE(10,220) INFO
         PRINT 435, I
  435    FORMAT(1H+,15X,'R E S T O R I N G  R E C O R D  N O ....  ',I5)
  440   CONTINUE
  450   ENDFILE (UNIT=10)
        CLOSE(UNIT=10)
        CLOSE(UNIT=11,STATUS='DELETE')
        IF(IADD.EQ.1) THEN
	  OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1     STATUS='OLD',ERR=250)
	  CALL INDX
          CLOSE(5)
        END IF
        PRINT 30
      END IF
 5000 RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE SRCH(IVAL)
c
c  Searching
c  INPUT:  Dam name, Dam I.D., River, Nearest town downstream, or
c          County FIPS code.
c
c  OUTPUT:  Sorted and searched Dambrk Catalog.
c
c
      CHARACTER KEY*37, WH
      INTEGER DECIDE, MAXV, SZ
      PARAMETER(SZ=5031)
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/RECNO/IREC
c
c
c  /////////////////////////////////////////////////////////////////////
c  / (1) Dam Name.                                                     /
c  / (2) Dam I.D.                                                      /
c  / (3) Nearest Town Downstream.                                      /
c  / (4) Name of River.                                                /
c  / (5) County Fips Code.                                             /
c  / (6) County Name.						       /
c  / (7) MAIN MENU.						       /
c  / (8) EXIT PROGRAM.						       /
c  /////////////////////////////////////////////////////////////////////
c
      ISW=2
      IF(IVAL.EQ.1) ISW=9
   50 CALL SCREEN(ISW,DECIDE)
      GO TO (70,80,90,100,110,130,120,60), DECIDE
      GO TO 50
c
   60 CALL EXIT
   70 PRINT 75
   75 FORMAT(  ' ENTER FULL DAMNAME: '/)
      READ '(A)', KEY
      CALL ORDER(DECIDE,KEY,0)
      GO TO 115
c
   80 PRINT 85
   85 FORMAT(  ' ENTER IN 5-DIGIT I.D.: '/)
      READ '(A)', KEY
      CALL ORDER(DECIDE,KEY,0)
      GO TO 115
c
   90 PRINT 95
   95 FORMAT(  ' ENTER IN NEAREST TOWN DOWNSTREAM: '/)
      READ '(A)', KEY
      CALL ORDER(DECIDE,KEY,0)
      GO TO 115
c
  100 PRINT 102
  102 FORMAT(  ' ENTER IN RIVER NAME: '/)
      READ '(A)', KEY
      CALL ORDER(DECIDE,KEY,0)
      GO TO 115
c
  110 PRINT 112
  112 FORMAT(  ' ENTER IN THE LAST 3-DIGITS OF COUNTY FIPS CODE: '/)
      READ '(A)', KEY
      CALL ORDER(8,KEY,0)
      GO TO 115
c
  130 PRINT 132
  132 FORMAT(  ' ENTER IN THE COUNTY NAME: '/)
      READ '(A)', KEY
      CALL ORDER(9,KEY,0)
c
  115 ISEL=0
      IF(ISW.EQ.9) ISEL=3
CC	PRINT 10, ISW,IVAL,ISEL
CC   10 FORMAT(5X,'ISW=',I5,5X,'IVAL=',I5,5X,'ISEL=',I5)
CC	PAUSE ' '
      IF(NVAL.GT.0) CALL DISPLA(ISEL,WH)
      IF(IREC.GT.0) GO TO 120
      GO TO 50
CC120   CLOSE(5)
  120 RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE RETRVE
c
c  Retrieve Information
c  INPUT:  Menu Selection.
c
c  OUTPUT:  Chronolized Dam name, Dam I.D., or regional forecast list.
c
c
      CHARACTER KEY*37
      INTEGER DECIDE, MAXV, SZ
      PARAMETER(SZ=5031)
      CHARACTER WH, TYPE*4
      CHARACTER DAMNAM*37, COUNTY*26
      CHARACTER*3 CTYFIP, CWO, WFO, WSFO, FO
      CHARACTER*5 DAMID, RFC
      CHARACTER*6 LAT, LONG
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG,TOWN,RIVER
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON /FOFF/FO, TYPE
c
c  /////////////////////////////////////////////////////////////////////
c  / (1) Index of Dam Name in descending chronological order.          /
c  / (2) Index of Dam I.D. in descending chronological order.          /
c  / (3) Index of County Fips Code in ascending chronological order.   /
c  / (4) Index of County Names in ascending chronological order.       /
c  / (5) Scope of Regional Information. 			       /
c  / (6) MAIN MENU						       /
c  /////////////////////////////////////////////////////////////////////
c
65    CALL SCREEN(3,DECIDE)
      IF (DECIDE .EQ. 1) THEN
        CALL SHELL1('DAMNAM')
        CALL DISPLA(0,WH)
c
      ELSE IF (DECIDE .EQ. 2) THEN
        CALL SHELL1('DAMID')
        CALL DISPLA(0,WH)
c
      ELSE IF (DECIDE .EQ. 3) THEN
        CALL SHELL1('CTYFIP')
        CALL DISPLA(0,WH)
c
      ELSE IF (DECIDE .EQ. 4) THEN
	CALL SHELL1('COUNTY')
        CALL DISPLA(0,WH)
c
      ELSE IF (DECIDE .EQ. 5) THEN
c
c  /////////////////////////////////////////////////////////////////////
c  / (1) CWO (County Warning Office).                                  /
c  / (2) WFO (Weather Forecast Office).                                /
c  / (2) WSFO (Weather Service Forecast Office).                       /
c  / (3) RFC (River Forecast Center).                                  /
c  / (4) Retrieve Menu.                                                /
c  /////////////////////////////////////////////////////////////////////
c
70      CALL SCREEN(4,DECIDE)
        IF (DECIDE .EQ. 1) THEN
          CALL SHELL1('CWO')
          PRINT 71
71	  FORMAT(  ' ENTER CWO CODE: '/)
          READ '(A)', KEY
          CALL ORDER(5,KEY,0)
c
        ELSE IF (DECIDE .EQ. 2) THEN
	  CALL SHELL1(TYPE)
	  PRINT 72, TYPE
72	  FORMAT(  ' ENTER 'A4,' CODE: '/)
          READ '(A)', KEY
          CALL ORDER(6,KEY,0)
c
        ELSE IF (DECIDE .EQ. 3) THEN
          CALL SHELL1('RFC')
          PRINT 73
73	  FORMAT(  ' ENTER RFC CODE: '/)
          READ '(A)', KEY
          CALL ORDER(7,KEY,0)
c
        ELSE IF (DECIDE .EQ. 4) THEN
          GO TO 65
        ELSE
          PRINT 75
75    FORMAT(1H1//,2X,'É',25('Í'),'»',/,2X,'º',25X,'º',/,2X,'º',5X,
     .'INVALID ENTRY!',5X,'º',/,2X,'º',25X,'º',/,2X,'È',25('Í'),
     .'Œ')
          PAUSE 'HIT ENTER KEY TO CONTINUE'
          GO TO 70
        END IF
        IF (NVAL .GT. 0) CALL DISPLA(0,WH)
        GO TO 70
c
      ELSE IF (DECIDE .EQ. 6) THEN
        RETURN
      ELSE
        PRINT 75
        DO 85 NPAUSE =1,2000
85      CONTINUE
        GO TO 65
      END IF
      GO TO 65
      END
c ---------------------------------------------------------------------
       SUBROUTINE EDITDC(KEY,IADD)
c
c  Editing Dambrk Catalog Files
c  INPUT:  The Name of the field you wish to change or correct.
c
c  OUTPUT:  After your editing, a screen displays your new contents.
c
c
      CHARACTER*37 KEY
      CHARACTER SV,WH,UPDT
      INTEGER RECNO, DECIDE, SZ
      PARAMETER(SZ=5031)
      CHARACTER ANSW, DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26, INFO*272
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO, WSFO
      CHARACTER*4 HEIGHT, TIMEFL
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE, NEWDAT
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG,TOWN,RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/UPDATE/ DATE, UPDT
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/STATEID/STATE
c
c
      ICNT = 0
90    CALL CLSCR
      PRINT 100
100   FORMAT(/20X,'EDITING A DAM CATALOG ENTRY'/20X,27('Ä')//
     .	5X,'ENTER THE DAMID AND STATE.  THE DEFAULT VALUE FOR THE STATE'
     . /5X,' IS THE FIRST STATE ENCOUNTERED IN THE CATALOG.'// )

CC      PRINT 10, KEY(1:5)
CC 10   FORMAT(' DAMID = ',A5)
CC      CALL WAIT

      IF(KEY(1:5).NE.'DAMID') GO TO 110
101   PRINT 102
102   FORMAT(  ' ENTER DAMID: '/)
      READ '(A)', KEY
      PRINT 103
103   FORMAT(  ' ENTER STATE: '/)
      READ '(A)', STATE

      CALL ORDER(2,KEY,1)
110   IF (NVAL .EQ. 1) THEN
        RECNO=LARAY(NVAL)
        CALL DISPLA(1,WH)
        IF(WH.EQ.'X'.OR.WH.EQ.'x') GO TO 500
      ELSE
        PRINT 112
112     FORMAT(//  ' ***** N O T   F O U N D   I N   D A T A B A S E ***
     .**'//)
        PAUSE 'HIT ENTER KEY TO CONTINUE'
        ICNT = ICNT + 1
        IF (ICNT .EQ. 3) RETURN
        KEY(1:5)='DAMID'
        GO TO 90
      END IF
CC114   CALL SCREEN(6,DECIDE)
  114 PRINT 100
C      READ(5,120,REC=RECNO,END=130) INFO
      READ(5,120,REC=RECNO) INFO
120   FORMAT(A272)
      CALL GETDTA(INFO)
  150 CALL EDITSC(DECIDE)
      GO TO(200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,
     . 350,360,370,380,390,400,410,420,430,440,450,460,470,480), DECIDE
      GO TO 150

130   PRINT*,' ***** E N D   O F   F I L E   E N C O U N T E R E D ****'
      CALL WAIT
      GO TO 500

  200 PRINT 205
  205 FORMAT(' Enter Dam I. D.: '/)
      READ '(A)', DAMID
      GO TO 150
  210 PRINT 215
  215 FORMAT(' Enter Name of Dam: '/)
      READ '(A)', DAMNAM
      GO TO 150
  220 PRINT 225
  225 FORMAT(' Enter Name of River: '/)
      READ '(A)',RIVER
      GO TO 150
  230 PRINT 235
  235 FORMAT(' Enter Nearest Town Downstream: '/)
      READ '(A)',TOWN
      GO TO 150
  240 PRINT 245
  245 FORMAT(' Enter Weather Service Forecast Office (WSFO): '/)
      READ '(A)',WSFO
      GO TO 150
  250 PRINT 255
  255 FORMAT(' Enter Name of County: '/)
      READ '(A)',COUNTY
      GO TO 150
  260 PRINT 265
  265 FORMAT(' Enter State ID: '/)
      READ '(A)',STATE
      GO TO 150
  270 PRINT 275
  275 FORMAT('  Enter Latitude: '/)
      READ '(A)',LAT
      GO TO 150
  280 PRINT 285
  285 FORMAT(' County Warning Office (CWO): '/)
      READ '(A)',CWO
      GO TO 150
  290 PRINT 295
  295 FORMAT(' Invert Elevation at Town: '/)
      READ '(A)',INVERT
      GO TO 150
  300 PRINT 305
  305 FORMAT(' Height of Dam: '/)
      READ '(A)',HEIGHT
      GO TO 150
  310 PRINT 315
  315 FORMAT(' Volume: '/)
      READ '(A)',VOLUME
      GO TO 150
  320 PRINT 325
  325 FORMAT(' Failure Time: '/)
      READ '(A)',TIMEFL
      GO TO 150
  330 PRINT 335
  335 FORMAT(' Peak Flow at Dam: '/)
      READ '(A)',PKFLDM
      GO TO 150
  340 PRINT 345
  345 FORMAT(' Peak Flow at Town: '/)
      READ '(A)',PKFLTN
      GO TO 150
  350 PRINT 355
  355 FORMAT(' Flood Depth at Town '/)
      READ '(A)',FLDPTN
      GO TO 150
  360 PRINT 365
  365 FORMAT(' Travel Time: '/)
      READ '(A)',TRAVEL
      GO TO 150
  370 PRINT 375
  375 FORMAT(' County Fips Code: '/)
      READ '(A)',CTYFIP
      GO TO 150
  380 PRINT 385
  385 FORMAT(' Longitude: '/)
      READ '(A)',LONG
      GO TO 150
  390 PRINT 395
  395 FORMAT(' River Forecast Center (RFC): '/)
      READ '(A)',RFC
      GO TO 150
  400 PRINT 405
  405 FORMAT(' Slope: '/)
      READ '(A)',SLOPE
      GO TO 150
  410 PRINT 415
  415 FORMAT(' Crest Length: '/)
      READ '(A)',CREST
      GO TO 150
  420 PRINT 425
  425 FORMAT(' Breach Width: '/)
      READ '(A)',BREECH
      GO TO 150
  430 PRINT 435
  435 FORMAT(' Distance to Nearest Town: '/)
      READ '(A)',DIST
      GO TO 150
  440 PRINT 445
  445 FORMAT(' Peak Depth Below Dam: '/)
      READ '(A)',PKDPDM
      GO TO 150
  450 PRINT 455
  455 FORMAT(' Peak Depth at Town: '/)
      READ '(A)',PKDPTN
      GO TO 150
  460 PRINT 465
  465 FORMAT(' Time of Flooding at Town: '/)
      READ '(A)',TFLDP
      GO TO 150
  470 PRINT 475
  475 FORMAT(' Do you wish to Save this record? '/)
      READ '(A)', ANSW
      IF(ANSW.EQ.'Y') THEN
        PRINT*,' ENTER "DAMBRK", "SMPDBK", "SLOPE", OR "INFO" '
        READ '(A)', NEWDAT
        IF(NEWDAT.NE.'      ') DATE=NEWDAT
        CALL SAVDAT(INFO)
        WRITE(5,120,REC=RECNO) INFO
        UPDT='Y'
      END IF
  480 CALL DISPLA(1,WH)
      IF(WH.EQ.'E'.OR.WH.EQ.'e') GO TO 150

  500 IF(IADD.EQ.0) THEN
CC	  IF(UPDT.EQ.'Y') GO TO 600
        PRINT 505
  505	FORMAT( ' SELECT ANOTHER DAM (Y or N) '/)
        READ '(A)', SV
        IF (SV .EQ. 'Y' .OR. SV .EQ. 'y') GO TO 101
      END IF
  600 RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE EDITSC(J)
c
c  OUTPUT:  Display of Dam to be Edited with its current values.
c
      CHARACTER DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO, WSFO
      CHARACTER*4 HEIGHT, TIMEFL
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/STATEID/STATE

   10 CALL CLSCR
CC      CALL GETDTA
      PRINT*,'[ 1] Dam I. D.: ',DAMID
      PRINT*,'[ 2] Name of Dam: ',DAMNAM
      PRINT*,'[ 3] Name of River: ',RIVER
      PRINT*,'[ 4] Nearest Town Downstream: ',TOWN
      PRINT*,'[ 5] Weather Service Forecast Office (WSFO): ',WSFO
      PRINT*,'[ 6] County: ', COUNTY
      PRINT 100, STATE,CTYFIP
  100 FORMAT(' [ 7] State: ',A2,25X,' [18] County Fips Code: ',A3)
      PRINT 200, LAT,LONG
  200 FORMAT(' [ 8] Latitude: ',A6,18X,' [19] Longitude: ',A6)
      PRINT 300, CWO,RFC
  300 FORMAT(' [ 9] County Warning Office (CWO): ',A3,2X,
     .       ' [20] River Forecast Center (RFC): ',A5)
      PRINT 400, INVERT,SLOPE
  400 FORMAT(' [10] Invert Elevation at Town: ',A7,1X,
     .       ' [21] Slope: ', A3)
      PRINT 500, HEIGHT,CREST
  500 FORMAT(' [11] Height of Dam: ',A4,15X,' [22] Crest Length: ',A5)
      PRINT 600, VOLUME,BREECH
  600 FORMAT(' [12] Volume: ',A8,18X,' [23] Breach Width: ',A5)
      PRINT 700, TIMEFL,DIST
  700 FORMAT(' [13] Failure Time: ',A4,16X,
     .       ' [24] Distance to Nearest Town: ',A3)
      PRINT 800, PKFLDM,PKDPDM
  800 FORMAT(' [14] Peak Flow at Dam: ',A7,9X,
     .      ' [25] Peak Depth Below Dam: ',A5)
      PRINT 900, PKFLTN,PKDPTN
  900 FORMAT(' [15] Peak Flow at Town: ',A7,8X,
     .       ' [26] Peak Depth at Town: ',A5)
      PRINT 1000, FLDPTN,TFLDP
 1000 FORMAT(' [16] Flood Depth at Town ',A5,9X,
     .       ' [27] Time of Flooding at Town: ',A5)
      PRINT 1100, TRAVEL
 1100 FORMAT(' [17] Travel Time: ',A5,16X,' [28] Save Dam Information')
      PRINT 1200
 1200 FORMAT(40X,' [29] Finished Editing this Dam')
      WRITE(*,2000)
 2000 FORMAT(/'         ENTER SELECTION: '/)
      READ(*,*,ERR=10) J
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE SCREEN(J,C)
c
c  INPUT:  Menu Selection.
c
c  OUTPUT:  Various menus for the editing of the Dambrk Catalog.
c
c  DEFINITION OF VARIABLES:
c          J = SCREEN OPTION
c          C = SELECTION NO. FROM SCREEN
c
      CHARACTER FO*3, TYPE*4
      INTEGER J,C
      COMMON/FOFF/FO, TYPE
c
c
c
110   IF (J .NE. 6) THEN
        CALL CLSCR
        PRINT 120, FO, TYPE
120     FORMAT(/25X,'DAM CATALOG INFORMATION',/,30X,'FOR ',A3,1X,A4)
      END IF
      IF (J .EQ. 1) THEN
        PRINT 125
125     FORMAT( 2(/),20X,'D A M   C A T A L O G   M E N U',3(/),
     .  12X,'(1) Search for a Particular Dam(s) in the Catalog',2(/),
     .  12X,'(2) Review Previously Selected Dams'//
     .  12X,'(3) Scan All Dams in Catalog in a Specific Order',2(/),
     .  12X,'(4) Edit a Particular Dam in the Catalog',2(/),
     .  12X,'(5) Add More Dams to the Catalog',2(/),
     .  12X,'(6) Generate Pointer Files for Sorting'//
     .	12X,'(7) MAIN MENU',2(/),30X,'ENTER SELECTION: '/)
c
      ELSE IF (J.EQ.2.OR.J.EQ.9) THEN
        PRINT 130
130     FORMAT( 2(/),20X,'S E A R C H I N G   M E N U',3(/),25X,
     .  '(1) Dam Name',2(/),25X,'(2) Dam ID',2(/),25X,'(3) Nearest Town',
     .  ' Downstream',2(/),25X,'(4) Name of River',2(/),25X,
     .	'(5) County FIPS Code'//25X,'(6) County Name')
        IF(J.EQ.2) PRINT 131
131	FORMAT(/25X,'(7) DAM CATALOG MENU')
        IF(J.EQ.9) PRINT 132
132	FORMAT(/25X,'(7) FINISHED SEARCHING')
        PRINT 133
133	FORMAT(/25X,'(8) EXIT PROGRAM'//25X,'ENTER SELECTION: '/)
c
      ELSE IF (J .EQ. 3) THEN
        PRINT 135
135     FORMAT( 4(/),20X,'S C A N N I N G   M E N U',3(/),20X,
     .  '(1) Index of Dam Names',2(/),20X,'(2) Index of Dam IDs',2(/),
     .  20X,'(3) Index of County Fips Codes'//
     .	20X,'(4) Index of County Names'//
     .	20X,'(5) Scope of Forecast Regions'//
     .	20X,'(6) DAM CATALOG MENU',4(/),25X,'ENTER SELECTION: '/)
c
      ELSE IF (J .EQ. 4) THEN
        PRINT 140
140     FORMAT( 4(/),25X,'SCOPE OF FORECAST REGIONS',3(/),30X,
c     .  '(1) Specific CWOs',2(/),30X,'(2) Specific Future WFOs',
     .	'(1) Specific CWOs')
	IF(TYPE.EQ.'WSFO') PRINT 142
142	FORMAT(/30X,'(2) Specific WSFOs')
	IF(TYPE.EQ.'WFO ') PRINT 143
143	FORMAT(/30X,'(2) Specific WFOs')
	PRINT 144
144	FORMAT(/30X,'(3) Specific RFCs',2(/),
     .	30X,'(4) RETRIEVING MENU',6(/),30X,'ENTER SELECTION: '/)
c
      ELSE IF (J .EQ. 5) THEN
        PRINT*,' ',C,' RECORDS OF THIS TYPE.'
        RETURN
c
      ELSE IF (J .EQ. 6) THEN
        CALL CLSCR
        PRINT 145
145     FORMAT(//'[ 1] Dam I.D.',17X,'[14] Peak Depth Below Dam'/
     .  '[ 2] Name of Dam',14X,'[15] Distance to Nearest Town'/
     .  '[ 3] County FIPS Code',9X,'[16] Peak Flow at Town'/
     .  '[ 4] Latitude',17X,'[17] Peak Depth at Town'/
     .  '[ 5] Longitude',16X,'[18] Travel Time'/
     .  '[ 6] Name of River',12X,'[19] Slope'/
     .  '[ 7] Nearest Downstream Town',2X,'[20] County Warning Office'/
     .  '[ 8] Height of Dam',12X,'[21] Weather Service Forecast Office'/
     .  '[ 9] Crest Length',13X,'[22] River Forecast Center (RFC)'/
     .  '[10] Volume',19X,'[23] County'/
     .  '[11] Breach Width',13X,'[24] Flood Depth at Town'/
     .  '[12] Failure Time',13X,'[25] Time of Fooding at Town'/
     .  '[13] Peak Flow at Dam',9X,'[26] Invert Elevation at Town'//)

        WRITE(*,150)
150	FORMAT('          ENTER SELECTION: '/)
c
      ELSE IF (J .EQ. 7) THEN
        PRINT 160
160     FORMAT( 4(/),30X,'M A I N   M E N U',3(/),
     .  30X,'(1) Run DAMCAT'//
     .  30X,'(2) Run SMPDBK'//
     .  30X,'(3) Update a Dam'//
     .	30X,'(4) EXIT',///30X,'ENTER SELECTION: '/)

      ELSE IF (J .EQ. 8) THEN
        PRINT 170
170     FORMAT( 4(/),15X,'D A M   U P D A T I N G   M E N U',3(/),
     .  15X,'(1) Search for Dam to be Updated'//
     .  15X,'(2) Manually Update Dam Using DAMCAT'//
     .  15X,'(3) Update Dam Using SMPDBK'//
     .  15X,'(4) MAIN MENU'//
     .	15X,'(5) EXIT'///20X,'ENTER SELECTION: '/)

      END IF
c
      READ(*,*,ERR=110) C
      IF(J.NE.6) PRINT 155
  155 FORMAT(/)
      RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE DISPLA(IVAL,WHERE)
c
c  INPUT:  Dambrk Catalog.
c
c  OUTPUT:  Formated display for the Dambrk Catalog.
c
c  DEFINITION OF VARIABLES:
c       IVAL = 0  NORMAL DISPLAY
c              1  EDIT DISPLAY
c              2  VIEW ONLY (AFTER UPDATING)
c              3  NORMAL DISPLAY WITH OPTION TO SELECT RECORD NO.
c      WHERE = SELECTION FROM SCREEN
c
      CHARACTER WHERE, UPDT
      INTEGER N, MAXV, SZ
      PARAMETER(SZ=5031)
      CHARACTER DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26, INFO*272
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO, WSFO, FO
      CHARACTER*4 HEIGHT, TIMEFL, TYPE
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/RECNO/IREC
      COMMON/UPDATE/ DATE, UPDT
      COMMON/FOFF/ FO, TYPE
      COMMON/STATEID/STATE
c
c
c
      IF(IVAL.EQ.2) THEN
        N=IREC
        NVAL=1
        L=1
        IF(MAXV.EQ.0) MAXV=SZ
        GO TO 185
      END IF
      IREC=0
      IF(NVAL.EQ.0) GO TO 230
      L=1
      N=LARAY(1)
185   IF (N .LE. MAXV .AND. N .GE. 1) THEN

        CALL CLSCR
C        READ(5,60,REC=N,END=230) INFO
        READ(5,60,REC=N) INFO
60	FORMAT(A272)
        CALL GETDTA(INFO)
        PRINT 190, STATE,N,MAXV,DAMNAM,RIVER,TOWN,DIST,INVERT,TRAVEL
190   FORMAT(1H1,//,1X,A2,27X,'DAMBRK CATALOG',25X,I4,' of',I4/25X,
     .'NATIONAL WEATHER SERVICE',//,' É',77('Í'),'»'/' º',4X,
     .'Name of Dam',31X,'Name of River',18X,'º'/' º',4X,A37,5X,A22,9X,
     .'º'/' º',77X,'º'/' º',4X,'Nearest Town Downstream',5X,
     .'Distance',5X,'Invert Elev.',5X,'Travel Time',4X,'º'/' º',4X,A22
     .,5X,A3,' miles',7X,A7,' ft',4X,A5,' hours',5X,'º'/' º',77X,'º')
c
        PRINT 195, PKFLDM, PKDPDM, PKFLTN, PKDPTN,FLDPTN,TFLDP
195     FORMAT(' º',3X,'Peak Flow',2X,'Peak Depth',2X,'Peak Flow',2X,
     .  'Peak Depth',2X,'Fld Depth'2X,'Time Fld Depth',2X,' º'/
     .  ' º',3X,' at Dam  ',2X,'  at Dam  ',2X,' at Town ',2X,
     .  ' at Town  ',2X,' at Town '2X,'   at Town    ',2X,' º'/' º',
     .   1X,A7,' cfs',4X,A5,' ft',1X,A7,' cfs',3X,A5,' ft',3X,
     .   A5,' ft',4X,A5,' hrs',6X,'º'/' È',
     .  77('Í'),'Œ'/)
c
        PRINT 200, DAMID, CTYFIP, LAT, LONG
200     FORMAT(2X,'Dam I.D.: ',A5,2X,'County Fips Code: ',A3,3X,
     .  'Latitude: ',A6,'ø',2X,'Longitude: ',A6,'ø'/)
c
        PRINT 205, HEIGHT,CREST,VOLUME,BREECH,TIMEFL,
     .  SLOPE
205     FORMAT(2X,'Dam Height: ',A4,' ft',4X,'Crest Length: ',A5,' ft',
     .  5X,'Volume: ',A8,' Acre-ft'//5X,'Breach Width: ',A5,' ft',3X,
     .  'Failure Time: ',A4,' minutes',2X,'Slope: ',A3,' ft/mile'/)
c
        IF (TYPE .EQ. 'WSFO') THEN
          PRINT 210, CWO, TYPE, WSFO, RFC, COUNTY
        ELSE
          PRINT 210, CWO, TYPE, WFO, RFC, COUNTY
        ENDIF
210     FORMAT(5X,'CWO: ',A3,5X,A4,': ',A3,5X,'RFC: ',A5,5X,'COUNTY:',
     .   A26/)
	IF(IVAL.EQ.3) PRINT 211, DATE
211     FORMAT('   [(E)dit]    [ pg(U)p ]    [ pg(D)n ]   [  e(X)it ]',
     .  '       LAST UPDATED: ',A6)
        IF(IVAL.EQ.0) PRINT 212, DATE
212     FORMAT('             [ pg(U)p ]    [ pg(D)n ]   [  e(X)it ]',
     .	'       LAST UPDATED: ',A6)
	IF(IVAL.EQ.1) PRINT 213, DATE
  213	FORMAT('             [ (E)dit ]    [ e(X)it ]              ',
     .	'   LAST UPDATED: ',A6)
        IF(IVAL.EQ.2) PRINT 214, DATE
214     FORMAT('                           [ e(X)it ]              ',
     .  '   LAST UPDATED: ',A6)
c
215     READ '(A)', WHERE
        IF (WHERE .EQ. 'D'.OR. WHERE .EQ. 'd') THEN
          L = L + 1
        ELSE IF (WHERE .EQ. 'U' .OR. WHERE .EQ. 'u') THEN
          L = L - 1
        ELSE IF (WHERE .EQ. 'E' .OR. WHERE .EQ. 'e') THEN
          GO TO 227
        ELSE IF (WHERE .EQ. 'X' .OR. WHERE .EQ. 'x') THEN
          GO TO 230
        ELSE
          GO TO 215
        END IF
        IF (L .LT. 1) THEN
          L=1
          PRINT 220
220       FORMAT(///20X,'TOP  OF  FILE !')
          READ(*,*)
          RETURN
        END IF
        IF (L .GT. NVAL) THEN
          L=NVAL
          PRINT 225
225       FORMAT(1H1///20X,'END  OF  FILE !')
          READ(*,*)
          RETURN
        END IF
        N=LARAY(L)
        GO TO 185
      END IF
227   IREC=LARAY(L)
230   RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE SHELL1(FIELD)
c        this subroutine stores the dam locations  into an array when
c        sorting on damnam in SCAN mode.
c
      INTEGER SZ, MAXV
      PARAMETER(SZ=5031)
      CHARACTER FIELD*(*), DATFIL3*12, FO*3, TYPE*4
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/FOFF/FO, TYPE

      NVAL=MAXV
      LENFLD=LEN_TRIM(FIELD)
      DATFIL3=FIELD(1:LENFLD)//FO//'.NDX'
      IF(FIELD.EQ.'DAMNAM') DATFIL3='DMNAM'//FO//'.NDX'
      IF(FIELD.EQ.'CTYFIP') DATFIL3='FIPS'//FO//'.NDX'
      IF(FIELD.EQ.'COUNTY') DATFIL3='CTY'//FO//'.NDX'
      OPEN(UNIT=7,FILE=DATFIL3,ACCESS='DIRECT',RECL=4)

      READ(7) DUMM
      DO 10 I=1,MAXV
      READ(7) LARAY(I)
   10 CONTINUE

      CLOSE(7)

CC      OPEN(15,FILE='NDEX')
CC      WRITE(15,15) MAXV
CC   15 FORMAT(1X,'MAXV=',I5)
CC      WRITE(15,20) (LARAY(I),I=1,MAXV)
CC   20 FORMAT(1X,20I3)
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE ORDER(DECID,KEY,IVAL)
      INTEGER SZ, MAXV,DECID
      PARAMETER(SZ=5031)
      CHARACTER INFO*272, ARRAY*37, STATE*2, FO*3, TYPE*4
      CHARACTER*(*) KEY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/STATEID/STATE
      COMMON /FOFF/FO, TYPE
c
      GO TO (10,20,30,40,50,60,70,80,85) DECID
      RETURN
c           sorting on dam name
10    I1=7
      IN=43
      GO TO 90
c           sorting on dam i.d. no.
20    I1=1
      IN=5
      GO TO 90
c           sorting on nearest downstream town name
30    I1=85
      IN=106
      GO TO 90
c           sorting on river name
40    I1=63
      IN=83
      GO TO 90
c          sorting on county warning office (CWO)
50    I1=199
      IN=201
      GO TO 90
c          sorting on WSFO
60    IF(TYPE.EQ.'WSFO') THEN
	I1=213
	IN=215
      ELSE
	I1=203
	IN=205
      ENDIF
      GO TO 90
c          sorting on RFC
70    I1=207
      IN=211
      GO TO 90
c          sorting on county fips code (CTYFIP)
80    I1=45
      IN=47
      GO TO 90
c	   sorting on county name (COUNTY)
85    I1=247
      IN=272

90    LKEY=LEN(KEY)
      LENKEY=LEN_TRIM(KEY)
c
      NVAL=0
      DO 100 L=1,MAXV
C     READ(5,95,REC=L,END=200) INFO
      READ(5,95,REC=L) INFO
95    FORMAT(A272)
      IF(STATE.EQ.'  ') STATE=INFO(217:219)
      ARRAY=INFO(I1:IN)
      ICOL=INDEX(ARRAY,KEY(1:LENKEY))
      IF(ICOL.EQ.0) GO TO 100
      IF(IVAL.EQ.1) THEN
	IF(STATE.EQ.INFO(217:218)) THEN
	  NVAL=1
	  LARAY(NVAL)=L
	  GO TO 200
	ENDIF
	GO TO 100
      ENDIF
      NVAL=NVAL+1
      LARAY(NVAL)=L
  100 CONTINUE
  200 RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE GETDTA(INFO)
c
c         this subroutine breaks the dam info into individual components
c
      INTEGER  MAXV, SZ
      PARAMETER(SZ=5031)
      CHARACTER DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26, INFO*272, UPDT
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO, WSFO
      CHARACTER*4 HEIGHT, TIMEFL
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/UPDATE/ DATE, UPDT
      COMMON/STATEID/STATE
c
      DAMID = INFO(1:5)
      DAMNAM = INFO(7:43)
      CTYFIP = INFO(45:47)
      LAT = INFO(49:54)
      LONG = INFO(56:61)
      RIVER = INFO(63:84)
      TOWN = INFO(85:106)
      HEIGHT = INFO(108:111)
      CREST = INFO(113:117)
      VOLUME = INFO(119:126)
      BREECH = INFO(128:132)
      TIMEFL = INFO(134:137)
      PKFLDM = INFO(139:145)
      PKDPDM = INFO(147:151)
      DIST = INFO(170:172)
      PKFLTN = INFO(174:180)
      PKDPTN = INFO(182:186)
      TRAVEL = INFO(188:192)
      SLOPE = INFO(195:197)
      CWO = INFO(199:201)
      WFO = INFO(203:205)
      RFC = INFO(207:211)
      WSFO = INFO(213:215)
      STATE = INFO(217:218)
      DATE = INFO(220:225)
      FLDPTN = INFO(227:231)
      TFLDP = INFO(233:237)
      INVERT = INFO(239:245)
      COUNTY = INFO(247:272)
   10 FORMAT(10X,'DATE=',A6)
      IF (DATE .EQ. '      ') DATE = ' NONE '
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE SAVDAT(INFO)
c
c        this subroutine stores the dam info back into its original form
c
      INTEGER  MAXV, SZ
      PARAMETER(SZ=5031)
      CHARACTER DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26, INFO*272, UPDT
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WFO, WSFO
      CHARACTER*4 HEIGHT, TIMEFL
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/UPDATE/ DATE, UPDT
      COMMON/STATEID/STATE

CC	DO 920 I =1, MAXV
          INFO(1:5) = DAMID
          INFO(7:43) = DAMNAM
          INFO(45:47) = CTYFIP
          INFO(49:54) = LAT
          INFO(56:61) = LONG
          INFO(63:84) = RIVER
          INFO(85:106) = TOWN
          INFO(108:111) = HEIGHT
          INFO(113:117) = CREST
          INFO(119:126) = VOLUME
          INFO(128:132) = BREECH
          INFO(134:137) = TIMEFL
          INFO(139:145) = PKFLDM
          INFO(147:151) = PKDPDM
          INFO(170:172) = DIST
          INFO(174:180) = PKFLTN
          INFO(182:186) = PKDPTN
          INFO(188:192) = TRAVEL
          INFO(195:197) = SLOPE
          INFO(199:201) = CWO
          INFO(203:205) = WFO
          INFO(207:211) = RFC
          INFO(213:215) = WSFO
          INFO(217:218) = STATE
          INFO(220:225) = DATE
          INFO(227:231) = FLDPTN
          INFO(233:237) = TFLDP
          INFO(239:245) = INVERT
          INFO(247:272) = COUNTY
CC920	  CONTINUE
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE ADDC(ISKIP)
c
c           this subroutine adds a new dam to the database
c
      INTEGER  MAXV, SZ
      PARAMETER(SZ=5031)
      CHARACTER INFO*272, DAMNAM*37, CTYFIP*3, UPDT, SV
      CHARACTER*5 DAMID,DAMID2
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*22 RIVER, TOWN
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/RECNO/IREC
      COMMON/UPDATE/ DATE, UPDT
C
C       SORT ON DAMID TO GET THE LAST ID NO.

      PRINT 2, ISKIP
    2 FORMAT('  ISKIP=',I2)
      UPDT='N'
      CALL CLSCR
      CALL SHELL1('DAMID')
      LMAX=LARAY(MAXV)
      READ(5,5,REC=LMAX) INFO
5     FORMAT(A272)
   10 DAMID=INFO(1:5)
      CALL GETID(DAMID,DAMID2)
      PRINT 6, DAMID2
6     FORMAT('  DAMID2=',A5)
CC      PRINT 10, DAMID
CC10    FORMAT(//' ** THE LAST VALID DAMID IS ',A5,'.  ANY NUMBER GREATER
CC     1THAN THIS MAY BE USED **'//5X,' ** ',10X,'BE SURE TO ENTER ALL FIV
CC     2E DIGITS!!!',10X,' **'///)
CC      PAUSE '                    HIT ENTER KEY TO CONTINUE'
c
c        store the new dam into the database
c
   15 MAXV=MAXV+1
      IF(MAXV.LT.SZ) THEN
        NVAL=1
        LARAY(NVAL)=MAXV
        DAMID=DAMID2
        CALL NEWDAT(ISKIP)
        CALL SAVDAT(INFO)
        PRINT 20
20	FORMAT( ' SAVE RECORD INTO DATA BASE? (Y or N) '/)
        READ '(A)', SV
        IF (SV .EQ. 'Y' .OR. SV .EQ. 'y')  THEN
          UPDT='Y'
          WRITE(5,5,REC=MAXV) INFO
          IF(ISKIP.EQ.0) CALL EDITDC(DAMID,1)
          IF(ISKIP.EQ.1) IREC=MAXV
        ELSE
          MAXV=MAXV-1
        ENDIF
      ELSE
        PRINT 25
25      FORMAT(//'***** MAXIMUM NO. OF RECORDS HAS BEEN EXCEEDED!',24X,
     1' *****'// ' ***** CONTACT DR. FREAD AT HRL ABOUT INCREASING THE S
     2IZE OF THE PROGRAM *****')
        MAXV=MAXV-1
        RETURN
      END IF
30    IF(ISKIP.EQ.1) GO TO 5000
      PRINT 35
35    FORMAT( ' ADD ANOTHER DAM? (Y or N) '/)
      READ '(A)', SV
      IF (SV .EQ. 'Y' .OR. SV .EQ. 'y') THEN
	IF(UPDT.EQ.'Y') THEN
	  UPDT='N'
	  GO TO 10
	ELSE
	  GO TO 15
	END IF
      END IF
5000  RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE NEWDAT(ISKIP)
c
      CHARACTER DAMNAM*37, VOLUME*8, STATE*2, COUNTY*26, BLANK*37, UPDT
      CHARACTER*3 CTYFIP, DIST, SLOPE, CWO, WSFO, WFO
      CHARACTER*4 HEIGHT, TIMEFL
      CHARACTER*5 DAMID,CREST,BREECH,PKDPDM,PKDPTN,TRAVEL,FLDPTN,TFLDP
      CHARACTER*5 RFC
      CHARACTER*6 LAT, LONG, DATE
      CHARACTER*7 PKFLDM, PKFLTN, INVERT
      CHARACTER*22 RIVER, TOWN
c
c        this subroutine reads in the dam info for the dam to be added
c
      COMMON/DAM/ DAMNAM,DAMID, CTYFIP, LAT, LONG, TOWN, RIVER
      COMMON/ATTRIB/ HEIGHT, CREST, VOLUME, BREECH, TIMEFL, INVERT
      COMMON/FCST/ PKFLDM, PKDPDM, DIST, PKFLTN, PKDPTN, TRAVEL,SLOPE,
     .             FLDPTN, TFLDP
      COMMON/REGION/ CWO, WFO, RFC, WSFO, COUNTY
      COMMON/UPDATE/ DATE, UPDT
      COMMON/STATEID/STATE
      DATA BLANK/'                                     '/

      CALL CLSCR
      PRINT 5, DAMID
5     FORMAT( //' Dam ID= ',A5)
CC      DAMID=BLANK
CC      READ 10, DAMID
10    FORMAT(A5)
      IF(ISKIP.EQ.1) GO TO 22
      PRINT 15
15    FORMAT(' ENTER Nam of Dam: '/)
      DAMNAM=BLANK
      READ 20, DAMNAM
20    FORMAT(A37)
22    PRINT 25
25    FORMAT(' ENTER County Fips Code: '/)
      CTYFIP=BLANK
      READ 30, CTYFIP
30    FORMAT(A3)
      PRINT 35
35    FORMAT(' ENTER Latitude: '/)
      LAT=BLANK
      READ 40, LAT
40    FORMAT(A6)
      PRINT 45
45    FORMAT(' ENTER Longitude: '/)
      LONG=BLANK
      READ 40, LONG
      IF(ISKIP.EQ.1) GO TO 62
      PRINT 55
55    FORMAT(' ENTER Name of River: '/)
      RIVER=BLANK
      READ 60, RIVER
60    FORMAT(A22)
62    PRINT 65
65    FORMAT(' ENTER Nearest Town Downstream: '/)
      TOWN=BLANK
      READ 60, TOWN
      PRINT 75
75    FORMAT(' ENTER Height of Dam: '/)
      HEIGHT=BLANK
      READ 80, HEIGHT
80    FORMAT(A4)
      PRINT 85
85    FORMAT(' ENTER Crest Length: '/)
      CREST=BLANK
      READ 10, CREST
      IF(ISKIP.EQ.1) GO TO 192
      PRINT 95
95    FORMAT(' ENTER Volume: '/)
      VOLUME=BLANK
      READ 100, VOLUME
100   FORMAT(A8)
      PRINT 105
105   FORMAT(' ENTER Breach Width: '/)
      BREECH=BLANK
      READ 10, BREECH
      PRINT 115
115   FORMAT(' ENTER Failure Time: '/)
      TIMEFL=BLANK
      READ 80, TIMEFL
      PRINT 125
125   FORMAT(' ENTER Peak Flow at Dam: '/)
      PKFLDM=BLANK
      READ 130, PKFLDM
130   FORMAT(A7)
      PRINT 135
135   FORMAT(' ENTER Peak Depth Below Dam: '/)
      PKDPDM=BLANK
      READ 10, PKDPDM
      PRINT 145
145   FORMAT(' ENTER Distance to Nearest Town: '/)
      DIST=BLANK
      READ 30, DIST
      PRINT 155
155   FORMAT(' ENTER Peak Flow at Town: '/)
      PKFLTN=BLANK
      READ 160, PKFLTN
160   FORMAT(A7)
      PRINT 165
165   FORMAT(' ENTER Peak Depth at Town: '/)
      PKDPTN=BLANK
      READ 10, PKDPTN
      PRINT 175
175   FORMAT(' ENTER Travel Time: '/)
      TRAVEL=BLANK
      READ 10, TRAVEL
      PRINT 185
185   FORMAT(' ENTER Slope: '/)
      SLOPE=BLANK
      READ 30, SLOPE
192   PRINT 195
195   FORMAT(' ENTER County Warning Office (CWO): '/)
      CWO=BLANK
      READ 30, CWO
      PRINT 205
205   FORMAT(' ENTER Weather Service Forecast Office (WSFO): '/)
      WSFO=BLANK
      READ 30, WSFO
      PRINT 215
215   FORMAT(' ENTER River Forecast Center (RFC): '/)
      RFC=BLANK
      READ 10, RFC
      PRINT 225
225   FORMAT(' ENTER State ID: '/)
      STATE=BLANK
      READ 230, STATE
230   FORMAT(A2)
      DATE='      '
      PRINT 235
235   FORMAT(' ENTER County Name: '/)
      COUNTY=BLANK
      READ 240, COUNTY
240   FORMAT(A26)
      IF(ISKIP.EQ.1) GO TO 5000
      PRINT 245
245   FORMAT(' ENTER Flood Depth at Town: '/)
      FLDPTN=BLANK
      READ 10, FLDPTN
      PRINT 255
255   FORMAT(' ENTER Time of Flood Depth at Town: '/)
      TFLDP=BLANK
      READ 10, TFLDP
      PRINT 265
265   FORMAT(' ENTER Invert Elevation at Town: '/)
      INVERT=BLANK
      READ 130, INVERT
5000  RETURN
      END
c ----------------------------------------------------------------------
      SUBROUTINE INDX
c
c  Indexing Using Heap Sort
c  INPUT:  Array of length MAXV.
c
c  OUTPUT:  Sorted index of the array in an increasing order.
c
c
c$PRAGMA C (gettim)
      INTEGER SZ, MAXV
      INTEGER*2 IHR,IMIN,ISEC,I100TH
      PARAMETER(SZ=5031)
      CHARACTER*272 INFO
      CHARACTER FIELD*6, DATFIL3*12, FO*3, TYPE*4
      CHARACTER*26 ARRAY(SZ)
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON /FOFF/FO, TYPE
c
c          READ IN THE DAMID FIELD AND STORE ITS LOCATION
c
CC      OPEN(16,FILE='NDEXES')
      CALL CLSCR
      DO 100 I=1,7
      GO TO (1,2,3,4,5,6,7), I
    1 FIELD='DAMID'
      GO TO 8
    2 FIELD='DAMNAM'
      GO TO 8
    3 FIELD='CWO'
      GO TO 8
    4 IF(TYPE.EQ.'WSFO') THEN
	FIELD='WSFO'
      ELSE
	FIELD='WFO'
      ENDIF
      GO TO 8
    5 FIELD='RFC'
      GO TO 8
    6 FIELD='CTYFIP'
      GO TO 8
    7 FIELD='COUNTY'

    8 CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      PRINT 900, FIELD,IHR,IMIN,ISEC,I100TH
  900 FORMAT(/ '     GENERATING INDEX POINTERS FOR ',A6,' TIME IS ',
     . 2(I2.2,1H:),I2.2,1H.,I2.2)

      LENFLD=LEN_TRIM(FIELD)
      NDX=0
      DO 10 L=1,SZ
      READ(5,910,REC=L,ERR=12) INFO
C      READ(5,910,REC=L,END=15,ERR=12) INFO
  910 FORMAT(A272)
      NDX=NDX+1
      IF(FIELD(1:LENFLD).EQ.'DAMID') ARRAY(L)=INFO(1:5)
      IF(FIELD(1:LENFLD).EQ.'CWO') ARRAY(L)=INFO(199:201)
      IF(FIELD(1:LENFLD).EQ.'WFO') ARRAY(L)=INFO(203:205)
      IF(FIELD(1:LENFLD).EQ.'WSFO') ARRAY(L)=INFO(213:215)
      IF(FIELD(1:LENFLD).EQ.'RFC') ARRAY(L)=INFO(207:211)
      IF(FIELD(1:LENFLD).EQ.'CTYFIP') ARRAY(L)=INFO(45:47)
      IF(FIELD(1:LENFLD).EQ.'COUNTY') ARRAY(L)=INFO(247:272)
      LARAY(L)=L
10    CONTINUE
      GO TO 15
12    PRINT*, '  ******  YOU HAVE A PROBLEM READING THE CATALOG DATA!!!'
      STOP
c
c          SORT THE ARRAY IN ASCENDING ORDER USING HEAP SORT
c
15    MAXV=NDX
      IF(I.NE.2) CALL HINDX(ARRAY,FIELD)
      NVAL=MAXV

40    IF(LENFLD.GT.5) LENFLD=5
      DATFIL3=FIELD(1:LENFLD)//FO//'.NDX'
      IF(FIELD.EQ.'DAMNAM') DATFIL3='DMNAM'//FO//'.NDX'
      IF(FIELD.EQ.'CTYFIP') DATFIL3='FIPS'//FO//'.NDX'
      IF(FIELD.EQ.'COUNTY') DATFIL3='CTY'//FO//'.NDX'
      OPEN(UNIT=7,FILE=DATFIL3,ACCESS='DIRECT',RECL=4)
      WRITE(7) NVAL
      DO 50 L=1,NVAL
      WRITE(7) LARAY(L)
50    CONTINUE
      CLOSE(7)

CC      WRITE(16,55) NVAL,MAXV
CC   55 FORMAT(1X,'NVAL=',I5,5X,'MAXV=',I5)
CC      WRITE(16,60) (LARAY(L),L=1,MAXV)
CC   60 FORMAT(1X,20I3)
CC      READ(5,9,REC=1,END=70,ERR=12) INFO
CC      WRITE(16,65) INFO(1:272)
CC   65 FORMAT(' INFO='/A272)
CC      READ(5,9,REC=NVAL,END=70,ERR=12) INFO
CC      WRITE(16,65) INFO(1:272)
CC      GO TO 100
CC 70   PRINT*, ' YOU HAVE REACHED THE END OF THE DATA FILE'
CC      STOP

100   CONTINUE
      PRINT 110
110   FORMAT(/)
      PAUSE '                  HIT <ENTER> KEY TO CONTINUE'
      RETURN
      END
c ---------------------------------------------------------------------
      SUBROUTINE HINDX(ARRAY,FIELD)
c
c$PRAGMA C (gettim)
      INTEGER SZ, MAXV
      INTEGER*2 IHR,IMIN,ISEC,I100TH
      PARAMETER(SZ=5031)
      CHARACTER*(*) ARRAY(SZ)
      CHARACTER ARR*26,FIELD*6
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL

      L=MAXV/2+1
      IR=MAXV

      CALL gettim(IHR,IMIN,ISEC,I100TH)
      PRINT 7, FIELD,IHR,IMIN,ISEC,I100TH
    7 FORMAT(1H+ '     GENERATING INDEX POINTERS FOR ',A6,' TIME IS ',
     . 2(I2.2,1H:),I2.2,1H.,I2.2)

10    CONTINUE
      IF(L.GT.1) THEN
        L=L-1
        INDXT=LARAY(L)
        ARR=ARRAY(INDXT)
      ELSE
        INDXT=LARAY(IR)
        ARR=ARRAY(INDXT)
        LARAY(IR)=LARAY(1)
        IR=IR-1
        IF(IR.EQ.1) THEN
          LARAY(1)=INDXT
          RETURN
        END IF
      END IF
      I=L
      J=L+L
20    IF(J.LE.IR) THEN
        IF(J.LT.IR) THEN
          IF(ARRAY(LARAY(J)).LT.ARRAY(LARAY(J+1))) J=J+1
        END IF
        IF(ARR.LT.ARRAY(LARAY(J))) THEN
          LARAY(I)=LARAY(J)
          I=J
          J=J+J
        ELSE
          J=IR+1
        END IF
        GO TO 20
      END IF
      LARAY(I)=INDXT
      CALL gettim(IHR,IMIN,ISEC,I100TH)
      PRINT 7, FIELD,IHR,IMIN,ISEC,I100TH
      GO TO 10
      END
c ----------------------------------------------------------------------
      SUBROUTINE GETREC(DATFIL)
c
c
c  Searching
c  INPUT:  Damname, Dam I.D., River, Nearest town downstream, or
c          County FIPS code.
c
c  OUTPUT:  Sorted and searched Dambrk Catalog.
c
c
      CHARACTER ANSW, DATFIL*10,DATFIL3*12,FO*3,TYPE*4
      INTEGER MAXV, SZ
      PARAMETER(SZ=5031)
      COMMON/CONST/ MAXV,LARAY(SZ),NVAL
      COMMON /FOFF/FO,TYPE
      COMMON/RECNO/IREC
c
c
c  /////////////////////////////////////////////////////////////////////
c  / (1) Dam Name.                                                     /
c  / (2) Dam I.D.                                                      /
c  / (3) Nearest Town Downstream.                                      /
c  / (4) Name of River.                                                /
c  / (5) County Fips Code.                                             /
c  / (6) County Name.						       /
c  / (7) FINISHED SEARCHING.					       /
c  / (8) EXIT PROGRAM.						       /
c  /////////////////////////////////////////////////////////////////////
c

      OPEN(5,FILE=DATFIL,ACCESS='DIRECT',RECL=272,FORM='FORMATTED',
     1     STATUS='OLD',ERR=300)

      DATFIL3='DMNAM'//FO//'.NDX'
      OPEN(7,FILE=DATFIL3,ACCESS='DIRECT',RECL=4)
      READ(7,END=100) MAXV
      GO TO 200
  100 CALL INDX

  200 CLOSE(7)
      CALL SRCH(1)
      IF(IREC.GT.0) GO TO 5000
      PRINT 210
  210 FORMAT(/' DAM WAS NOT FOUND IN THE CATALOG.  DO YOU WANT TO ADD IT
     .? '/)
      READ '(A)', ANSW
      IF(ANSW.NE.'Y') GO TO 5000
      CALL ADDC(1)
      CALL INDX
CC      PRINT 220
CC  220 FORMAT((///1X,79('=')/20X,' SPECIAL NOTICE TO DAM CATALOG USERS '
CC     .//5X,'A NEW DAM HAS BEEN ADDED TO THE DAM CATALOG.  IN ORDER FOR T
CC     .DAM TO BE ACCESSED, THE INDEX POINTER FILES MUST BE GENERATED.DAMC
CC     .AT" OPTION IN THE MENU'/1X,79('=')/)

      GO TO 5000
c
  300 PRINT 310, DATFIL
  310 FORMAT(//5X,'FILENAME(',A12,')DOES NOT EXIST.'/5X,'PROGRAM TERMINA
     .TED')
      IREC=0
      CALL EXIT

 5000 CLOSE(5)
      RETURN
      END
