C MEMBER URHORD
C-----------------------------------------------------------------------
C
      SUBROUTINE URHORD (ICNFLG)
C
C                SUBROUTINE:  HCCOPY
C
C     7/16/86 TASK 282 UPDATES, RENAME ALL ROUTINES TO START WITH UR
C              VERSION: 1.0.1  UDATED FOR REORDER AND COMPRESS PROGRAM
C                        11-8-85   JF
C             VERSION:  1.0.0
C
C                DATE:  06/20/82
C
C              AUTHOR:  JONATHAN D. GERSHAN
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE COMPRESSES THE FOLLOWING FILES:
C           HCL GLOBAL INDEX (LDR RECORDS ONLY)
C           HCL GLOBAL DEFINITIONS
C           HCL LOCAL DEFINITIONS
C           HCL LOCAL DEFAULTS FOR GLOBAL DEFINITIONS
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        ICNFLG   I      I    1     PARAMETER FLAG
C                                    0 = NEWLY CREATED FILES
C                                    1 = SCRATCH FILES
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'ufreei'
      INCLUDE 'urcommon/urcdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
C
       DIMENSION MDUM(10) , IVER(2) , LSTCMD(2,5) , IWORD(4),
     1       NAMES(2,50)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhord.f,v $
     . $',                                                             '
     .$Id: urhord.f,v 1.1 1995/09/17 19:17:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C***********************************************************************
C
C          DATA:
C
       DATA LSTCMD/4hDEBU,4hG   ,4hCMPR,4hLDRS,4hCMPR,4hGDFN,4hCMPR,
     1            4hLDFN,4hSTOP,4h    /
       DATA MAXR/4hMAXR/,MAXL/4hMAXL/
C
C
C***********************************************************************
C
C
C
C
C
        NOBUG = 0
        NUMCMD = 5
        LPE = 6
        CALL URHCTG(ISTAT)
C
  100 CONTINUE
C
C  READ A COMMAND
C
        CALL RCMND2( LSTCMD , NUMCMD , INDXC)
        IF(INDXC.EQ.0) GO TO 200
        IF(INDXC.EQ.-1) GO TO 750
C
C
        IF(NOBUG.GT.0) WRITE(LPD,900) INDXC
900     FORMAT(' BACK IN HCLCOMPR, INDEX = ',I4)
C
        GO TO ( 700, 300 , 400 , 500  , 800 ), INDXC
C
C **********************************************************************
C
  200 CONTINUE
      WRITE( LPD , 210)
  210 FORMAT(' BAD CARD ------- NO COMMAND!! ')
      GO TO  100
C
C***********************************************************************
C
  300 CONTINUE
C
C  COMPRESS LOCAL DEFAULT RECORDS FOR GLOBAL DEFINITIONS
C
      WRITE(LP,310)
  310 FORMAT('0*** NOTE BEGIN TO COMPRESS THE HCL  << LOCAL DEFAULT >>',
     1       '  FILES FOR THE REORDER PROGRAM.')
      NEOFRC = 0
      ISTCOD = 0
C
C
       CALL UGETPC (2,1,IWORD,INUM,RNUM ,ISTAT)
       IPASS = IWORD(1)
       CALL URHCKP( IPASS , ISTCOD)
C
       IF (ISTCOD.EQ.0) GO TO 330
       WRITE(LPD,320)
 320   FORMAT(1X,'**ERROR** GLOBAL PASSWORD ILLEGAL OR MISSING')
       GO TO 100
C
330    IF (NFIELD.EQ.4) CALL UGETPC (4,2,IWORD,NEOFRC,RNUM,ISTAT)
C
C
       IF(NEOFRC.NE.0) WRITE(LPD,340)NEOFRC
340    FORMAT('0*** NOTE - FILE EXPANSION PARAMETER IS - ',I6)
       CALL URHLDR (NEOFRC , ISTAT)
C
       WRITE(LPD , 350)
  350  FORMAT('0*** NOTE - LOCAL DEFAULTS FOR GLOBAL DEFINITIONS',
     1         ' HAVE BEEN SUCCESSFULLY REORDERED.')
C
       GO TO 100
C
C **********************************************************************
400     CONTINUE
C
C   COMPRESS GLOBAL DEFINITIONS:
C
      WRITE(LP,410)
  410 FORMAT('0*** NOTE BEGIN TO COMPRESS THE HCL  << GLOBAL DEFINIT',
     1       'ION >>  FILES FOR THE REORDER PROGRAM.')
       NEOFRC = 0
       NEOLOC = 0
       CALL UGETPC (2,1,IWORD,INUM,RNUM,ISTAT)
       IPASS = IWORD(1)
       CALL URHCKP( IPASS , ISTCOD)
       IF (ISTCOD.EQ.0) GO TO 450
          WRITE(LPD,440)
 440     FORMAT(1X,'**ERROR** GLOBAL PASSWORD ILLEGAL OR MISSING')
          GO TO 100
  450  IF (NFIELD.GT.2) CALL UGETPC (3,1,IWORD,INUM,RNUM,ISTAT)
       IF(IWORD(1).EQ.MAXR)CALL UGETPC (4,2,IWORD,NEOFRC,RNUM,ISTAT)
       IF(IWORD(1).EQ.MAXL)CALL UGETPC (4,2,IWORD,NEOLOC,RNUM,ISTAT)
       IF(NFIELD.LT.5)GO TO 475
        CALL UGETPC (5,1,IWORD,INUM,RNUM,ISTCOD)
       IF(IWORD(1).EQ.MAXR)CALL UGETPC (6,2,IWORD,NEOFRC,RNUM,ISTAT)
       IF(IWORD(1).EQ.MAXL)CALL UGETPC (6,2,IWORD,NEOLOC,RNUM,ISTAT)
C
  475  CONTINUE
       IF(NEOFRC.EQ.0.OR.NEOLOC.EQ.0) GO TO 480
       WRITE(LPD,430) NEOFRC,NEOLOC
 430   FORMAT('0*** NOTE - GLOBAL DEFINITION FILE WILL BE EXPANDED',
     1       ' TO ',I5/12X,'LOCAL DEFAULT FILE WILL BE EXPANDED TO ',I5)
  480 CONTINUE
C
C  READ USER CARD FOR USER NAMES FOR LOCAL DEFAULT FILE ALLOCATION
C
       CALL URHGTU(NAMES,NUMNAM,ISTAT)
       IF(ISTAT.NE.0) GO TO 775
       CALL URHGDN(NEOFRC,NEOLOC,NAMES,NUMNAM,ISTAT)
C
       WRITE(LPD,490)
 490   FORMAT('0*** NOTE - GLOBAL DEFINITIONS HAVE BEEN SUCCESSFULLY ',
     1        'REORDERED.')
       GO TO 100
C
C **********************************************************************
C
500     CONTINUE
C
C   COMPRESS LOCAL DEFINITIONS:
C
       WRITE(LP,510)
  510  FORMAT('0*** NOTE BEGIN TO COMPRESS THE HCL  << LOCAL DEFINIT',
     1      'ION >>  FILES FOR THE REORDER PROGRAM.')
       NEOFRC = 0
       CALL UGETPC (2,1,IWORD,INUM,RNUM,ISTAT)
       IPASS = IWORD(1)
       CALL URHPSL( IPASS , ISTCOD)
       IF (ISTCOD.EQ.0) GO TO 530
       WRITE(LPD,520)
 520   FORMAT(1X,'**ERROR** LOCAL PASSWORD ILLEGAL OR MISSING')
       GO TO 100
  530  IF (NFIELD.EQ.4) CALL UGETPC (4,2,IWORD,NEOFRC,RNUM,ISTAT)
C
C    ABOUT TO COMPRESS LOCAL DEFINITIONS
C
       CALL URHLDN(NEOFRC,ISTAT)
       WRITE(LPD,540)
 540   FORMAT(1X,//,10('*'),2X,'LOCAL DEFINITION COMPRESS COMPLETE')
       GO TO 100
C
C **********************************************************************
C
  700 CONTINUE
C
C  SET DEBUG FLAGS
C
        CALL UGETPC (2,2,IWORD,INUM,RNUM,ISTAT)
        NOBUG = INUM
        IF(NOBUG.EQ.0) GO TO 100
        WRITE(LPD,710) NOBUG
710     FORMAT(' NOBUG HAS JUST BEEN SET TO ',I2)
C
        GO TO 100
C
C **********************************************************************
C
  750 CONTINUE
C
C  NO STOP CARD
C
      WRITE(LP,760)
  760 FORMAT(' NO STOP CARD. STOP ASSUMED.')
      GO TO 999
  775 CONTINUE
      IWURFL = ISTAT
      GO TO 999
800   CONTINUE
C
999   RETURN
C
      END
