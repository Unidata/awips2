C MEMBER PDFNDI
C  (from old member PDPDFNDI)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/14/94.14:59:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE PDFNDI (INTID,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
C
C          ROUTINE:  PDFNDI
C
C             VERSION:  1.0.0
C    8/1/86 SRS ALLOW OPTION TO USE REORDER COMMON BLOCKS
C
C                DATE:  12-28-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE WILL SEARCH THE PREPROCESSOR DATA BASE INTEGER INDEX
C  FOR THE INTEGER STATION ID OR FOR A FREE SLOT IN THE INTEGER HASH
C  ARRAY IF NOT FOUND.  IT USES A HASHING ALGORITHM TO COMPUTE
C  THE HASH SLOT IN THE INTEGER HASH ARRAY, GETS A RECORD
C  NUMBER FROM THE SLOT AND READS THE STATION INFORMATION RECORD
C  IF FOUND.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM     DESCRIPTION
C
C       INTID       I    I     1      STATION INTID
C       LSIBUF      I    I     1      LENGTH OF ISIBUF
C       IFIND       I    O     1      HASH NUMBER IF FOUND
C                                       (0 IF NOT FOUND)
C       ISIREC      I    O     1      STATION INFORMATION RECORD NUMBER
C                                       (0 IF NOT FOUND)
C       ISIBUF      I    O    LSIBUF  STATION INFORMATION RECORD IF
C                                       FOUND
C       IFREE       I    O      1     FREE SLOT IF NOT FOUND
C                                       (0 IF FOUND)
C       ISTAT       I    O      1     STATUS
C                                       0=OK
C                                       1=DAIO READ ERROR
C                                       2=FILE FULL
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urhshi'
      INCLUDE 'urcommon/ursifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfndi.f,v $
     . $',                                                             '
     .$Id: pdfndi.f,v 1.1 1995/09/17 18:43:51 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,140) IAMORD
C
      ISTAT=0
C
      IPRIME=17
      ITRY=0
      ISAVE=0
      IFREE=0
      IFIND=0
C
C  SET UNIT NUMBER
      LUNIT=KPDSIF
      IF (IAMORD.EQ.1) LUNIT=KURSIF
C
C  NHASHR IS NUMBER OF RECORDS IN INDEX
C  IHASHR IS NUMBER OF RECORDS IN NEW INDEX
      IF (IAMORD.NE.1) IOVFL=NHASHR*.87225
      IF (IAMORD.EQ.1) IOVFL=IHASHR*.87225
      IHOLD=IOVFL-1
      IRETRY=IPRIME/2+1
C
C  HASH ON STATION INTID
      CALL PDHASI (IHOLD,INTID,IHASH)
C
C  CHECK FOR IDENTIFIER
      DO 30 I=1,IRETRY
         IF (IAMORD.NE.1) ISIREC=IPDHSI(IHASH)
         IF (IAMORD.EQ.1) ISIREC=IURHSI(IHASH)
         IF (IPDDB.GT.0) WRITE (IOGDB,150) ISIREC
C     CHECK FOR DELETED SLOT
         IF (ISIREC.EQ.-1) GO TO 10
C        CHECK FOR UNUSED SLOT
            IF (ISIREC.EQ.0) GO TO 60
            CALL UREADT (LUNIT,ISIREC,ISIBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
C        COMPARE INTEGER ID
            IF (INTID.EQ.ISIBUF(6)) GO TO 80
            GO TO 20
C     FOUND A DELETED SLOT - SAVE LOCATION
10       IF (ISAVE.EQ.0) ISAVE=IHASH
C     RESET HASH VALUE
20       ITRY=ITRY+1
         IHASH=IHASH+IPRIME
         IF (IHASH.LE.IHOLD) GO TO 30
            IHASH=IHASH-IHOLD+1
30       CONTINUE
C
C  CHECK OVERFLOW AREA
      IF (IPDDB.GT.0) WRITE (IOGDB,160)
      IF (IAMORD.NE.1) LSTREC=NHASHR
      IF (IAMORD.EQ.1) LSTREC=IHASHR
      IF (IPDDB.GT.0) WRITE (IOGDB,165) IOVFL,LSTREC
      DO 50 IHASH=IOVFL,LSTREC
         IF (IAMORD.NE.1) ISIREC=IPDHSI(IHASH)
         IF (IAMORD.EQ.1) ISIREC=IURHSI(IHASH)
         IF (IPDDB.GT.0) WRITE (IOGDB,150) ISIREC
C     CHECK IF SLOT NOT USED
         IF (ISIREC.EQ.0) GO TO 60
C     CHECK FOR DELETED SLOT
         IF (ISIREC.NE.-1) GO TO 40
            IF (ISAVE.EQ.0) ISAVE=IHASH
            GO TO 50
40       CALL UREADT (LUNIT,ISIREC,ISIBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 110
C     COMPARE INTEGER ID
         IF (INTID.EQ.ISIBUF(6)) GO TO 80
50       CONTINUE
C
C  NOT FOUND OR INDEX IS FULL
      IF (ISAVE.NE.0) GO TO 70
      WRITE (LPE,170)
      ISTAT=2
      GO TO 130
C
C  FOUND EMPTY SLOT AND INTID NOT FOUND
60    IFREE=IHASH
C
C  CHECK IF FREE SLOT FOUND
70    IF (ISAVE.NE.0) IFREE=ISAVE
      GO TO 130
C
C  STATION NUMBER FOUND
80    IFIND=IHASH
C
C  CHECK SIZE OF SIF ARRAY
      IF (ISIBUF(1).LE.LSIBUF) GO TO 90
         WRITE (LPE,180) ISIBUF(1),LSIBUF
         ISTAT=1
         GO TO 130
C
C  COMPUTE NUMBER OF RECORDS IS SIF RECORD
90    IF (IAMORD.NE.1) LRCPD2=LRCPDI*2
      IF (IAMORD.EQ.1) LRCPD2=LRLURI*2
      N=ISIBUF(1)
      NREC=IUNRCD(N,LRCPD2)
      IF (NREC.EQ.1) GO TO 100
      NREC=NREC-1
C
C  READ THE REST OF THE STATION INFORMATION RECORD
      ISIRC1=ISIREC+1
      CALL RVLRCD (LUNIT,ISIRC1,NREC,ISIBUF(LRCPD2+1),LRCPDI,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
C
100   IF (IPDDB.GT.0) WRITE (IOGDB,190) (ISIBUF(I),I=1,22)
      GO TO 130
C
C  READ ERROR
110   WRITE (LPE,200) ISIREC,LUNIT,ISTAT
      GO TO 130
120   WRITE (LPE,200) ISIRC1,LUNIT,ISTAT
C
130   IF (IPDTR.GT.0) WRITE (IOGDB,210) INTID,IFREE,IFIND
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' *** ENTER PDFNDI - IAMORD=',I2)
150   FORMAT (' ISIREC=',I6)
160   FORMAT (' CHECK OVERFLOW AREA')
165   FORMAT (' IOVFL=',I6,3X,'LSTREC=',I6)
170   FORMAT ('0*** ERROR - IN PDFNDI - PREPROCESSOR DATA BASE ',
     *   'INTEGER INDEX IS FULL.')
180   FORMAT ('0*** ERROR - IN PDFNDI - STATION INFORMATION RECORD ',
     *   'NEEDS ',I4,' WORDS BUT ONLY ',I4,' ARE AVAILABLE.')
190   FORMAT (' ISIBUF=',I3,4A2,5I4,4(2A2,I4))
200   FORMAT ('0*** ERROR - IN PDFNDI - DAIO READ ERROR READING ',
     *   'RECORD ',I6,' FROM UNIT ',I2,3X,'. ISTAT=',I3)
210   FORMAT (' *** EXIT PDFNDI - INTID=',I5,3X,'IFREE=',I6,3X,
     *   'IFIND=',I6)
C
      END
