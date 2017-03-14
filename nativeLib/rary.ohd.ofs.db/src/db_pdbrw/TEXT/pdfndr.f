C MODULE PDFNDR
C-----------------------------------------------------------------------

C
      SUBROUTINE PDFNDR (ID,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
C
C  THIS ROUTINE SEARCHES THE PREPROCESSOR DATA BASE CHARACTER INDEX
C  FOR THE CHARACTER STATION ID OR FOR A FREE SLOT IN THE CHARACTER HASH
C  ARRAY IF NOT FOUND.  IT USES A HASHING ALGORITHM TO COMPUTE
C  THE HASH SLOT IN THE CHARACTER HASH ARRAY, GETS A RECORD
C  NUMBER FROM THE SLOT AND READS THE STATION INFORMATION RECORD
C  IF FOUND.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM      DESCRIPTION
C       ------    ----  ---   ---      -----------
C       ID         A8    I     2       STATION ID
C       LSIBUF      I    I     1       LENGTH OF ISIBUF
C       IFIND       I    O     1       HASH NUMBER IF FOUND
C                                        0=NOT FOUND
C       ISIREC      I    O     1       STATION INFORMATION RECORD NUMBER
C                                        0=IF NOT FOUND
C       ISIBUF      I    O    LSIBUF   STATION INFORMATION RECORD IF
C                                        FOUND
C       IFREE       I    O     1       FREE SLOT IF NOT FOUND
C                                        0=IF NOT FOUND
C       ISTAT       I    O     1       STATUS
C                                        0=OK
C                                        1=OTHER ERROR
C                                        2=FILE FULL
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/ursifc'
C
      DIMENSION ID(2)
      INTEGER*2 ISIBUF(LSIBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfndr.f,v $
     . $',                                                             '
     .$Id: pdfndr.f,v 1.2 2000/07/21 18:33:01 page Exp $
     . $' /
C    ===================================================================
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
C  HASH ON STATION ID
      CALL PDHASH (IHOLD,ID,IHASH)
C
C SEE IF THIS IS THE ONE
      DO 30 I=1,IRETRY
         IF (IAMORD.NE.1) ISIREC=IPDHSC(IHASH)
         IF (IAMORD.EQ.1) ISIREC=IURHSC(IHASH)
         IF (IPDDB.GT.0) WRITE (IOGDB,*) 'ISIREC=',ISIREC
C     CHECK FOR DELETED SLOT
         IF (ISIREC.EQ.-1) GO TO 10
C        CHECK FOR UNUSED SLOT
            IF (ISIREC.EQ.0) GO TO 60
            CALL UREADT (LUNIT,ISIREC,ISIBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
C        COMPARE STATION ID
            CALL UCMPAR (ID,ISIBUF(2),2,MATCH)
            IF (MATCH.EQ.0) GO TO 80
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
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'CHECKING OVERFLOW AREA'
      IF (IAMORD.NE.1) LSTREC=NHASHR
      IF (IAMORD.EQ.1) LSTREC=IHASHR
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IOVFL=',IOVFL,' LSTREC=',LSTREC
      DO 50 IHASH=IOVFL,LSTREC
         IF (IAMORD.NE.1) ISIREC=IPDHSC(IHASH)
         IF (IAMORD.EQ.1) ISIREC=IURHSC(IHASH)
         IF (IPDDB.GT.0) WRITE (IOGDB,*) 'ISIREC=',ISIREC
C     CHECK FOR DELETED SLOT
         IF (ISIREC.NE.-1) GO TO 40
            IF (ISAVE.EQ.0) ISAVE=IHASH
            GO TO 50
C     CHECK IF SLOT NOT USED
40       IF (ISIREC.EQ.0) GO TO 60
            CALL UREADT (LUNIT,ISIREC,ISIBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
            CALL UCMPAR (ISIBUF(2),ID,2,MATCH)
            IF (MATCH.EQ.0) GO TO 80
50       CONTINUE
C
C  NOT FOUND OR INDEX IS FULL
      IF (ISAVE.NE.0) GO TO 70
         WRITE (LPE,170)
         ISTAT=2
         GO TO 130
C
C  FOUND EMPTY SLOT AND ID NOT FOUND
60    IFREE=IHASH
C
C  CHECK IF FREE SLOT FOUND
70    IF (ISAVE.NE.0) IFREE=ISAVE
      GO TO 130
C
C   FOUND STATION ID
80    IFIND=IHASH
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'ISIREC=',ISIREC
C
C  COMPUTE NUMBER OF RECORDS IN SIF RECORD
      IF (IAMORD.NE.1) LRCPD2=LRCPDI*2
      IF (IAMORD.EQ.1) LRCPD2=LRLURI*2
      N=ISIBUF(1)
      NREC=IUNRCD(N,LRCPD2)
      IF (N.LE.LSIBUF) GO TO 90
         WRITE (LPE,180) ISIBUF(1),LSIBUF
         ISTAT=1
         GO TO 130
C
C  READ THE REST OF THE STATION INFORMATION RECORD
90    IF (NREC.EQ.1) GO TO 100
      NREC=NREC-1
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
130   IF (IPDTR.GT.0) WRITE (IOGDB,210) ID,IFREE,IFIND
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' ENTER PDFNDR : IAMORD=',I2)
170   FORMAT ('0*** ERROR - IN PDFNDR - PREPROCESSOR DATA BASE ',
     *   'CHARACTER INDEX IS FULL.')
180   FORMAT ('0*** ERROR - IN PDFNDR - STATION INFORMATION RECORD ',
     *   'NEEDS ',I4,' WORDS BUT ONLY ',I4,' ARE AVAILABLE.')
190   FORMAT (' ISIBUF=',I3,4A2,5I4,4(2A2,I4))
200   FORMAT ('0*** ERROR - IN PDFNDR - DAIO READ ERROR READING ',
     *   'RECORD ',I6,' FROM UNIT ',I2,3X,'. ISTAT=',I3)
210   FORMAT (' EXIT PDFNDR : ID=',2A4,3X,'IFREE=',I6,3X,
     *   'IFIND=',I6)
C
      END
