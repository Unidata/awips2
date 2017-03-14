C MODULE DFGSPD
C-----------------------------------------------------------------------
C
C  pgm: DFGSPD .. Read SHEFPPDB file, place data into array ISHPDB
C
C  use:     CALL DFGSPD (CMD,MSHPDB,ISHPDB,NSHPDB,ISTAT)
C
C   in: CMD ..... Routine control: - CHAR*(*)   (uses first 4 chars)
C   in:             'INIT' .. (or first pass) causes the file to be
C   in:                       read and ISHPDB to be initialized
C  out: ISHPDB .. Shefpost paramtrs for up to 50 data types - INT (9,50)
C  out: NSHPDB ... Number of data types found in file SHEFPPDB - INT
C  out: ISTAT ... Error status: - INT
C  out:             0 .. no error
C  out:             1 .. could not make full pathname for SHEFPPDB
C  out:             2 .. could not open SHEFPPDB
C  out:             3 .. error during reading of SHEFPPDB
C  out:             4 .. error in format of SHEFPPDB line
C  out:                  (note, file limited to 50 data types and there
C  out:                   is no error for too many lines, it just stops
C  out:                   reading the file)
C   in: (file) .. ascii sequential file with data type parameters:
C   in:             example lines:
C   in:               PP24 PPDRZZZ  PP 2001 1024 2 R Z
C   in:               PP24 PPPRZZZ  PP 5004      2 R Z
C   in:               PP06 PPQRZZZ  PP 1006      4 R Z
C
C  =====================================================================
C
      SUBROUTINE DFGSPD (CMD,MSHPDB,ISHPDB,NSHPDB,ISTAT)
C
      CHARACTER*(*) CMD
      CHARACTER*4   BLNK/' '/
      CHARACTER*32  FILNAM
      CHARACTER*80  LINE
      CHARACTER*160 PTHNAM
C
      DIMENSION ISHPDB(9,MSHPDB)
C
      SAVE INITZ,NOFL
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfgspd.f,v $
     . $',                                                             '
     .$Id: dfgspd.f,v 1.3 2002/02/11 21:29:18 dws Exp $
     . $' /
C    ===================================================================
C
      DATA INITZ/0/,NOFL/0/
C
C
C  CHECK IF COMMAND IS 'INIT' OR IF FIRST PASS
      IF (CMD.EQ.'INIT' .OR. INITZ.EQ.0) THEN
C     READ FILE SHEFPPDB AND FILL VARIABLES ISHPDB AND NSHPDB
         INITZ=1
         NSHPDB=0
C     OPEN FILE
         FILNAM = 'SHEFPPDB'
         CALL UPPFIX ('SYST',FILNAM,PTHNAM,LENPTH)
         IF (LENPTH .LE. 0) THEN
            ISTAT=1
            ELSE
               CALL UPOPEN (LSYS,PTHNAM(1:LENPTH),0,'F',IERR)
               IF (IERR .NE. 0) ISTAT=2
            ENDIF
         NOFL=0
         IF (ISTAT.EQ.0) THEN
            NUM=0
  100       IF (NUM+1.GT.MSHPDB) THEN
               WRITE (LP,101) MSHPDB
101   FORMAT ('0**ERROR** IN DFGSPD - MAXIMUM NUMBER OF DATA TYPES (',
     *   I2,') EXCEEDED.')
               GO TO 120
               ENDIF
            IF (ISTAT.NE.0) GO TO 120
C        READ RECORD
            READ (LSYS,'(A)',END=120,IOSTAT=IERR) LINE
            IF (IERR.GT.0) THEN
               ISTAT=3
               ELSE
               IF (IERR.LT.0.OR.LINE.EQ.'END') THEN
                  NOFL=NUM
                  WRITE (BLNK,'(A4)') ISHPDB(1,NUM+1)
                  NUM=MSHPDB
                  GO TO 120
                  ENDIF
C            CHECK FOR COMMENT
               IF (LINE.EQ.' '.OR.LINE(1:1).EQ.'$') GO TO 100
               NUM=NUM+1
               READ (LINE,110,IOSTAT=IERR) (ISHPDB(J,NUM),J=1,9)
  110          FORMAT (A4,1X,2A4,1X,A2,I5,I5,I2,1X,A1,1X,A1)
               IF (IERR .NE. 0) THEN
                  ISTAT=4
                  ELSE
                     NOFL=NUM
                  ENDIF
                GO TO 100
                ENDIF
120         IF (NUM.LT.MSHPDB) THEN
C           ISHPDB NOT FULL - PUT BLANKS INTO THE FIRST POSITION OF
C           FIRST UNUSED ROW
               WRITE (BLNK,'(A4)') ISHPDB(1,NUM+1)
               ENDIF
C        CLOSE FILE
            CALL UPCLOS (LSYS,' ',IERR)
            ENDIF
         NSHPDB=NOFL
         ENDIF
C
      RETURN
C
      END
