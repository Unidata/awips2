C$PRAGMA C (UPFSIZ)
C MODULE UDSATR
C-----------------------------------------------------------------------
C
      SUBROUTINE UDSATR (DDN,PATHNAME,VOL,RECFM,LRECL,LBLOCK,DSORG,
     *   NTRK,NTRKU,IPRERR,ISTAT)

      INCLUDE 'uiox'
      INCLUDE 'upvrsx'
      INCLUDE 'udsatx'
      INCLUDE 'udebug'

      CHARACTER*4 KNTL
      CHARACTER*8 DDN
      CHARACTER*32 FILENAME
      CHARACTER*(*) PATHNAME,VOL,RECFM,DSORG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/udsatr.f,v $
     . $',                                                             '
     .$Id: udsatr.f,v 1.4 2002/02/11 20:44:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      LDEBUG=0       
C       
      PATHNAME= ' '
      VOL=' '
      RECFM=' '
      DSORG=' '
      LRECL=-99
      LBLOCK=-99
      NTRK=-99
      NTRKU=-99
C
      IF (DDN.EQ.'STEPLIB') GO TO 800
C
C  GET UNIT NUMBER
      IBEG=3
      NCHAR=2
      IPRERR=1
      CALL UFA2I (DDN,IBEG,NCHAR,IUNIT,IPRERR,LP,IERR)
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 800
         ENDIF
C
C  GET FILE ATTRIBUTES
      KNTL='NORM'
      IF (PGMNAM.EQ.'REORDER') KNTL='REOR'
      IF (PGMNAM.EQ.'XUR'    ) KNTL='REOR'
      FILENAME = '  '      
      CALL UPFNCU (KNTL,FILENAME,IUNIT,PATHNAME,NUNT,LRECL,LBLOCK,IERR)
C
      IF (LRECL.LE.0) GO TO 300
C
C  GET FILE SIZE
      CALL UPFSIZ (PATHNAME,LENSTR(PATHNAME),ISIZE)
      IF (ISIZE.LT.0) THEN
         ISTAT=1
         GO TO 800
         ENDIF
C
C  COMPUTE NUMBER OF RECORDS 
      NREC=(ISIZE+LRECL-1)/LRECL
      IF (LBLOCK.LE.0) LBLOCK=(ISIZE+3)/4
C
C  COMPUTE FILE SPACE
      NPUNIT=0
      DSUNIT='3380'
      IPRINT=0
      CALL UDKBLK (' ',NPUNIT,DSUNIT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 800
         ENDIF
      IF (LBLOCK.LT.LRECL ) LBLOCK=LRECL
      NPRBLK=LBLOCK/LRECL
      NPRTRK=NPRBLK*NBLKS
      IF (NPRTRK.LE.0) NPRTRK=1
      NTRK=NREC/NPRTRK
      IF ((NREC/NPRTRK)*NPRTRK.NE.NREC) NTRK=NTRK+1
      NTRKU=NTRK
C
300   IF (LDEBUG.GT.0) THEN
         WRITE(LP,*)
     *      ' LRECL=',LRECL,
     *      ' LBLOCK=',LBLOCK,
     *      ' ISIZE=',ISIZE,
     *      ' NREC=',NREC, 
     *      ' NBLKS=',NBLKS, 
     *      ' NPRBLK=',NPRBLK, 
     *      ' NPRTRK=',NPRTRK, 
     *      ' NTRK=',NTRK,
     *      ' '
         ENDIF
C
800   RETURN
C
      END
