C  =====================================================================
C  pgm: URRPDS(NUMREC,DDN,MEMBER,REC,LRECL,ISTAT) .. Open/read seq file
C
C  i/o: NUMREC ........ record number to be read next, if 0 then open
C   in:                 the file and read record 1, becomes next record
C   in:                 to be read
C   in: DDN ........... input data set (or directory), if FT03F001 then
C   in:                 make a pathname using subroutine UPPFIX for
C   in:                 the system files "SYS.FILES", else if MODSPDS
C   in:                 then make pathname using subroutine UPPFIX for
C   in:                 the "MODS" directory, else if not blank
C   in:                 use it as a dir in the pathname for fil "MEMBER"
C   in: MEMBER ........ name of the sequential file to be read
C  out: REC ........... packed char array for next record read
C  out: LRECL ......... number of bytes read from file (always 80)
C  out: ISTAT ......... status code, 0 = no error, pos = error
C                         ISTAT  4 = END-OF-FILE
C                         ISTAT  8 = FILE NOT FOUND
C                         ISTAT 12 = FILENAME TO BE OPENED, NOT GIVEN
C                         ISTAT 20 = CANNOT ACCESS FILE
C                         ISTAT 24 = READ ERROR
C                         ISTAT 28 = CANNOT OPEN FILE
C                         ISTAT 32 = CANNOT CLOSE OPENED FILE
C   in: (LSYS) ........ from COMMON UIOX is the unit number to be used
C  i/o: (IFILES) ...... in COMMON UFILES flags for open file on gvn unit
C
C  rqd: subrtns: KKTRIM,UDOE,UPOPEN,UPCLOS,UPEXIS
C
C  cmt: Note, opened files in this rtn are all formatted.
C  cmt: Note, if NUMREC=0 and DDN=' ', this is a sneeky message to close
C  cmt:       the currently opened file on unit LSYS.
C  =====================================================================
      SUBROUTINE URRPDS(NUMREC,DDN,MEMBER,REC,LRECL,ISTAT)

      EXTERNAL   KKTRIM,UPOPEN,UPCLOS,UPEXIS,UDOE

      LOGICAL*1      REC(1),RECLOG(80)
      CHARACTER*80   RECCHR
      EQUIVALENCE  ( RECCHR,RECLOG(1) )

      CHARACTER*128  NEWNAM
      CHARACTER*8    DDN,MEMBER
      CHARACTER*32   XMEMBER
      INTEGER        DD1,DD2,MM1,MM2
      INTEGER        LRECL,ISTAT,NUMREC,CC,I,JRECL,IER

      INCLUDE 'uiox'
      INCLUDE 'ufiles'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/urrpds.f,v $
     . $',                                                             '
     .$Id: urrpds.f,v 1.2 1999/04/22 13:56:41 page Exp $
     . $' /
C    ===================================================================
C

      ISTAT =  0
      LRECL = 80
      CC    =  0

      IF (NUMREC .LE. 0) THEN

        CALL KKTRIM(DDN,DD1,DD2)
        CALL KKTRIM(MEMBER,MM1,MM2)

        IF (MEMBER(MM1:MM2) .EQ. ' ') THEN
          IF (IFILES(LSYS) .GT. 0) THEN
            CALL UPCLOS(LSYS,' ',CC)
            IF (CC .EQ. 0) IFILES(LSYS) = 0
            IF (CC .NE. 0) ISTAT = 32
          ELSE
            ISTAT = 12
          ENDIF
          CC = 1

        ELSE
          IF( DDN(DD1:DD2) .EQ. 'FT03F001' ) THEN
            XMEMBER = MEMBER
            CALL UPPFIX('SYST',XMEMBER,NEWNAM,CC)
          ELSEIF( DDN(DD1:DD2) .EQ. 'MODSPDS' ) THEN
            XMEMBER = MEMBER
            CALL UPPFIX('MODS',XMEMBER,NEWNAM,CC)
          ELSE
            NEWNAM = DDN(DD1:DD2) // '/' // MEMBER(MM1:MM2)
          ENDIF

          CALL UPEXIS(LSYS,NEWNAM,CC)
          IF(CC.LT.0) CALL UDOE(LSYS,CC,'seq file does not exist ')
          IF(CC.GT.0) CALL UDOE(LSYS,CC,'cannot access seq file  ')
          IF(CC.LT.0) ISTAT =  8
          IF(CC.GT.0) ISTAT = 20

C                Close any old file that may be on unit LSYS (3)

          IF (CC .EQ. 0) THEN
            CALL UPCLOS(LSYS,' ',CC)
            IF(CC.EQ.0) IFILES(LSYS) = 0

C                Open or create a formatted sequential file

            JRECL = 0
            CALL UPOPEN(LSYS,NEWNAM,JRECL,'F',CC)
            IF(CC.EQ.0) IFILES(LSYS) = 2
            IF(CC.NE.0) CALL UDOE(LSYS,CC,'cannot open seqntl file ')
            IF(CC.NE.0) ISTAT = 28
          ENDIF

        ENDIF

      ENDIF

C                Read next line in file

      IF (CC .EQ. 0) THEN
        READ (LSYS,'(A)',IOSTAT=IER) RECCHR
        IF (IER .LT. 0) THEN
          ISTAT  = 4
          NUMREC = 0
        ELSEIF (IER .GT. 0) THEN
          ISTAT  = 24
          NUMREC =  0
        ELSE
          DO 14 I=1,LRECL
   14     REC(I) = RECLOG(I)
          NUMREC = NUMREC+1
        ENDIF
      ENDIF

      RETURN
      END
