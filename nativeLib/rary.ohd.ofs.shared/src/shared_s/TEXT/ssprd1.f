C MODULE SSPRD1
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT TIME SERIES HEADER INFORMATION.
C
      SUBROUTINE SSPRD1 (LARRAY,ARRAY,STAIDX,NFLD,ISTAT)
C
      CHARACTER*4 DTYPE,CTYPE,TTYPE
      CHARACTER*8 TSID,STAIDX,STAID
      CHARACTER*20 STRNG/' '/
      INTEGER MAP/5hMAP  /
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IARRAY(3,1)
      DIMENSION ISWORK(1),ISPTR(1),IWKBUF(1)
      DIMENSION IXBUF(4)
C
      INCLUDE 'uiox'
      INCLUDE 'udatas'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pmaxdm'
C
      INCLUDE 'scommon/dimrrs'
C
      EQUIVALENCE (IARRAY(1,1),SWRK2(1))
      EQUIVALENCE (ISWORK(1),SWORK(1))
      EQUIVALENCE (ISPTR(1),SWORK(101))
      EQUIVALENCE (IWKBUF(1),SWORK(201))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssprd1.f,v $
     . $',                                                             '
     .$Id: ssprd1.f,v 1.4 2001/06/13 13:51:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPRD1'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      STRNG=' '
      LSTRNG=-LEN(STRNG)
C
C  CHECK IF ANY TIME SERIES DEFINED
      IF (NUMTMS.EQ.0) THEN
         WRITE (LP,110)
         CALL SULINE (LP,2)
         GO TO 100
         ENDIF
C
C  CHECK IDENTIFIER SPECIFIED
      IF (STAIDX.NE.' ') THEN
C     GET RRS PARAMETERS
         STAID=STAIDX
         IPTR=0
         IREAD=1
         IPRERR=0
         INCLUDE 'scommon/callsrrrs'
         IF (IERR.GT.0) THEN
            WRITE (LP,120) STAID
            CALL SULINE (LP,2)
            GO TO 100
            ENDIF
         IF (NRRSTP.EQ.0) THEN
            WRITE (LP,130) STAID
            CALL SULINE (LP,2)
            GO TO 100
            ENDIF
         IFOUND=0
         NUMTS=0
         IFORMO=-999
         NCOUNT=0
         DO 10 I=1,NRRSTP
            IF (IRTIME(I).LT.0) GO TO 10
            DTYPE=RRSTYP(I)
C        FIND DATA TYPE IN DIRECTORY
            CALL PFDTYP (DTYPE,INDXT)
C        SET LOGICAL UNIT
            LUNIT=DATFIL(2,INDXT)
C        SET FORM
            IFORM=DATFIL(7,INDXT)
C        FIND TIME SERIES
            CALL PSERCH (STAIDX,DTYPE,IFREE,IXREC,IXBUF)
            IF (IXREC.EQ.0) THEN
               WRITE (LP,140) DTYPE,STAIDX
               CALL SUWRNS (LP,2,-1)
               GO TO 10
               ENDIF
            IREC=IXBUF(4)
            LWKBUF=LSWORK
            CALL SSPRD2 (NUMTS,LUNIT,STAIDX,DTYPE,IREC,
     *         IFORM,IFORMO,NCOUNT,IARRAY,LWKBUF,IWKBUF,
     *         LARRAY,ARRAY,IERR)
            NUMTS=NUMTS+1
            IF (NCOUNT.GT.0) IFOUND=1
10          CONTINUE
         IF (IFOUND.EQ.0) THEN
            WRITE (LP,150) STAID
            CALL SULINE (LP,2)
            ENDIF
         GO TO 100
         ENDIF
C
      IALL=0
      NUMOPT=0
C
      IF (NFLD.EQ.-1) THEN
         IALL=1
         GO TO 40
         ENDIF
C
      ISTRT=1
C
C  GET NEXT FIELD
20    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LSTRNG,
     *   STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) THEN
         IALL=1
         GO TO 40
         ENDIF
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LSTRNG,
     *      STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'NULL FIELD FOUND IN FIELD ',NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 20
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.GT.0) THEN
         IF (NUMOPT.GT.0) GO TO 90
         IALL=1
         GO TO 40
         ENDIF
C
30    NUMOPT=NUMOPT+1
      CTYPE=STRNG
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
40    MAXTYP=100
      LWKBUF=LSWORK-2*MAXTYP
      IF (NUMDTP.GT.MAXTYP) THEN
         WRITE (LP,160) NUMDTP,MAXTYP
         CALL SUERRS (LP,2,-1)
         GO TO 100
         ENDIF
C
C  STORE DATA TYPES FOR SORTING
      NUMTYP=NUMDTP
      IFMAP=0
      DO 50 I=1,NUMTYP
         CALL SUBSTR (DATFIL(1,I),1,4,ISWORK(I),1)
         IF (ISWORK(I).EQ.MAP) THEN
            IDSTRT=DATFIL(8,I)
            IF (IDSTRT.GT.0) IFMAP=1
            ENDIF
50       CONTINUE
      IF (IFMAP.EQ.1) THEN
         NUMTYP=NUMTYP+1
         CALL SUBSTR ('FMAP',1,4,ISWORK(NUMTYP),1)
         ENDIF
      NDIM=1
      ISPTR(1)=1
      CALL SUSORT (NDIM,NUMTYP,ISWORK,ISWORK,ISPTR,IERR)
C
      NWORDS=3
      MAXID=LSWRK2/NWORDS
      MAXIDS=0
      NUMREC=MAXTMS*2
C
      WRITE (LP,220)
      CALL SULINE (LP,2)
      WRITE (LP,170)
      CALL SULINE (LP,2)
C
C  PROCESS ALL TIME SERIES HEADERS FOR EACH TYPE
      IFORMO=-999
      DO 80 I=1,NUMTYP
         IPOS=ISPTR(I)
         IDSTRT=DATFIL(8,IPOS)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,180) IPOS,DATFIL(1,IPOS),IDSTRT
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IDSTRT.GT.0) THEN
            NUMID=0
            CALL SUBSTR (DATFIL(1,IPOS),1,4,TTYPE,1)
            IF (IALL.EQ.0.AND.TTYPE.NE.CTYPE) GO TO 80
            CALL SUBSTR (DATFIL(1,IPOS),1,4,IDTYPE,1)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,190) IPOS,IDTYPE
               CALL SULINE (IOSDBG,1)
               ENDIF
            DO 60 IXREC=1,NUMREC
               CALL UREADT (KINDEX,IXREC,IXBUF,IERR)
               IF (IERR.NE.0) THEN
                  WRITE (LP,200) IXREC,KINDEX
                  CALL SUERRS (LP,2,-1)
                  GO TO 60
                  ENDIF
               IF (IXBUF(3).NE.IDTYPE) GO TO 60
               NUMID=NUMID+1
               IF (NUMID.GT.MAXID) THEN
                  WRITE (LP,210) MAXID,IDTYPE
                  CALL SUERRS (LP,2,-1)
                  GO TO 80
                  ENDIF
               CALL SUBSTR (IXBUF,1,8,IARRAY(1,NUMID),1)
               IARRAY(3,NUMID)=IXBUF(4)
60             CONTINUE
            IF (NUMID.EQ.0) GO TO 80
C        SORT LIST
            IPTRS=0
            CALL SUSORT (NWORDS,NUMID,IARRAY,IARRAY,IPTRS,IERR)
            CALL PFDTYP (DATFIL(1,IPOS),IX)
            LUNIT=DATFIL(2,IPOS)
            IFORM=DATFIL(7,IX)
            NCOUNT=0
            DO 70 NUMTS=1,NUMID
               IREC=IARRAY(3,NUMTS)
               TSID=' '
               CALL SSPRD2 (NUMTS,LUNIT,TSID,IDTYPE,IREC,
     *            IFORM,IFORMO,NCOUNT,IARRAY,LWKBUF,IWKBUF,
     *            LARRAY,ARRAY,IERR)
70             CONTINUE
            IF (NUMID.GT.MAXIDS) MAXIDS=NUMID
            IF (I.LT.NUMTYP) THEN
C           CHECK NUMBER OF LINES LEFT ON PAGE
               NLINES=7
               NSLEFT=ISLEFT(NLINES)
               IF (NSLEFT.EQ.0) THEN
                  WRITE (LP,*) ' '
                  CALL SULINE (LP,1)
                  ELSE
                     CALL SUPAGE
                  ENDIF
               ENDIF
            ENDIF
80       CONTINUE
C
      IF (IALL.EQ.0) GO TO 20
C
90    WRITE (LP,230) MAXIDS,MAXID
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPRD1 : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** NOTE - NO TIME SERIES ARE DEFINED.')
120   FORMAT ('0*** NOTE - RRS PARAMETERS NOT FOUND ',
     *   'FOR STATION ',A,'.')
130   FORMAT ('0*** ERROR - RRS PARAMETERS FOUND ',
     *   'FOR STATION ',A,' ',
     *   'BUT THE NUMBER OF RRS DATA TYPES IS ZERO.')
140   FORMAT ('0*** WARNING - ',A,' TIME SERIES ',
     *   'FOR STATION ',A,' ',
     *   'NOT FOUND IN PROCESSED DATA BASE.')
150   FORMAT ('0*** NOTE - TIME SERIES IDENTIFIER ',A,' ',
     *   'NOT FOUND IN PROCESSED DATA BASE.')
160   FORMAT ('0*** ERROR - IN SSPRD1 - NUMBER OF DATA TYPES DEFINED (',
     *  I3,') EXCEEDS MAXIMUM THAT CAN BE PROCESSED (',I3,')')
170   FORMAT ('0- TIME SERIES HEADERS SORTED BY TYPE AND IDENTIFIER -')
180   FORMAT (' IPOS=',I2,3X,'DATFIL(1,IPOS)=',A4,3X,'IDSTRT=',I4)
190   FORMAT (' IPOS=',I2,3X,'IDTYPE=',A4)
200   FORMAT ('0*** ERROR - IN SSPRD1 - DAIO ERROR AT RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
210   FORMAT ('0*** ERROR - IN SSPRD1 - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED FOR DATA TYPE ',A4,
     *   '.')
220   FORMAT ('0')
230   FORMAT ('0*** NOTE - THE MOST TIME SERIES PROCESSED FOR ANY ',
     *   'TYPE IS ',I5,'. A MAXIMUM OF ',I5,' CAN BE PROCESSED.')
C
      END
