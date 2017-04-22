C MODULE RPPREC
C-----------------------------------------------------------------------
C
      SUBROUTINE RPPREC (ID,ITYPE,IPTRO,LARRAY,ARRAY,NFILL,IPTRNX,
     *   ISTAT)
C
C  THIS ROUTINE READS A PARAMETER RECORD FROM THE PREPROCESSOR
C  PARAMETRIC DATA BASE.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       ID         A8    I     1    STATION ID
C       ITYPE      A4    I     1    PARAMETER TYPE
C       IPTRO      I    I/O    1    RECORD NUMBER
C                                   IF NO ERRORS, IPTRO IS SET TO CORRECT
C                                   RECORD NUMBER
C       LARRAY     I     I     1    LENGTH OF ARRAY
C       ARRAY      R     O   LARRAY THE ARRAY TO BE FILLED
C       NFILL      I     O     1    NUMBER OF WORDS USED IN ARRAY OR
C                                   NUMBER NEEDED IF NOT ENOUGH
C       IPTRNX     I     O     1    RECORD NUMBER OF NEXT RECORD OF SAME
C                                    TYPE
C       ISTAT      I     O     1    STATUS CODE:
C                                    0=SUCCESSFUL READ
C                                    1=SYSTEM ERROR
C                                    2=RECORD NOT FOUND OR
C                                      NO PARAMETER RECORDS FOR TYPE
C                                    3=PARAMETER ARRAY TO SMALL
C                                    4=DATA TYPE NOT FOUND
C                                    5=RECORD NUMBER OUT OF RANGE
C                                    6=LAST RECORD DELETED
C                                    7=RECORD IN INDEX DOES NOT MATCH FILE
C                                    8=STATION WANTED NOT AT RECORD NUMBER
C                                      ENTERED (IPTRO LT 0)
C                                    9=DELETED STATION FOUND AT RECORD
C                                      NUMBER ENTERED (IPTRO LT 0)
C
C
      INCLUDE 'uiox'
      INCLUDE 'udatas'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urppdt'
C
      INTEGER     JTYPE
      CHARACTER*4 JCTYPE, ITYPE
      EQUIVALENCE ( JTYPE,JCTYPE )
C
      CHARACTER*8 ID
      LOGICAL BLNKID
C
      DIMENSION IXBUF(4),IWORK(16),ARRAY(1)
      DIMENSION LDELET(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rpprec.f,v $
     . $',                                                             '
     .$Id: rpprec.f,v 1.4 2002/02/11 18:55:34 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LDELET/4hDELE,4hTED /
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,250)
C
      JCTYPE = ITYPE
      IPTR=IABS(IPTRO)
      BLNKID=.FALSE.
      IF (ID.EQ.' ') BLNKID=.TRUE.
C
      IF (IPPDB.GT.0) WRITE (IOGDB,260) ID,JCTYPE,IPTR,BLNKID
C
      ISTAT=0
      IPTRNX=0
C
C  EXTRACT THE FOLLOWING VALUES FROM THE APPROPRIATE COMMON BLOCKS
C  DEPENDING ON WHETHER THIS IS A REORDER RUN (IAMORD.EQ.1)
C  OR NOT (IAMORD.NE.1)
C    1. IDXDAT - NUMBER OF DATA TYPE IN LIST
C    2. NPARM  - NUMBER OF PARAMETER RECORDS DEFINED FOR TYPE
C    3. LUPRMF - LOGICAL FILE UNIT FOR TYPE
C    4. IFPTR  - RECORD # OF FIRST STATION WITH DATA TYPE TYPE ON FILE
C    5. MXPTR  - MAXIMUM RECORD NUMBER FOR HASH FILE
C
C  CHECK FOR VALID DATA TYPE
      IDXDAT=IPCKDT(JTYPE)
      IF (IDXDAT.NE.0) GO TO 10
         IF (IPPDB.GT.0) WRITE (IOGDB,270) JCTYPE
         ISTAT=4
         GO TO 240
C
10    IF (IAMORD.EQ.1) GO TO 20
         NPARM=IPDTDR(5,IDXDAT)
         LUPRMF=KPPRMU(IPDTDR(2,IDXDAT))
         IFPTR=IPDTDR(3,IDXDAT)
         MXPTR=IPMCTL(2,IPDTDR(2,IDXDAT))
         GO TO 30
C
C  A REORDER RUN
20    NPARM=JPDTDR(5,IDXDAT)
      LUPRMF=KUPRMI(JPDTDR(2,IDXDAT))
      IFPTR=JPDTDR(3,IDXDAT)
      MXPTR=JPMCTL(2,JPDTDR(2,IDXDAT))
C
C  CHECK IF ANY PARAMETER DEFINED
30    IF (NPARM.GT.0) GO TO 40
         IF (IPPDB.GT.0) WRITE (IOGDB,280) JCTYPE
         ISTAT=2
         GO TO 240
C
40    IF (IPPDB.GT.0) WRITE (IOGDB,290) IAMORD,IDXDAT,LUPRMF,
     *   IFPTR,MXPTR,NPARM,IPTRO
C
C  CHECK IF RECORD NUMBER IS IN VALID RANGE
      IF (IPTR.GT.MXPTR) GO TO 230
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  BRANCH FOR RECORD NUMBER ENTERED (IPTRO) LT, EQ, OR GT ZERO
C
      IF (IPTRO) 50,70,90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO LT 0
C
50    IF (BLNKID) GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO LT 0 AND NONBLANK ID
C
C  GET RECORD USING RECORD NUMBER
      CALL UREADT (LUPRMF,IPTR,IWORK,ISTAT)
      IF (IPPDB.GT.0) WRITE (IOGDB,300) LUPRMF,IPTR,ISTAT,
     *   (IWORK(I),I=2,5)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF ID ENTERED MATCHES ID ON FILE
      CALL UCMPAR (IWORK(2),ID,2,IMATCH)
      IF (IMATCH.NE.0) GO TO 200
C
C  ID MATCHES
      GO TO 140
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO LT 0 AND BLANK ID
C
C  GET RECORD USING RECORD NUMBER
60    CALL UREADT (LUPRMF,IPTR,IWORK,ISTAT)
      IF (IPPDB.GT.0) WRITE (IOGDB,300) LUPRMF,IPTR,ISTAT,
     *   (IWORK(I),I=2,5)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF ID IS DELETED
      CALL UCMPAR (IWORK(2),LDELET,2,IMATCH)
      IF (IMATCH.EQ.0) GO TO 210
C
C  NOT A DELETED STATION - MOVE ID ON FILE INTO ID
      CALL UMEMOV (IWORK(2),ID,2)
      GO TO 140
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO EQ 0
C
70    IF (BLNKID) GO TO 80
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO EQ 0 AND NONBLANK ID - HASH
C
      GO TO 120
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO EQ 0 AND BLANK ID - GET FIRST OF TYPE
C
80    IPTR=IFPTR
      GO TO 100
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO GT 0
C
90    IF (BLNKID) GO TO 100
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO GT 0 AND NONBLANK ID
C
C  GET RECORD USING RECORD NUMBER
      CALL UREADT (LUPRMF,IPTR,IWORK,ISTAT)
      IF (IPPDB.GT.0) WRITE (IOGDB,300) LUPRMF,IPTR,ISTAT,
     *   (IWORK(I),I=2,5)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF ID ENTERED MATCHES ID ON FILE
      CALL UCMPAR (IWORK(2),ID,2,IMATCH)
C
C  IF ID DOES NOT MATCH
      IF (IMATCH.NE.0) GO TO 120
C
C  ID MATCHES
      GO TO 140
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IPTRO GT 0 AND BLANK ID
C
C  GET RECORD USING RECORD NUMBER
100   CALL UREADT (LUPRMF,IPTR,IWORK,ISTAT)
      IF (IPPDB.GT.0) WRITE (IOGDB,300) LUPRMF,IPTR,ISTAT,
     *   (IWORK(I),I=2,5)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK IF ID IS DELETED
      CALL UCMPAR (IWORK(2),LDELET,2,IMATCH)
      IF (IMATCH.EQ.0) GO TO 110
C
C  NOT A DELETED STATION - MOVE ID ON FILE INTO ID
      CALL UMEMOV (IWORK(2),ID,2)
      GO TO 140
C
C  TRY NEXT STATION WITH TYPE=ITYPE
110   IPTR=IWORK(5)
      IF (IPTR.GT.0) GO TO 100
C
C  LAST STATION OF TYPE IS DELETED
C  IF YOU GET HERE HAVE RUN OUT OF STATIONS WITH TYPE=ITYPE ON FILE
      ISTAT=6
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NO MATCH OR USING ID TO GET RECORD
120   IFIND=1
      CALL PPFNDR (ID,JTYPE,IFIND,IXBUF,IFREE,ISTAT)
      IXREC=IFIND
      IF (IPPDB.GT.0) WRITE (IOGDB,310) ID,JCTYPE,IFIND,IFREE,ISTAT
      IF (IXREC.NE.0) GO TO 130
C
C  STATION NOT FOUND BY HASHING
      ISTAT=2
      GO TO 240
C
C  GOT IT IN THE INDEX - NOW READ THE FIRST PARM RECORD.
C  THE PARM RECORD MUST BE READ IN 3 STEPS. 1 RECORD, MIDDLE
C  RECORDS AND LAST RECORD SO THAT CALLER DOESNT NEED TO HAVE EXTRA
C  WORDS IN ARRAY
130   IF (IXBUF(4).LE.0.OR.IXBUF(4).GT.MXPTR) GO TO 220
      CALL UREADT (LUPRMF,IXBUF(4),IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C
C  CHECK FOR FILE MATCHING INDEX
      CALL UCMPAR (ID,IWORK(2),2,IMATCH)
      IF (IMATCH.NE.0) GO TO 190
      IF (JTYPE .NE.IWORK(4)) GO TO 190
      IPTR=IXBUF(4)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR MATCH OF DATA TYPE
140   IF (JTYPE.EQ.IWORK(4)) GO TO 150
         ISTAT=2
         GO TO 240
C
C  MOVE IWORK INTO ARRAY
150   IPTRNX=IWORK(5)
C
C  MAKE SURE ARRAY IS BIG ENOUGH
      NFILL=IWORK(1)-NPPHED
      IF (LARRAY.LT.NFILL) GO TO 180
      N=LRECPP-NPPHED
      CALL UMEMOV (IWORK(NPPHED+1),ARRAY(1),N)
C
C  CHECK IF NEED TO READ THE MIDDLE RECORDS
      IF (IWORK(1).LE.LRECPP) GO TO 240
      IREC=IPTR+1
      NREC=IUNRCD(IWORK(1),LRECPP)-2
      IF (NREC.LE.0) GO TO 160
C
C  READ THE MIDDLE RECORDS
      IF (IPPDB.GT.0) WRITE (IOGDB,320) IREC,NREC
      CALL RVLRCD (LUPRMF,IREC,NREC,ARRAY(N+1),LRECPP,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      IREC=IREC+NREC
C
C  CHECK IF NEED TO READ LAST RECORD
160   N=IWORK(1)-((NREC+1)*LRECPP)
      IX=IWORK(1)-(N+NPPHED)+1
      IF (IPPDB.GT.0) WRITE (IOGDB,330) N,IX,IREC
      IF (N.LE.0) GO TO 240
C
C  READ LAST RECORD INTO WORK THEN MOVE TO ARRAY
      CALL UREADT (LUPRMF,IREC,IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      CALL UMEMOV (IWORK,ARRAY(IX),N)
      GO TO 240
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ ERROR
170   WRITE (IOGDB,340) ISTAT
      ISTAT=1
      GO TO 240
C
C  NOT ENOUGH ROOM
180   ISTAT=3
      GO TO 240
C
C  FILE DOES NOT MATCH INDEX
190   ISTAT=7
      GO TO 240
C
C  STATION WANTED NOT AT RECORD NUMBER ENTERED (IPTRO LT 0)
200   ISTAT=8
      GO TO 240
C
C  DELETED STATION FOUND AT RECORD NUMBER ENTERED (IPTRO LT 0)
210   IPTRO=IPTR
      IPTRNX=IWORK(5)
      ISTAT=9
      GO TO 240
C
C  RECORD OUT OF RANGE
220   IPTRO=IXBUF(4)
230   IF (IPPDB.GT.0) WRITE (IOGDB,350) IPTR
      ISTAT=5
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
240   IF (IPPTR.GT.0) WRITE (IOGDB,360) ID,JCTYPE,IPTR,ISTAT
C
C  ONLY UPDATE IPTRO IF NO ERRORS
      IF (ISTAT.LT.1) IPTRO=IPTR
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   FORMAT (' ENTER RPPREC')
260   FORMAT (' ID=',A,3X,'ITYPE=',A4,3X,'IPTR=',I6,', BLNKID=',L4)
270   FORMAT(' IN RPPREC - DATA TYPE ',A4,' NOT IN ',
     *   'PARAMETER TYPE DIRECTORY')
280   FORMAT(' IN RPPREC - DATA TYPE ',A4,' FOUND BUT ',
     *   'NO PARAMETER RECORDS DEFINED')
290   FORMAT('  IAMORD, IDXDAT, LUPRMF,  IFPTR,  MXPTR,  NPARM,  ',
     *   'IPTRO'/7(1X,I7))
300   FORMAT(' UREADT CALLED:  LUPRMF=',I3,', IPTR=',I7,', ISTAT=',I2,
     *   ', IWORK(2-5)=',2A4,1X,A4,I7)
310   FORMAT(' HASHING - ID=',A,', ITYPE=',A4,', IFIND=',I7,
     *   ', IFREE=',I7,', ISTAT=',I3)
320   FORMAT (' SUB RPPREC, READING MIDDLE IREC,NREC:',2I6)
330   FORMAT (' FINAL READ: N=',I6,3X,'IX=',I6,3X,'IREC=',I6)
340   FORMAT (' IN RPPREC - READ ERROR - ISTAT=',I6)
350   FORMAT (' IN RPPREC - RECORD NUMBER ',I6,' OUT OF ',
     *   'RANGE')
360   FORMAT (' EXIT RPPREC : ID=',A,3X,'ITYPE=',A4,3X,
     *   'IPTR=',I6,3X,'ISTAT=',I2)
C
      END
