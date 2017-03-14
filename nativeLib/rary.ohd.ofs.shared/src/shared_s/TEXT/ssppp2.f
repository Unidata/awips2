C MODULE SSPPP2
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT SUMMARY OF ALL PARAMETER RECORDS IN THE PREPROCESSOR
C  PARAMETRIC DATA BASE.
C
      SUBROUTINE SSPPP2 (LARRAY,IARRAY,LEVEL,NFLD,ISTAT)
C
      CHARACTER*8 PARMID
      PARAMETER (MPTYPES=50)
      CHARACTER*4 CTYPE,PTYPES(MPTYPES)
      CHARACTER*20 STRNG/' '/
C      
      DIMENSION IARRAY(LARRAY)
      DIMENSION IBUF1(5),IBUF2(5)
      DIMENSION IWORK(3,1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
C
      EQUIVALENCE (IWORK(1,1),SWORK(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssppp2.f,v $
     . $',                                                             '
     .$Id: ssppp2.f,v 1.7 2001/06/13 14:06:35 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPPP2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ' )
C
      ISTAT=0
C
      STRNG=' '
      LSTRNG=-LEN(STRNG)
C
      NUMERR=0
      NUMWRN=0
      MAXLAR=0
      MAXIDS=0
C
C  CHECK LEVEL OPTION
      IF (LEVEL.EQ.2.OR.LEVEL.EQ.3) THEN
         ELSE
            WRITE (LP,140) LEVEL
            CALL SUWRNS (LP,2,NUMWRN)
            LEVEL=2
         ENDIF
C
      WRITE (LP,150)
      CALL SULINE (LP,2)
C
      IALL=0
      NUMOPT=0
C
      IF (NFLD.EQ.-1) THEN
         IALL=1
         GO TO 15
         ENDIF
C
      ISTRT=1
C
C  GET NEXT FIELD
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LSTRNG,
     *   STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) THEN
         IALL=1
         GO TO 15
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
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.GT.0) THEN
         IF (NUMOPT.GT.0) GO TO 105
         IALL=1
         GO TO 15
         ENDIF
C
13    NUMOPT=NUMOPT+1
      CTYPE=STRNG
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
15    NWORDS=3
C  Upped length to 60K to match sorder.f
C   for MARFC FFG/MAPX work - JTO 2006
C      LAPARM=17500
      LAPARM=60000
C  End of MARFC change
      MAXID=0
      MAXID1=LSWORK/NWORDS
      MAXID2=LARRAY-LAPARM
      IF (MAXID1.LE.MAXID2) MAXID=MAXID1
      IF (MAXID2.LE.MAXID1) MAXID=MAXID2
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MAXID=',MAXID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (LEVEL.EQ.3) THEN
         WRITE (LP,200)
         CALL SULINE (LP,3)
         ENDIF
C
C  CHECK NUMBER OF PARAMETER TYPES DEFINED
      IF (NMPTYP.GT.MPTYPES) THEN
         WRITE (LP,160) NMPTYP,MPTYPES
         CALL SUERRS (LP,2,NUMERR)
         GO TO 110
         ENDIF
C
C  GET LIST OF PARAMETER TYPES
      DO 20 I=1,NMPTYP
         CALL UMEMOV (IPDTDR(1,I),PTYPES(I),1)
20       CONTINUE
C
C  SORT LIST OF PARAMETER TYPES
      ISPTR=0
      CALL SUSORT (1,NMPTYP,PTYPES,PTYPES,ISPTR,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ ALL PARAMETER RECORDS FOR EACH TYPE
C
      DO 100 I=1,NMPTYP
         IF (PTYPES(I).EQ.'CHAR'.OR.PTYPES(I).EQ.'MMMT') GO TO 100
         IF (IALL.EQ.0.AND.PTYPES(I).NE.CTYPE) GO TO 100
C     CHECK IF SINGLE RECORD PARAMETER TYPE
         ISNGL=0
         IPOS=IPCKDT(PTYPES(I))
         IF (IPDTDR(6,IPOS).EQ.1) ISNGL=1
         NUMID=0
         IPTR=0
         IPTRO=0
         MAXWDS=0
         NUMWDS=0
C        -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
C     READ NEXT PARAMETER RECORD
30       PARMID=' '
         IPTR=-IPTR
         CALL RPPREC (PARMID,PTYPES(I),IPTR,LAPARM,IARRAY,NFILL,
     *      IPTRNX,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*)
     *         ' PARMID=',PARMID,
     *         ' IPTR=',IPTR,
     *         ' IPTRNX=',IPTRNX,
     *         ' IERR=',IERR,
     *         ' '
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IERR.NE.0) THEN
C        CHECK IF NO PARAMETER RECORDS FOUND
            IF (IERR.EQ.2.AND.NUMID.EQ.0) THEN
               IF (ISLEFT(5).GT.0) CALL SUPAGE
               IF (LEVEL.EQ.3.AND.ISNWPG(LP).EQ.1) THEN
                  WRITE (LP,200)
                  CALL SULINE (LP,4)
                  ENDIF
               WRITE (LP,250) PTYPES(I)
               CALL SULINE (LP,2)
               IF (ISLEFT(5).GT.0) CALL SUPAGE
               IF (LEVEL.EQ.3.AND.ISNWPG(LP).EQ.1) THEN
                  WRITE (LP,200)
                  CALL SULINE (LP,4)
                  ENDIF
               WRITE (LP,130)
               CALL SULINE (LP,2)
               GO TO 100
               ENDIF
C        CHECK IF LAST PARAMETER RECORD IS DELETED
            IF (IERR.EQ.6) GO TO 60
C        CHECK DELETED PARAMETER RECORD
            IF (IERR.EQ.9) THEN
               IF (LEVEL.EQ.2) GO TO 50
               IF (LEVEL.EQ.3) GO TO 40
               GO TO 50
               ENDIF
            CALL SRPPST (PARMID,PTYPES(I),IPTR,LAPARM,NFILL,IPTRNX,IERR)
            WRITE (LP,190) PARMID,PTYPES(I),IABS(IPTR)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 60
            ENDIF
C     PARAMETER RECORD FOUND
40       NUMID=NUMID+1
C     CHECK IF MAXIMUM NUMBER OF ENTRIES EXCEEDED
         IF (NUMID.GT.MAXID) THEN
            WRITE (LP,170) MAXID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
            ENDIF
C     CHECK IF DELETED PARAMETER RECORD
         IF (ISNGL.EQ.0.AND.PARMID.EQ.' ') PARMID='DELETED'
C     STORE IN WORK ARRAY
         CALL SUBSTR (PARMID,1,8,IWORK(1,NUMID),1)
         IWORK(3,NUMID)=IPTR
         IF (NFILL.GT.MAXWDS) MAXWDS=NFILL
         NUMWDS=NUMWDS+NFILL
         IF (MAXWDS.GT.MAXLAR) MAXLAR=MAXWDS
C     CHECK IF LAST PARAMETER RECORD
50       IF (IPTRNX.GT.0) THEN
            IPTRO=IPTR
            IPTR=IPTRNX
            IF (IPTR.LE.IPTRO) THEN
               WRITE (LP,270) IPTR,IPTRO,PTYPES(I)
               CALL SUERRS (LP,2,NUMERR)
               GO TO 60
               ENDIF
            GO TO 30
            ENDIF
C        -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
60       IF (NUMID.EQ.0) THEN
            WRITE (LP,275) PTYPES(I)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
            ENDIF
C     SORT LIST OF IDENTIFIERS
         IARRAY(LAPARM+1)=1
         CALL SUSORT (NWORDS,NUMID,IWORK,IWORK(1,NUMID+1),
     *      IARRAY(LAPARM+1),IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,180) PTYPES(I)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 100
            ENDIF
         IF (NUMID.GT.MAXIDS) MAXIDS=NUMID
         IF (LEVEL.EQ.2) THEN
C        PRINT PARAMETER RECORD IDENTIFIERS
            IF (ISLEFT(5).GT.0) CALL SUPAGE
            WRITE (LP,220) PTYPES(I)(1:LENSTR(PTYPES(I)))
            CALL SULINE (LP,2)
            IF (ISNGL.EQ.1) THEN
               WRITE (LP,230) PTYPES(I)
               CALL SULINE (LP,2)
               GO TO 90
               ENDIF
            NPER=10
            NTIME=NUMID/NPER
            IF (MOD(NUMID,NPER).GT.0) NTIME=NTIME+1
            NUM1=1
            NUM2=NUMID
            DO 70 N=1,NTIME
               WRITE (LP,240) N,
     *            ((IWORK(N1,IARRAY(LAPARM+N2)),N1=1,2),
     *            N2=NUM1,NUM2,NTIME)
               NUM1=NUM1+1
               CALL SULINE (LP,1)
               IF (ISNWPG(LP).EQ.1) THEN
                  WRITE (LP,220) PTYPES(I)
                  CALL SULINE (LP,3)
                  ENDIF
70             CONTINUE
            ENDIF
         IF (LEVEL.EQ.3) THEN
C        PRINT PARAMETER RECORD IDENTIFIER, RECORD LOCATION, POINTER TO
C        NEXT PARAMETER RECORD OF SAME TYPE AND NUMBER OF WORDS IN
C        PARAMETER RECORD
            DO 80 N=1,NUMID
               IF (ISLEFT(5).GT.0) CALL SUPAGE
               IF (ISNWPG(LP).EQ.1) THEN
                  WRITE (LP,200)
                  CALL SULINE (LP,4)
                  ENDIF
C           GET FIRST PARAMETER RECORD
               PARMID=' '
               IPTR=-IWORK(3,N)
               CALL RPPREC (PARMID,PTYPES(I),IPTR,LAPARM,IARRAY,NFILL,
     *            IPTRNX,IERR)
               IF (ISNGL.EQ.0.AND.PARMID.EQ.' ') PARMID='DELETED'
               CALL SUBSTR (PARMID,1,8,IBUF1,1)
               IBUF1(3)=IPTR
               IBUF1(4)=IPTRNX
               IBUF1(5)=NFILL
C           GET SECOND PARAMETER RECORD
               PARMID=' '
               IPTR=-IWORK(3,IARRAY(LAPARM+N))
               CALL RPPREC (PARMID,PTYPES(I),IPTR,LAPARM,IARRAY,NFILL,
     *            IPTRNX,IERR)
               IF (ISNGL.EQ.0.AND.PARMID.EQ.' ') PARMID='DELETED'
               CALL SUBSTR (PARMID,1,8,IBUF2,1)
               IBUF2(3)=IPTR
               IBUF2(4)=IPTRNX
               IBUF2(5)=NFILL
C           PRINT INFORMATION
               WRITE (LP,210) N,PTYPES(I),IBUF1,PTYPES(I),IBUF2
               CALL SULINE (LP,1)
80             CONTINUE
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,200)
               CALL SULINE (LP,4)
               ENDIF
            ENDIF
90       NAVWDS=NUMWDS/NUMID
         WRITE (LP,260) NUMID,PTYPES(I),MAXWDS,NAVWDS
         CALL SULINE (LP,2)
         IF (LEVEL.EQ.3.AND.ISNWPG(LP).EQ.1) THEN
            WRITE (LP,200)
            CALL SULINE (LP,4)
            ENDIF
         WRITE (LP,130)
         CALL SULINE (LP,2)
100      CONTINUE
C
      IF (IALL.EQ.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
105   WRITE (LP,280) MAXLAR
      CALL SULINE (LP,2)
      WRITE (LP,290) MAXIDS,MAXID
      CALL SULINE (LP,2)
C
110   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPPP2 - ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT ('0')
140   FORMAT ('0*** WARNING - INVALID LEVEL OPTION (',I3,
     *   ') SPECIFIED. LEVEL OPTION SET TO 2.')
150   FORMAT ('0- DUMP OF PARAMETER RECORD INFORMATION ',
     *   'FOR EACH PARAMETER TYPE -')
160   FORMAT ('0*** ERROR - IN SSPPP2 - NUMBER OF PARAMETER TYPES ',
     *   'DEFINED (',I3,') EXCEEDS MAXIMUM THAT CAN BE PROCESSED (',
     *   I3,').')
170   FORMAT ('0*** ERROR - IN SSPPP2 - MAXIMUM NUMBER OF PARAMETER ',
     *   'RECORDS THAT CAN BE PROCESSED (',I5,') EXCEEDED. NEXT ',
     *   'TYPE WILL BE TRIED.')
180   FORMAT ('0*** ERROR - IN SSPPP2 - ERROR ENCOUNTERED SORTING ',
     *    A,' PARAMETER RECORD INFORMATION.')
190   FORMAT ('0*** ERROR - IN SSPPP2 - PARAMETER RECORD FOR ',
     *   'IDENTIFIER ',A,' AND TYPE ',A,' ',
     *   'AT RECORD ',I6,' ',
     *   'NOT SUCCESSFULLY READ.')
200   FORMAT ('0',T23,'- NOT SORTED - ',
     *      T75,'- SORTED BY IDENTIFIER -'
     *   /
     *   T10,
     *  2('TYPE',3X,'IDENTIFIER',3X,'IPTR  ',3X,'IPTRNX',3X,'NFILL',15X)
     *   /
     *   T10,
     *  2('----',3X,'----------',3X,'------',3X,'------',3X,'-----',15X)
     *   )
210   FORMAT (2X,I4,3X,2(A,3X,2A4,2X,3X,I6,3X,I6,3X,I5,15X))
220   FORMAT ('0IDENTIFIERS FOR PARAMETER TYPE ',A,':')
230   FORMAT ('0*** NOTE - PARAMETER TYPE ',A,' ',
     *   'IS A SINGLE RECORD TYPE.')
240   FORMAT (3X,'(',I4,')',3X,10(2A4,3X))
250   FORMAT ('0*** NOTE - NO ',A,' PARAMETER RECORDS FOUND.')
260   FORMAT ('0*** NOTE - ',I4,' ',A,' PARAMETER RECORDS FOUND. ',
     *   'LONGEST PARAMETER ARRAY IS ',I5,' WORDS. ',
     *   'AVERAGE PARAMETER ARRAY IS ',I5,' WORDS.')
270   FORMAT ('0*** ERROR - RECORD NUMBER OF NEXT PARAMETER ARRAY (',I6,
     *   ') IS LESS THAN FOR THE LAST PARAMETER ARRAY (',I6,
     *   ') FOR PARAMETER TYPE ',A,'.')
275   FORMAT ('0*** ERROR - IN SSPPP2 - NO PARAMETER RECORDS FOUND ',
     *   'FOR PARAMETER TYPE ',A,'.')
280   FORMAT ('0*** NOTE - LONGEST PARAMETER ARRAY IS ',I5,' WORDS.')
290   FORMAT ('0*** NOTE - MAXIMUM NUMBER OF IDENTIFIERS OF ANY ',
     *   'PARAMETER TYPE PROCESSED IS ',I5,'. MAXIMUM THAT CAN BE ',
     *   'PROCESSED IS ',I5,'.')
C
      END
