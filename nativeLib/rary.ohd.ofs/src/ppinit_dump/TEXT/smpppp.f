C MODULE SMPPPP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT PREPROCESSOR DATA BASE POINTERS IN GENL PARAMETER
C  ARRAYS.
C
      SUBROUTINE SMPPPP (NARRAY,ARRAY,ISTAT)
C
C
      CHARACTER*4 RDISP
      INTEGER ARRAY(NARRAY)
C
      DIMENSION IBUF1(9),IBUF2(9)
      DIMENSION IWORK(3,1)
      DIMENSION UNUSED(5)
C
      INCLUDE 'scommon/dimstan'
C
      INCLUDE 'uio'
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smpppp.f,v $
     . $',                                                             '
     .$Id: smpppp.f,v 1.2 1998/04/07 17:57:24 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
      NUMERR=0
C
      WRITE (LP,110)
      CALL SULINE (LP,2)
C
      NWORDS=3
      LARRAY=200
      MAXID=0
      MAXID1=LSWORK/NWORDS
      MAXID2=NARRAY-LARRAY
      IF (MAXID1.LE.MAXID2) MAXID=MAXID1
      IF (MAXID2.LE.MAXID1) MAXID=MAXID2
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,120) MAXID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  READ ALL GENL PARAMETER RECORDS
      NUMID=0
      IPRERR=1
      RDISP='OLD'
      IPTR=0
10    CALL UREPET (' ',STAID,8)
      CALL RPPREC (STAID,'GENL',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL SULINE (IOSDBG,1)
         WRITE (LP,130) IERR,IPTR,IPTRNX
         ENDIF
      IF (IERR.EQ.0) GO TO 30
      IF (IERR.EQ.6) GO TO 50
      IF (IERR.EQ.2) GO TO 20
         CALL SRPPST (STAID,'GENL',IPTR,LARRAY,NFILL,IPTRNX,IERR)
         GO TO 80
C
C  NO PARAMETER RECORDS FOUND
20    WRITE (LP,150)
      CALL SULINE (LP,2)
      GO TO 80
C
C  PARAMETER RECORD FOUND
30    NUMID=NUMID+1
      IF (NUMID.LE.MAXID) GO TO 40
         WRITE (LP,160) MAXID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 80
40    CALL SUBSTR (STAID,1,8,IWORK(1,NUMID),1)
      IWORK(3,NUMID)=IPTR
C
C  CHECK IF LAST PARAMETER RECORD READ
      IF (IPTRNX.EQ.0) GO TO 50
         IPTR=IPTRNX
         GO TO 10
C
50    IF (LDEBUG.GT.0) THEN
         WRITE (LP,140) NUMID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SORT ARRAYS
      ARRAY(LARRAY+1)=1
      CALL SUSORT (NWORDS,NUMID,IWORK,IWORK(1,NUMID+1),
     *   ARRAY(LARRAY+1),IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (LP,170)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 80
C
C  PRINT ARRAYS
60    IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,180)
      CALL SULINE (LP,4)
      DO 70 N=1,NUMID
         CALL UREPET (' ',STAID,8)
         IPTR=IWORK(3,N)
         INCLUDE 'scommon/callsrstan'
         IF (LDEBUG.GT.0) THEN
            CALL SULINE (IOSDBG,1)
            WRITE (LP,130) IERR,IPTR,IPTRNX
            ENDIF
         CALL SUBSTR (STAID,1,8,IBUF1,1)
         IBUF1(3)=IPPP24
         IBUF1(4)=IPPPVR
         IBUF1(5)=IPCHAR
         IBUF1(6)=IPTM24
         IBUF1(7)=IPTAVR
         IBUF1(8)=IPTF24
         IBUF1(9)=IPEA24
         CALL UREPET (' ',STAID,8)
         IPTR=IWORK(3,ARRAY(LARRAY+N))
         INCLUDE 'scommon/callsrstan'
         IF (LDEBUG.GT.0) THEN
            CALL SULINE (IOSDBG,1)
            WRITE (LP,130) IERR,IPTR,IPTRNX
            ENDIF
         CALL SUBSTR (STAID,1,8,IBUF2,1)
         IBUF2(3)=IPPP24
         IBUF2(4)=IPPPVR
         IBUF2(5)=IPCHAR
         IBUF2(6)=IPTM24
         IBUF2(7)=IPTAVR
         IBUF2(8)=IPTF24
         IBUF2(9)=IPEA24
         WRITE (LP,190) N,IBUF1,IBUF2
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,180)
            CALL SULINE (LP,4)
            ENDIF
70       CONTINUE
C
80    WRITE (LP,200) NUMID,MAXID
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,210) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' *** ENTER SMPPPP')
110   FORMAT ('0- DUMP OF POINTERS IN GENL PARAMETER RECORDS -')
120   FORMAT (' MAXID=',I5)
130   FORMAT (' IERR=',I2,3X,'IPTR=',I5,3X,'IPTRNX=',I5)
140   FORMAT (' NUMID=',I4)
150   FORMAT ('0*** NOTE - NO GENL PARAMETER RECORDS FOUND.')
160   FORMAT ('0*** ERROR - IN SMPPPP - MAXIMUM NUMBER OF PARAMETER ',
     *   'RECORDS THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
170   FORMAT ('0*** ERROR - IN SMPPPP - ERROR ENCOUNTERED SORTING ',
     *    'GENL PARAMETER RECORD INFORMATION.')
180   FORMAT ('0',T25,'- NOT SORTED - ',
     *   T85,'- SORTED BY IDENTIFIER -'
     *   / T10,
     *   2('IDENTIFIER',1X,'PP24 ',1X,'PPVR ',1X,'PCHR ',1X,
     *     'TM24 ',1X,'TAVR ',1X,'TF24 ',1X,'EA24 ',10X)
     *   / T10,
     *   2('----------',7(1X,'-----'),10X))
190   FORMAT (2X,I4,3X,2(2A4,2X,7(1X,I5),10X))
200   FORMAT ('0*** NOTE - NUMBER OF GENL PARAMETER ARRAY ',
     *   'PROCESSED IS ',I5,'. MAXIMUM THAT CAN BE ',
     *   'PROCESSED IS ',I5,'.')
210   FORMAT (' *** EXIT SMPPPP - STATUS CODE=',I2)
C
      END
