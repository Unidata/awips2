C MODULE SUSEQN
C-----------------------------------------------------------------------
C
C  ROUTINE TO STORE AND CHECK CARD SEQUENCE NUMBER.
C
      SUBROUTINE SUSEQN (STRING,NRDCRD,SEQNUM)
C
      CHARACTER*1 STRING(1)
      CHARACTER*8 SEQNUM
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suseqn.f,v $
     . $',                                                             '
     .$Id: suseqn.f,v 1.2 1998/07/06 12:46:06 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      LSEQNUM=LEN(SEQNUM)
C
C  CHECK IF SEQUENCE NUMBER FIELD IS BLANK
      DO 10 I=1,LSEQNUM
         IF (STRING(I).NE.' ') GO TO 20
10       CONTINUE
C
C  SEQUENCE NUMBER FIELD IS BLANK - SET TO CARD NUMBER
      CALL UINTCH (NRDCRD,LSEQNUM,SEQNUM,NFILL,IERR)
      GO TO 60
C
C  SEQUENCE NUMBER FIELD NOT BLANK - CHECK IF VALID SEQUENCE NUMBER
20    DO 30 I=1,LSEQNUM
         CALL SUBSTR (STRING(I),1,1,SEQNUM,I)
30       CONTINUE
      CALL UINDXC (SEQNUM,LSEQNUM,'0123456789',10,'ONLY',IRETRN)
      IF (IRETRN.EQ.0) THEN
         CALL SUPCRD
         WRITE (LP,90) SEQNUM
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
60    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SUSEQN')
90    FORMAT ('0*** WARNING - INVALID CARD SEQUENCE NUMBER : ',A)
100   FORMAT (' *** EXIT SUSEQN')
C
      END
