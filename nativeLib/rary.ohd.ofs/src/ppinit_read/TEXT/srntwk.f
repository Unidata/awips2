C MEMBER SRNTWK
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C DESC READ NETWORK PARAMETER RECORD
C
      SUBROUTINE SRNTWK (LARRAY,ARRAY,IVNTWK,INWDTE,NNWFLG,INWFLG,
     *     UNUSED,IPRERR,ISTAT)
C
C
      DIMENSION INWDTE(1),INWFLG(1),ARRAY(1),UNUSED(1)
C
      REAL XNTWK/4HNTWK/
      REAL BLNKID(2)/2*4H    /
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srntwk.f,v $
     . $',                                                             '
     .$Id: srntwk.f,v 1.1 1995/09/17 19:14:55 dws Exp $
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
      LDEBUG=ISBUG(XNTWK)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,80) LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NUMERR=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,4HPPP ,IERR)
      IPTR=0
      CALL RPPREC (BLNKID,XNTWK,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *     IERR)
      IF (IERR.EQ.0) GO TO 10
         ISTAT=IERR
         IF (IPRERR.EQ.0) GO TO 50
            CALL SRPPST (BLNKID,XNTWK,IPTR,LARRAY,NFILL,IPTRNX,IERR)
            WRITE (LP,110) 'ERROR'
            CALL SUERRS (LP,2,NUMERR)
            GO TO 50
C
C  SET PARAMETER ARRAY VERSION NUMBER
10    IVNTWK=ARRAY(1)
C
      NPOS=1
C
C  SET CURRENT DATE
      DO 20 I=1,4
         NPOS=NPOS+1
20       INWDTE(I)=ARRAY(NPOS)
C
C  SET NUMBER OF INDICATORS
      NPOS=NPOS+1
      NNWFLG=ARRAY(NPOS)
C
C  SET NETWORK INDICATORS
      DO 30 I=1,NNWFLG
         NPOS=NPOS+1
30       INWFLG(I)=ARRAY(NPOS)
C
C  LAST TWO POSITIONS ARE UNUSED
      DO 40 I=1,2
         NPOS=NPOS+1
40       UNUSED(I)=ARRAY(NPOS)
C
      IF (LDEBUG.EQ.0) GO TO 50
         WRITE (IOSDBG,90) NPOS,NFILL,IPTRNX,IVNTWK
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP (XNTWK,4HREAL,0,NPOS,ARRAY,ARRAY)
C
50    IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (LDEBUG.GT.0.AND.ISTAT.GT.0) THEN
         WRITE (IOSDBG,110) 'NOTE'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
60    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,120)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SRNTWK')
80    FORMAT (' LARRAY=',I5)
90    FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVNTWK=',I3)
100   FORMAT ('0*** NOTE - NETWORK PARAMETERS SUCCESSFULLY ',
     *     'READ.')
110   FORMAT ('0*** ',A,' - NETWORK PARAMETERS NOT SUCCESSFULLY ',
     *     'READ.')
120   FORMAT (' *** EXIT SRNTWK')
C
      END
