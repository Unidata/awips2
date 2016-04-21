C MEMBER FCRDRC
C  (from old member FCFCRDRC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/02/95.08:37:42 BY $WC20SV
C
C @PROCESS LVL(77)
C
C
      SUBROUTINE FCRDRC (IRREC,REQRID,ISTAT)
C
C
C  ROUTINE TO READ RATING CURVE DEFINITION FROM FILE AND STORE IN
C  COMMON BLOCK FRATNG.
C
C  ARGUMENT LIST:
C       IRREC  - RECORD TO BE READ
C       REQRID - IDENTIFIER OF RATING CURVE REQUESTED
C       ISTAT  - STATUS CODE
C
C
      DIMENSION REQRID(2),OLDNAM(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fratng'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fcrdrc.f,v $
     . $',                                                             '
     .$Id: fcrdrc.f,v 1.1 1995/09/17 19:05:00 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
      CALL UMEMOV (OPNAME,OLDNAM,2)
      CALL UMEMOV ('FCRDRC  ',OPNAME,2)
      IOLDOP=IOPNUM
      IOPNUM=-1
C
      IBUG=0
      IF (IFBUG('RTCV').EQ.1) IBUG=1
C
      IF (ITRACE.GE.2) WRITE (IODBUG,10)
C
C  READ RECORD FROM FILE
      CALL UREADT (KFRTCV,IRREC,RTCVID(1),IERR)
C
C  CHECK IDENTIFIER
      IF (REQRID(1).EQ.RTCVID(1).AND.REQRID(2).EQ.RTCVID(2)) THEN
         ELSE
            WRITE (IPR,20) REQRID,RTCVID,IRREC
            ISTAT=1
            CALL ERROR
         ENDIF
C
      CALL UMEMOV (OLDNAM,OPNAME,2)
      IOPNUM=IOLDOP
C
      IF (ITRACE.GE.2) WRITE (IODBUG,30)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER FCRDRC')
20    FORMAT ('0**ERROR** IN FCRDRC - THE IDENTIFIER PASSED (',2A4,
     *   ') DOES NOT MATCH THE IDENTIFIER (',2A4,
     *   ') FOUND AT RECORD ',I4,'.')
30    FORMAT (' *** EXIT FCRDRC')
C
      END
