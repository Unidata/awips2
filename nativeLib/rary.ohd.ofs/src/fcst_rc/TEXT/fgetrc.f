C MODULE FGETRC
C----------------------------------------------------------------------
C
C  ROUTINE TO GET A RATING CURVE DEFINTION
C
      SUBROUTINE FGETRC (GRCID,IERR)
C
C  SUBROUTINE FGETRC RETRIEVES A RATING CURVE FROM FILE FOR BOTH THE
C  OPERATIONAL PROGRAM AND AN MCP RUN. FOR THE OPER PROGRAM, THE
C  RATING CURVE HAS ONLY ONE DEFINITION AND IS FOUND ON A PERMANENT
C  FILE. FOR AN MCP RUN, A RATING CURVE CAN HAVE MULTIPLE DEFINITIONS
C  OVER TIME. THESE DEFINITIONS ARE STORED ON A TEMPORARY FILE AS
C  DEFINED WITH INPUT TO EACH MCP RUN.
C
C  ARGUMENT LIST:
C        GRCID - RATING CURVE IDENTIFIER TO BE GOTTEN
C        IERR  - ERROR RETURN CODE:
C                0 = NO ERROR
C                1 = ERROR
C
C....................................................................
C
C  ROUTINE ORIGINALLY WRITTEN BY - JOE OSTROWSKI - HRL - 8/1980
C
      CHARACTER*8 OLDOPN
C
      DIMENSION GRCID(2),ZRCBUF(108)
      EQUIVALENCE (RTCVID(1),ZRCBUF(1))
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fprog'
      INCLUDE 'common/frcfil'
      INCLUDE 'common/fratng'
C jgg added to fix MR 1653 - 5/31/02
      INCLUDE 'common/rcnew'
C jgg      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fgetrc.f,v $
     . $',                                                             '
     .$Id: fgetrc.f,v 1.4 2002/10/10 13:32:23 xfan Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('FGETRC  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FGETRC'
C
      IBUG=IFBUG('RTCV')

      IERR=0
C
C  CHECK IF RATING CURVE IS ALREADY IN /FRATNG/
      IF (GRCID(1).EQ.RTCVID(1).AND.GRCID(2).EQ.RTCVID(2)) GO TO 70
C
C  CHECK IF PROGRAM IS MCP
      IF (MAINUM.EQ.3) GO TO 40
C
      CALL FINDRC (GRCID,IRCREC,IRETRN)
      IF (IRETRN.EQ.1) GO TO 30
         WRITE (IPR,20) GRCID
20    FORMAT('0**ERROR** RATING CURVE ',2A4,' NOT FOUND.')
         IERR = 1
         CALL ERROR
         GO TO 70
C
C  FILL /FRATNG/ WITH DEFINITION
C jgg First clear the rcnew mod point count, because we're
C jgg  reading in a different RC - fixes HSD bug 20-32 MR 1653 - 5/31/02
 
C jgg 30    CALL FCRDRC (IRCREC,GRCID,KERROR)
30    NRCPMD = 0
      CALL FCRDRC (IRCREC,GRCID,KERROR)
      IF (KERROR.EQ.0)GO TO 70
         CALL ERROR
         IERR = 1
         GO TO 70
C
C  THIS SECTION IS FOR USE IN AN MCP RUN.
C  FIND THE RATING CURVE IN THE LIST OF DEFINED CURVES.
C  LOCK(I) IS THE LOCATION OF THE CURRENT RATING CURVE DEFINITION
C  BEING USED.
40    DO 50 I=1,NDEF
      IF (GRCID(1).NE.RCNAME(1,I).OR.GRCID(2).NE.RCNAME(2,I)) GO TO 50
         ICRNT = LOCK(I)
         GO TO 60
50       CONTINUE
      WRITE (IPR,20) GRCID
      CALL ERROR
      IERR = 1
      GO TO 70
C
C  READ CURVE FROM SCRATCH FILE
60    READ (IRC,REC=ICRNT) ZRCBUF
C
70    IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT FGETRC'
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
