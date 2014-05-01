C MEMBER SUPPPN
C-----------------------------------------------------------------------
C
C  GET NUMBER OF PARAMETER RECORDS DEFINED FOR PARAMETER TYPE
C
      SUBROUTINE SUPPPN (ITYPE,NUMDEF,ISTAT)
C
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/supppn.f,v $
     . $',                                                             '
     .$Id: supppn.f,v 1.1 1995/09/17 19:15:51 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,1280)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSYS )
C
      ISTAT=0
      NUMDEF=-999
C
C  OPEN PARAMETRIC DATA BASE
      CALL SUDOPN (1,4HPPP ,IERR)
      IF (IERR.EQ.0) GO TO 5
         ISTAT=1
         GO TO 30
C
C  SEARCH DIRECTORY FOR TYPE
5     DO 10 I=1,NMPTYP
         IF (ITYPE.EQ.IPDTDR(1,I)) GO TO 20
10       CONTINUE
         ISTAT=1
         GO TO 30
C
C  SET NUMBER OF PARAMETER RECORDS FOR TYPE
20    NUMDEF=IPDTDR(5,I)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,90) ITYPE,NUMDEF
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
30    IF (ISTRCE.GT.0) WRITE (IOSDBG,1290)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
1280  FORMAT (' *** ENTER SUPPPN')
90    FORMAT (' ITYPE=',A4,3X,'NUMDEF=',I5)
1290  FORMAT (' *** EXIT SUPPPN')
C
      END
