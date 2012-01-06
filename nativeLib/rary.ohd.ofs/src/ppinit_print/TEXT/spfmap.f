C MEMBER SPFMAP
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/04/94.13:13:14 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC PRINT FUTURE MAP TIME SERIES HEADER
C
      SUBROUTINE SPFMAP (TSID,IPRERR,ISTAT)
C
      DIMENSION TSID(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spfmap.f,v $
     . $',                                                             '
     .$Id: spfmap.f,v 1.1 1995/09/17 19:14:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,10)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('FMAP')
C
      ISTAT=0
C
      CALL SPTSHD (TSID,'FMAP',IPRERR,IPTRNX,IERR)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,20)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SPFMAP')
20    FORMAT(' *** EXIT SPFMAP')
C
      END
