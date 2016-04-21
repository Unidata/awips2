C MODULE SUGTOR
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ GENERAL COMPUTATIONAL ORDER PARAMETERS.
C
      SUBROUTINE SUGTOR (LARRAY,ARRAY,IVORDR,ICOMPL,
     *   NORDMO,NORDDA,NORDYR,NORDHM,
     *   NAMAP,NAFMAP,NAMAPX,ICBASN,ICFMAP,
     *   UNUSED,ISTAT)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(10)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtor.f,v $
     . $',                                                             '
     .$Id: sugtor.f,v 1.3 1999/07/07 11:18:33 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
C  READ GENERAL COMPUTATIONAL ORDER PARAMETERS
      IPRERR=0
      INCLUDE 'scommon/callsrordr'
      IF (IERR.NE.0) THEN
C     PARAMETERS NOT FOUND
         NAMAP=0
         NAFMAP=0
         NAMAPX=0
         ICBASN=0
         GO TO 30
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
C     PRINT PARAMETERS
         IPRNT=1
         INCLUDE 'scommon/callspordr'
         ENDIF
C
30    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20) 'ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUGTOR')
20    FORMAT (' *** EXIT SUGTOR : ',A,I2)
C
      END
