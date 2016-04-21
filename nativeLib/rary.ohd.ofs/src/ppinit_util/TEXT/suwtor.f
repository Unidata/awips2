C MODULE SUWTOR
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE GENERAL COMPUTATIONAL ORDER PARAMETERS.
C
      SUBROUTINE SUWTOR (LARRAY,ARRAY,IVORDR,ICOMPL,
     *   NORDMO,NORDDA,NORDYR,NORDHM,
     *   NAMAP,NAFMAP,NAMAPX,ICBASN,ICFMAP,
     *   ISTAT)
C
      CHARACTER*4 WDISP
      CHARACTER*8 PARMID
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suwtor.f,v $
     . $',                                                             '
     .$Id: suwtor.f,v 1.2 1999/07/07 11:19:06 page Exp $
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
      ICOMPL=0
      WDISP='OLD'
C      
C  CHECK IF ORDR PARAMETERS EXIST
      IPTR=0
      CALL SUDOPN (1,'PPP ',IERR)
      PARMID=' '
      CALL RPPREC (PARMID,'ORDR',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.EQ.2) THEN
         ICOMPL=1
         WDISP='NEW'
         ENDIF
C
C  WRITE PARAMETERS
      IVORDR=4
      ICOMPL=1
      UNSD=-999.
      INCLUDE 'scommon/callswordr'
      IF (IERR.NE.0) THEN
         ISTAT=1
         ELSE
            IF (LDEBUG.GT.0) THEN
C           PRINT PARAMETERS
               IPRNT=1
               INCLUDE 'scommon/callspordr'
               ENDIF
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20) 'ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUWTOR')
20    FORMAT (' *** EXIT SUWTOR : ',A,I2,A,A)
C
      END
