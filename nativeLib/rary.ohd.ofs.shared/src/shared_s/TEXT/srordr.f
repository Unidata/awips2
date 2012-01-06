C MODULE SRORDR
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ GENERAL ORDER PARAMETERS.
C
      SUBROUTINE SRORDR (LARRAY,ARRAY,IVORDR,ICOMPL,
     *   NORDMO,NORDDA,NORDYR,NORDHM,
     *   NAMAP,NAFMAP,NAMAPX,ICBASN,ICFMAP,
     *   UNUSED,IPRERR,ISTAT)
C
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(2)
C
      CHARACTER*8 PARMID
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/srordr.f,v $
     . $',                                                             '
     .$Id: srordr.f,v 1.3 1999/07/07 11:17:23 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('ORDR')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'LARRAY=',LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      PARMID=' '
      CALL RPPREC (PARMID,'ORDR',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IPRERR.EQ.1) THEN
            CALL SRPPST (PARMID,'ORDR',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ENDIF
         GO TO 30
         ENDIF
C
      NPOS=0         
C
C  SET PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      IVORDR=ARRAY(NPOS)
C
C  SET COMPLETE FLAG
      NPOS=NPOS+1
      ICOMPL=ARRAY(NPOS)
C
C  SET CURRENT DATE
      NPOS=NPOS+1
      NORDMO=ARRAY(NPOS)
      NPOS=NPOS+1
      NORDDA=ARRAY(NPOS)
      NPOS=NPOS+1
      NORDYR=ARRAY(NPOS)
      NPOS=NPOS+1
      NORDHM=ARRAY(NPOS)
C
C  SET NUMBER OF AREAS DEFINED SINCE LAST ORDER RUN
      NPOS=NPOS+1
      NAMAP=ARRAY(NPOS)
      NPOS=NPOS+1
      NAFMAP=ARRAY(NPOS)
      IF (IVORDR.EQ.1) THEN
         NPOS=NPOS+1
         UNUSED(1)=ARRAY(NPOS)
         NPOS=NPOS+1
         UNUSED(2)=ARRAY(NPOS)
         ENDIF
      IF (IVORDR.GT.1) THEN
         NPOS=NPOS+1
         NAMAPX=ARRAY(NPOS)
         IF (IVORDR.EQ.2) THEN
            NPOS=NPOS+1
            UNUSED(1)=ARRAY(NPOS)
            UNUSED(2)=-999.
            ENDIF
         ENDIF
C
C  SET INDICATOR IF ANY BASINS USED BY MAPX AREA CHANGED 
C  SINCE ORDER LAST RUN
      IF (IVORDR.GT.2) THEN
         NPOS=NPOS+1
         ICBASN=ARRAY(NPOS)
         IF (IVORDR.EQ.3) THEN
            NPOS=NPOS+1
            UNUSED(1)=ARRAY(NPOS)
            NPOS=NPOS+1
            UNUSED(2)=ARRAY(NPOS)
            ENDIF
         ENDIF
C
C  SET INDICATOR IF ANY FMAP AREAS USED BY MAP AREAS CHANGED 
C  SINCE ORDER LAST RUN
      IF (IVORDR.GT.3) THEN
         NPOS=NPOS+1
         ICFMAP=ARRAY(NPOS)
         IF (IVORDR.EQ.4) THEN
            NPOS=NPOS+1
            UNUSED(1)=ARRAY(NPOS)
            NPOS=NPOS+1
            UNUSED(2)=ARRAY(NPOS)
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NPOS=',NPOS,
     *      ' NFILL=',NFILL,
     *      ' IPTRNX=',IPTRNX,
     *      ' IVORDR=',IVORDR,
     *      ' ICOMPL=',ICOMPL,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' NAMAP=',NAMAP,
     *      ' NAFMAP=',NAFMAP,
     *      ' NAMAPX=',NAMAPX,
     *      ' '
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('ORDR','REAL',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
30    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,90) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SRORDR')
90    FORMAT (' *** EXIT SRORDR : ISTAT=',I2)
C
      END
