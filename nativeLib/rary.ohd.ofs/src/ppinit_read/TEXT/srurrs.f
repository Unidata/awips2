C MODULE SRURRS
C-----------------------------------------------------------------------
C
C  ROUTINE READ GENERAL USER RRS PARAMETERS
C
      SUBROUTINE SRURRS (LARRAY,ARRAY,IVURRS,
     *   MTYPE,TYPE,NTYPE,MNDAY,NMOBS,
     *   UNUSED,IPRERR,ISTAT)
C
C
      CHARACTER*8 BLNKID/' '/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION TYPE(MTYPE),MNDAY(MTYPE),NMOBS(MTYPE)
      DIMENSION UNUSED(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srurrs.f,v $
     . $',                                                             '
     .$Id: srurrs.f,v 1.2 1998/04/07 18:08:24 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('URRS')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,90) LARRAY,MTYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL RPPREC (BLNKID,'URRS',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *     IERR)
      IF (IERR.NE.0) THEN
         ISTAT=IERR
         IF (IPRERR.GT.0) THEN
            CALL SRPPST (BLNKID,'URRS',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            WRITE (LP,100) IERR
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 70
         ENDIF
C
      NPOS=0         
C
C  SET PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      IVURRS=ARRAY(NPOS)
C
C  POSITIONS 2 AND 3 ARE UNUSED
      NPOS=NPOS+1
      UNUSED(1)=ARRAY(NPOS)
      NPOS=NPOS+1
      UNUSED(2)=ARRAY(NPOS)
C
C  SET NUMBER OF RRS DATA TYPES
      NPOS=NPOS+1
      NTYPE=ARRAY(NPOS)
C
C  CHECK FOR SUFFICIENT SPACE FOR DATA TYPE CODES
      IF (NTYPE.GT.MTYPE) THEN
         WRITE (LP,120) NTYPE,MTYPE
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=2
         GO TO 70
         ENDIF
C
C  SET RRS DATA TYPE CODES
      DO 30 I=1,NTYPE
         NPOS=NPOS+1
         TYPE(I)=ARRAY(NPOS)
30       CONTINUE 
C
C  SET MINIMUM NUMBER OF DAYS TO BE RETAINED ON PPDB
      DO 40 I=1,NTYPE
         NPOS=NPOS+1
         MNDAY(I)=ARRAY(NPOS)
40       CONTINUE 
C
C  SET TYPICAL NUMBER OF OBSERVATIONS STORED ON PPDB FOR TYPE
      DO 50 I=1,NTYPE
         NPOS=NPOS+1
         NMOBS(I)=ARRAY(NPOS)
50       CONTINUE 
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,110) NPOS,NFILL,IPTRNX,IVURRS
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('URRS','REAL',0,NPOS,ARRAY,ARRAY)
         IF (ISTAT.EQ.0) THEN
            WRITE (IOSDBG,140)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISTAT.GT.0) THEN
            WRITE (IOSDBG,150)
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
70    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SRURRS')
90    FORMAT (' LARRAY=',I6,3X,'MTYPE=',I3)
100   FORMAT ('0*** ERROR - IN SRURRS - UNSUCCESSFUL CALL TO ',
     *   'RPPREC : STATUS CODE=',I2)
110   FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVURRS=',I3)
120   FORMAT ('0*** ERROR - IN SRURRS - ',I3,' RRS DATA TYPES ',
     *   'DEFINED. MAXIMUM NUMBER ALLOWED IN WORK SPACE IS ',I3,'.')
140   FORMAT ('0*** NOTE - URRS PARAMETERS SUCCESSFULLY READ.')
150   FORMAT ('0*** NOTE - GENERAL USER RRS PARAMETERS NOT ',
     *     'SUCCESSFULLY READ.')
160   FORMAT (' *** EXIT SRURRS')
C
      END
