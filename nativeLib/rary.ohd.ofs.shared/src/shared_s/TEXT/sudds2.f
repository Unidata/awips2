C MODULE SUDDS2
C-----------------------------------------------------------------------
C
      SUBROUTINE SUDDS2 (IUNIT,NUNIT,NOTFND,NALLOC,LDEBUG,ISTAT)
C
      CHARACTER*8 DDNAME
C
      DIMENSION NOTFND(*)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudds2.f,v $
     . $',                                                             '
     .$Id: sudds2.f,v 1.2 2001/06/13 13:31:06 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUDDS2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IPRERR=0
      IF (LDEBUG.GT.0) IPRERR=1
C
      DDNAME='FTXXF001'
C
      NUNIT=NUNIT+1
C
C  SET DDNAME
      CALL UFXDDN (DDNAME,IUNIT,IERR)
      IF (IERR.GT.0) THEN
         IF (NPSPAG.EQ.0) CALL SUPAGE
         WRITE (LP,30) IERR
         CALL SULINE (LP,1)
         ISTAT=1
         NOTFND(NUNIT)=IUNIT
         GO TO 10
         ENDIF
C
C  CHECK IF DDNAME IS ALLOCATED
      CALL UDDST (DDNAME,IPRERR,IERR)
      IF (LDEBUG.GT.2) THEN
         IF (NPSPAG.EQ.0) CALL SUPAGE
         WRITE (IOSDBG,40) DDNAME,IERR,IPRERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IERR.EQ.0) THEN
         NALLOC=NALLOC+1
         ELSE
            ISTAT=1
            NOTFND(NUNIT)=IUNIT
         ENDIF
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUDDS2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' UFXDDN STATUS CODE=',I3)
40    FORMAT (' DDNAME=',A,3X,'IERR=',I2,3X,'IPRERR=',I2)
C
      END
