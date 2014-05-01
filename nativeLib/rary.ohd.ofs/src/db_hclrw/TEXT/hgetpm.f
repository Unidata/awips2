C MODULE HGETPM
C-----------------------------------------------------------------------
C
      SUBROUTINE HGETPM (ISTAT)
C
C  ROUTINE TO READ RECORD 1 OF FILE USERPARM.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                     0=RECORD READ
C                                     OTHER=ERROR
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'uunits'
      INCLUDE 'hclcommon/hdflts'
C
      DIMENSION IDFLTS(60)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgetpm.f,v $
     . $',                                                             '
     .$Id: hgetpm.f,v 1.3 2001/06/13 12:06:58 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HGETPM'
C
      ISTAT=0
C
C  READ RECORD
      IREC=1
      CALL UREADT (KUPARM,IREC,IDFLTS,ISTAT)
C
      IF (IHCLDB.GT.0) WRITE (IOGDB,20) (IDFLTS(I),I=1,20)
20    FORMAT (' IDFLTS IN 20(A4,1X)=',20(A4,1X))
      IF (IHCLDB.GT.0) WRITE (IOGDB,30) (IDFLTS(I),I=1,20)
30    FORMAT (' IDFLTS IN 20(I4,1X)=',20(I4,1X))
C
C  MOVE INTO COMMON BLOCK
      CALL UMEMOV (IDFLTS(1),TIME(1),25)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HGETPM'
C
      RETURN
C
      END
