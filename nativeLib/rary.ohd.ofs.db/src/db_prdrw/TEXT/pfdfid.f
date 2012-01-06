C MODULE PFDFID
C-----------------------------------------------------------------------
C
      SUBROUTINE PFDFID (IFREC,IRTYPE,IFARR,ISTAT)
C
C  ROUTINE TO CHECK IF A TIME SERIES HAS A FUTURE TIME SERIES.
C
C  ARGUMENT LIST:
C
C     NAME     TYPE   I/O   DIM   DESCRIPTION
C     ------   ----   ---   ---   -----------
C     IFREC     I      I     1    FUTURE RECORD NUMBER
C     IRTYPE    A4     I     1    REGULAR DATA TYPE
C     IFARR     I      O     3    ARRAY FOR ID AND TYPE
C     ISTAT     I      O     1    STATUS CODE
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urunts'
C
      INTEGER IFARR(3),IBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pfdfid.f,v $
     . $',                                                             '
     .$Id: pfdfid.f,v 1.3 2001/06/13 10:57:00 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,40) IFREC,IRTYPE
C
      ISTAT=0      
C
      CALL UMEMOV ('  NONE  ',IFARR(1),2)
      IFARR(3)=IBLNK      
C
C  CHECK IF THERE IS A FUTURE TYPE
      CALL PFDTYP (IRTYPE,INDXR)
      INDXF=0
      IF (IAMORD.EQ.0) INDXF=DATFIL(7,INDXR)
      IF (IAMORD.EQ.1) INDXF=IDATFL(7,INDXR)
      IF (IPRDB.GT.0) WRITE (IOGDB,50) IAMORD,INDXF
      IF (INDXF.LE.0) GO TO 30
C
C  READ THE FUTURE RECORD
      IUNIT=0
      IF (IAMORD.EQ.0) IUNIT=DATFIL(2,INDXF)
      IF (IAMORD.EQ.1) IUNIT=IDATFL(2,INDXF)-KUPRDO
      IF (IPRDB.GT.0) WRITE (IOGDB,60) IUNIT
      CALL UREADT (IUNIT,IFREC,IBUF,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,90)
         ISTAT=1
         GO TO 30
         ENDIF
C
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,70) IBUF
         WRITE (IOGDB,80) IBUF
         ENDIF
C
C  MOVE IN ID AND TYPE
      CALL UMEMOV (IBUF(4),IFARR(1),2)
      IFARR(3)=IRTYPE
C
30    IF (IPRTR.GT.0) WRITE (IOGDB,100) IFARR
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' ENTER PFDFID - IFREC=',I6,3X,'IRTYPE=',A4)
50    FORMAT (' IAMORD=',I2,3X,'INDXF=',I2)
60    FORMAT (' IUNIT=',I3)
70    FORMAT (' IBUF IN 16(A4,1X)=',16(A4,1X))
80    FORMAT (' IBUF IN 16(I4,1X)=',16(I4,1X))
90    FORMAT ('0**ERROR** IN PFDFID - SYSTEM OR DAIO READ ERROR.')
100   FORMAT (' EXIT PFDFID - IFARR=',3(A4,1X))
C
      END
