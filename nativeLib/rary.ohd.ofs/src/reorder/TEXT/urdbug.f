C MODULE URDBUG
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DEBUGGING PROGRAM REORDER.
C
      SUBROUTINE URDBUG
C
      CHARACTER*8 STAID
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      DIMENSION DESCRP(5)
C
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'common/fcunit'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urdbug.f,v $
     . $',                                                             '
     .$Id: urdbug.f,v 1.2 1998/04/07 18:57:10 page Exp $
     . $' /
C    ===================================================================
C
C
C
      LDEBUG=0
      IF (LDEBUG.EQ.0) GO TO 30
C
      WRITE (IOGDB,10)
10    FORMAT (' IN ROUTINE URDBUG')
C
      WRITE (IOGDB,*)
     *   ' KFPARM=',KFPARM,
     *   ' '
      IF (LDEBUG.EQ.1) GO TO 30
C
      IAMORD=1
      IREC=0
      LSTREC=0
      IF (IAMORD.EQ.0) IREC=INFREC+1
      IF (IAMORD.EQ.1) IREC=ISIFRC+1
      IF (IAMORD.EQ.0) LSTREC=LSTSIF
      IF (IAMORD.EQ.1) LSTREC=LTSIFR
C
C  CHECK IF SIF RECORD NUMBER IS GREATER THAN LAST RECORD USED
20    IF (IREC.GT.LSTREC) GO TO 30
C
C  READ SIF RECORD
      CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,ISTAT)
C
C  CHECK IF THIS IS SPECIFIED SIF RECORD
      CALL UMEMOV (ISIBUF(2),STAID,2)
      IF (LDEBUG.GT.1) WRITE (IOGDB,*)
     *   ' IAMORD=',IAMORD,
     *   ' IREC=',IREC,
     *   ' LSTREC=',LSTREC,
     *   ' STAID=',STAID,
     *   ' '
      IF (STAID.EQ.'STPW3') THEN
         ISUMRY=0
         CALL SMPPD2 (1,STATE,DESCRP,ISUMRY,ISIBUF,IERR)
         ELSE
            IREC=NXREC
            GO TO 20
         ENDIF
C
30    RETURN
C
      END
