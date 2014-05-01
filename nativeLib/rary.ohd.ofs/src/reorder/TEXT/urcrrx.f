C MODULE URCRRX
C-----------------------------------------------------------------------
C
      SUBROUTINE URCRRX (LRRBUF,IRRBUF,IDSTA,ISTAT)
C
C   THIS ROUTINE WILL REORDER RRS PARAMETER AND DATA RECORDS
C   FOR THE STATIONS THAT ARE NOT IN THE ORRS PARAMETER RECORD.
C
C  ARGUMENT LIST:
C
C     NAME     TYPE    I/O   DIM     DESCRIPTION
C     ------   ----    ---   ------  -----------
C     LRRBUF    I*4     I      1     LENGTH OF IRRBUF
C     IRRBUF    I*4    I/O   LRRBUF  RRS DATA RECORD
C     IDSTA     I*4     I      1     STATION IDENTIFIER
C     ISTAT     I*4     I      1     STATUS CODE:
C                                       0=NO ERRORS
C                                       1=ERRORS ENCOUNTERED
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdi2max'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urhshi'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urppdt'
C
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      DIMENSION IRRBUF(LRRBUF)
      DIMENSION IDSTA(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urcrrx.f,v $
     . $',                                                             '
     .$Id: urcrrx.f,v 1.3 2002/10/10 15:52:31 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,150)
         ENDIF
C
      ISTAT=0
C
      LIDX=LRLURI*2
      NFREC=LFREE1
C
C  GET STATION INFORMATION RECORD USING STATION NAME
      IAMORD=1
      CALL PDFNDR (IDSTA,LSIBUF,IFINDC,ISCREC,ISIBUF,IFREE,ISTAT)
C
C  CHECK IF SIF RECORD FOUND
      IF (IFINDC.GT.0) GO TO 10
         ISTAT=1
         GO TO 140
10    IF (ISTAT.NE.0) GO TO 110
C
C  GET STATION INFORMATION RECORD USING STATION NUMBER
      NUMID=ISIBUF(6)
      IF (NUMID.EQ.0) GO TO 30
      CALL PDFNDI (NUMID,LSIBUF,IFINDI,ISIREC,ISIBUF,IFREE,ISTAT)
C
C  CHECK IF SIF RECORD FOUND
      IF (IFINDI.GT.0) GO TO 20
         WRITE (LP,160)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 130
20    IF (ISTAT.NE.0) GO TO 110
C
C  CHECK IF SIF RECORD NUMBERS THE SAME
      IF (ISCREC.EQ.ISIREC) GO TO 30
         WRITE (LP,170)
         CALL SUERRS (LP,2,-1)
         GO TO 130
C
C  CHECK NUMBER OF ADDITIONAL DATA TYPES
30    NTYPES=ISIBUF(10)
      IF (NTYPES.GT.0) GO TO 40
         WRITE (LP,180) IDSTA
         CALL SUERRS (LP,2,-1)
         GO TO 130
C
C  PROCESS EACH ADDITIONAL TYPE
40    IPOS=11
      DO 100 I=1,NTYPES
C     CHECK IF THIS IS AN RRS TYPE
         CALL UMEMOV (ISIBUF(IPOS),IRTYPE,1)
         IRX=IPDCKR(IRTYPE)
         IF (IRX.EQ.0) GO TO 90
         IRSREC=ISIBUF(IPOS+2)
         IF (IPDDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,190) IRSREC
            ENDIF
C     READ RRS RECORD FOR THIS TYPE
         CALL PDRRRR (IRSREC,LRCPDR,LRRBUF,IRRBUF,ISTAT)
         IF (ISTAT.EQ.2) THEN
            WRITE (LP,200)
            CALL SUERRS (LP,2,-1)
            GO TO 130
            ENDIF
         IF (ISTAT.NE.0) GO TO 110
C     CHECK FOR FREEPOOL RECORDS
         IF (IRRBUF(13).EQ.0) GO TO 70
         IREC=IRRBUF(13)
C     GET FREEPOOL RECORDS AND COPY THEM TO THE NEW FILE
         IF (IPDDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,210) IREC,NFREC
            ENDIF
         CALL URFPRD (IDSTA,IREC,NFREC,NFREC2,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,220) IDSTA
            CALL SUERRS (LP,2,-1)
            GO TO 130
            ENDIF
         IRRBUF(13)=NFREC2
C     WRITE RRS DATA TO NEW FILE
70       IREC=LRRSXR+1
         NRECS=IUNRCD(IRRBUF(1),LRLURS)
         NR=NRECS*LRLURS
         IF (NR.GT.MXSIZE) MXSIZE=NR
         CALL WVLRCD (KURRRS,IREC,NRECS,IRRBUF,LRLURS,ISTAT)
         IF (ISTAT.NE.0) GO TO 110
         IF (IPDDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,230) IREC,NRECS
            ENDIF
CSCV  9/2002 IF IREC IS GREATER THEN I2MAX THEN CONVERT IREC TO A 
C     NEGATIVE NUMBER TO STORE IN THE ISIBUF ARRAY (AN I2 ARRAY). 
C     IF IREC IS GREATER THEN 2*I2MAX THEN YOU HAVE USED ALL POSITIVE 
C     AND NEGATIVE NUMBERS IN THE ISIBUF ARRAY AND CAN NOT ADD ANY 
C     MORE RRS RECORDS. 
         IREC2=IREC
	 IF (IREC2.GT.2*I2MAX) THEN
C        INTEGER*4 VALUE CAN NOT BE STORED AS INTEGER*2
            WRITE (LP,225) STAID
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 90
            ENDIF
         IF (IREC2.GT.I2MAX) IREC2=IREC2-2*I2MAX
C     UPDATE RRS RECORD NUMBER IN SIF RECORD
         ISIBUF(JPOS+2)=IREC2
C     SET NUMBER OF LAST USED RRS RECORD
         LRRSXR=LRRSXR+NRECS
         IF (IPDDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,240) LRRSXR
            ENDIF
90       IPOS=IPOS+3
100      CONTINUE
C
C  WRITE SIF RECORD TO NEW FILE
      NWDS=ISIBUF(1)
      NRECS=IUNRCD(NWDS,LIDX)
      CALL WVLRCD (KURSIF,ISCREC,NRECS,ISIBUF,LRLURI,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
C
      IF (IPDDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,250) ISCREC
         ENDIF
C         
      GO TO 140
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   WRITE (LP,120)
120   FORMAT ('0*** ERROR - IN URCRRX - READ OR WRITE ERROR.')
      CALL SUERRS (LP,2,-1)
      GO TO 130
C
C  SET ERROR FLAG
130   IWURFL=1
C
140   IF (IPDTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,260)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (' ENTER URCRRX')
160   FORMAT ('0*** ERROR - IN URCRRX - NO SIF RECORD FOUND FOR ',
     *   'STATION NUMBER ',I7,'.')
170   FORMAT ('0*** ERROR - IN URCRRX - SYSTEM ERROR READING SIF ',
     *   'FOR STATION ',2A4,'.')
180   FORMAT ('0*** ERROR - IN URCRRX - NO DATA TYPES FOUND IN SIF  ',
     *   'RECORD FOR STATION ',2A4,'.')
190   FORMAT (' IRSREC=',I6)
200   FORMAT ('0*** ERROR - IN URCRRX - WORK BUFFER TOO SMALL TO ',
     *   'HOLD RRS DATA.')
210   FORMAT (' IREC=',I6,3X,'NFREC=',I6)
220   FORMAT ('0*** ERROR - IN URCRRX - ACCESSING FREEPOOL RECORD ',
     *    'FOR STATION ',2A4,'.')
225   FORMAT ('0*** ERROR - IN URCRRS - INVALID INTEGER*2 VALUE ',
     *   'ENCOUNTERED WHEN TRYING TO UPDATE SIF RECORD ',
     *   'FOR STATION ',A,'.')
230   FORMAT (' IREC=',I4,3X,'NRECS=',I4)
240   FORMAT (' LRRSXR=',I6)
250   FORMAT (' ISCREC=',I5)
260   FORMAT (' EXIT URCRRX')
C
      END
