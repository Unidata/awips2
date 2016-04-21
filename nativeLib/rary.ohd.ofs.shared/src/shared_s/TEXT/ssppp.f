C MODULE SSPPP
C-----------------------------------------------------------------------
C
      SUBROUTINE SSPPP (LARRAY,ARRAY,LEVEL,NFLD,IFLCHK,ISTAT)
C
C  ROUTINE TO PRINT STATUS OF PREPROCESSOR PARAMETRIC DATA BASE.
C
      CHARACTER*3 XSNGL
      INTEGER ICHAR/4HCHAR/,IMMMT/4HMMMT/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IBUF(100)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssppp.f,v $
     . $',                                                             '
     .$Id: ssppp.f,v 1.5 2001/06/13 14:06:03 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPPP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
C  PRINT HEADER
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,90)
      CALL SULINE (LP,2)
      WRITE (LP,110)
      CALL SULINE (LP,0)
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,110)
         CALL SULINE (LP,0)
         WRITE (LP,110)
         CALL SULINE (LP,0)
         ENDIF
C
C  OPEN PARAMETRIC DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) GO TO 70
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT INDEX FILE STATUS
C
      IF (LEVEL.GT.0) THEN
         WRITE (LP,120) USERPP
         CALL SULINE (LP,2)
         MAXIDX=MXPXRC-IPXRC1+1
         IOVFL=MAXIDX*.87225+IPXRC1
         WRITE (LP,130) MXPXRC,MAXIDX,IPXRC1,IOVFL
         CALL SULINE (LP,2)
         NDEF=0
         NDEL=0
         LSTREC=IOVFL-1
         DO 10 IREC=IPXRC1,LSTREC
            CALL UREADT (KPPIDX,IREC,IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,140) IREC,KPPIDX
               CALL SUERRS (LP,2,-1)
               GO TO 10
               ENDIF
            IF (IBUF(1).EQ.0) GO TO 10
            IF (IBUF(1).EQ.-1) NDEL=NDEL+1
            IF (IBUF(1).NE.-1) NDEF=NDEF+1
10          CONTINUE
         MREC=LSTREC-IPXRC1+1
         WRITE (LP,150) MREC,NDEF,NDEL
         CALL SULINE (LP,2)
         NDEF=0
         NDEL=0
         DO 20 IREC=IOVFL,MXPXRC
            CALL UREADT (KPPIDX,IREC,IBUF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,140) IREC,KPPIDX
               CALL SUERRS (LP,2,-1)
               GO TO 20
               ENDIF
            IF (IBUF(1).EQ.0) GO TO 20
            IF (IBUF(1).EQ.-1) NDEL=NDEL+1
            IF (IBUF(1).NE.-1) NDEF=NDEF+1
20          CONTINUE
         MREC=MXPXRC-IOVFL+1
         WRITE (LP,160) MREC,NDEF,NDEL
         CALL SULINE (LP,2)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT FILE CONTROL STATUS
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,220)
      CALL SULINE (LP,2)
      WRITE (LP,230)
      CALL SULINE (LP,4)
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      DO 30 I=1,NMPFIL
         MAXREC=IPMCTL(1,I)
         LSTREC=IPMCTL(2,I)
         NFREE=MAXREC-LSTREC
         IPCT=(FLOAT(LSTREC)/FLOAT(MAXREC))*100.+.5
         WRITE (LP,240) I,
     *      MAXREC,
     *      LSTREC,
     *      NFREE,
     *      IPCT,
     *      IPMCTL(3,I),
     *      KPPRMU(I),
     *      'PPPPARM',I
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,230)
            CALL SULINE (LP,4)
            ENDIF
         IF (IFLCHK.NE.0)
     *      CALL SSCHK (KPPRMU(I),IPMCTL(1,I),IPCT,IFLCHK,'PPP ',IERR)
30       CONTINUE
C
C  PRINT PARAMETER TYPE DIRECTORY RECORDS
C
      IF (LEVEL.GT.0) THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,170)
         CALL SULINE (LP,2)
         WRITE (LP,180) MXPTYP,NMPTYP
         CALL SULINE (LP,2)
         WRITE (LP,190)
         CALL SULINE (LP,3)
         ENDIF
      ISPECL=0
      NCOUNT=0
      MCOUNT=5
      DO 50 I=1,NMPTYP
         IF (LEVEL.GT.0) THEN
            IF (ISNWPG(LP).EQ.1) GO TO 40
            IF (NCOUNT.LT.MCOUNT) GO TO 40
               IF (ISLEFT(2).EQ.1) THEN
                  CALL SUPAGE
                  NCOUNT=0
                  GO TO 40
                  ENDIF
               WRITE (LP,100)
               CALL SULINE (LP,1)
               NCOUNT=0
40          IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,190)
               CALL SULINE (LP,3)
               NCOUNT=0
               ENDIF
C        CHECK IF SINGLE RECORD PARAMETER TYPE
            XSNGL='NO'
            IF (IPDTDR(6,I).EQ.1) XSNGL='YES'
            WRITE (LP,210) I,(IPDTDR(J,I),J=1,5),XSNGL
            CALL SULINE (LP,1)
            NCOUNT=NCOUNT+1
            ENDIF
C     CHECK IF SPECIAL PARAMETER TYPE
         IF (IPDTDR(1,I).EQ.ICHAR.OR.IPDTDR(1,I).EQ.IMMMT) ISPECL=1
50       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS SPECIAL PARAMETER TYPES
C
      IF (ISPECL.EQ.1) THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,250)
         CALL SULINE (LP,2)
         NSPECL=0
         DO 60 I=1,NMPTYP
            IF (IPDTDR(6,I).EQ.1) THEN
               IF (IPDTDR(1,I).EQ.ICHAR.OR.IPDTDR(1,I).EQ.IMMMT) THEN
                  IREC=IPDTDR(3,I)
                  IF (IREC.GT.0) THEN
                     IUNIT=KPPRMU(IPDTDR(2,I))
                     CALL UREADT (IUNIT,IREC,IBUF,IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (LP,140) IREC,IUNIT
                        CALL SUERRS (LP,2,-1)
                        GO TO 60
                        ENDIF
                     NSTA=IBUF(7)
                     MSTA=IBUF(8)
                     NDEL=IBUF(9)-IBUF(7)                    
                     NSPECL=NSPECL+1
                     IPCT=(FLOAT(NSTA)/FLOAT(MSTA))*100.+.5
                     WRITE (LP,260) IBUF(5),MSTA,NSTA,NDEL,IPCT
                     CALL SULINE (LP,2)
                     MINPCT=95
                     IF (IPCT.GT.MINPCT) THEN
                        WRITE (LP,270) IPCT,MINPCT,IBUF(5)
                        CALL SUWRNS (LP,2,-1)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
60          CONTINUE
         IF (NSPECL.EQ.0) THEN
            WRITE (LP,280)
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (LEVEL.GT.1) THEN
        CALL SSPPP2 (LARRAY,ARRAY,LEVEL,NFLD,IERR)
        ENDIF
C
70    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPPP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT ('0')
100   FORMAT (' ')
110   FORMAT ('+*--> PREPROCESSOR PARAMETRIC DATA BASE STATUS')
120   FORMAT ('0- INDEX FILE STATUS -' /
     *   '0',2X,'USER NAME = ',2A4)
130   FORMAT ('0',2X,'RECORDS IN FILE = ',I6,4X,
     *   'MAXIMUM INDEX RECORDS = ',I6,4X,
     *   'FIRST INDEX RECORD = ',I6,4X,
     *   'FIRST INDEX OVERFLOW RECORD = ',I6)
140   FORMAT ('0*** ERROR - IN SSPPP - DAIO ERROR AT RECORD ',I6,
     *    ' OF UNIT ',I2,'.')
150   FORMAT ('0',2X,'PRIMARY  AREA:',3X,
     *   'MAXIMUM ENTRIES = ',I6,4X,
     *   'ENTRIES DEFINED = ',I6,4X,
     *   'ENTRIES DELETED = ',I6)
160   FORMAT ('0',2X,'OVERFLOW AREA:',3X,
     *   'MAXIMUM ENTRIES = ',I6,4X,
     *   'ENTRIES DEFINED = ',I6,4X,
     *   'ENTRIES DELETED = ',I6)
170   FORMAT ('0- PARAMETER TYPE DIRECTORY STATUS -')
180   FORMAT ('0',2X,'MAXIMUM PARAMETER TYPES = ',I3,5X,
     *   'PARAMETER TYPES DEFINED = ',I3)
190   FORMAT (
     *   '0',2(' '),1X,
     *       4(' '),3X,
     *       4(' '),3X,
     *       'FIRST ',3X,
     *       'LAST  ',3X,
     *       'NUMBER OF',3X,
     *       'SINGLE' /
     *   ' ',2(' '),1X,
     *      'TYPE',3X,
     *      'FILE',3X,
     *      'RECORD',3X,
     *      'RECORD',3X,
     *      'RECORDS  ',3X,
     *      'RECORD TYPE' /
     *   ' ',2(' '),1X,
     *      4('-'),3X,
     *      4('-'),3X,
     *      6('-'),3X,
     *      6('-'),3X,
     *      9('-'),3X,
     *      11('-'))
210   FORMAT (
     *   ' ',
     *   I2,1X,
     *   A4,3X,
     *   I4,3X,
     *   I6,3X,
     *   I6,3X,
     *   I9,3X,
     *   A)
220   FORMAT ('0- DATA FILE STATUS -')
230   FORMAT (
     *   '0',2X,
     *       4(' '),3X,
     *       'MAXIMUM',3X,
     *       'LAST USED',3X,
     *       'UNUSED ',3X,
     *       'PERCENT',3X,
     *       'PARAMETER',3X,
     *       'UNIT  ',3X,
     *       'FILE    ',3X /
     *   ' ',2X,
     *       'FILE',3X,
     *       'RECORDS',3X,
     *       'RECORD   ',3X,
     *       'RECORDS',3X,
     *       'USED   ',3X,
     *       'RECORDS  ',3X,
     *       'NUMBER',3X,
     *       'NAME    ' /
     *   ' ',2X,
     *       4('-'),3X,
     *       7('-'),3X,
     *       9('-'),3X,
     *       7('-'),3X,
     *       7('-'),3X,
     *       9('-'),3X,
     *       6('-'),3X,
     *       8('-'))
240   FORMAT (' ',2X,
     *   I4,3X,
     *   I7,3X,
     *   I9,3X,
     *   I7,3X,
     *   I7,3X,
     *   I9,3X,
     *   I6,3X,
     *   A,I1)
250   FORMAT ('0- SPECIAL PARAMETER TYPE STATUS -')
260   FORMAT ('0',2X,'TYPE = ',A4,5X,'MAXIMUM STATIONS = ',I4,5X,
     *   'STATIONS DEFINED = ',I4,5X,
     *   'STATIONS DELETED = ',I3,5X,
     *   'PERCENT USED = ',I3)
270   FORMAT ('0*** WARNING - PERCENT OF STATIONS USED (',I3,
     *   ') EXCEEDS ',I3,' PERCENT FOR PARAMETER TYPE ',A4,'.')
280   FORMAT ('0*** NOTE - NO SPECIAL PARAMETER TYPE RECORDS ',
     *   'DEFINED.')
C
      END
