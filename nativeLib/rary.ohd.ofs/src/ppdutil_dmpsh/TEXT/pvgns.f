C MODULE PVGNS
C-----------------------------------------------------------------------
C
C  PGM: PVGNS(NXSTA,STAID,NUMID,IPGENL,NOFTY,TYPS,LOCA,ICOND) .. NXT STA
C
C  I/O: NXSTA ...... REC NUM OF NXT STA IN PPDB INDX FILE, INITZ TO ZERO
C  I/O:                FOR FIRST ENTRY INTO RTN, BECOMES ZERO AGAIN
C  I/O:                AFTER LAST STATION IS FOUND
C  OUT: STAID ...... STA ID NAME FOR NEXT STA FOUND, ELSE BLANK
C  OUT: NUMID ...... STA ID NUMBER FOR NEXT STA FOUND, ELSE ZERO
C  OUT: IPGENL ..... POINTER TO GENERAL STA PARAMETER REC IN PDB
C  OUT: NOFTY ...... NUMBER OF DATA TYPES FOR STATION FOUND
C  OUT: TYPS  ...... LIST OF DATA TYPES (MAX 64)
C  OUT: LOCA  ...... LIST OF ARRAY LOCATNS OR REC NUM OF RRS DATA
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1 = ERR, NO STA

C  HIS: WRITTEN BY D. STREET IN APRIL 1988
C
C  =====================================================================
C
      SUBROUTINE PVGNS (ISORT,NINFSTA,MINFSTA,LINFSTA,INFSTA,
     *   NXSTA,STAID,NUMID,IPGENL,NOFTY,TYPS,LOCA,ICOND)
C
      INCLUDE 'uiox'
      INCLUDE 'pdbcommon/pdsifc'
C
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
      CHARACTER*4 TYPS(*)
      CHARACTER*8 STAID
      CHARACTER*8 ROUTN/'PVGNS'/
      DIMENSION INFSTA(LINFSTA,MINFSTA)
      DIMENSION LOCA(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvgns.f,v $
     . $',                                                             '
     .$Id: pvgns.f,v 1.4 2002/02/11 20:53:43 dws Exp $
     . $' /
C    ===================================================================
C
C
        CALL PVSUBB (ROUTN,ICOND)
        IF (ICOND.NE.0) GO TO 40
C
        LDEBUG=0
C
        STAID=' '
        NUMID=0
        NOFTY=0
C
      IF (ISORT.EQ.1.AND.NINFSTA.EQ.0) THEN
         NXSTA=INFREC+1
10       IREC=NXSTA
C     READ SIF RECORD
         CALL PDRSIF (IREC,NXSTA,LSIBUF,ISIBUF,IERR)
         IF (IERR.NE.0) THEN
            ICOND=2
            GO TO 40
            ENDIF
         IF (NINFSTA+1.GT.MINFSTA) THEN
            WRITE (LP,20) MINFSTA
20    FORMAT ('0**ERROR** IN PDRSIF - ',
     *   'MAXIMUM NUMBER OF SIF RECORDS THAT CAN BE PROCESSED (',I4,
     *   ') EXCEEDED.')
            CALL UEROR (LP,0,-1)
            ICOND=2
            GO TO 40
            ENDIF
         NINFSTA=NINFSTA+1
C     STORE STATION IDENTIFIER
         CALL UMEMOV (ISIBUF(2),INFSTA(1,NINFSTA),2)
C     STORE RECORD NUMBER
         INFSTA(3,NINFSTA)=IREC
C     CHECK RECORD NUMBER OF NEXT SIF RECORD
         IF (NXSTA.LE.LSTSIF) GO TO 10
C     SORT ARRAY
         ISPTR=0
         CALL USORT1 (LINFSTA,NINFSTA,INFSTA,INFSTA,ISPTR,IERR)
         NXSTA=0
         ENDIF
C
      IF (ISORT.EQ.0) THEN
         IF (NXSTA.EQ.0) NXSTA=INFREC+1
C     CHECK RECORD NUMBER OF NEXT SIF RECORD
         IF (NXSTA.GT.LSTSIF) THEN
            ICOND=1
            NXSTA=0
            GO TO 40
            ENDIF
         IREC=NXSTA
         ENDIF
C
      IF (ISORT.EQ.1) THEN
         NXSTA=NXSTA+1
         IF (NXSTA.GT.NINFSTA) THEN
            ICOND=1
            NXSTA=0
            GO TO 40
            ENDIF
         IREC=INFSTA(3,NXSTA)
         ENDIF
C
C  READ SIF RECORD
      CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,IERR)
      IF (IERR.NE.0) THEN
         ICOND=2
         GO TO 40
         ENDIF
      IF (ISORT.EQ.0) NXSTA=NXREC
C
C  SET STATION IDENTIFIER
      CALL UMEMOV (ISIBUF(2),STAID,2)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,*)
     *      ' IREC=',IREC,
     *      ' STAID=',STAID
         ENDIF
C
C  SET STATION NUMBER
      NUMID=ISIBUF(6)
C
C  SET RECORD NUMBER OF GENL PARAMETER RECORD
      IPGENL=ISIBUF(7)
C
C  CHECK IF STATION HAS PCPN DATA
      IF (ISIBUF(8).GT.0) THEN
         NOFTY=NOFTY+1
         TYPS(NOFTY)='PP24'
         LOCA(NOFTY)=ISIBUF(8)
         ENDIF
C
C  CHECK IF STATION HAS TEMP DATA
      IF (ISIBUF(9).GT.0) THEN
         NOFTY=NOFTY+1
         TYPS(NOFTY)='TM24'
         LOCA(NOFTY)=ISIBUF(9)
         ENDIF
C
C  GET NUMBER OF ADDITIONAL DATA TYPES
      NADDTP=ISIBUF(10)
      J=11
      IF (NADDTP.GT.0) THEN
         DO 30 I=1,NADDTP
            NOFTY=NOFTY+1
            CALL UMEMOV (ISIBUF(J),TYPS(NOFTY),1)
            LOCA(NOFTY)=ISIBUF(J+2)
            J=J+3
            IF (NOFTY.GT.64) GO TO 40
30          CONTINUE
         ENDIF
C
40    CALL PVSUBE (ROUTN,ICOND)
C
      RETURN
C
      END
