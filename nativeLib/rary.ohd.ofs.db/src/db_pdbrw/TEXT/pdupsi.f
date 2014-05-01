C MODULE PDUPSI
C-----------------------------------------------------------------------
C
      SUBROUTINE PDUPSI (IDTYPE,IDTAPT,LSIBUF,ISIBUF,NXTISI,ISTAT)
C
C  ROUTINE TO UPDATE STATION INFORMATION ARRAY WITH ADDITIONAL
C  DATA TYPE.
C
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdupsi.f,v $
     . $',                                                             '
     .$Id: pdupsi.f,v 1.2 1998/04/07 14:57:06 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
C
      ISTAT=0
C
C  CHECK IF ENOUGH ROOM IN SIF ARRAY - RESERVE ONE WORD FOR LENGTH OF
C  OLD SIF ENTRY WHEN DEFINITION IS CHANGED
      IF (NXTISI+3.GT.LSIBUF-1) THEN
         ISTAT=1
         GO TO 5
         ENDIF
C
      IBEG=NXTISI
C
C  SET DATA TYPE
      CALL UMEMOV (IDTYPE,ISIBUF(NXTISI),1)
C
C  SET RECORD NUMBER OR POINTER
      ISIBUF(NXTISI+2)=IDTAPT
C
      NXTISI=NXTISI+3
C
C  UPDATE NUMBER OF ADDITIONAL DATA TYPES
      ISIBUF(10)=ISIBUF(10)+1
C
      IF (IPDDB.GT.0) WRITE (IOGDB,20) (ISIBUF(I),I=IBEG,NXTISI)
C
5     IF (IPDTR.GT.0) WRITE (IOGDB,30) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER PDUPSI')
20    FORMAT (' ADDED TO ISIBUF=',2A2,2I4)
30    FORMAT (' *** EXIT PDUPSI - ISTAT=',I2)
C
      END
