C MODULE PDCOLD
C-----------------------------------------------------------------------
C
      SUBROUTINE PDCOLD (JSIBUF,ISIBUF,LDELTP,DELTP,NDELTP,
     *   ISAVPP,ISAVTA,ISTAT)
C
C          ROUTINE:  PDCOLD
C          VERSION:  1.0.0
C             DATE:  2-17-83
C           AUTHOR:  SONJA R SIEGEL
C                    DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE WILL RESET THE POINTER SLOTS IN THE POINTER RECORDS
C  FOR DATA TYPES THAT ARE NO LONGER BEING USED BY A STATION DEFINED
C  IN THE PREPROCESSOR DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       JSIBUF     I     I     ?     OLD SIF RECORD
C       ISIBUF     I     I     ?     NEW SIF RECORD
C       LDELTP     I     I     1     DIMENSION OF ARRAY DELTP
C       NDELTP     I     O     1     NUMBER OF ENTRIES IN DELTP
C       DELTP      A4    O   LDELTP  ARRAY OF DELETED TYPES
C       ISAVPP     I     I     1     SAVED VALUE OF PP24 POINTER
C       ISAVTA     I     I     1     SAVED VALUE OF TA24 POINTER
C       ISTAT      I     O     1     STATUS CODE
C                                      O=OK
C                                      1=DELTP TOO SMALL
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 DELTP(LDELTP)
      INTEGER*2 ISIBUF(*),JSIBUF(*)
      INTEGER*2 IPTREC(32)
      INTEGER*2 L2PP24
C
      EQUIVALENCE (L2PP24,LPP24)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcold.f,v $
     . $',                                                             '
     .$Id: pdcold.f,v 1.2 1998/04/07 14:56:30 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LTM24/4hTM24/,LPP24/4hPP24/
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,60)
C
      ISTAT=0
C
      LRCPD2=LRCPDD*2
      ISTATZ=0
      NDELTP=0
C
C  SET NUMBER OF TYPES
      NTYPES=JSIBUF(10)
C
C  CHECK NUMBER OF ADDITIONAL DATA TYPES
      IF (NTYPES.GT.0) THEN
C     CHECK EACH DATA TYPE
         JPOS=11
         DO 40 I=1,NTYPES
            CALL UMEMOV (JSIBUF(JPOS),ITYPE,1)
            IX=IPDCKD(JSIBUF(JPOS))
C        CHECK IF A DAILY DATA TYPE
            IF (IX.EQ.0) THEN
C           CHECK IF RRS TYPE IS IN NEW DEFINITION
               IXX=IPDFDT(ISIBUF,ITYPE)
               IF (IXX.NE.0) GO TO 30
               IF (NDELTP+1.GT.LDELTP) THEN
                  ISTATZ=-1
                  GO TO 30
                  ENDIF
               NDELTP=NDELTP+1
               CALL UMEMOV (JSIBUF(JPOS),DELTP(NDELTP),1)
               GO TO 30
               ENDIF
C        CHECK IF DAILY TYPE IS IN NEW DEFINITION
            IXX=IPDFDT(ISIBUF,ITYPE)
            IF (IXX.NE.0) GO TO 30
C        CHECK IF TAVR OR PPVR
            IF (IDDTDR(6,IX).LT.0) THEN
C           NEED TO GET POINTER FROM 24 HOUR POINTER RECORD
               IF (IDDTDR(2,IX).EQ.L2PP24) THEN
                  IF (ISAVPP.GT.0) THEN
                     ISLOT=ISAVPP
                     IF (ISLOT.LE.0) GO TO 40
                     GO TO 20
                     ENDIF
                  IPP24=IPDCKD(LPP24)
                  IPPSLT=(JSIBUF(8)-1)*IDDTDR(5,IPP24)+3
                  ISLOT=IPPSLT
                  IF (ISLOT.LE.0) GO TO 40
                  IIXX=IPP24
                  GO TO 10
                  ENDIF
C           TYPE IS TAVR
               IF (ISAVTA.GT.0) THEN
                  ISLOT=ISAVTA
                  GO TO 20
                  ENDIF
               ITM24=IPDCKD(LTM24)
               IIXX=ITM24
               ITMSLT=(JSIBUF(9)-1)/IDDTDR(6,ITM24)*IDDTDR(5,ITM24)+3
               ISLOT=ITMSLT
C           READ POINTER RECORD
10             IFILE=IDDTDR(4,IIXX)
               NREC=IUNRCD(ISLOT,LRCPD2)-1
               IREC=IDDTDR(14,IIXX)+NREC
               CALL UREADT (KPDDDF(IFILE),IREC,IPTREC,ISTAT)
               IF (ISTAT.NE.0) GO TO 50
C           GET SLOT FOR VR TYPE
               JSLOT=ISLOT-(NREC*LRCPD2)
               ISLOT=IPTREC(JSLOT)
               IF (ISLOT.LE.0) GO TO 30
               GO TO 20
               ENDIF
C        NOT A VR TYPE - COMPUTE ISLOT
            ISLOT=(JSIBUF(JPOS+2)-1)/IDDTDR(6,IX)*IDDTDR(5,IX)+1
C        RESET DATA SLOT
            KSLOT=JSIBUF(JPOS+2)
            CALL PDZDAT (ITYPE,IX,KSLOT,IDUM,IREC,ISTAT)
C        RESET POINTER SLOT
20          CALL PDZAPP (ITYPE,ISLOT,IX,LDELTP,DELTP,NDELTP,ISTATZ)
            IF (ISTATZ.NE.0.AND.ISTATZ.NE.-1) GO TO 50
C        SET POINTER TO NEXT DATA TYPE
30          JPOS=JPOS+3
40          CONTINUE
         ENDIF
C
C  CHECK IF OLD DEFINITION HAS PCPN DATA BUT NEW DOES NOT
      IF (JSIBUF(8).GT.0.AND.ISIBUF(8).EQ.0) THEN
C     RESET POINTER SLOTS
         IPP24=IPDCKD(LPP24)
         ITYPE=LPP24
         ISLOT=(JSIBUF(8)-1)*IDDTDR(5,IPP24)+1
         CALL PDZAPP (ITYPE,ISLOT,IPP24,LDELTP,DELTP,NDELTP,ISTATZ)
C     RESET DATA SLOTS
         KSLOT=JSIBUF(8)
         CALL PDZDAT (ITYPE,IPP24,KSLOT,IDUM,IREC,ISTAT)
         IF (ISTATZ.NE.0.AND.ISTATZ.NE.-1) GO TO 50
         ENDIF
C
C  CHECK IF OLD DEFINITION HAS TEMP DATA BUT NEW DOES NOT
      IF (JSIBUF(9).GT.0.AND.ISIBUF(9).EQ.0) THEN
C     RESET POINTER SLOTS
         ITM24=IPDCKD(LTM24)
         ITYPE=LTM24
         ISLOT=(JSIBUF(9)-1)/IDDTDR(6,ITM24)*IDDTDR(5,ITM24)+1
         CALL PDZAPP (ITYPE,ISLOT,ITM24,LDELTP,DELTP,NDELTP,ISTATZ)
C     RESET DATA SLOTS
         KSLOT=JSIBUF(9)
         CALL PDZDAT (ITYPE,ITM24,KSLOT,IDUM,IREC,ISTAT)
         IF (ISTATZ.NE.0.AND.ISTATZ.NE.-1) GO TO 50
         ENDIF
C
50    IF (ISTATZ.EQ.-1) ISTATZ=1
      IF (ISTAT.EQ.0) ISTAT=ISTATZ
C
      IF (IPDTR.GT.0) WRITE (IOGDB,70) ISTAT,
     *   NDELTP,(DELTP(I),I=1,NDELTP)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER PDCOLD')
70    FORMAT (' *** EXIT PDCOLD : ISTAT=',I4,' NDELTP=',I4,
     *   ' DELTP=',10(1X,A4))
C
      END
