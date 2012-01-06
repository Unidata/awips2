C MODULE UROP24
C-----------------------------------------------------------------------
C
      SUBROUTINE UROP24 (LPPP24,IPPP24,LAOP24,IAOP24,LAOPVR,IAOPVR,
     *   LPPPVR,IPPPVR,LARRAY,ARRAY,ISTAT)
C
C  THIS ROUTINE REORDERS THE PCPN PARAMETRIC RECORDS BASED
C  ON THE ALPHABETICAL ORDERED LIST OF PCPN STATIONS STORED IN
C  THE PARAMETER TYPES OP24 AND OPVR. THE 24 HOUR STATIONS WILL
C  BE ORDERED BEFORE THE LESS THAN 24-HOUR DATA.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   SIZE    DESCRIPTION
C       ------    ----  ---   ------  -----------
C       LPPP24     I     I      1     LENGTH OF IPPP24
C       IPPP24    I*2   I/O   LPPP24  PP24 POINTERS
C       LAOP24     I     I      1     LENGTH OF IAOP24
C       IAOP24    I*2   I/O   LAOP24  ARRAY FOR OP24 PARAMETERS
C       LAOPVR     I     I      1     LENGTH OF IAOPVR
C       IAOPVR    I*2   I/O   LAOPVR  ARRAY FOR OPVR PARAMETERS
C       LPPPVR     I     I      1     LENGTH OF IPPPVR
C       IPPPVR    I*2   I/O   LPPPVR  ARRAY FOR PPVR POINTERS
C       LARRAY     I     I      1     LENGTH OF ARRAY
C       ARRAY      I    I/O   LARRAY  ARRAY FOR PARAMETER RECORD
C       ISTAT      I     I      1     STATUS CODE:
C                                       0=NORMAL RETURN
C                                       OTHER=ERROR
C
      CHARACTER*4 XDISP,PPTYPE,PDTYPE
      CHARACTER*8 PARMID,STAID
C
      INTEGER*2 IPPP24(LPPP24)
      INTEGER*2 IPPPVR(LPPPVR)
      INTEGER*2 IAOP24(LAOP24)
      INTEGER*2 IAOPVR(LAOPVR)
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urcdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urop24.f,v $
     . $',                                                             '
     .$Id: urop24.f,v 1.5 2002/02/11 21:12:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPPTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER UROP24'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      IF (IPPDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' LPPP24=',LPPP24,
     *      ' LAOP24=',LAOP24,
     *      ' LAOPVR=',LAOPVR,
     *      ' LPPPVR=',LPPPVR,
     *      ' LARRAY=',LARRAY,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
      WRITE (LP,380)
      CALL SULINE (LP,2)
C
C  GET OP24 PARAMETER RECORD
      PARMID=' '
      PPTYPE='OP24'
      IRECNX=0
      L4OP24=LAOP24/2
      IAMORD=0
      CALL RPPREC (PARMID,PPTYPE,IRECNX,L4OP24,IAOP24,NFILL,IRECNX,
     *   ISTAT)
      IF (IPPDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' PPTYPE=',PPTYPE,
     *      ' IRECNX=',IRECNX,
     *      ' LAOP24=',LAOP24,
     *      ' L4OP24=',L4OP24,
     *      ' NFILL=',NFILL,
     *      ' IRECNX=',IRECNX,
     *      ' ISTAT=',ISTAT,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
      IF (ISTAT.NE.0) THEN
         IF (ISTAT.EQ.2.OR.ISTAT.EQ.6) THEN
C        OP24 PARAMETERS NOT FOUND
            CALL SULINE (LP,2)
            WRITE (LP,390) PPTYPE,' '
            ISTAT=0
            GO TO 260
            ENDIF
         CALL SRPPST (PARMID,PPTYPE,IRECNX,L4OP24,NFILL,IRECNX,ISTAT)
         GO TO 330
         ENDIF
C
C  GET OPVR PARAMETER RECORD
      IPPVR=1
      PARMID=' '
      PPTYPE='OPVR'
      IRECNX=0
      L4OPVR=LAOPVR/2
      IAMORD=0
      CALL RPPREC (PARMID,PPTYPE,IRECNX,L4OPVR,IAOPVR,NFILL,IRECNX,
     *   ISTAT)
      IF (IPPDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' PPTYPE=',PPTYPE,
     *      ' IRECNX=',IRECNX,
     *      ' LAOPVR=',LAOPVR,
     *      ' L4OPVR=',L4OPVR,
     *      ' NFILL=',NFILL,
     *      ' IRECNX=',IRECNX,
     *      ' ISTAT=',ISTAT,
     *      ' '
         CALL SULINE (IOGDB,1)
         ENDIF
      IF (ISTAT.NE.0) THEN
         IF (ISTAT.EQ.2.OR.ISTAT.EQ.6) THEN
C        OPVR PARAMETERS NOT FOUND
            CALL SULINE (LP,2)
            WRITE (LP,390) PPTYPE
            IPPVR=0
            GO TO 40
            ENDIF
         CALL SRPPST (PARMID,PPTYPE,IRECNX,L4OPVR,NFILL,IRECNX,ISTAT)
         GO TO 330
         ENDIF
C
40    PPTYPE='PCPN'
C
C  FIND TYPE IN OLD DIRECTORY
      XDISP='OLD'
      IAMORD=0
      IDXOLD=IPCKDT(PPTYPE)
      IF (IDXOLD.EQ.0) THEN
         WRITE (LP,400) PPTYPE,XDISP
         CALL SUERRS (LP,2,-1)
         GO TO 350
         ENDIF
C
C  FIND TYPE IN OLD DIRECTORY
      XDISP='NEW'
      IAMORD=1
      IDXNEW=IPCKDT(PPTYPE)
      IF (IDXNEW.EQ.0) THEN
         WRITE (LP,400) PPTYPE,XDISP
         CALL SUERRS (LP,2,-1)
         GO TO 350
         ENDIF
C
C  SEE IF TYPE DEFINED ON FILE
      IF (IPDTDR(5,IDXOLD).EQ.0) THEN
         WRITE (LP,410) PPTYPE
         CALL SUERRS (LP,2,-1)
         GO TO 350
         ENDIF
C
C  GET PP24 POINTER ARRAY FOR 24-HR STATIONS
      PDTYPE='PP24'
      CALL URDPTR (PDTYPE,LPPP24,IPPP24,LUSED,ISTAT)
      IF (ISTAT.GT.0) GO TO 330
C
C  CHECK FOR INVALID PARAMETRIC RECORD NUMBER
      IXPPD=IPDCKD(PDTYPE)
      NPRSTA=IDDTDR(5,IXPPD)
      IXPPP=IPCKDT(PPTYPE)
      DO 50 I=1,LUSED,NPRSTA
         IRECPP=IPPP24(I)
         IRECPP=IABS(IRECPP)
         IF (IRECPP.EQ.0) GO TO 50
         IF (IRECPP.GE.IPDTDR(3,IXPPP).AND.
     *       IRECPP.LE.IPDTDR(4,IXPPP)) THEN
            IAMORD=0
            PARMID=' '
            IRECPP=-IRECPP
            CALL RPPREC (PARMID,PPTYPE,IRECPP,LARRAY,ARRAY,NUMFIL,
     *         IRECNX,ISTAT)
            IF (ISTAT.NE.0) THEN
               ISTAT=-ISTAT
               CALL SRPPST (PARMID,PPTYPE,IRECPP,LARRAY,NUMFIL,
     *            IRECNX,ISTAT)
               WRITE (LP,43) PPTYPE,IRECPP,PDTYPE,I,I,I+NPRSTA-1
43    FORMAT ('0*** WARNING - ',A,' PARAMETER RECORD NUMBER ',I5,
     *       ' AT ',A,' POINTER POSITION ',I5,' NOT SUCCESSFULLY READ.'
     *       /
     *   15X,'POINTER POSITIONS ',I5,' THRU ',I5,' WILL BE SET TO ZERO.'
     *   )
               CALL SUWRNS (LP,3,-1)
               CALL UMEMS2 (0,IPPP24,I,NPRSTA)
               ENDIF
            ELSE
               WRITE (LP,45) PPTYPE,IRECPP,PDTYPE,I,
     *            IPDTDR(3,IXPPP),IPDTDR(4,IXPPP)
45    FORMAT ('0*** WARNING - ',A,' PARAMETER RECORD NUMBER ',I5,
     *   ' AT ',A,' POINTER POSITION ',I5,' IS INVALID. ',
     *   'VALID VALUES ARE ',I5,' THRU ',I5,'.')
               CALL SUWRNS (LP,2,-1)
            ENDIF
50       CONTINUE
C
C  GET NUMBER OF POINTERS
      IRDATE=0
      CALL RPDFIL (PPTYPE,IRDATE,LPFIL,LDFIL,ISTAT)
C
      IF (IPPVR.EQ.0) GO TO 100
C
C  GET PPVR POINTER ARRAY FOR <24-HR STATIONS
      PDTYPE='PPVR'
      CALL URDPTR (PDTYPE,LPPPVR,IPPPVR,LUSED,ISTAT)
      IF (ISTAT.GT.0) GO TO 330
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  REORDER PCPN PARAMETER RECORDS FOR STATIONS IN THE OP24 RECORD
C
C  CHECK NUMBER OF STATIONS DEFINED
100   CALL UMEMOV (IAOP24(9),REAL,1)
      NUMSTA=REAL
      IF (NUMSTA.EQ.0) THEN
         IF (IPPVR.EQ.1) THEN
            CALL SULINE (LP,2)
            WRITE (LP,415) 'OP24'
            GO TO 180
            ENDIF
         WRITE (LP,420) 'OP24'
         CALL SUERRS (LP,2,-1)
         GO TO 330
         ENDIF
C
      CALL SULINE (LP,2)
      WRITE (LP,430) 'OP24'
C
      IF (IPPDB.GT.0) THEN
         WRITE (IOGDB,440) (IAOP24(I),I=11,32),NUMSTA
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  PROCESS EACH STATION
      IPOS=11
      PPTYPE='PCPN'
      DO 170 I=1,NUMSTA
         IPT24=IAOP24(IPOS)
         IF (IPT24.EQ.0) THEN
            WRITE (LP,480) 'OP24',I
            CALL SUERRS (LP,2,-1)
            GO TO 160
            ENDIF
C     GET POINTER FOR PCPN PARAMETER RECORD FROM POINTER ARRAY USING
C     SUBSCRIPT FROM OP24 RECORD
         LPNT=IPPP24(IPT24)
         IRECPP=IABS(LPNT)
         IF (IRECPP.EQ.0) THEN
            WRITE (LP,490) 'OP24',I
            CALL SUERRS (LP,2,-1)
            GO TO 160
            ENDIF
         ISIGN=LPNT/IRECPP
         IAMORD=0
         STAID=' '
         CALL RPPREC (STAID,PPTYPE,IRECPP,LARRAY,ARRAY,NUMFIL,IRECNX,
     *      ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL SRPPST (STAID,PPTYPE,IRECPP,LARRAY,NUMFIL,IRECNX,ISTAT)
            IF (ISTAT.EQ.2.OR.ISTAT.EQ.5) GO TO 160
            GO TO 330
            ENDIF
         IF (IPPDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         'PROCESSING OP24:',
     *         ' STAID=',STAID,
     *         ' PPTYPE=',PPTYPE,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
C     WRITE PARAMETER RECORD TO NEW FILE
         IAMORD=1
         IRECNW=0
         CALL WPPREC (STAID,PPTYPE,NUMFIL,ARRAY,IRECNW,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL SWPPST (STAID,PPTYPE,NUMFIL,IRECNW,ISTAT)
            GO TO 330
            ENDIF
C     UPDATE RECORD NUMBER IN THE GENL PARAMETER RECORD
         IAMORD=0
         CALL URUDGL (STAID,PPTYPE,IRECNW,LARRAY,ARRAY,ISTAT)
         IF (ISTAT.GT.0) THEN
            IF (ISTAT.EQ.1) GO TO 330
            GO TO 160
            ENDIF
         IPPP24(IPT24)=IRECNW*ISIGN
         IPPP24(IPT24+4)=-IPPP24(IPT24+4)
160      IPOS=IPOS+1
170      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  REORDER PCPN PARAMETER RECORDS FOR STATIONS IN THE OPVR RECORD
C
C  CHECK IF PPVR PARAMETERS FOUND
180   IF (IPPVR.EQ.0) GO TO 250
C
C  CHECK NUMBER OF STATION DEFINED
      CALL UMEMOV (IAOPVR(9),REAL,1)
      NUMSTA=REAL
      IF (NUMSTA.EQ.0) THEN
         WRITE (LP,450)
         CALL SUERRS (LP,2,-1)
         GO TO 330
         ENDIF
C
      CALL SULINE (LP,2)
      WRITE (LP,460) 'OPVR'
      PPTYPE='PCPN'
C
      IF (IPPDB.GT.0) THEN
         WRITE (IOGDB,470) (IAOPVR(I),I=11,32),NUMSTA
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  PROCESS EACH STATION
      IPOS=11
      DO 240 I=1,NUMSTA
         IPTVR=IAOPVR(IPOS)
C     GET POINTER FOR PCPN PARAMETER RECORD FROM POINTER ARRAY USING
C     SUBSCRIPT FOR OPVR RECORD
         IPT24=IPPPVR(IPTVR+1)
         IF (IPT24.EQ.0) THEN
            WRITE (LP,480) 'OP24',I
            CALL SUERRS (LP,2,-1)
            GO TO 230
            ENDIF
         IF (IPPDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         ' I=',I,
     *         ' NUMSTA=',NUMSTA,
     *         ' IPOS=',IPOS,
     *         ' IPTVR=',IPTVR,
     *         ' IPT24=',IPT24,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         LPNT=IPPP24(IPT24)
         IRECPP=IABS(LPNT)
         IF (IRECPP.EQ.0) THEN
            WRITE (LP,490) 'OPVR',I
            CALL SUERRS (LP,2,-1)
            GO TO 230
            ENDIF
         ISIGN=LPNT/IRECPP
         IF (IPPDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         ' LPNT=',LPNT,
     *         ' IPT24=',IPT24,
     *         ' IRECPP=',IRECPP,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         IAMORD=0
         STAID=' '
         CALL RPPREC (STAID,PPTYPE,IRECPP,LARRAY,ARRAY,NUMFIL,IRECNX,
     *      ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL SRPPST (STAID,PPTYPE,IRECPP,LARRAY,NUMFIL,IRECNX,ISTAT)
            IF (ISTAT.EQ.2.OR.ISTAT.EQ.5) GO TO 230
            ENDIF
         IF (IPPDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         'PROCESSING OPVR:',
     *         ' STAID=',STAID,
     *         ' PPTYPE=',PPTYPE,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
C     WRITE PARAMETER RECORD TO NEW FILE
         IAMORD=1
         IRECNW=0
         CALL WPPREC (STAID,PPTYPE,NUMFIL,ARRAY,IRECNW,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL SWPPST (STAID,PPTYPE,NUMFIL,IRECNW,ISTAT)
            GO TO 330
            ENDIF
         IAMORD=0
         CALL URUDGL (STAID,PPTYPE,IRECNW,LARRAY,ARRAY,ISTAT)
         IF (ISTAT.GT.0) THEN
            IF (ISTAT.EQ.3) GO TO 230
            GO TO 330
            ENDIF
         IPPP24(IPT24)=IRECNW*ISIGN
         IPPP24(IPT24+4)=-IPPP24(IPT24+4)
230      IPOS=IPOS+1
240      CONTINUE
C
250   ICOPY=0
      GO TO 310
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY RECORDS
C
C  GET PP24 POINTERS SO PCPN LOCATION CAN BE UPDATED
260   PPTYPE='PP24'
      CALL URDPTR (PPTYPE,LPPP24,IPPP24,LUSED,ISTAT)
      IF (ISTAT.GT.0) GO TO 330
      IF (LPFIL.EQ.0) THEN
         CALL SULINE (LP,2)
         WRITE (LP,510) PPTYPE
         GO TO 350
         ENDIF
C
C  COPY PCPN RECORDS FROM OLD TO NEW FILES
      PPTYPE='PCPN'
      DO 300 I=1,LPFIL,5
         LPNT=IPPP24(I)
         IRECPP=IABS(LPNT)
         IF (IRECPP.EQ.0) GO TO 300
            ISIGN=LPNT/IRECPP
            STAID=' '
            IAMORD=0
            CALL RPPREC (STAID,PPTYPE,IRECPP,LARRAY,ARRAY,NUMFIL,IRECNX,
     *         ISTAT)
            IF (ISTAT.NE.0) THEN
               CALL SRPPST (STAID,PPTYPE,IRECPP,LAOP24,NUMFIL,IRECNX,
     *            ISTAT)
               IF (ISTAT.EQ.2.OR.ISTAT.EQ.6) GO TO 300
               GO TO 330
               ENDIF
         IAMORD=1
         IRECNW=0
         CALL WPPREC (STAID,PPTYPE,NUMFIL,ARRAY,IRECNW,ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL SWPPST (STAID,PPTYPE,NUMFIL,IRECNW,ISTAT)
            GO TO 330
            ENDIF
         IAMORD=0
         CALL URUDGL (STAID,PPTYPE,IRECNW,LARRAY,ARRAY,ISTAT)
         IF (ISTAT.GT.0) THEN
            IF (ISTAT.EQ.3) GO TO 300
            GO TO 330
            ENDIF
         IPPP24(I)=IRECNW*ISIGN
         IPPP24(I+4)=-IPPP24(I+4)
300      CONTINUE
C
      ICOPY=1
C
C  WRITE POINTER ARRAY TO NEW FILES
310   CALL URWPTR ('PP24',IPPP24,ISTAT)
      IF (ISTAT.NE.0) GO TO 330
C
      IF (ICOPY.EQ.0) THEN
         WRITE (LP,540) JPDTDR(5,IDXNEW),'REORDERED'
         CALL SULINE (LP,2)
         ENDIF
      IF (ICOPY.EQ.1) THEN
         WRITE (LP,540) JPDTDR(5,IDXNEW),'COPIED'
         CALL SULINE (LP,2)
         ENDIF
      GO TO 350
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET ERROR FLAG
330   IWURFL=1
      CALL SULINE (LP,2)
      WRITE (LP,340)
340   FORMAT ('0*** NOTE - ERRORS HAVE OCCURRED IN THE REORDER OF ',
     *   'PCPN PARAMETER RECORDS.')
      GO TO 350
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
350   IF (IPPTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT UROP24: ISTAT=',ISTAT
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
380   FORMAT ('0*** NOTE - BEGIN TO REORDER  << PCPN PARAMETER >>  ',
     *   'RECORDS.')
390   FORMAT ('0*** NOTE - PARAMETER TYPE ',A,' NOT FOUND.' : A,
     *   'PCPN PARAMETER RECORDS WILL BE COPIED AND NOT REORDERED.')
400   FORMAT ('0*** ERROR - IN UROP24 - TYPE ',A,' NOT FOUND IN ',
     *   A,' PREPROCESSOR PARAMETRIC DATA BASE DIRECTORY.')
410   FORMAT ('0*** NOTE - NO PCPN PARAMETERS ARE DEFINED IN ',
     *   'OLD PREPROCESSOR PARAMETRIC DATA BASE.')
415   FORMAT ('0*** NOTE - NO STATIONS DEFINED IN ',A,' PARAMETER ',
     *   'RECORD.')
420   FORMAT ('0*** ERROR - NO STATIONS DEFINED IN ',A,' PARAMETER ',
     *   'RECORD BUT PP24 STATIONS EXIST.')
430   FORMAT ('0*** NOTE - BEGIN TO REORDER PCPN PARAMETERS ',
     *   'FOR 24-HOUR STATIONS BY USING ',A,'.')
440   FORMAT (' IAOP24=',22I5,' NUMSTA=',I5)
450   FORMAT ('0*** ERROR - NO STATIONS DEFINED IN OPVR RECORD ',
     *   'BUT PPVR STATIONS EXIST.')
460   FORMAT ('0*** NOTE - BEGIN TO REORDER PCPN PARAMETERS ',
     *   'FOR LESS THAN 24-HOUR STATIONS BY USING ',A,'.')
470   FORMAT (' IAOPVR=',22I5,' NUMSTA=',I5)
480   FORMAT ('0*** ERROR - IN UROP24 - PP24 POINTER FOR STATION ',
     *   'AT ',A,' ARRAY POSITION ',I5,' IS NOT GREATER THAN ZERO.')
490   FORMAT ('0*** ERROR - IN UROP24 - PCPN POINTER FOR STATION ',
     *   'AT ',A,' ARRAY POSITION ',I5,' IS NOT GREATER THAN ZERO.')
510   FORMAT ('0*** NOTE - NO STATIONS DEFINED FOR TYPE ',A,
     *   'IN PREPROCESSOR DATA BASE.')
540   FORMAT ('0*** NOTE - ',I4,' PCPN PARAMETER RECORDS HAVE BEEN ',
     *   'SUCCESSFULLY ',A,'.')
C
      END
