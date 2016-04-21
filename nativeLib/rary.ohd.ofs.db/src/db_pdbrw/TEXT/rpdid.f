C MEMBER RPDID
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDID (IDSTA,NUMSTA,IDTYPE,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE OBTAINS THE 8-CHARACTER STATION
C    IDENTIFIER, GIVEN THE USER SUPPLIED STATION
C    NUMBER OR VICE VERSA.  IT CALLS ROUTINES PDFND OR
C    PDFNDR WHICH SEARCHES FOR THE MISSING VALUE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IDSTA    A8    ?     2    STATION IDENTIFIER
C         NUMSTA   I     ?     1    STATION NUMBER
C         IDTYPE   I     I     1    =0,IDSTA IS INPUT;NUMSTA IS OUTPUT
C                                   =1,NUMSTA IS INPUT;IDSTA IS OUTPUT
C         ISTAT    O     I     1    STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=IDSTA OR NUMSTA NOT FOUND
C                                      2=READ ERROR ON FILE
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDSTA(2)
      INTEGER*2 ISIBUF(128)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdid.f,v $
     . $',                                                             '
     .$Id: rpdid.f,v 1.1 1995/09/17 18:44:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
      LSIBUF=128
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50)
C
C  CHECK TYPE OF DATA INPUT
      IF (IDTYPE.EQ.0) GO TO 10
C
C  CHECK STATION NUMBER
      CALL PDFNDI (NUMSTA,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      IF (IFIND.EQ.0) GO TO 20
C
C  FOUND STATION IDENTIFIER
      CALL UMEMOV (ISIBUF(2),IDSTA,2)
      GO TO 40
C
C  CHECK STATION ID
10    CALL PDFNDR (IDSTA,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      IF (IFIND.EQ.0) GO TO 20
C
C  FOUND STATION NUMBER
      NUMSTA=ISIBUF(6)
      GO TO 40
C
C  STATION ID OR STATION NUMBER NOT FOUND
20    ISTAT=1
      GO TO 40
C
C  SYSTEM ERROR
30    ISTAT=2
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,60)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER RPDID')
60    FORMAT (' *** EXIT RPDID')
C
      END
