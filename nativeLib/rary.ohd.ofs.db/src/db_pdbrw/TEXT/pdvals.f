C MEMBER PDVALS
C  (from old member PDCRSUBS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/19/95.13:35:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDVALS (IX,NREC1D,NRECAL,LSTREC)
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C          IX       I    I      1    SUBSCRIPT OF DATA TYPE IN DIRECTORY
C          NREC1D   I    O      1    NUMBER OF RECORDS FOR 1 DAY DATA
C          NRECAL   I    O      1    NUMBER OF DATA RECORDS TOTAL
C          LSTREC   I    O      1    RECORD NUMBER OF LAST DATA RECORD
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdvals.f,v $
     . $',                                                             '
     .$Id: pdvals.f,v 1.1 1995/09/17 18:44:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDVALS')
C
      NREC1D=IDDTDR(21,IX)
      NRECAL=NREC1D*IDDTDR(7,IX)
      LSTREC=IDDTDR(15,IX)+NRECAL-NREC1D
C
      IF (IPDDB.GT.0) WRITE (IOGDB,20)
     *   IDDTDR(2,IX),IDDTDR(3,IX),
     *   NREC1D,NRECAL,LSTREC
20    FORMAT (' *** EXIT PDVALS - TYPE=',2A2,
     *   ' NREC1D=',I6,
     *   ' NRECAL=',I6,
     *   ' LSTREC=',I6)
C
      RETURN
C
      END
