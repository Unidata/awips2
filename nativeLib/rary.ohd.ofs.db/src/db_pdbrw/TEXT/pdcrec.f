C MEMBER PDCREC
C  (from old member PDCRSUBS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/19/95.13:35:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDCREC (IFDAY,IEDATE,ITX,IREC)
C
C  THIS ROUTINE COMPUTES THE CORRECT RECORD FOR DATA GIVEN THE FIRST
C  DAY AND THE SUBSCRIPT OF THE DATA TYPE IN THE DIRECTORY
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcrec.f,v $
     . $',                                                             '
     .$Id: pdcrec.f,v 1.1 1995/09/17 18:43:36 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IREC=IDDTDR(10,ITX)
      IF (IFDAY.EQ.IEDATE) GO TO 10
      N=IFDAY-IEDATE
      CALL PDVALS (ITX,NREC1D,NRECAL,LSTREC)
      IF (IPDDB.GT.0) WRITE (IOGDB,20) NREC1D,NRECAL,LSTREC
20    FORMAT (' *** IN PDCREC - NREC1D=',I6,' NRECAL=',I6,
     *   ' LSTREC=',I6)
      IREC=IREC+(N*NREC1D)
      IF (IREC.LE.LSTREC) GO TO 10
C
C MUST RECOMPUTE THE RECORD BECAUSE OF WRAP AROUND
C
      N=IREC-LSTREC
      IREC=IDDTDR(15,ITX)+(N-NREC1D)
C
10    RETURN
C
      END
