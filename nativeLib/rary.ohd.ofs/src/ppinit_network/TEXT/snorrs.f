C MEMBER SNORRS
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 02/09/94.11:41:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  ROUTINE FOR PERFORMING NETWORK COMPUTATIONS FOR RRS STATIONS.
C
      SUBROUTINE SNORRS (ISORT,LARRAY,ARRAY,IUEND,ISTAT)
C
      CHARACTER*8 TYPERR
      INTEGER*2 ISTATE(1)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION ISRTBY(1),IPNTRS(1)
      DIMENSION UNUSED(10)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
      EQUIVALENCE (ISTATE(1),STATNW(1))
      EQUIVALENCE (IPNTRS(1),PP24NW(1))
      EQUIVALENCE (ISRTBY(1),STIDNW(1,1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/snorrs.f,v $
     . $',                                                             '
     .$Id: snorrs.f,v 1.2 1997/04/06 12:24:17 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('NTWK','NTWKRRS ','SNORRS  ',LDEBUG)
C
      ISTAT=0
      NUMERR=0
C
      WRITE (LP,30)
      CALL SULINE (LP,1)
      WRITE (LP,40)
      CALL SULINE (LP,2)
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
      ICKRGN=0
      ICKCPU=1
      MINCPU=10
      IPRERR=1
      IPUNIT=LP
      TYPERR='ERROR'
      INCLUDE 'clugtres'
      IF (IERR.NE.0) THEN
         CALL SUFATL
         IUEND=1
         GO TO 10
         ENDIF
C
      NSTA=0
C
C  GET SORTED LIST OF NAMES AND PPPDB POINTERS
      IXSORT=-ISORT
      ISTATE(1)=1
      IPNTRS(1)=1
      NWORDS=5
      IPRMSG=1
      IPRERR=0
      CALL SURIDS ('RRS ',IXSORT,MAXSNW,ISTATE,NWORDS,ISRTBY,
     *   IPNTRS,NSTA,LARRAY,ARRAY,IPRMSG,IPRERR,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,60)
            CALL SULINE (LP,2)
            ELSE
               WRITE (LP,50)
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
            ENDIF
         GO TO 10
         ENDIF
C
C  SET NTWK COMMON BLOCK INDICATORS
      INWFIL=0
      INWTYP=0
C
C  WRITE PARAMETERS
      IVORRS=1
      UNSD=-999.
      CALL SWORRS (IVORRS,UNSD,ISORT,IPNTRS,NSTA,
     *   LARRAY,ARRAY,IWORRS)
C
      IF (LDEBUG.GT.0) THEN
         IPRERR=0
         CALL SRORRS (IPRERR,IVORRS,UNUSED,ISORT,MAXSNW,IPNTRS,
     *        NUMSTA,LARRAY,ARRAY,IERR)
         IF (IERR.EQ.0) THEN
            CALL SPORRS (IVORRS,UNUSED,ISORT,IPNTRS,NUMSTA,
     *         LARRAY,ARRAY,IERR)
            ENDIF
        ENDIF
C
      WRITE (LP,70) NSTA
      CALL SULINE (LP,2)
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SNORRS')
30    FORMAT (' ')
40    FORMAT ('0*--> DETERMINE RRS STATION ALPHABETICAL ORDER')
50    FORMAT ('0*** ERROR - IN SNORRS - SORTED LIST OF STATIONS NOT ',
     *   'SUCCESSFULLY OBTAINED.')
60    FORMAT ('0*** NOTE - NO RRS STATIONS ARE DEFINED.')
70    FORMAT ('0*** NOTE - ',I4,' STATIONS PROCESSED ',
     *   'FOR ORRS PARAMETERS.')
80    FORMAT (' *** EXIT SNORRS')
C
      END
