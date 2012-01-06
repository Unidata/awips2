C MEMBER MTSTLN
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE MTSTLN (REAL,NFLD,ISTRT,IERR)
C
      DIMENSION CHAR(2)
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pproc_shared/RCS/mtstln.f,v $
     . $',                                                             '
     .$Id: mtstln.f,v 1.1 1997/01/23 15:27:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C   THIS SUBROUTINE READS THE STATION LONGITUDE AND TRANSFORMS
C     DEGREES,MINUTES INTO DECIMAL DEGREES
C
      NCHAR=2
      CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,NCHAR,CHAR,
     *   LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      NDEC=0
      IPRERR=1
C
C  CHECK IF INPUT AS DDMM
      IF (ITYPE.NE.0) GO TO 640
         IBEG=1
         IEND=LENGTH-2
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,DEG,IPRERR,LP,ISTAT)
         IBEG=LENGTH-1
         IEND=LENGTH
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,XMIN,IPRERR,LP,ISTAT2)
         GO TO 670
C  CHECK IF INPUT AS DD-MM
640   IF (ITYPE.NE.2) GO TO 700
         CALL UINDEX (CHAR,NCHAR*4,'-',1,ICOL)
         IF (ICOL.GT.0) GO TO 660
650         WRITE (LP,1410) NFLD
            IERR=1
            CALL SUERRS (LP,2,NUMERR)
            GO TO 700
660      IBEG=1
         IEND=ICOL-1
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,DEG,IPRERR,LP,ISTAT)
         IBEG=ICOL+1
         IEND=LENGTH
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,XMIN,IPRERR,LP,ISTAT2)
670      IF (ISTAT.GT.0.OR.ISTAT2.GT.0) GO TO 650
            IF (XMIN.GE.0.0.AND.XMIN.LE.59.) GO TO 680
               WRITE (LP,1560) XMIN,NFLD
               IERR=1
               CALL SUERRS (LP,2,NUMERR)
               GO TO 700
680          REAL=DEG+XMIN/60.
  700 CONTINUE
C
      RETURN
C
1410  FORMAT ('0*** ERROR - NON-CHARACTER DATA EXPECTED IN INPUT ',
     *   'CARD F  FIELD ',I2)
1560  FORMAT ('0*** ERROR - INVALID VALUE FOR MINUTES (',F6.2,
     *   ') FOUND IN CARD F  (FIELD ',I2,'). VALID ',
     *   'VALUES ARE 0 THRU 59.')
C
      END
