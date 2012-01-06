C MEMBER MTSTLT
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE MTSTLT (REAL,NFLD,ISTRT,IERR)
C
      DIMENSION CHAR(2)
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pproc_shared/RCS/mtstlt.f,v $
     . $',                                                             '
     .$Id: mtstlt.f,v 1.1 1997/01/23 15:27:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C   THIS SUBROUTINE READS THE STATION LATITUDE AND TRANSFORMS
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
      IF (ITYPE.NE.0) GO TO 550
         IBEG=1
         IEND=LENGTH-2
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,DEG,IPRERR,LP,ISTAT)
         IBEG=LENGTH-1
         IEND=LENGTH
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,XMIN,IPRERR,LP,ISTAT2)
         GO TO 580
C  CHECK IF INPUT AS DD-MM
550   IF (ITYPE.NE.2) GO TO 600
         CALL UINDEX (CHAR,NCHAR*4,'-',1,ICOL)
         IF (ICOL.GT.0) GO TO 570
560         WRITE (LP,1410) NFLD
            IERR=1
            CALL SUERRS (LP,2,NUMERR)
            GO TO 600
570      IBEG=1
         IEND=ICOL-1
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,DEG,IPRERR,LP,ISTAT)
         IBEG=ICOL+1
         IEND=LENGTH
         LCHAR=IEND-IBEG+1
         CALL UFA2F (CHAR,IBEG,LCHAR,NDEC,XMIN,IPRERR,LP,ISTAT2)
580      IF (ISTAT.GT.0.OR.ISTAT2.GT.0) GO TO 560
            IF (XMIN.GE.0.0.AND.XMIN.LE.59.) GO TO 590
               WRITE (LP,1560) XMIN,NFLD
               IERR=1
               CALL SUERRS (LP,2,NUMERR)
               GO TO 600
590         REAL=DEG+XMIN/60.
C
  600 CONTINUE
C
      RETURN
C
1410  FORMAT ('0*** ERROR - NON-CHARACTER DATA EXPECTED IN INPUT ',
     *   'CARD F  FIELD ',I2)
1560  FORMAT ('0*** ERROR - INVALID VALUE FOR MINUTES (',F6.2,
     *   ') FOUND IN  CARD F  (FIELD ',I2,'). VALID ',
     *   'VALUES ARE 0 THRU 59.')
C
      END
