C MODULE USETDB
C-----------------------------------------------------------------------
C
C   THIS ROUTINE SETS THE TRACE AND DEBUG VALUES.
C
      SUBROUTINE USETDB
C
      CHARACTER*4 DCODE
      EQUIVALENCE (IDCODE,DCODE)
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/usetdb.f,v $
     . $',                                                             '
     .$Id: usetdb.f,v 1.3 2000/03/14 13:55:21 page Exp $
     . $' /
C    ===================================================================
C
C
      NFLD=1
C
C  GET VALUE FOR TRACE LEVEL
      NFLD=NFLD+1
      CALL UINTFX (LTRACE,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.NE.0) WRITE (LP,100) NFLD
C
C  GET VALUE FOR DEBUG LEVEL
      NFLD=NFLD+1
      CALL UINTFX (LDEBUG,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.NE.0) WRITE (LP,100) NFLD
C
      NCODES=NFIELD-3
C
C  PROCESS EACH DEBUG CODE
      DO 10 ICODES=1,NCODES
         NFLD=NFLD+1
         IF (IFTYPE(NFLD).EQ.1) THEN
            WRITE (LP,80) NFLD
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         MCHAR=LEN(DCODE)
         IF (NCHAR.GT.MCHAR) NCHAR=MCHAR
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),DCODE,NCHAR)
         IF (DCODE.EQ.'$') GO TO 20
         IF (DCODE.EQ.'HCL') THEN
C        SET VARIABLES FOR HYDROLOGIC COMMAND LANGUAGE ROUTINES
            IHCLTR=LTRACE
            IHCLDB=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'DE') THEN
C        SET VARIABLES FOR DATA ENTRY
            IDETR=LTRACE
            IDEDB=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'PDB') THEN
C        SET VARIABLES FOR PREPROCESSOR DATA BASE
            IPDTR=LTRACE
            IPDDB=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'PPP') THEN
C        SET VARIABLES FOR PREPROCESSOR PARAMETRIC DATA BASE ROUTINES
            IPPTR=LTRACE
            IPPDB=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'PRD') THEN
C        SET VARIABLES FOR PROCESSED DATA BASE
            IPRTR=LTRACE
            IPRDB=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'FC') THEN
C        SET VARIABLES FOR FORECAST COMPONENT DATA BASE
            ITRACE=LTRACE
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'FC-D') THEN
C        SET VARIABLES FOR FORECAST COMPONENT DATA BASE
            ITRACE=LTRACE
            CALL FSETBG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'PPIN') THEN
C     SET VARIABLES FOR PPINIT ROUTINES
            NPFLD=0
            IPSTRT=0
            CALL SBDBUG (NPFLD,IPSTRT,IERR)
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'NBUG') THEN
C        SET NOBUG VARIABLE
            NOBUG=LDEBUG
            GO TO 10
            ENDIF
         IF (DCODE.EQ.'ESP') THEN
C     SET VARIABLES FOR ESP ROUTINES
            NDEBGS=NDEBGS+1
            IDEBGS(NDEBGS)=IDCODE
            GO TO 10
            ENDIF
         WRITE (LP,90) DCODE
10       CONTINUE
C
C  SET OTHER VARIABLES
20    IOGDB=LP
      IODBUG=LP
C
C  PRINT DEBUG OPTIONS IN EFFECT
      WRITE (LP,30)
30    FORMAT ('0- DEBUG OPTIONS IN EFFECT -')
      WRITE (LP,40) IOGDB,IODBUG
40    FORMAT ('0',2X,'IOGDB=',I2,3X,'IODBUG=',I2)
      WRITE (LP,50) IHCLTR,IHCLDB,
     *  IUTLTR,IUTLDB,
     *  IPDTR,IPDDB,
     *  IPPTR,IPPDB,
     *  IPRTR,IPRDB,
     *  IDETR,IDEDB,
     *  ITRACE
50    FORMAT ('0',2X,'IHCLTR=',I2,3X,'IHCLDB=',I2,3X,
     *      'IUTLTR=',I2,3X,'IUTLDB=',I2 /
     *   '0',2X,'IPDTR=',I2,3X,'IPDDB=',I2,3X,
     *      'IPPTR=',I2,3X,'IPPDB=',I2,3X,
     *      'IPRTR=',I2,3X,'IPRDB=',I2 /
     *   '0',2X,'IDETR=',I2,3X,'IDEDB=',I2,3X,
     *      'ITRACE=',I2)
C
      RETURN
C
80    FORMAT ('0**WARNING** FIELD ',I2,' IS AN INTEGER AND ',
     *   'SHOULD BE CHARACTER.')
90    FORMAT ('0**WARNING** DEBUG CODE ',A4,' IS INVALID.')
100   FORMAT ('0**WARNING** INVALID CHARACTER IN FIELD ',I2,'.')
C
      END
