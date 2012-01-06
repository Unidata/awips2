C MEMBER UDTATR
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  ROUTINE UDTATR RETURNS THE ATTRIBUTES FOR A DATA TYPE.
C
      SUBROUTINE UDTATR (SRCH,DTYPE,
     *   XTDIMN,XTUNIT,IXTMIS,NXTVAL,XTTIME,NXTADD,IXTWRT,
     *   ISTAT)
C
C  INPUT VARIABLES:
C     SRCH  - SEARCH CODE
C     DTYPE - DATA TYPE CODE
C
C  OUTPUT VARIABLES:
C     XTDIMN - DIMENSION CODE
C     XTUNIT - CODE FOR THE STANDARD FORECAST SYSTEM INTERNAL UNITS
C     IXTMIS - MISSING DATA INDICATOR FOR EACH DATA TYPE
C     NXTVAL - NUMBER OF VALUES PER TIME INTERVAL
C     XTTIME - CODE FOR TIME SCALE
C     NXTADD - NUMBER OF PIECES OF ADDITIONAL INFORMATION
C     IXTWRT - INDICATOR SPECIFYING WHICH COMPONENT CAN WRITE TYPE
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=ERROR READING DATA UNIT FILE
C                2=INVALID DATA TYPE CODE
C
      CHARACTER*4 SRCH,DTYPE,XTDIMN,XTUNIT,XTTIME
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'udtypx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/udtatr.f,v $
     . $',                                                             '
     .$Id: udtatr.f,v 1.2 2002/02/11 21:10:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UDTATR -',
     *      ' SRCH=',SRCH,
     *      ' DTYPE=',DTYPE,
     *      ' '
         ENDIF
C
      ISTAT=0
C
C  CHECK IF COMMON BLOCK FILLED
      IF (IDTFIL.EQ.1) GO TO 10
C
C  FILL DATA TYPE COMMON BLOCK
      DTSRCH=SRCH
      CALL UDTYPE (SRCH,MDTYPE,NDTYPE,
     *   DTCODE,DTDIMN,DTUNIT,IDTMIS,NDTVAL,DTTIME,NDTADD,IDTWRT,
     *   IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,40) IERR
         ISTAT=1
         GO TO 30
         ENDIF
C
10    IDTFIL=1
C
C  CHECK IF DIMENSION CODE IS VALID
      DO 20 I=1,NDTYPE
         IF (DTYPE.EQ.DTCODE(I)) THEN
            XTDIMN=DTDIMN(I)
            XTUNIT=DTUNIT(I)
            IXTMIS=IDTMIS(I)
            NXTVAL=NDTVAL(I)
            XTTIME=DTTIME(I)
            NXTADD=NDTADD(I)
            IXTWRT=IDTWRT(I)
            IF (ICMDBG.GT.0) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,*)
     *            ' DTYPE=',DTYPE,
     *            ' XTDIMN=',XTDIMN,
     *            ' XTUNIT=',XTUNIT,
     *            ' IXTMIS=',IXTMIS,
     *            ' NXTVAL=',NXTVAL,
     *            ' XTTIME=',XTTIME,
     *            ' NXTADD=',NXTADD,
     *            ' IXTWRT=',IXTWRT,
     *            ' '
               ENDIF
            GO TO 30
            ENDIF
20       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,50) DTYPE
      ISTAT=2
C
30    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UDTATR -',
     *      ' ISTAT=',ISTAT,
     *      ' '
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('+*** ERROR - IN UDTATR - ERROR READING DATA UNIT FILE. ',
     *   'UDTYPE STATUS CODE = ',I2,'.')
50    FORMAT ('+*** ERROR - IN UDTATR - ',A,' IS AN INVALID ',
     *   'DATA TYPE CODE.')
C
      END
C
C-----------------------------------------------------------------------
C
cAV eliminate multiple define on linux linking.  It already includes in ublock
ccAV      INCLUDE 'udtypb'

