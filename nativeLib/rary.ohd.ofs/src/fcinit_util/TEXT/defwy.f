C MODULE DEFWY
C-----------------------------------------------------------------------
C
      SUBROUTINE DEFWY
C
C  THIS ROUTINE CALLS THE DEFINE FILE STATEMENT FOR THE WATER
C        YEAR SCRATCH FILE.
C
C  ROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL - 3/1980
C
      common /CMFCINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'common/platform'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fprog'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/defwy.f,v $
     . $',                                                             '
     .$Id: defwy.f,v 1.4 2001/06/13 09:57:53 mgm Exp $
     . $' /
C    ===================================================================
C

C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER DEFWY'
C
C  CHECK ROUTINE HAS BEEN CALLED
      IF (IDEFWY.EQ.1) GO TO 20
C
C  SET COMMON BLOCK VALUES
      IRWY=10
      NXWY=1
      NTWY=300
C
      IF (PGMNAM(1:4) .EQ.'MCP3'.OR.PGMNAM(1:4).EQ.'OPT3' ) THEN
C     OPEN OUTPUT FILE
C     RECORD LENGTH (BYTES) = 31 DAYS * 24 HOURS * 4 BYTES/VALUE = 2976
         IRECL=2976
         IF (OPERSYS(1:6).EQ.'ULTRIX') THEN
            IRECL=IRECL/4
            ENDIF
         CALL OPFILE (' ','WY-SCRATCH ','DIRECT','SCRATCH',' ',
     *      IRECL,IRWY,IERR)
         IF ( IERR .ne. 0 ) THEN
            WRITE(IPR,10)
10    FORMAT ('0**ERROR** IN ROUTINE DEFWY - UNABLE TO OPEN ',
     *   'SCRATCH FILE')
            CALL ERROR
            CALL STOP
            ENDIF
         ENDIF

      IDEFWY=1
C
20    IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT DEFWY'
C
      RETURN
C
      END
