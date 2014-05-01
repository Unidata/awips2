C  MODULE DFPRMAIN
C-----------------------------------------------------------------------
C    THIS IS THE MAIN DRIVER FOR THE PARSING OF THE SHEF DATA THAT
C    WILL BE ENCODED TO CREATE THE SHEFOUT FILE.  AN ADDITIONAL
C    COMMAND ALSO PROVIDES THE USER OPTION TO DUMP THE CONTENTS OF
C    THE NEWLY CREATED FILE.
C-----------------------------------------------------------------------
C
      SUBROUTINE DFPRMAIN_MAIN

      CHARACTER*12   TMS1,TMS2
      CHARACTER*4    KSHOUT(32)
      INTEGER        ICD,LP,LCHN,JCHN,LUPARM,NN,IERR,IPRINT
      INTEGER        NUM,NWAR,NERR

      INCLUDE 'updaio'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_main/RCS/dfprmain.f,v $
     . $',                                                             '
     .$Id: dfprmain.f,v 1.11 2004/10/27 19:21:15 hank Exp $
     . $' /
C    ===================================================================
C
      DATA  KSHOUT / 'ZCZC',31*'    ' /

C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()

C  Start cpu timer, set logical units and other i/o variables
C                    LP ....... unit number for output messages
C                    ICD ...... unit number for input control file
C                    LCHN ..... unit number for shef-input file
C                    JCHN ..... unit number for shefout file
C                    LUPARM ... unit number for SHEFPARM file

        UU = -1
        CALL UTIMF('SetBeg',' ',UU,NN)
        CALL UPRIMO_PARS (ICD,LP,LCHN,JCHN,LUPARM,IERR)
         IF (IERR .NE. 0) STOP 16

C  PRINT PAGE HEADER
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (LP)
      CALL USETO1 ('NOPAGHDR',IERR)

C  Read command card from program control file, then close the file

        CALL DAPRCMX (ICD,LP,IPRINT,IERR)
        CALL UPCLOS (ICD,' ',IERR)

C  Call the SHEF parsing routines

        CALL SHDRIV (LCHN,JCHN,LUPARM,LP,LP)
        CALL UTIMF('SetWriLapCpu',' un-tim        shdriv time is',UU,NN)

C  Write 'ZCZC' at end of shefout file

        WRITE (JCHN) KSHOUT

C  Print out a text version of the shefout file if IPRINT = 1

        IF (IPRINT.EQ.1) CALL DFOUT (JCHN,LP)

C  Get cpu time, number of warnings and errors, and output one liner
C  Print trailing message and cpu time (was in 'USTOP'), close files

        CALL SHERRK('N',NUM,NWAR,NERR)
        CALL UTIMF('SetObtTotCpu',TMS1,LP,NN)
        CALL UTIMF('AndObtTotRea',TMS2,LP,NN)

        WRITE(LP,'(/,1X,78(''*''))',IOSTAT=IERR)
        WRITE(LP,'(A,'' clock_tm'',A,'' cpu_tm'',I8,'' warnings'',
     $        I8,'' errors'')') TMS2,TMS1,NWAR,NERR
        WRITE(LP,'(  1X,78(''*''))',IOSTAT=IERR)

C  Close files, free lock, output total cpu time to log file

        CALL UPCLOS (LUPARM,' ',IERR)
        CALL UPCLOS (LCHN,' ',IERR)
        CALL UPCLOS (JCHN,' ',IERR)
        CALL UPCLOS (LP,' ',IERR)

CHDH SHEFPARS does not need locks.  (Hank Herr, 2004-07-20)
C        CALL FREE_OFS_LOCK(IERR)

        CALL UTIMF('SetWriTotCpu',' un-tim        total  time is',UU,NN)

      STOP
      END
