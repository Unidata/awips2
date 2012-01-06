C MEMBER DAPRCMX
C****************************************************************
C          ROUTINE:     DAPRCMX - READS FILE LOOKING FOR OPTIONS
C
C                DATE:  971112
C              AUTHOR:  SHIRLEY F BARTASH - DATA SCIENCES INC
C           REWRITTEN:  D. ST. - HRL
C****************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DESCRIPTION
C
C          ICD      I    I    UNIT NUMBER FOR INPUT FILE
C           LP      I    I    UNIT NUMBER FOR OUTPUT MESSAGE
C       IPRINT      I    O    PRINT FLAG
C       ISTAT       I    O    READ/WRITE STATUS (0 IF NO ERRORS)
C****************************************************************
      SUBROUTINE DAPRCMX (ICD,LP,IPRINT,ISTAT)

      EXTERNAL       KKTRIM

      CHARACTER*20   LINE
      CHARACTER*3    XCHAR
      INTEGER        ICD,LP,IERR,IBEG,IEND,IPRINT,ISTAT

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_sub/RCS/daprcmx.f,v $
     . $',                                                             '
     .$Id: daprcmx.f,v 1.3 1997/12/31 20:08:19 page Exp $
     . $' /
C    ===================================================================
C

        ISTAT  = 0
        IPRINT = 0
        XCHAR  = 'NO'

C  If unit number ICD is defined, then read one line to see if option
C   "PRIN" is given.  If so set IPRINT = 1 to output the shef message

        IF (ICD .GT. 0) THEN

          READ(ICD,'(A)',IOSTAT=IERR) LINE
          IF (IERR .GT. 0) THEN

            ISTAT = IERR
            WRITE(LP,10,IOSTAT=IERR) IERR
   10       FORMAT(' ** WARNING ** IN DAPRCMX - Cannot access input',
     $             ' line from options file -',I6)

          ELSEIF (IERR .EQ. 0) THEN

CC          WRITE(LP,12,IOSTAT=IERR) LINE
CC 12       FORMAT(' >>>>>>>>>>  ',A20,2X,20('<'))

            CALL KKTRIM(LINE,IBEG,IEND)
            IF (LINE(IBEG:IEND) .EQ. 'PRIN') THEN
              IPRINT = 1
              XCHAR  = 'YES'
            ENDIF

          ENDIF

        ENDIF

C  Output options message

        WRITE(LP,14,IOSTAT=IERR) XCHAR
   14   FORMAT(' OPTIONS IN EFFECT : PRINT SHEFOUT FILE = ',A,/)

      RETURN
      END
