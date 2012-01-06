C MODULE URCPYA
C-----------------------------------------------------------------------
C
      SUBROUTINE URCPYA (LIWORK,IWORK,ISTAT)
C
C  THIS ROUTINE COPIES THE PARAMETER RECORDS FOR THOSE PARAMETER TYPES
C  THAT ARE NOT REORDERED.
C
C  THE COMPUTATIONAL PARAMETERS MPCO, MPFO AND FMPO ARE CREATED IN THE
C  NEW FILES WITH THE PPINIT COMPUATIONAL ORDER ROUTINES.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE  I/O   DIM    DESCRIPTION
C       ------   ----  ---   ---    -----------
C       LWORK      I    I     1     LENGTH OF ARRAY WORK
C       IWORK      I   I/O  LIWORK  WORK ARRAY
C       ISTAT      I    O     1     STATUS CODE:
C                                     0=NORMAL RETURN
C                                     >0=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urxctl'
C
      CHARACTER*4 XPTYPE
      DIMENSION IWORK(LIWORK)
      PARAMETER (NPARM=16)
      INTEGER IPARM(NPARM)
     *        /4hPCPN,4hTEMP,4hPE  ,4hRRS ,4hGENL,
     *         4hORRS,4hOG24,
     *         4hCHAR,4hMMMT,
     *         4hMPCO,4hMPFO,4hFMPO,
     *         4hMAPS,4hMAP ,4hMAT ,4hMAPE/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urcpya.f,v $
     . $',                                                             '
     .$Id: urcpya.f,v 1.2 1999/07/07 11:30:32 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPPTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,50)
         ENDIF
C
      ISTAT=0
C
      DO 40 I=1,NUMPTP
C     CHECK IF TYPE NOT TO BE PROCESSED
         DO 10 K=1,NPARM
            IF (JPDTDR(1,I).EQ.IPARM(K)) GO TO 40
10          CONTINUE
         CALL UMEMOV (JPDTDR(1,I),XPTYPE,1)
C     CHECK IF TYPE NOT IN OLD FILES
         IAMORD=0
         IDX=IPCKDT(XPTYPE)
         IF (IDX.EQ.0) THEN
            CALL SULINE (LP,2)
            WRITE (LP,60) XPTYPE
            GO TO 40
            ENDIF
C     CHECK IF ANY RECORDS DEFINED
         IF (IPDTDR(5,IDX).EQ.0) THEN
            CALL SULINE (LP,2)
            WRITE (LP,70) XPTYPE
            GO TO 40
            ENDIF
         LARAY=1
         CALL URCPYR (XPTYPE,LIWORK,IWORK,
     *      LARAY,ARAY,LARAY,ARAY,LARAY,ARAY,LARAY,ARAY,ISTAT)
40       CONTINUE
C
      IF (IPPTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,80) ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER URCPYA')
60    FORMAT ('0*** NOTE - PARAMETER TYPE ',A4,' NOT FOUND IN OLD ',
     *   'PREPROCESSOR PARAMETRIC DATA BASE.')
70    FORMAT ('0*** NOTE - NO ',A4,' PARAMETER RECORDS FOUND IN OLD ',
     *   'PREPROCESSOR PARAMETRIC DATA BASE.')
80    FORMAT (' *** EXIT URCPYA : ISTAT=',I2)
C
      END
