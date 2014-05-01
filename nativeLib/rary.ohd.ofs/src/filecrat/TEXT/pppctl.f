C MEMBER PPPCTL
C  (from old member UXPPPCTL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/05/94.09:46:44 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PPPCTL (LDEBUG)
C
C          ROUTINE:  PPPCTL
C
C             VERSION:  1.0.0
C
C                DATE:  12-9-1982
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE FORMATS AND PRINTS THE CONTENTS OF THE INDEX
C    AND PARAMETER FILES CONTROL RECORDS.  IT ALSO PRINTS THE
C    CONTENTS OF THE PARAMETER TYPE DATA DIRECTORY.
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IPPNDX(8)
      EQUIVALENCE (IPPNDX(1),MXPXRC)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/pppctl.f,v $
     . $',                                                             '
     .$Id: pppctl.f,v 1.1 1995/09/17 19:08:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) THEN
         CALL ULINE (IOGDB,1)
         WRITE (IOGDB,30)
         ENDIF
C
C  PRINT INDEX FILE INFORMATION
      CALL ULINE (LP,6)
      WRITE (LP,40) (IPPNDX(I),I=1,7)
C
C  PRINT PARAMETER CONTROL RECORDS
      CALL ULINE (LP,5)
      WRITE (LP,70)
      DO 10 I=1,NMPFIL
         CALL ULINE (LP,1)
         WRITE (LP,80) I,(IPMCTL(J,I),J=1,3),KPPRMU(I)
10       CONTINUE
C
C  PRINT DATA DIRECTORY RECORDS
      CALL ULINE (LP,5)
      WRITE (LP,50)
      DO 20 I=1,NMPTYP
         CALL ULINE (LP,1)
         WRITE (LP,60) I,(IPDTDR(J,I),J=1,6)
20       CONTINUE
C
      IF (IPPTR.GT.0) THEN
         CALL ULINE (IOGDB,1)
         WRITE (IOGDB,90)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER PPPCTL')
40    FORMAT ('0- PARAMETRIC DATA BASE INDEX FILE CONTROL RECORD -' //
     *       4X,'MXPXRC=',I6,4X,'MXPTYP=',I6 /
     *       4X,'NMPTYP=',I6,4X,'NMPFIL=',I6 /
     *       4X,'IPXRC1=',I6,4X,'USERPP=',2A4)
50    FORMAT ('0- PARAMETRIC DATA BASE DATA DIRECTORY RECORDS -' //
     *   4X,'TYPE',3X,'FILE',3X,'FIRST RECORD',3X,
     *       'LAST RECORD',3X,'# OF RECORDS',3X,'SPECIAL KEY' /
     *   4X,4('-'),3X,4('-'),3X,12('-'),3X,
     *       11('-'),3X,12('-'),3X,11('-'))
60    FORMAT (' ',I2,1X,A4,4X,I2,6X,I6,9X,I6,8X,I6,8X,I6)
70    FORMAT ('0- PARAMETRIC DATA BASE PARAMETER FILE CONTROL ',
     *   'RECORDS -' //
     *   ' ',4X,'FILE',3X,'MAX # RECORDS',3X,
     *       'LAST RECORD USED',3X,'# OF PARMS',3X,'UNIT #' /
     *   ' ',4X,4('-'),3X,13('-'),3X,
     *       16('-'),3X,10('-'),3X,6('-'))
80    FORMAT (5X,I2,7X,I6,12X,I6,10X,I6,9X,I2)
90    FORMAT (' *** EXIT PPPCTL')
C
      END
