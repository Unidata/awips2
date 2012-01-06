C MEMBER PDBCTL
C  (from old member UXPDBCTL)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDBCTL (LDEBUG)
C
C          ROUTINE:  PDBCTL
C
C             VERSION:  1.0.0
C
C                DATE:  1-6-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PRINTS THE CONTENTS OF THE INDEX
C    AND RRS CONTROL RECORDS AND THE DAILY DATA TYPE DIRECTORY
C    RECORDS FOR THE PREPCESSOR DATA BASE.
C
C***********************************************************************
C
C         DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IPDSIF(16)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
      EQUIVALENCE (IPDSIF(1),NWDCTL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/pdbctl.f,v $
     . $',                                                             '
     .$Id: pdbctl.f,v 1.1 1995/09/17 19:08:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) THEN
         CALL ULINE (IOGDB,1)
         WRITE (IOGDB,60)
         ENDIF
C
C  PRINT INDEX FILE CONTROL RECORDS
      CALL ULINE (LP,7)
      WRITE (LP,70) (IPDSIF(I),I=1,16)
C
C  PRINT DAILY DATA CONTROL RECORDS
      CALL ULINE (LP,5)
      WRITE (LP,80)
      DO 10 I=1,NUMDDF
         CALL ULINE (LP,1)
         WRITE (LP,90) KPDDDF(I),(IPDDFC(J,I),J=1,2)
10       CONTINUE
C
C
C  PRINT DAILY DATA TYPE DIRECTORY RECORDS
      CALL ULINE (LP,5)
      WRITE (LP,100)
      DO 30 I=1,NMDTYP
         CALL UMEMOV (IDDTDR(8,I),IEDATE,1)
         CALL UMEMOV (IDDTDR(11,I),LDATE,1)
         CALL ULINE (LP,1)
         WRITE (LP,110) I,(IDDTDR(J,I),J=2,7),IEDATE,IDDTDR(10,I),
     *      LDATE,IDDTDR(13,I)
30       CONTINUE
      CALL ULINE (LP,4)
      WRITE (LP,120)
      DO 50 I=1,NMDTYP
         CALL ULINE (LP,1)
         WRITE (LP,130) I,(IDDTDR(J,I),J=2,3),(IDDTDR(J,I),J=14,21)
50       CONTINUE
C
C  PRINT RRS FILE CONTROL RECORD
      CALL ULINE (LP,7)
      WRITE (LP,140) MXRRSF,LXRRSR,IFREE1,IFREEN,IFREEL,LUFREE,
     *   MXFRER,MAXRSZ
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,150)
         CALL ULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER PDBCTL')
70    FORMAT ('0- PREPROCESSOR DATA BASE INDEX FILE CONTROL ',
     *   'RECORDS - ' //
     *       4X,'NWDCTL=',I5,4X,'LRCPDD=',I5,
     *       4X,'LRCPDR=',I5,4X,'LRCPDI=',I5 /
     *       4X,'MXDTYP=',I5,4X,'NMDTYP=',I5,
     *       4X,'IPTTYP=',I5,4X,'NHASHR=',I5 /
     *       4X,'IH8CHR=',I5,4X,'IHINRC=',I5,
     *       4X,'INFREC=',I5,4X,'MXSIFR=',I5 /
     *       4X,'LSTSIF=',I5,4X,'MXDDOD=',I5,
     *       4X,'MAXDDF=',I5,4X,'NUMDDF=',I5)
80    FORMAT ('0- PREPROCESSOR DATA BASE DAILY DATA FILE CONTROL ',
     *   'RECORDS - ' //
     *       4X,'FILE',3X,'MAX # RECORDS',3X,'LAST RECORD',
     *       ' USED' /
     *   ' ',3X,4('-'),3X,13('-'),3X,16('-'))
90    FORMAT (' ',5X,I2,7X,I6,12X,I6)
100   FORMAT ('0- PREPROCESSOR DATA BASE DAILY DATA TYPE DIRECTORRY ',
     *   'RECORDS - ' //
     *   4X,'DATA TYPE',3X,'FILE',3X,'# OF POINTERS',
     *      3X,'# OF VALUES',3X,'MAXDAYS',3X,'EARLIEST JDATE',
     *      3X,'REC # OF EDATA',3X,'LATEST JDATE',3X,'REC # OF LDATA' /
     *   ' ',3X,9('-'),3X,4('-'),3X,13('-'),3X,11('-'),3X,
     *       7('-'),3X,14('-'),3X,14('-'),3X,12('-'),3X,14('-'))
110   FORMAT (1X,I2,3X,2A2,6X,I2,10X,I2,13X,I2,6X,I6,7X,I8,9X,I6,
     *   11X,I8,10X,I6)
120   FORMAT ('0',68X,'LST USED',5X,'LST USED' /
     *   ' ',3X,'DATA TYPE',3X,
     *       'REC # OF PNTREC',3X,'# FRST DATAREC',3X,'MAXSTA',3X,
     *       'NUMSTA',3X,'WRD PNTREC',3X,
     *       'WRD DATAREC',3X,'# STATS STA-TYPE',3X,'# REC 1DAY' /
     *   ' ',3X,9('-'),3X,15('-'),3X,14('-'),3X,6('-'),3X,
     *       6('-'),3X,10('-'),3X,11('-'),3X,16('-'),3X,10('-'))
130   FORMAT (1X,I2,3X,2A2,10X,I6,14X,I6,5X,I6,3X,I6,6X,I6,8X,I6,
     *   8X,I6,12X,I6)
140   FORMAT ('0- PREPROCESSOR DATA BASE RRS FILE CONTROL ',
     *   'RECORD - ' //
     *        4X,'MXRRSF=',I5,4X,'LXRRSR=',I5 /
     *        4X,'IFREE1=',I5,4X,'IFREEN=',I5 /
     *        4X,'IFREEL=',I5,4X,'LUFREE=',I5 /
     *        4X,'MXFRER=',I5,4X,'MAXRSZ=',I5)
150   FORMAT (' *** EXIT PDBCTL')
C
      END
