C MODULE VDEBUG
C-----------------------------------------------------------------------
C
C  THIS ROUTINE SETS DEBUG AND TRACE VALUES FOR DEBUGGING FUNCTION MAPE.
C
C  INPUTS:
C    - DEBUG CODES RETURNED FROM HCL CALLS.
C
C  OUTPUTS:
C    - VALUES PLACED IN COMMON BLOCK NDBUG WHICH
C      CONTROL PRINTING OF DEBUG PRINT STATEMENTS IN THE
C      OTHER ROUTINES
C
      SUBROUTINE VDEBUG 
C
      CHARACTER*8 OLDOPN,TECHNM
      PARAMETER (LIARGS=80)
      DIMENSION IARGS(LIARGS)
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vdebug.f,v $
     . $',                                                             '
     .$Id: vdebug.f,v 1.4 2002/02/11 20:43:56 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLNK/4H    /
      DATA LALL/4HALL /
C
C
      IOPNUM=-1
      CALL FSTWHR ('VDEBUG  ',IOPNUM,OLDOPN,IOLDOP)
C
C
      IOUNT=0
      IF (IOUNT.EQ.1) THEN
C      
      TECHNM='DEBUGPR'
      CALL HPAST (TECHNM,IOPDBG,ISTAT)
      CALL FPHPWN (ISTAT,TECHNM)
      CALL FTEKCK (IOPDBG,TECHNM,6,IOERR,1,9)
C
      TECHNM='ERRORPR'
      CALL HPAST (TECHNM,IOERR,ISTAT)
      CALL FPHPWN (ISTAT,TECHNM)
      CALL FTEKCK (IOERR,TECHNM,9,IOERR,1,9)
C
      ENDIF
C
      TECHNM='PPTRACE'
      CALL HPAST (TECHNM,IPTRCE,ISTAT)
      CALL FTEKCK (IPTRCE,TECHNM,0,IPTRCE,0,1)
      CALL FPHPWN (ISTAT,TECHNM)
C
      TECHNM='PPDEBUG'
      CALL HPASTA (TECHNM,LIARGS,NVAL,NWORDS,IARGS,ISTAT)
      CALL FPHPWN (ISTAT,TECHNM)
      CALL FTEKCK (NVAL,TECHNM,0,NVAL,0,9)
      IF (ISTAT.GT.0) THEN
         DO 40 I=1,20
            PDBUG(I)=IBLNK
40          CONTINUE
         GO TO 75
         ENDIF
      NTIME=NWORDS/3
      DO 51 I=1,NTIME
         CALL HGTSTR (1,IARGS(1+(I-1)*3),PCODE,IFL,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (IPR,35) ISTAT
   35 FORMAT ('0**ERROR** STATUS OF ',I3,' RETURNED',
     1 ' FROM HGTSTR WHILE GETTING DEBUG CODES.')
            CALL ERROR
            GO TO 75
            ENDIF
         IF (PCODE.NE.BLNK) THEN
            NDBUG=NDBUG+1
            PDBUG(NDBUG)=PCODE
            IF (PDBUG(NDBUG).EQ.LALL) IPALL=1
            ENDIF
51       CONTINUE
C
   75 CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.0) WRITE(IOPDBG,*) 'EXIT VDEBUG'
C
      RETURN
C
      END
