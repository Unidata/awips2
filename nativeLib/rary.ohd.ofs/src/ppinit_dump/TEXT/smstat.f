C MODULE SMSTAT
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATISTICS.
C
      SUBROUTINE SMSTAT (LARRAY,ARRAY,SUMARY,SORT,NFLD,ISTAT)
C    
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      DIMENSION ARRAY(LARRAY)
      PARAMETER (MOPTN=8)
      CHARACTER*8 OPTN(MOPTN)
     *   /'NEWPAGE ','ALL     ','PCPN    ','RRS     ',
     *    'STATE   ','        ','        ','        '/
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smstat.f,v $
     . $',                                                             '
     .$Id: smstat.f,v 1.3 2001/06/13 14:03:00 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SMSTAT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' NFLD=',NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C    
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C      
      IALL=0
      NUMOPT=0
      NUMERR=0
      NUMWRN=0
      IENDIN=0
      ISTRT=1
C
C  PRINT CARD
      CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DUMP STATS OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (NFLD.EQ.-1) THEN
         IENDIN=1
         GO TO 180
         ENDIF
       IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,280) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.NE.0) THEN
         IENDIN=1
         GO TO 180
         ENDIF
C
C  CHECK FOR PARENTHESES
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
      IF (MOPTN.EQ.0) GO TO 70
C
C  CHECK FOR OPTION
      DO 50 IOPTN=1,MOPTN
         NCOMP=2
         CALL SUCOMP (NCOMP,CHK,OPTN(IOPTN),IMATCH)
         IF (IMATCH.EQ.1) GO TO 70
50       CONTINUE
C
C  CHECK FOR GROUP
      CALL SUIDCK ('DUMP',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.0) GO TO 60
         IENDIN=1
         GO TO 180
C
C  INVALID OPTION
60    WRITE (LP,230) CHK
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    GO TO (90,130,140,150,140,80,80,80),IOPTN
80       WRITE (LP,270) IOPTN
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
C
C  NEWPAGE OPTION
90    NEWPAG=0
      CALL SUNEWP (NFLD,ISTRT,CHK,LCHK,LLPAR,LRPAR,LENGTH,IRPFND,
     *   OPTN(IOPTN),NUMERR,NUMWRN,NEWPAG,IERR)
      GO TO 10
C
C  ALL OPTION
130   IALL=1
C
C  PRINT PREPROCESSOR DATA BASE DAILY STATION STATISTICS
140   CALL SMPPDD (LARRAY,ARRAY,SUMARY,SORT,NFLD,ISTRT,IERR)
      IF (IALL.EQ.0) GO TO 160
C
C  PRINT PREPROCESSOR DATA BASE RRS STATION STATISTICS
150   CALL SMPPDR (LARRAY,ARRAY,IERR)
C
C  CHECK FOR END OF INPUT
160   IF (IENDIN.EQ.1) GO TO 190
C
      NUMOPT=NUMOPT+1
      GO TO 10
C
C  CHECK NUMBER OF KEYWORDS FOUND
180   IF (NUMOPT.GT.0) GO TO 190
C
C  NO DUMP STATS OPTIONS FOUND
      WRITE (LP,290) (OPTN(I)(1:LENSTR(OPTN(I))),I=1,5),
     *   OPTN(2)(1:LENSTR(OPTN(2)))
      CALL SULINE (LP,2)
      GO TO 130
C
C  CHECK NUMBER OF ERRORS
190   IF (NUMERR.GT.0) THEN
         WRITE (LP,300) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SMSTAT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
230   FORMAT ('0*** ERROR - INVALID DUMP STATS OPTION : ',A)
270   FORMAT ('0*** ERROR - PROCESSING DUMP STATS OPTION NUMBER ',I2,
     *   '.')
280   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
290   FORMAT ('0*** NOTE - NO KEYWORD (',
     *   4(A,', '),'OR ',A,') WAS FOUND. ',A,' IS ASSUMED.')
300   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED BY DUMP STATS ',
     *   'COMMAND.')
C
      END
