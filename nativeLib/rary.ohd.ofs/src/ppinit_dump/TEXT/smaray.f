C MODULE SMARAY
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINTING THE CONTENTS OF PARAMETER ARRAYS.
C
      SUBROUTINE SMARAY (LARRAY,ARRAY,NFLD,ISTAT)
C
      CHARACTER*4 DMPOPT
      PARAMETER (MOPTN=8)
      CHARACTER*4 OPTN(MOPTN)
     *   /'INT ','INT2','CHAR','REAL',
     *    'BOTH','    ','    ','    '/
      CHARACTER*8 TYPE,XTYPE,PARMID
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smaray.f,v $
     . $',                                                             '
     .$Id: smaray.f,v 1.2 1998/04/07 17:41:20 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,180)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' LARRAY=',LARRAY,
     *      ' NFLD=',NFLD,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C      
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NXTFLD=1
      NPRINT=0
      DMPOPT=OPTN(5)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,220) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 160
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 160
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,230) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
C      
      IF (NUMFLD.GT.1) GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK DUMP KEYWORD
      IF (CHK.NE.'ARRAY') THEN
         WRITE (LP,200) CHK(1:LENSTR(CHK))
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C         
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK DUMP OPTION
      IF (LLPAR.EQ.0) GO TO 80
      IF (LRPAR.GT.0) GO TO 60
         WRITE (LP,230) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
60    CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      DO 65 I=1,MOPTN
         IF (CHK.EQ.OPTN(I)) GO TO 70
65       CONTINUE
         WRITE (LP,240) NFLD,CHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 80
70    DMPOPT=CHK
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,*) 'DMPOPT=',DMPOPT
         CALL SULINE (LP,1)
         ENDIF
80    WRITE (LP,250) DMPOPT
      CALL SULINE (LP,2)
      GO TO 10
C
C  CHECK FOR KEYWORD
90    CALL SUIDCK ('DMPG',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 160
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      GO TO (100,110),NXTFLD
C
C  SET IDENTIFIER
100   PARMID=CHAR
      NXTFLD=2
      GO TO 10
C
C  SET PARAMETER TYPE
110   TYPE=CHAR
      IPTYPE=1
      MAXCHR=4
      IF (LENGTH.GT.MAXCHR) THEN
         WRITE (LP,270) LENGTH,CHAR(1:LENSTR(CHAR)),MAXCHR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
      IF (PARMID.EQ.'SNGL') THEN
         IPTYPE=2
         PARMID=' '
         ENDIF
C
C  READ PARAMETERS FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      XTYPE=TYPE
      IF (XTYPE.EQ.'STAN') THEN
         XTYPE='GENL'
         WRITE (LP,275) XTYPE,TYPE
         CALL SULINE (LP,2)
         ENDIF
      IF (XTYPE.EQ.'UGNL') THEN
         XTYPE='USER'
         WRITE (LP,275) XTYPE,TYPE
         CALL SULINE (LP,2)
         ENDIF
      IPTR=0
      CALL RPPREC (PARMID,XTYPE,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.GT.0) THEN
         CALL SRPPST (PARMID,XTYPE,IPTR,LARRAY,NFILL,IPTRNX,IERR)
         GO TO 150
         ENDIF
C
C  PRINT PARAMETER ARRAY
      CALL SUBLID (PARMID,IERR)
      WRITE (LP,280) XTYPE,PARMID,IPTR
      CALL SULINE (LP,2)
      CALL SUPDMP (XTYPE,DMPOPT,0,NFILL,ARRAY,ARRAY)
      NPRINT=NPRINT+1
C
150   NXTFLD=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF PARAMETERS PRINTED
160   IF (NPRINT.EQ.0) THEN
         WRITE (LP,290)
         CALL SULINE (LP,2)
         ENDIF
      IF (NPRINT.GT.0) THEN
         WRITE (LP,300) NPRINT
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,310)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SMARAY')
200   FORMAT ('0*** ERROR - IN SMARAY - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
220   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
230   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
240   FORMAT ('0*** ERROR - CARD FIELD ',I2,' HAS AN INVALID DUMP ',
     *   'OPTION : ',A)
250   FORMAT ('0DUMP ARRAY OPTION IN EFFECT = ',A)
270   FORMAT ('0*** ERROR - NUMBER OF CHARACTERS (',I2,
     *   ') IN SINGLE RECORD PARAMETER TYPE SPECIFIED (',A,
     *   ') EXCEEDS ',I2,'.')
275   FORMAT ('0*** NOTE - PARAMETER TYPE SET TO ',A,' ',
     *   'FOR SPECIFIED TYPE ',A,'.')
280   FORMAT ('0DUMP OF PARAMETER ARRAY :   TYPE=',A,5X,
     *   'IDENTIFIER=',A,5X,'RECORD NUMBER=',I5)
290   FORMAT ('0*** NOTE - NO  PARAMETER ARRAYS SUCCESSFULLY PRINTED.')
300   FORMAT ('0*** NOTE - ',I3,' PARAMETER ARRAYS ',
     *   'SUCCESSFULLY PRINTED.')
310   FORMAT (' *** EXIT SMARAY')
C
      END
