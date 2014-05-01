C MODULE SMPPDP
C-----------------------------------------------------------------------
C
C  ROUTINE TO OUTPUT PREPROCESSOR DATA BASE POINTERS.
C
      SUBROUTINE SMPPDP (LPNTRS,IPNTRS,NFLD,PDTYPE,NDELPT,NDELDT,ISTAT)
C
      CHARACTER*4 PDTYPE,PPTYPE
      CHARACTER*8 TYPMSG
      PARAMETER (MOPTN=8)
      CHARACTER*4 OPTN(8)/
     *   'ALL ','PP24','PPVR','TM24',
     *   'TAVR','TF24','EA24','    '/
      CHARACTER*20 CHAR,CHK
C
      INTEGER*2 IPNTRS(LPNTRS)
C
      INCLUDE 'uio'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pppcommon/ppdtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/smppdp.f,v $
     . $',                                                             '
     .$Id: smppdp.f,v 1.4 2003/11/10 18:37:28 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SMPPDP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C      
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      ISTAT=0
C
      IENDIN=0
      ISTRT=0
      ILPFND=0
      IRPFND=0
      NUMERR=0
      NUMOPT=0
      IOPTN=0
      IALL=1
      IPRINTP=1
      IPRINTD=1
C
      NDELSL=0
      NDELDT=0     
C
C  CHECK IF ONLY DELETED POINTER SLOT INFORMATION TO BE PROCESSED
      IF (NFLD.EQ.0) THEN
         IENDIN=1
         IPRINTP=0
         IF (PDTYPE.NE.' ') THEN
            IALL=0
            IPRINTD=0
            DO 5 I=1,MOPTN
               IF (PDTYPE.EQ.OPTN(I)) THEN
                  IOPTN=I
                  GO TO 125
                  ENDIF
5              CONTINUE
            WRITE (LP,205) PDTYPE
            CALL SUERRS (LP,2,NUMERR)
            GO TO 175
            ENDIF
         GO TO 110
         ENDIF
C
C  PRINT HEADER LINE
      WRITE (LP,190)
      CALL SULINE (LP,2)
      WRITE (LP,200)
      CALL SULINE (LP,1)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DUMP PPDPTRS OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,CHAR,
     *   LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0)
     *   CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,250) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 100
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 100
C
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,210) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR OPTION
      IOPTN=0
      DO 60 I=1,MOPTN
         CALL SUCOMP (1,CHK,OPTN(I),IMATCH)
         IF (IMATCH.EQ.1) THEN
            IF (I.EQ.1) THEN
               IALL=1
               IF (NFLD.EQ.1) CALL SUPCRD
               GO TO 110
               ENDIF
            IOPTN=I
            IF (IOPTN.GE.2.AND.IOPTN.LE.7) IALL=0
            GO TO 70
            ENDIF
60       CONTINUE
C
C  CHECK FOR GROUP
      CALL SUIDCK ('DUMP',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.0) GO TO 70
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,260) CHAR
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 100
C
70    IF (NFLD.EQ.1) CALL SUPCRD
      ILPFND=0
      IRPFND=0
C
      IF (IOPTN.EQ.0) THEN
         WRITE (LP,220) CHAR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
C
      GO TO (110,110,110,110,110,110,110,90),IOPTN
C
90    WRITE (LP,230) OPTN(IOPTN)
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C  CHECK IF ANY TYPES FOUND AND END OF OPTION FOUND
100   IF (NUMOPT.GT.0) GO TO 170
         IENDIN=1
         WRITE (LP,240)
         CALL SULINE (LP,2)
         IALL=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   IF (IALL.NE.1) GO TO 120
      IOPTN=2
C
C  SET POINTER TYPE
120   PDTYPE=OPTN(IOPTN)
      IF (PDTYPE.EQ.' ') GO TO 160
C
C  GET POINTERS
125   TYPMSG='NOTE'
      CALL SUGTPT (PDTYPE,LPNTRS,IPNTRS,LPFILL,TYPMSG,IERR)
      IF (IERR.GT.0) GO TO 160
C
C  CHECK IF TO PRINT POINTERS
      IF (IPRINTP.GT.0) THEN
         IF (ISLEFT(10).GT.0) CALL SUPAGE
         CALL SUDPTR (PDTYPE,IPNTRS,LPFILL)
         ENDIF
C
C  FIND TYPE IN PREPROCESOR DATA BASE DIRECTORY
      IXPPD=IPDCKD(PDTYPE)       
C
C  SET NUMBER OF POINTERS PER STATION
      NPRSTA=IDDTDR(5,IXPPD)
C      
      IF (LDEBUG.GT.0) THEN               
         WRITE (LP,*)
     *      ' IXPPD=',IXPPD,
     *      ' NPRSTA=',NPRSTA,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C         
      IF (NPRSTA.EQ.0) GO TO 160
C
      PPTYPE=' '
      IF (PDTYPE.EQ.'PP24') PPTYPE='PCPN'
      IF (PDTYPE.EQ.'PPVR') PPTYPE='GENL'
      IF (PDTYPE.EQ.'TM24') PPTYPE='TEMP'
      IF (PDTYPE.EQ.'TAVR') GO TO 135
      IF (PDTYPE.EQ.'EA24') GO TO 135
      IF (PDTYPE.EQ.'EA24') PPTYPE='PE'
C
      IF (PPTYPE.EQ.' ') GO TO 135
C      
C  OPEN PREPROCESSOR PARAMETRIC DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) GO TO 160
C
C  FIND TYPE IN PREPROCESSOR PARAMETRIC DATA BASE DIRECTORY
      IXPPP=IPCKDT(PPTYPE)
      IF (IXPPP.EQ.0) THEN
         WRITE (LP,270) PPTYPE
         CALL SUWRNS (LP,2,-1)
         GO TO 135
         ENDIF
C
C  CHECK FOR INVALID PARAMETRIC RECORD NUMBER
      DO 130 I=1,LPFILL,NPRSTA
         IPTR=IPNTRS(I)
         IPTR=IABS(IPTR)
         IF (IPTR.EQ.0) GO TO 130
         IF (IPTR.GE.IPDTDR(3,IXPPP).AND.IPTR.LE.IPDTDR(4,IXPPP)) THEN
            ELSE
               WRITE (LP,280) PPTYPE,IPTR,PDTYPE,I,
     *            IPDTDR(3,IXPPP),IPDTDR(4,IXPPP)
               CALL SUWRNS (LP,2,-1)
            ENDIF
130      CONTINUE
C
C  CHECK FOR DELETED SLOTS
135   NCHK=0
      IF (PDTYPE.EQ.'PP24') NCHK=NPRSTA
      IF (PDTYPE.EQ.'PPVR') NCHK=1
      IF (PDTYPE.EQ.'TM24') NCHK=NPRSTA
      IF (PDTYPE.EQ.'TAVR') NCHK=1
      IF (PDTYPE.EQ.'TF24') NCHK=1
      IF (PDTYPE.EQ.'EA24') NCHK=1
      IF (NCHK.EQ.0) GO TO 160
      NDELSL=0
      NDELDT=0
      DO 150 I=1,LPFILL,NPRSTA
         DO 140 N=1,NCHK
            IF (IPNTRS(I+N-1).NE.0) GO TO 150
140         CONTINUE
C     SET NUMBER OF DELETED POINTER SLOTS
         NDELSL=NDELSL+1
C     SET NUMBER OF DELETED POINTER WORDS
         NPRSTA=IDDTDR(5,IXPPD)
         NDELPT=NDELPT+NPRSTA
C     SET NUMBER OF DELETED DATA WORDS        
         IF (IDDTDR(6,IXPPD).GT.0) THEN
            NDELDT=NDELDT+IDDTDR(6,IXPPD)
            ELSE
C           SET DATA TIME INTERVAL
               IF (PDTYPE.EQ.'PPVR') IOFSET=2
               IF (PDTYPE.EQ.'TAVR') IOFSET=1
               ITMINT=IPNTRS(I+IOFSET)
               IF (ITMINT.GT.0) THEN
                  NDELDT=NDELDT+24/ITMINT
                  ELSE
                     WRITE (LP,285) PDTYPE,ITMINT,I
                     CALL SUWRNS (LP,2,-1)
                  ENDIF                  
            ENDIF
150      CONTINUE
      IF (IPRINTD.EQ.1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,290) NDELSL,PDTYPE
         ENDIF
C
160   IF (IALL.EQ.1) THEN
         IOPTN=IOPTN+1
         IF (IOPTN.LE.MOPTN) GO TO 120
         ENDIF
C
      NUMOPT=NUMOPT+1
      IF (IENDIN.EQ.1) GO TO 170
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK NUMBER OF OPTIONS PROCESSED
170   IF (NUMOPT.EQ.0) THEN
         WRITE (LP,300)
         CALL SULINE (LP,2)
         ENDIF
C
C  CHECK NUMBER OF ERRORS ENCOUNTERED
      IF (NUMERR.GT.0) THEN
         WRITE (LP,310) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
175   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SMPPDP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT ('0*--> DUMP PREPROCESSOR DATA BASE POINTERS')
200   FORMAT (' ')
205   FORMAT ('0*** ERROR - IN SMPPDP - INVALID PPD POINTER TYPE : ',A)
210   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
220   FORMAT ('0*** ERROR - INVALID DUMP PPDPTRS OPTION : ',A)
230   FORMAT ('0*** ERROR - ERROR PROCESSING OPTION : ',A)
240   FORMAT ('0*** NOTE - NO PREPROCESSOR DATA TYPES FOUND. ',
     *   '''ALL'' IS ASSUMED.')
250   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
260   FORMAT (' CHAR = ',A)
270   FORMAT ('0*** WARNING - DATA TYPE ',A,' NOT FOUND IN ',
     *   'PARAMETRIC DATA BASE DIRECTORY. ',
     *   'RECORD NUMBERS WILL NOT BE CHECKED.')
280   FORMAT ('0*** WARNING - ',A,' PARAMETER RECORD NUMBER ',I5,
     *   ' AT ',A,' POINTER POSITION ',I5,' IS INVALID. ',
     *   'VALID VALUES ARE ',I5,' THRU ',I5,'.')
285   FORMAT ('0*** WARNING - ',A,' DATA TIME INTERVAL (',I5,
     *   ') AT POINTER AT POSITION ',I5,' IS NOT GREATER THAN ZERO.')
290   FORMAT ('0*** NOTE - ',I4,' DELETED SLOTS FOUND IN ',A,
     *   ' POINTER ARRAY.')
300   FORMAT ('0*** NOTE - NO DUMP PPDPTRS OPTIONS SUCCESSFULLY ',
     *   'PROCESSED.')
310   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED IN DUMP PPDPTRS ',
     *   'COMMAND.')
C
      END
