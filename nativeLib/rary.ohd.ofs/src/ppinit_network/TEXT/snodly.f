C MODULE SNODLY
C-----------------------------------------------------------------------
C
C  ROUTINE TO DETERMINE ALPHABETICAL ORDER FOR DAILY STATIONS.
C
      SUBROUTINE SNODLY (IALL,IOPTN,ISORT,LARRAY,ARRAY,NNWFLG,INWFLG,
     *   IUEND,ISTAT)
C
      CHARACTER*4 PPTYPE,PDTYPE
      CHARACTER*8 TYPERR
      CHARACTER*20 STRING
      INTEGER*2 IWORK(1)
C
      DIMENSION ARRAY(LARRAY),INWFLG(NNWFLG)
      DIMENSION UNUSED(10)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/sntwkx'
      INCLUDE 'pdbcommon/pddtdr'
C
      EQUIVALENCE (SWORK(1),IWORK(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/snodly.f,v $
     . $',                                                             '
     .$Id: snodly.f,v 1.5 2003/08/11 20:31:26 wkwock Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('NTWK','NTWKDLY ','SNODLY  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SNODLY'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('NTWK','NTWKDLY ','SNODLY  ',LDEBUG)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' LARRAY=',LARRAY,
     *      ' ISORT=',ISORT,
     *      ' IALL=',IALL,
     *      ' IOPTN=',IOPTN,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NUMERR=0
C
      WRITE (LP,200)
      CALL SULINE (LP,1)
      IF (ISORT.EQ.1) THEN
         WRITE (LP,210)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISORT.EQ.2) THEN
         WRITE (LP,220)
         CALL SULINE (LP,2)
         ENDIF
C
      UNSD=-999.
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
      ICKRGN=0
      MINRGN=0
      ICKCPU=1
      MINCPU=0
      MINCPU=10
      IPRERR=1
      IPUNIT=LP
      TYPERR='ERROR'
      INCLUDE 'clugtres'
      IF (IERR.NE.0) THEN
         CALL SUFATL
         IUEND=1
         GO TO 170
         ENDIF
C
C  LOAD NETWORK COMMON BLOCK WITH SORTED LIST
      CALL SNSORT (ISORT,LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 170
C
      CALL SUDOPN (1,'PPP ',IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (IABS(IALL).EQ.1) GO TO 10
      IF (IOPTN.NE.8) GO TO 40
C
10    PPTYPE='OP24'
      IF (INWFLG(12).EQ.0) THEN
         IF (IOPTN.EQ.0) GO TO 40
         IF (IOPTN.EQ.9) THEN
            WRITE (LP,230) PPTYPE
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  GET PPDB POINTERS FOR STATIONS WITH ONLY 24-HOUR PCPN DATA
      NSTA=0
      NADD=0
      DO 30 I=1,INWFIL
         IF (PP24NW(I).EQ.0) GO TO 30
         IF (PPVRNW(I).GT.0) GO TO 30
            NSTA=NSTA+1
            IF (NSTA.LE.LSWORK) GO TO 20
               IF (NADD.GT.0) GO TO 30
                  WRITE (LP,240) LSWORK
                  CALL SUERRS (LP,2,NUMERR)
                  NADD=NADD+1
                  GO TO 30
20          ILOC=PP24NW(I)
            JLOC=IABS(ILOC)
            IF (JLOC .GT. 99999 ) JLOC = JLOC-99999
            IWORK(NSTA) = JLOC

CCC         IWORK(NSTA)=IABS(ILOC)
CCC         IF (IWORK(NSTA).GT.99999) IWORK(NSTA)=IWORK(NSTA)-99999

30       CONTINUE
C
      IF (NADD.GT.0) THEN
         WRITE (LP,250) NADD
         CALL SULINE (LP,2)
         ENDIF
C
C  GET NUMBER OF STATION IN PREPROCESSOR DATA BASE
      PDTYPE='PP24'
      IDX=IPDCKD(PDTYPE)
      NPDPP24=IDDTDR(17,IDX)
      IDX=IPDCKD('PPVR')
      NPDPPVR=IDDTDR(17,IDX)
      NPDSTA=NPDPP24-NPDPPVR
      IF (NPDSTA.NE.NSTA) THEN
         STRING=' ONLY'
         WRITE (LP,260) STRING(1:LENSTR(STRING)),
     *      PDTYPE,NPDSTA,PPTYPE,NSTA
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
C  WRITE PARAMETERS
      IVODLY=1
      CALL SWODLY (PPTYPE,IVODLY,UNSD,ISORT,IWORK,NSTA,
     *   LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 40
C
      IF (LDEBUG.GT.0) THEN
         CALL SRODLY (PPTYPE,IPRERR,IVODLY,UNUSED,ISORT,LSWORK,
     *      IWORK,NUMSTA,LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) GO TO 40
            CALL SPODLY (PPTYPE,IVODLY,UNUSED,ISORT,IWORK,
     *      NUMSTA,LARRAY,ARRAY,IERR)
         ENDIF
C
      WRITE (LP,270) NSTA,PPTYPE
      CALL SULINE (LP,2)
C
      INWFLG(12)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (IABS(IALL).EQ.1) GO TO 50
      IF (IOPTN.NE.8) GO TO 80
C
50    PPTYPE='OPVR'
      IF (INWFLG(13).EQ.0) THEN
         IF (IOPTN.EQ.0) GO TO 80
         IF (IOPTN.EQ.9) THEN
            WRITE (LP,230) PPTYPE
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  GET  PPDB POINTERS FOR STATIONS WITH LESS THAN 24-HOUR PCPN DATA
      NSTA=0
      NADD=0
      DO 70 I=1,INWFIL
         IF (PPVRNW(I).EQ.0) GO TO 70
            NSTA=NSTA+1
            IF (NSTA.LE.LSWORK) GO TO 60
               IF (NADD.GT.0) GO TO 70
                  WRITE (LP,240) LSWORK
                  CALL SUERRS (LP,2,NUMERR)
                  NADD=NADD+1
                  GO TO 70
60          ILOC=PPVRNW(I)
            IWORK(NSTA)=IABS(ILOC)
70       CONTINUE
C
      IF (NADD.GT.0) THEN
         WRITE (LP,250) NADD
         CALL SULINE (LP,2)
         ENDIF
C
C  GET NUMBER OF STATION IN PREPROCESSOR DATA BASE
      PDTYPE='PPVR'
      IDX=IPDCKD(PDTYPE)
      NPDSTA=IDDTDR(17,IDX)
      IF (NPDSTA.NE.NSTA) THEN
         STRING=' '
         WRITE (LP,260) STRING(1:LENSTR(STRING)),
     *      PDTYPE,NPDSTA,PPTYPE,NSTA
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
C  WRITE PARAMETERS
      IVODLY=1
      CALL SWODLY (PPTYPE,IVODLY,UNSD,ISORT,IWORK,NSTA,
     *   LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 80
C
      IF (LDEBUG.GT.0) THEN
         CALL SRODLY (PPTYPE,IPRERR,IVODLY,UNUSED,ISORT,LSWORK,
     *      IWORK,NUMSTA,LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) GO TO 80
         CALL SPODLY (PPTYPE,IVODLY,UNUSED,ISORT,IWORK,
     *      NUMSTA,LARRAY,ARRAY,IERR)
         ENDIF
C
      WRITE (LP,270) NSTA,PPTYPE
      CALL SULINE (LP,2)
C
      INWFLG(13)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (IABS(IALL).EQ.1) GO TO 90
      IF (IOPTN.NE.8) GO TO 120
C
90    PPTYPE='OT24'
      IF (INWFLG(14).EQ.0) THEN
         IF (IOPTN.EQ.0) GO TO 120
         IF (IOPTN.EQ.9) THEN
            WRITE (LP,230) PPTYPE
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  GET PPDB POINTERS FOR TEMP STATIONS
      NSTA=0
      NADD=0
      DO 110 I=1,INWFIL
         IF (TA24NW(I).EQ.0) GO TO 110
            NSTA=NSTA+1
            IF (NSTA.LE.LSWORK) GO TO 100
               IF (NADD.GT.0) GO TO 110
                  WRITE (LP,240) LSWORK
                  CALL SUERRS (LP,2,NUMERR)
                  NADD=NADD+1
                  GO TO 110
100         ILOC=TA24NW(I)
            IWORK(NSTA)=IABS(ILOC)
110      CONTINUE
C
      IF (NADD.GT.0) THEN
         WRITE (LP,250) NADD
         CALL SULINE (LP,2)
         ENDIF
C
C  GET NUMBER OF STATION IN PREPROCESSOR DATA BASE
      PDTYPE='TM24'
      IDX=IPDCKD(PDTYPE)
      NPDSTA=IDDTDR(17,IDX)
      IF (NPDSTA.NE.NSTA) THEN
         STRING=' '
         WRITE (LP,260) STRING(1:LENSTR(STRING)),
     *      PDTYPE,NPDSTA,PPTYPE,NSTA
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
C  WRITE PARAMETERS
      IVODLY=1
      CALL SWODLY (PPTYPE,IVODLY,UNSD,ISORT,IWORK,NSTA,
     *   LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 120
C
      IF (LDEBUG.GT.0) THEN
         CALL SRODLY (PPTYPE,IPRERR,IVODLY,UNUSED,ISORT,LSWORK,
     *      IWORK,NUMSTA,LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) GO TO 120
         CALL SPODLY (PPTYPE,IVODLY,UNUSED,ISORT,IWORK,
     *      NUMSTA,LARRAY,ARRAY,IERR)
         ENDIF
C
      WRITE (LP,270) NSTA,PPTYPE
      CALL SULINE (LP,2)
C
      INWFLG(14)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (IABS(IALL).EQ.1) GO TO 130
      IF (IOPTN.NE.8) GO TO 160
C
130   PPTYPE='OE24'
      IF (INWFLG(15).EQ.0) THEN
         IF (IOPTN.EQ.0) GO TO 160
         IF (IOPTN.EQ.9) THEN
            WRITE (LP,230) PPTYPE
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  GET PPDB POINTERS FOR PE STATIONS
      NSTA=0
      NADD=0
      DO 150 I=1,INWFIL
         IF (EA24NW(I).EQ.0) GO TO 150
            NSTA=NSTA+1
            IF (NSTA.LE.LSWORK) GO TO 140
               IF (NADD.GT.0) GO TO 150
                  WRITE (LP,240) LSWORK
                  CALL SUERRS (LP,2,NUMERR)
                  NADD=NADD+1
                  GO TO 150
140         ILOC=EA24NW(I)
            IWORK(NSTA)=IABS(ILOC)
150      CONTINUE
C
      IF (NADD.GT.0) THEN
         WRITE (LP,250) NADD
         CALL SULINE (LP,2)
         ENDIF
C
C  GET NUMBER OF STATION IN PREPROCESSOR DATA BASE
      PDTYPE='EA24'
      IDX=IPDCKD(PDTYPE)
      NPDSTA=IDDTDR(17,IDX)
      IF (NPDSTA.NE.NSTA) THEN
         STRING=' '
         WRITE (LP,260) STRING(1:LENSTR(STRING)),
     *      PDTYPE,NPDSTA,PPTYPE,NSTA
         CALL SUWRNS (LP,2,-1)
         ENDIF
C
C  WRITE PARAMETERS
      IVODLY=1
      CALL SWODLY (PPTYPE,IVODLY,UNSD,ISORT,IWORK,NSTA,
     *   LARRAY,ARRAY,IERR)
      IF (IERR.GT.0) GO TO 160
C
      IF (LDEBUG.GT.0) THEN
         CALL SRODLY (PPTYPE,IPRERR,IVODLY,UNUSED,ISORT,LSWORK,
     *      IWORK,NUMSTA,LARRAY,ARRAY,IERR)
         IF (IERR.GT.0) GO TO 160
         CALL SPODLY (PPTYPE,IVODLY,UNUSED,ISORT,IWORK,
     *      NUMSTA,LARRAY,ARRAY,IERR)
        ENDIF
C
      WRITE (LP,270) NSTA,PPTYPE
      CALL SULINE (LP,2)
C
      INWFLG(15)=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,190) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
170   IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SNODLY : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT (' INWFLG=',20(I2,1X))
200   FORMAT (' ')
210   FORMAT ('0*--> DETERMINE DAILY STATION ALPHABETICAL ORDER ',
     *   'BY STATE AND STATION IDENTIFIER')
220   FORMAT ('0*--> DETERMINE DAILY STATION ALPHABETICAL ORDER ',
     *   'BY STATE AND STATION DESCRIPTION')
230   FORMAT ('0*** NOTE - NO ',A,' COMPUTATIONS NEED TO BE DONE.')
240   FORMAT ('0*** ERROR - IN SNODLY - MAXIMUM NUMBER OF STATIONS ',
     *   'THAT CAN BE STORED IN WORK ARRAY (',I5,') EXCEEDED.')
250   FORMAT (T15,I5,' ADDITIONAL ARRAY LOCATION NEEDED.')
260   FORMAT ('0*** WARNING - NUMBER OF STATIONS WITH',A,' ',A,' ',
     *   'DATA (',I4,') ',
     *   'DOES NOT EQUAL NUMBER OF STATION IN ',A,' ',
     *   'PARAMETER RECORD (',I4,').')
270   FORMAT ('0*** NOTE - ',I4,' STATIONS PROCESSED ',
     *   'FOR ',A,' PARAMETERS.')
C
      END
