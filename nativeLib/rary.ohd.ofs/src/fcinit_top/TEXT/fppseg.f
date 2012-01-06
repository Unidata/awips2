C MEMBER FPPSEG
C  (from old member FCFPPSEG)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/14/95.11:20:18 BY $WC21DT
C
C @PROCESS LVL(77)
C
C
      SUBROUTINE FPPSEG (IPTYPE)
C
C  MAIN ROUTINE FOR PRINTING OR PUNCHING PARAMETERS AND/OR CARRYOVER
C  FOR SEGMENTS, FORECAST GROUPS OR CARRYOVER GROUPS.
C
C  ARGUMENT LIST:
C     IPTYPE - PRINT/PUNCH INDICATOR
C               0=PUNCH SEGMENT DEFINITIONS
C               1=PRINT SEGMENT DEFINITIONS
C
      PARAMETER (MCARD=30)
C
      CHARACTER*5 OTYPE
      CHARACTER*8 PTYPE,OLDNAM
      CHARACTER*80 NCARD(MCARD),IBUF,WORD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fppseg.f,v $
     . $',                                                             '
     .$Id: fppseg.f,v 1.3 1996/05/07 11:14:12 page Exp $
     . $' /
C    ===================================================================
C
      INCLUDE 'common/where'
      INCLUDE 'common/fconit'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
C
C
      IF (ITRACE.GE.1)  WRITE (IODBUG,10)
10    FORMAT (' *** ENTER FPPSEG')
C
      CALL UMEMOV (OPNAME,OLDNAM,2)
      CALL UMEMOV ('FPPSEG  ',OPNAME,2)
      IOLDOP=IOPNUM
      IOPNUM=0
C
      IBUG=IFBUG('PSEG')
C
      IF (IPTYPE.EQ.0) THEN
         OTYPE='PUNCH'
         PTYPE='PUNCHSEG'
         ENDIF
      IF (IPTYPE.EQ.1) THEN
         OTYPE='PRINT'
         PTYPE='PRINTSEG'
         ENDIF
C
      IF (OTYPE.EQ.'PRINT') WRITE (IPR,20)
20    FORMAT ('0',80('*') /
     *   ' *',78X,'*' /
     *   ' *',21X,'SEGMENT PARAMETER/CARRYOVER DISPLAY ',21X,'*' /
     *   ' *',78X,'*' /
     *   1H ,80('*') //)
      IF (OTYPE.EQ.'PUNCH') WRITE (IPR,30)
30    FORMAT ('1',80('*') /
     *   ' *',78X,'*' /
     *   ' *',27X,'SEGMENT DEFINITION PUNCH',27X,'*' /
     *   ' *',78X,'*' /
     *   ' ',80('*') //)
C
C  READ INPUT CARDS
      CALL CDINPT (NCARD,MCARD,LASTCD,PTYPE,IERR)
      IF (IERR.GT.0) GO TO 170
C
      ICARD=0
C
      IF (OTYPE.EQ.'PUNCH') GO TO 120
C
C  PROCESS INPUT CARDS
40    ICARD=ICARD+1
      IF (ICARD.GT.LASTCD) GO TO 170
C
      CALL UMEMOV (NCARD(ICARD),IBUF,20)
C
C  GET FIRST FIELD
      NSCAN=1
      CALL USCAN2 (IBUF,' ',NSCAN,WORD,LWORD,IERR)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'WORD=',WORD
C
      IF (WORD(1:1).EQ.'$') GO TO 40      
C
      IF (WORD.NE.'PRINT') THEN
50       WRITE (IPR,60) IBUF
60    FORMAT ('0**ERROR** FIRST FIELD ON FIRST CARD MUST BE ',
     *   '''PRINT'':'  /
     *   1X,A)
         CALL ERROR
         GO TO 130
         ENDIF
C
C  GET SECOND FIELD
      NSCAN=2
      CALL USCAN2 (IBUF,' ',NSCAN,WORD,LWORD,IERR)
      IF (IBUG.GE.1) WRITE( IODBUG,*) 'WORD=',WORD
C
C  CHECK FOR VALID KEYWORD
      IF (WORD(1:1).EQ.'$') GO TO 90
      IF (WORD.EQ.'CARRYOVER') GO TO 100
      IF (WORD.EQ.'CARRY') GO TO 100
      IF (WORD.EQ.'CO') GO TO 100
      IF (WORD.EQ.'PARAMETERS') GO TO 110
      IF (WORD.EQ.'PARMS') GO TO 110
      IF (WORD.EQ.'P') GO TO 110
C
70    WRITE (IPR,80) IBUF
80    FORMAT ('0**ERROR** SECOND FIELD ON CARD MUST BE EITHER ',
     *   '''PARAMETERS'' OR ''CARRYOVER'':' /
     *   1X,A)
      CALL ERROR
      GO TO 130
C
C  COMMENT
90    GO TO 40
C
C  PRINT CARRYOVER
100   CALL FPTCRY (ICARD,LASTCD,NCARD,IER)
      IF (IER.GT.0) GO TO 130
      GO TO 40
C
C  PRINT PARAMETERS
110   CALL FPTPRM (ICARD,LASTCD,NCARD,IER)
      IF (IER.GT.0) GO TO 130
      GO TO 40
C
C  PUNCH PARAMETERS
120   CALL FPPPRM (IPTYPE,ICARD,LASTCD,NCARD,IER)
      IF (IER.GT.0) GO TO 130
      GO TO 40
C
C  SCAN FOR END CARD
130   WRITE (IPR,140) PTYPE
140   FORMAT ('0**NOTE** ',A,
     *   ' COMMAND TERMINATED DUE TO ERRORS. ',
     *   'SCANNING FOR END CARD. ',
     *   'THE FOLLOWING CARDS WILL BE IGNORED:')
      ICARD=ICARD+1
      DO 160 K=ICARD,LASTCD
         WRITE (IPR,150) NCARD(K)
150   FORMAT (' ',A)
160      CONTINUE
C
170   IOPNUM=IOLDOP
      CALL UMEMOV (OLDNAM,OPNAME,2)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,180)
180   FORMAT (' *** EXIT FPPSEG')
C
      RETURN
C
      END
