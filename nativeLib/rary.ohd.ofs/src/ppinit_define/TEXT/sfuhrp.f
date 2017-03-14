C MODULE SFUHRP
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE USER HRAP GRID SUBSET PARAMETERS.
C
      SUBROUTINE SFUHRP (INULL,NFLD,NUMFLD,ITYPE,INTEGR,REAL,
     *   CHAR,ISTRT,LENGTH,LLPAR,LRPAR,
     *   DISP,PRNOTE,NOPFLD,NXTFLD,IOFLD3,IUNDEF,
     *   NHPSUB,NHPCHK,NHPOLD,
     *   IXGRD,NXA,NSEGS,IY,IXB,IXE,
     *   NUMERR,NUMWRN,ISTAT)
C
      CHARACTER*(*) CHAR
      CHARACTER*4 DISP,PRNOTE
      DIMENSION NHPSUB(*),NHPCHK(4),NHPOLD(*)
      DIMENSION NSEGS(*),IY(*),IXB(*),IXE(*)
C
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfuhrp.f,v $
     . $',                                                             '
     .$Id: sfuhrp.f,v 1.3 2003/06/03 14:40:25 xfan Exp $
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
      LDEBUG=ISBUG('UGNL')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NOPFLD=',NOPFLD,
     *      ' NXTFLD=',NXTFLD,
     *      ' IOFLD3=',IOFLD3,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' CHAR=',CHAR,
     *      ' ISTRT=',ISTRT,
     *      ' LENGTH=',LENGTH,
     *      ' LLPAR=',LLPAR,
     *      ' LRPAR=',LRPAR,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*) 'NHPCHK=',NHPCHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NLFLD=NXTFLD+1
      GO TO (10,40,60,80),NLFLD
      WRITE (LP,220) NXTFLD
      CALL SUERRS (LP,2,NUMERR)
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF OPTIONAL FIELD PREVIOUSLY PROCESSED
10    IF (IOFLD3.EQ.1) THEN
         WRITE (LP,260)
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
       IOFLD3=1
C
C  CHECK FOR NULL FIELD
      IF (INULL.EQ.1) THEN
         WRITE (LP,250) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
C
C  CHECK FOR CHARACTER DATA
      IF (ITYPE.NE.2) THEN
         WRITE (LP,240) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
C
C  CHECK FOR PARENTHESES IN FIELD
      IF (CHAR.EQ.'('.OR.LLPAR.EQ.LENGTH) GO TO 170

      IF (LLPAR.GT.0.AND.LRPAR.GT.0) THEN
         WRITE (LP,270) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         ENDIF
C
      IF (CHAR.EQ.')') THEN
         IF (NXTFLD.GT.0) GO TO 170
            WRITE (LP,280) NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 170
         ENDIF
C
      IF (LLPAR.EQ.0) GO TO 170
C
      IF (CHAR.EQ.'('.AND.LENGTH.EQ.1) GO TO 170
      IF (CHAR.EQ.')'.AND.LENGTH.EQ.1) GO TO 140
      IF (LRPAR.NE.1) GO TO 20
         IF (NXTFLD.GT.0) GO TO 170
            WRITE (LP,280) NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 140
20    IBEG=LLPAR+1
      IEND=LENGTH
      IF (LRPAR.GT.0) IEND=LRPAR-1
C
      CALL UFINFX (INTEGR,ISTRT,IBEG,IEND,IERR)
      IF (IERR.EQ.0) ITYPE=0
C
C  WESTERN MOST HRAP COLUMN
C
      IF (INULL.EQ.1) THEN
         NHPSUB(1)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,300) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 30
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,310) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
      NHPSUB(1)=INTEGR
C
cfan  bugr22-18
cfan  when NHPCHK(1) is negative (means extended HRAP area),
cfan  MAXCOL should be bigger than NHPCHK(2), not smaller than it. 
cfan      MAXCOL=NHPCHK(1)+NHPCHK(2)-1
C
      MAXCOL=IABS(NHPCHK(1))+NHPCHK(2)-1    !cfan bugr22-18
cfan
      IF (NHPSUB(1).GE.NHPCHK(1).AND.NHPSUB(1).LE.MAXCOL) GO TO 30
         WRITE (LP,320) NHPSUB(1),MAXCOL
         CALL SUWRNS (LP,2,NUMWRN)
         NHPSUB(1)=NHPCHK(1)
         WRITE (LP,400) NHPCHK(1)
         CALL SULINE (LP,1)
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,330) NHPSUB(1)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NUMBER OF HRAP COLUMNS
C
40    IF (INULL.EQ.1) THEN
         NHPSUB(2)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,300) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 50
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,310) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
      NHPSUB(2)=INTEGR
      IF (NHPSUB(2).GT.0.AND.NHPSUB(2).LE.NHPCHK(2)) GO TO 50
         WRITE (LP,340) NHPSUB(2),NHPCHK(2)
         CALL SUWRNS (LP,2,NUMWRN)
         NHPSUB(2)=NHPCHK(2)
         WRITE (LP,400) NHPCHK(2)
         CALL SULINE (LP,1)
50    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,350) NHPSUB(2)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SOUTHERN MOST HRAP ROW
C
60    IF (INULL.EQ.1) THEN
         NHPSUB(3)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,300) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 70
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,310) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
      NHPSUB(3)=INTEGR
      MAXROW=NHPCHK(3)+NHPCHK(4)-1
      IF (NHPSUB(3).GE.NHPCHK(3).AND.NHPSUB(3).LE.MAXROW) GO TO 70
         WRITE (LP,360) NHPSUB(3),MAXROW
         CALL SUWRNS (LP,2,NUMWRN)
         NHPSUB(3)=NHPCHK(3)
         WRITE (LP,400) NHPCHK(3)
         CALL SULINE (LP,1)
70    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,370) NHPSUB(3)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      GO TO 170
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NUMBER OF HRAP ROWS
C
80    IF (LRPAR.EQ.0) GO TO 90
         IF (LRPAR.EQ.LENGTH) GO TO 90
            WRITE (LP,290) NFLD
            CALL SUERRS (LP,2,NUMERR)
            GO TO 170
90    IF (ITYPE.EQ.0.OR.ITYPE.EQ.2) GO TO 110
         IF (LRPAR.NE.1) GO TO 100
100      IF (LRPAR.GT.1) GO TO 110
         WRITE (LP,230) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 130
110   IF (LLPAR.EQ.0) IBEG=1
      IF (LLPAR.GT.0) IBEG=LLPAR+1
      IF (LRPAR.EQ.0) IEND=LENGTH
      IF (LRPAR.GT.0) IEND=LRPAR-1
C
      CALL UFINFX (INTEGR,ISTRT,IBEG,IEND,IERR)
      IF (IERR.EQ.0) ITYPE=0
C
      IF (INULL.EQ.1) THEN
         NHPSUB(4)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,300) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 120
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,310) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 170
         ENDIF
      NHPSUB(4)=INTEGR
      IF (NHPSUB(4).GT.0.AND.NHPSUB(4).LE.NHPCHK(4)) GO TO 120
         WRITE (LP,380) NHPSUB(4),NHPCHK(4)
         CALL SUWRNS (LP,2,NUMWRN)
         NHPSUB(4)=NHPCHK(4)
         WRITE (LP,400) NHPCHK(4)
         CALL SULINE (LP,1)
120   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,390) NHPSUB(4)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
130   NXTFLD=0
      IF (LRPAR.EQ.0) GO TO 170
C
140   NXTFLD=0
      NOPFLD=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK HRAP SUBSET
C
      ICKOLD=0
      IF (DISP.EQ.'OLD'.AND.IXGRD.EQ.1.AND.ICKOLD.EQ.1) THEN
C     CHECK OLD HRAP SUBSET
         IF (NHPSUB(1).GT.NHPOLD(1)) THEN
            WRITE (LP,410)
     *          'WESTERN MOST COLUMN',NHPSUB(1),
     *        'GREATER THAN',NHPOLD(1)
            CALL SUWRNS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(2).LT.NHPOLD(2)) THEN
            WRITE (LP,410)
     *         'NUMBER COLUMNS',NHPSUB(2),
     *         'LESS THAN',NHPOLD(2)
            CALL SUWRNS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(3).GT.NHPOLD(3)) THEN
            WRITE (LP,410)
     *         'SOUTHERN MOST ROW', NHPSUB(3),
     *         'GREATER THAN',NHPOLD(3)
            CALL SUWRNS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(4).LT.NHPOLD(4)) THEN
            WRITE (LP,410)
     *         'NUMBER OF ROWS',NHPSUB(4),
     *         'LESS THAN',NHPOLD(4)
            CALL SUWRNS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
      IF (DISP.EQ.'OLD'.AND.IXGRD.EQ.1) THEN
C     CHECK IF ANY MAPX AREAS USE POINTS OUTSIDE NEW HRAP SUBSET
         IWC=NHPCHK(4)
         IEC=NHPCHK(3)
         ISR=NHPCHK(2)
         INR=NHPCHK(1)
         IPOS=1
         DO 160 I=1,NXA
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*)
     *            ' I=',I,
     *            ' NSEGS(I)=',NSEGS(I),
     *            ' '
               CALL SULINE (IOSDBG,1)
               ENDIF
            DO 150 N=1,NSEGS(I)
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,*)
     *               ' N=',N,
     *               ' IY(IPOS)=',IY(IPOS),
     *               ' IXB(IPOS)=',IXB(IPOS),
     *               ' IXE(IPOS)=',IXE(IPOS),
     *               ' '
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (IY(IPOS).LT.ISR) ISR=IY(IPOS)
               IF (IY(IPOS).GT.INR) INR=IY(IPOS)
               IF (IXB(IPOS).LT.IWC) IWC=IXB(IPOS)
               IF (IXE(IPOS).GT.IEC) IEC=IXE(IPOS)
               IPOS=IPOS+1
150            CONTINUE
160         CONTINUE
         NCOLS=IEC-IWC+1
         NROWS=INR-ISR+1
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*)
     *         ' IWC=',IWC,
     *         ' IEC=',IEC,
     *         ' ISR=',ISR,
     *         ' INR=',INR,
     *         ' NCOLS=',NCOLS,
     *         ' NROWS=',NROWS,
     *         ' '
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (NHPSUB(1).GT.IWC) THEN
            WRITE (LP,415)
     *          'WESTERN MOST COLUMN',NHPSUB(1),
     *        'GREATER THAN',IWC
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(2).LT.NCOLS) THEN
            WRITE (LP,415)
     *         'NUMBER COLUMNS',NHPSUB(2),
     *         'LESS THAN',NCOLS
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(3).GT.ISR) THEN
            WRITE (LP,415)
     *         'SOUTHERN MOST ROW', NHPSUB(3),
     *         'GREATER THAN',ISR
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (NHPSUB(4).LT.NROWS) THEN
            WRITE (LP,415)
     *         'NUMBER OF ROWS',NHPSUB(4),
     *         'LESS THAN',NROWS
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
      GO TO 175         
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   NXTFLD=NXTFLD+1
C
175   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,420)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SFUHRP')
220   FORMAT ('0*** ERROR - IN SFUHRP - INVALID VALUE OF NXTFLD (',I2,
     *   ').')
230   FORMAT ('0*** ERROR - NON-REAL DATA EXPECTED IN INPUT ',
     *   'FIELD ',I2,' (CARD FIELD ',I2,').')
240   FORMAT ('0*** ERROR - CHARACTER DATA EXPECTED IN INPUT ',
     *   'FIELD ',I2,' (CARD FIELD ',I2,').')
250   FORMAT ('0*** ERROR - NO VALUE FOUND FOR OPTIONAL INPUT FIELD ',
     *   I2,' (CARD FIELD ',I2,').')
260   FORMAT ('0*** WARNING - HRAP GRID SUBSET HAS ALREADY BEEN ',
     *   'DEFINED.')
270   FORMAT ('0*** ERROR - HRAP GRID SUBSET NOT ',
     *   'FOUND IN INPUT FIELD ',I2,' (CARD FIELD ',I2,').')
280   FORMAT ('0*** ERROR - HRAP GRID SUBSET NOT ',
     *   'FOUND IN INPUT FIELD ',I2,' (CARD FIELD ',I2,').')
290   FORMAT ('0*** ERROR - RIGHT PARENTHESES FOUND IN FIELD ',I2,
     *   ' BUT IS NOT LAST CHARACTER IN FIELD.')
300   FORMAT ('0*** NOTE - NO VALUE FOUND FOR REQUIRED FIELD ',I2,
     *   '. DEFAULT VALUE (',I8,') WILL BE USED.')
310   FORMAT ('0*** ERROR - INTEGER DATA EXPECTED IN INPUT FIELD ',
     *   I2,' (CARD FIELD ',I2,').')
320   FORMAT ('0*** WARNING - WESTERN MOST HRAP COLUMN (',I4,
     *   ') EXCEEDS MAXIMUM HRAP COLUMN ALLOWED (',I4,').')
330   FORMAT (' WESTERN HRAP COLUMN SET TO : ',I5)
340   FORMAT ('0*** WARNING - NUMBER OF HRAP COLUMNS (',I4,
     *   ') EXCEEDS MAXIMUM HRAP COLUMNS ALLOWED (',I4,').')
350   FORMAT (' NUMBER OF HRAP COLUMNS SET TO : ',I5)
360   FORMAT ('0*** WARNING - SOUTHERN MOST HRAP ROW (',I4,
     *   ') EXCEEDS MAXIMUM HRAP ROW ALLOWED (',I4,').')
370   FORMAT (' SOUTHERN HRAP ROW SET TO : ',I5)
380   FORMAT ('0*** WARNING - NUMBER OF HRAP ROWS (',I4,
     *   ') EXCEEDS MAXIMUM HRAP ROWS ALLOWED (',I4,').')
390   FORMAT (' NUMBER OF HRAP ROWS SET TO : ',I5)
400   FORMAT (T16,'THE DEFAULT VALUE (',I4,') WILL BE USED.')
410   FORMAT ('0*** WARNING - NEW VALUE FOR ',A,' (',I4,') ',
     *   'IS ',A,' THE OLD VALUE (',I4,').')
415   FORMAT ('0*** ERROR - NEW VALUE FOR ',A,' (',I4,') ',
     *   'IS ',A,' THAT USED BY MAPX AREAS (',I4,').')
420   FORMAT (' *** EXIT SFUHRP')
C
      END
