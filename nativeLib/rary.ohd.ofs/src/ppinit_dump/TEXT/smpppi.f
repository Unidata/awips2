C MEMBER SMPPPI
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/22/94.08:12:20 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO READ PREPROCESSOR PARAMETRIC INDEX ENTRIES.
C DESC: PPINIT COMMAND :  @DUMP PPPINDX
C
      SUBROUTINE SMPPPI (LARRAY,IARRAY,NFLD,ISTAT)
C
      CHARACTER*1 XASK/'*'/
      CHARACTER*4 CHAR(5),CHK(5)
      CHARACTER*8 XPRNT,XSORT,DPCHK
C
      DIMENSION IARRAY(5,1)
      DIMENSION IBUF(4)
      DIMENSION IFREQ(100)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppxctl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smpppi.f,v $
     . $',                                                             '
     .$Id: smpppi.f,v 1.1 1995/09/17 19:13:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      ISTAT=0
C
      ISTRT=-1
      LCHAR=5
      LCHK=5
      LDPCHK=2
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      ISORT=1
      IPRINT=1
      XSORT='SORT'
      XPRNT='PRINT'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR TYPE
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,180) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 70
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 70
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 30
         GO TO 40
30    IF (NFLD.EQ.1) CALL SUPCRD
      WRITE (LP,190) NFLD
      CALL SULINE (LP,2)
      ILPFND=0
      IRPFND=0
40    IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
      IF (NUMFLD.GT.1) GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK OPTION
      CALL SUBSTR (CHK,1,8,DPCHK,1)
      IF (DPCHK.NE.'PPPINDX') THEN
         WRITE (LP,160) CHK(1)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK DUMP TYPE
      IF (LLPAR.EQ.0) GO TO 10
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,190) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LDPCHK,DPCHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (DPCHK.EQ.'SORT'.OR.DPCHK.EQ.'NOSORT'.OR.DPCHK.EQ.'NOPRINT')
     *   GO TO 50
         WRITE (LP,200) NFLD,DPCHK
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
50    IF (DPCHK.EQ.'SORT'.OR.DPCHK.EQ.'NOSORT') XSORT=DPCHK
      IF (DPCHK.EQ.'NOPRINT') XPRNT='NOPRINT'
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,230) XSORT
         CALL SULINE (LP,1)
         ENDIF
      IF (XSORT.EQ.'NOSORT') ISORT=0
      IF (XSORT.EQ.'SORT') ISORT=1
      IF (XPRNT.EQ.'NOPRINT') IPRINT=0
      GO TO 10
C
C  CHECK FOR KEYWORD
60    CALL SUIDCK ('DMPG',CHK,NFLD,0,IKEYWD,IRIDCK)
      IF (IRIDCK.EQ.2) GO TO 70
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,170) CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  OPEN DATA BASE
70    CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) GO TO 140
C
C  CHECK IF NOPRINT AND SORT OPTIONS SPECIFIED
      IF (IPRINT.EQ.0.AND.ISORT.EQ.1) THEN
         XSORT='NOSORT'
         ISORT=0
         WRITE (LP,210) XSORT
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT OPTIONS IN EFFECT
      WRITE (LP,220) XPRNT,XSORT
      CALL SULINE (LP,2)
C
      NWORDS=5
      MAXID=LARRAY/NWORDS
C
80    NUMENT=0
      NUMADD=0
      NUMCHK=0
C
      MAXENT=MXPXRC-IPXRC1+1
      IOVFL=MAXENT*.87225+IPXRC1
      NOVFL=0
      NLOC=(MAXENT-1)/50+1
      IF (NLOC.GT.100) NLOC=100
      DO 90 I=1,NLOC
         IFREQ(I)=0
90       CONTINUE
C  READ INDEX ENTRIES
      DO 110 IREC=IPXRC1,MXPXRC
         CALL UREADT (KPPIDX,IREC,IBUF,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,270) IREC,KPPIDX
            CALL SUERRS (LP,2,NUMERR)
            GO TO 110
            ENDIF
         IF (IBUF(1).EQ.0.OR.IBUF(1).EQ.-1) GO TO 110
         NUMENT=NUMENT+1
         IF (IREC.GE.IOVFL) NOVFL=NOVFL+1
         ILOC=((IREC-1)*NLOC)/MAXENT+1
         IFREQ(ILOC)=IFREQ(ILOC)+1
         IF (ISORT.EQ.1) THEN
            IF (NUMENT.GT.MAXID) THEN
               IF (NUMADD.EQ.0) THEN
                  WRITE (LP,280) MAXID
                  CALL SUERRS (LP,2,NUMERR)
                  ENDIF
               NUMADD=NUMADD+1
               GO TO 110
               ENDIF
            CALL SUBSTR (IBUF,1,16,IARRAY(1,NUMENT),1)
            CALL SUBSTR (IREC,1,4,IARRAY(1,NUMENT),17)
            GO TO 110
            ENDIF
         IF (IPRINT.EQ.1) THEN
            IF (NUMENT.GT.1) GO TO 100
               WRITE (LP,240)
               CALL SULINE (LP,2)
               WRITE (LP,250)
               CALL SULINE (LP,3)
100         WRITE (LP,260) NUMENT,IBUF,IREC
            CALL SULINE (LP,1)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,250)
               CALL SULINE (LP,3)
               ENDIF
            GO TO 110
            ENDIF
C     READ PARAMETER RECORD
         IPTR=0
         CALL RPPREC (IBUF(1),IBUF(3),IPTR,LSWORK,SWORK,NFILL,IPTRNX,
     *      IERR)
         IF (IERR.GT.0) THEN
            CALL SRPPST (IBUF(1),IBUF(3),IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ENDIF
         NUMCHK=NUMCHK+1
110      CONTINUE
C
      IF (NUMADD.GT.0) THEN
         WRITE (LP,290) NUMENT,NUMADD
         CALL SULINE (LP,2)
         ISORT=0
         GO TO 80
         ENDIF
C
C  CHECK IF SORT OPTION SPECIFIED
      IF (ISORT.EQ.1) THEN
         ISPTR=0
         CALL SUSORT (NWORDS,NUMENT,IARRAY,IARRAY,ISPTR,IERR)
         IF (ISLEFT(10).GT.0) CALL SUPAGE
         WRITE (LP,240)
         CALL SULINE (LP,2)
         WRITE (LP,250)
         CALL SULINE (LP,3)
         DO 120 I=1,NUMENT
            CALL SUBLID (IARRAY(1,I),IERR)
            WRITE (LP,260) I,(IARRAY(J,I),J=1,NWORDS)
            CALL SULINE (LP,1)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,250)
               CALL SULINE (LP,3)
               ENDIF
120         CONTINUE
         ENDIF
C
C  PRINT HISTOGRAM
      WRITE (LP,300) MAXENT,NUMENT
      CALL SULINE (LP,2)
      WRITE (LP,310) IOVFL,NOVFL
      CALL SULINE (LP,2)
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,320)
      CALL SULINE (LP,2)
      WRITE (LP,330) (I,I=10,100,10)
      CALL SULINE (LP,4)
      DO 130 ILOC=1,NLOC
         IBEG=((ILOC-1)*MAXENT)/NLOC+1
         IEND=(ILOC*MAXENT)/NLOC
         NPLOT=IFREQ(ILOC)
         IF (NPLOT.LT.1) THEN
            WRITE (LP,340) IBEG,IEND
            CALL SULINE (LP,1)
            ENDIF
         IF (NPLOT.GT.0.AND.NPLOT.LT.101) THEN
            WRITE (LP,340) IBEG,IEND,(XASK,J=1,NPLOT)
            CALL SULINE (LP,1)
            ENDIF
         IF (NPLOT.GT.100) THEN
            WRITE (LP,350) IBEG,IEND,NPLOT
            CALL SULINE (LP,1)
            ENDIF
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,330) (I,I=10,100,10)
            CALL SULINE (LP,2)
            ENDIF
130      CONTINUE
C
      IF (NUMCHK.GT.0) THEN
         WRITE (LP,360) NUMCHK
         CALL SULINE (LP,2)
         ENDIF
      IF (ISORT.EQ.0) THEN
         WRITE (LP,370) NUMENT
         CALL SULINE (LP,2)
         ENDIF
      IF (ISORT.EQ.1) THEN
         WRITE (LP,380) NUMENT,MAXID
         CALL SULINE (LP,2)
         ENDIF
C
140   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,390)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (' *** ENTER SMPPPI')
160   FORMAT ('0*** ERROR - IN SMPPPI - ',A4,' IS AN INVALID OPTION.')
170   FORMAT (' INPUT FIELD = ',5A4)
180   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
190   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
200   FORMAT ('0*** ERROR - CARD FIELD ',I2,' HAS AN INVALID DUMP ',
     *   'PPPINDX OPTION : ',A)
210   FORMAT ('0*** NOTE - NOPRINT AND SORT OPTIONS BOTH SPECIFIED. ',
     *   'SORT OPTION SET TO ',A,'.')
220   FORMAT ('0DUMP PPPINDX OPTIONS IN EFFECT = ',A,3X,A)
230   FORMAT (' DUMP PPPINDX OPTION IS ',A8)
240   FORMAT ('0- DUMP OF PREPROCESSOR PARAMETRIC DATA BASE INDEX -')
250   FORMAT ('0',5X,1X,'ID      ',3X,'TYPE',3X,'PARM RECORD #',3X,
     *      'INDEX RECORD #' /
     *   ' ',5X,1X,8('-'),3X,4('-'),3X,13('-'),3X,14('-'))
260   FORMAT (1X,I5,1X,2A4,3X,A4,5X,I6,10X,I6)
270   FORMAT ('0*** ERROR - IN SMPPPI - DAIO ERROR AT RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
280   FORMAT ('0*** ERROR - IN SMPPPI - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
290   FORMAT ('0*** NOTE - ',I5,' ENTRIES ARE IN INDEX. ',I5,
     *   ' ENTRIES COULD NOT BE PROCESSED. SORT OPTION CANCELLED.')
300   FORMAT ('0*** NOTE - INDEX HAS ',I5,' RECORDS AND ',I5,
     *   ' ENTRIES.')
310   FORMAT ('0*** NOTE - INDEX OVERFLOW AREA BEGINS AT RECORD ',I5,
     *   ' AND HAS ',I5,' ENTRIES.')
320   FORMAT ('0',T40,'- FREQUENCY PLOT OF INDEX RECORDS -')
330   FORMAT ('0',3X,'RANGE OF',4X,'1',I9,9I10 /
     *   ' RECORD NUMBERS +---+',19('----+') / )
340   FORMAT (2X,I5,' -',I5,2X,100A1)
350   FORMAT (2X,I5,' -',I5,2X,'****  ',I5,' ENTRIES IN ',
     *   'THIS RANGE   ****')
360   FORMAT ('0*** NOTE - ',I5,' PARAMETER RECORDS CHECKED.')
370   FORMAT ('0*** NOTE - ',I5,' INDEX RECORDS PROCESSED.')
380   FORMAT ('0*** NOTE - ',I5,' INDEX RECORDS PROCESSED. A MAXIMUM ',
     *   'OF ',I5,' CAN BE PROCESSED.')
390   FORMAT (' *** EXIT SMPPPI')
C
      END
