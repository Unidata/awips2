C MEMBER UDUNIT
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C
C  ROUTINE UDUNIT READS A FILE CONTAINING THE VALID NWSRFS DATA UNIT
C  CODES AND THE ATTRIBUTES ASSOCIATED WITH EACH UNIT.
C
      SUBROUTINE UDUNIT (MDIMN,MUNIT,DIMNS,UNITS,FACTS,TEMPS,NDECML,
     *   ISTAT)
C
C  INPUT ARGUMENTS:
C       MDIMN  - MAXIMIM DIMENSIONS THAT CAN BE PROCESSED
C       MUNIT  - MAXIMIM UNITS THAT CAN BE PROCESSED FOR EACH DIMENSION
C
C  OUTPUT ARGUMENTS:
C       DIMNS  - DIMENSION CODES
C       UNITS  - UNITS CODES
C       FACTS  - CONVERSION FACTORS
C       TEMPS  - TEMPERATURE CONVERSION FACTORS
C       NDECML - NUMBER OF DECIMAL PLACES TO BE USED IN PRINTOUT
C       ISTAT  - STATUS INDICATOR
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CHARACTER*4 XTYPE,XDIMN,XUNIT
      CHARACTER*8 DDNAME/' '/
      CHARACTER*8 TMBR,SEQNUM
      CHARACTER*8 MBR/'DATAUNIT'/
      CHARACTER*8 BLNK8/' '/
      CHARACTER*32 XDESC
      CHARACTER*80 REC
C
      REAL*8 CONV1,CONV2,FACTS,TEMPS
C
      CHARACTER*4 DIMNS(MDIMN),UNITS(MDIMN,MUNIT)
      DIMENSION FACTS(MDIMN,MUNIT),TEMPS(MUNIT),NDECML(MDIMN,MUNIT)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/udunit.f,v $
     . $',                                                             '
     .$Id: udunit.f,v 1.2 2002/02/11 16:34:32 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UDUNIT'
         ENDIF
C
      ISTAT=0
C
      NTUNIT=0
      NUMREC=0
C
      IROW=0
      ICOL=0
C
C  CHECK IF DATA UNIT FILE ALLOCATED
      IPRERR=0
      CALL UDDNST (DDNAME,LSYS,IPRERR,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,100) DDNAME
         ISTAT=2
         GO TO 90
         ENDIF
      TMBR=MBR
      IPRERR=1
C
C  READ RECORD
10    CALL URDPDS (DDNAME,TMBR,IPRERR,REC,LRECL,NUMREC,IFLAG,IRCPDS)
      IF (IRCPDS.EQ.0.OR.IRCPDS.EQ.2) GO TO 20
         CALL UEROR (LP,1,-1)
         WRITE (LP,110) IRCPDS
         ISTAT=1
         GO TO 50
C
20    SEQNUM=REC(73:80)
      IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' LRECL=',LRECL,
     *      ' REC(1:LRECL)=',REC(1:LRECL),
     *      ' '
         ENDIF
C
C  CHECK FOR BLANK RECORD
      IF (REC(1:72).EQ.' ') THEN
         IF (IRCPDS.EQ.2) GO TO 50
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMENT CARD
      CALL UCKCMT (REC(1:72),'$*',IRETRN)
      IF (IRETRN.NE.0) THEN
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' SEQNUM=',SEQNUM,
     *         ' REC(1:72)=',REC(1:72),
     *         ' '
            ENDIF
         IF (IRCPDS.EQ.2) GO TO 50
         GO TO 10
         ENDIF
C
C  MOVE DATA FROM RECORD INTO VARIABLES
      XDIMN=REC(1:1+LEN(XDIMN)-1)
      XTYPE=REC(6:6+LEN(XTYPE)-1)
      XUNIT=REC(11:11+LEN(XUNIT)-1)
      XDESC=REC(17:17+LEN(XDESC)-1)
C
C  DECODE NUMBER OF DECIMAL PLACES
      IBEG=53
      NCHAR=1
      IPRERR=1
      CALL UFA2I (REC,IBEG,NCHAR,NDEC,IPRERR,LP,IERR1)
      IF (IERR1.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,120) REC(IBEG:IBEG+NCHAR-1)
         ENDIF
C
C  DECODE CONVERSION FACTORS
      CONV1=-999.
      IDEC=0
      IBEG=55
      NCHAR=10
      IPRERR=1
      CALL UFA2F8 (REC,IBEG,NCHAR,IDEC,CONV1,IPRERR,LP,IERR2)
      IF (IERR2.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,130) REC(IBEG:IBEG+NCHAR-1)
         ENDIF
      CONV2=-999.
      IDEC=0
      IBEG=65
      NCHAR=8
      IF (REC(IBEG:IBEG+1).EQ.'. ') THEN
         IERR3=0
         GO TO 30
         ENDIF
      IPRERR=1
      CALL UFA2F8 (REC,IBEG,NCHAR,IDEC,CONV2,IPRERR,LP,IERR3)
      IF (IERR3.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,130) REC(IBEG:IBEG+NCHAR-1)
         ENDIF
30    IF (IERR1.EQ.0.AND.IERR2.EQ.0.AND.IERR3.EQ.0) GO TO 40
         ISTAT=3
C
40    NTUNIT=NTUNIT+1
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' SEQNUM=',SEQNUM,
     *      ' XDIMN=',XDIMN,
     *      ' XTYPE=',XTYPE,
     *      ' XUNIT=',XUNIT,
     *      ' XDESC=',XDESC,
     *      ' NDEC=',NDEC,
     *      ' CONV1=',CONV1,
     *      ' CONV2=',CONV2,
     *      ' '
         ENDIF
C
      IF (XTYPE.EQ.'BASE') IROW=IROW+1
      IF (IROW.GT.MDIMN) THEN
         IF (ISTAT.LT.4) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,140) 'UNITS',MUNIT
            ENDIF
         IF (ISTAT.EQ.5) ISTAT=6
         IF (ISTAT.LT.4) ISTAT=4
         ENDIF
C
      ICOL=ICOL+1
      IF (XTYPE.EQ.'BASE') ICOL=1
      IF (ICOL.GT.MUNIT) THEN
         IF (ISTAT.LT.5) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,140) 'DIMENSIONS',MDIMN
            ENDIF
         IF (ISTAT.EQ.5) ISTAT=6
         IF (ISTAT.LT.5) ISTAT=5
         ENDIF
C
      IF (ISTAT.EQ.0) THEN
         DIMNS(IROW)=XDIMN
         UNITS(IROW,ICOL)=XUNIT
         NDECML(IROW,ICOL)=NDEC
         FACTS(IROW,ICOL)=CONV1
         IF (XDIMN.EQ.'TEMP') TEMPS(ICOL)=CONV2
         IF (IRCPDS.EQ.2) GO TO 50
         ENDIF
      GO TO 10
C
50    IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' NUMREC=',NUMREC,
     *      ' MBR=',MBR,
     *      ' NTUNIT=',NTUNIT,
     *      ' '
         ENDIF
C
C  CLOSE DATASET
      IPRERR=1
      CALL URDPDS (DDNAME,BLNK8,IPRERR,REC,LRECL,NUMREC,IFLAG,IRCPDS)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) 'DIMENSIONS'
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,160) (DIMNS(I),I=1,MDIMN)
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) 'UNITS'
         DO 60 I=1,MDIMN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,160) (UNITS(I,J),J=1,MUNIT)
60          CONTINUE
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) 'CONVERSION FACTORS'
         DO 70 I=1,MDIMN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,170) (FACTS(I,J),J=1,MUNIT)
70          CONTINUE
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) 'TEMPERATURE FACTORS'
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,170) (TEMPS(J),J=1,MUNIT)
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,150) 'NUMBER OF DECIMAL PLACES'
         DO 80 I=1,MDIMN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,180) (NDECML(I,J),J=1,MUNIT)
80       CONTINUE
         ENDIF
C
90    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UDUNIT -',
     *      ' ISTAT=',ISTAT,
     *      ' '
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('+*** ERROR - IN UDUNIT - DATASET NOT ALLOCATED TO ',
     *   'FILE ',A,'.')
110   FORMAT ('+*** ERROR - IN UDUNIT - UNEXPECTED RETURN CODE FROM ',
     *   'URDPDS : IRCPDS=',I3)
120   FORMAT ('+*** ERROR - IN UDUNIT - NUMBER OF DECIMAL PLACES ',
     *   'PLACES CANNOT BE DECODED : ',A1)
130   FORMAT ('+*** ERROR - IN UDUNIT - CONVERSION FACTOR CANNOT BE ',
     *   'DECODED : ',A10)
140   FORMAT ('+*** ERROR - IN UDUNIT - MAXIMUM NUMBER OF ',
     *   'DATA ',A,' (',I3,') EXCEEDED.')
150   FORMAT ('0THE ',A,' ARE AS FOLLOWS:')
160   FORMAT (' ',13(A,1X))
170   FORMAT (' ',12(E9.3,1X))
180   FORMAT (' ',12(I9,1X))
C
      END
