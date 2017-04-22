C MODULE HPRTOG
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT A HCL RUN-TIME OPTION RECORD.
C
      SUBROUTINE HPRTOG (KBUF,IWORK,LWORK)
C
C  ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       -----   ----  ---   ---   -----------
C       KBUF     I     I     ?    ARRAY CONTAINING RECORD
C
      CHARACTER*8 IFPRCP/'FUTPRECP'/
      DIMENSION KBUF(*)
      DIMENSION INDBUF(4),ITECNM(2),IFNAM(2)
      DIMENSION IWORK(1)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hword2'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprtog.f,v $
     . $',                                                             '
     .$Id: hprtog.f,v 1.2 2002/02/11 20:36:57 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HPRTOG'
C
      J=0
      IFUNNM=KBUF(2)
C
      CALL ULINE (LP,2)
      WRITE (LP,20)
20    FORMAT ('0',132('-'))
C
      IF (IFUNNM.EQ.0) GO TO 40
      CALL HFDFNM (IFUNNM,IFNAM,IFREC)
      IF (IFREC.EQ.0) GO TO 160
      CALL ULINE (LP,2)
      WRITE (LP,30) IFNAM
30    FORMAT ('0** RUN-TIME OPTIONS FOR FUNCTION ',2A4,' **')
      GO TO 60
40    CALL ULINE (LP,2)
      WRITE (LP,50)
50    FORMAT ('0** RUN-TIME OPTIONS FOR ALL FUNCTIONS **')
C
60    NWDS=KBUF(3+J)
      IF (NWDS.EQ.0) GO TO 180
      NKIND=KBUF(4+J)
      IF (NKIND.EQ.0) GO TO 150
C
C  HAVE A TECHNIQUE TO PRINT
      IXUNIT=KINDXL
      ISUB=3
      IF (NKIND.GT.0) GO TO 70
         IXUNIT=KINDXG
         ISUB=7
         NKIND=-NKIND
70    IXREC=HINDEX(2,ISUB)+NKIND-1
      IF (IXREC.LT.HINDEX(2,ISUB).OR.IXREC.GT.HINDEX(1,ISUB)) GO TO 160
      CALL UREADT (IXUNIT,IXREC,INDBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
      CALL UMEMOV (INDBUF,ITECNM,2)
C
C  SET VALUE OF GROUP IDENTIFIER
      IDFLG=0
      IF (KBUF(6+J).NE.0) IDFLG=1
C
C  SET VALUE OF DEFAULT AND WRITE TECHNIQUE
      ITECVL=KBUF(5+J)
      CALL UCMPAR (ITECNM,IFPRCP,2,IMATCH)
      IF (IMATCH.EQ.0) THEN
         IF (ITECVL.EQ.-1) THEN
            ITECVL=LYES
            GO TO 80
            ENDIF
         ENDIF
      IF (ITECVL.LT.0.OR.ITECVL.GT.1) GO TO 100
      IF (ITECVL.EQ.1) ITECVL=LYES
      IF (ITECVL.EQ.0) ITECVL=LNO
80    CALL ULINE (LP,2)
      WRITE (LP,90) ITECNM,ITECVL
90    FORMAT ('0',13X,'TECHNIQUE = ',2A4,'  VALUE = ',A4)
      GO TO 120
C
100   CALL ULINE (LP,2)
      WRITE (LP,110) ITECNM,ITECVL
110   FORMAT ('0',13X,'TECHNIQUE = ',2A4,'  VALUE = ',I7)
C
C  WRITE ANY IDENTIFIERS
120   IF (IDFLG.EQ.0) GO TO 130
      CALL HPRIDS (KBUF(6+J),IPOINT,1)
      IPOINT=IPOINT+5+J
      GO TO 140
130   IPOINT=7+J
C
C  WRITE ANY ARGUMENTS
140   NUMARG=KBUF(IPOINT)
      J=J+NWDS+1
      IF (NUMARG.EQ.0) GO TO 60
      ITYPE=3
      IF (IXUNIT.EQ.KINDXG) ITYPE=-3
      CALL HPRARS (KBUF(IPOINT),INDBUF(3),ITYPE,IWORK,LWORK)
      GO TO 60
C
C  HAVE A MOD
150   CALL HPRMOD (KBUF(5+J))
      J=J+NWDS+1
      GO TO 60
C
C  SYSTEM ERROR
160   CALL ULINE (LP,2)
      WRITE (LP,170) INDBUF
170   FORMAT ('0**ERROR** IN HPRTOG - SYSTEM ERROR. ',
     *    'INDBUF=',2A4)
C
180   IF (IHCLDB.GT.0) WRITE (IOGDB,*) 'EXIT HPRTOG'
C
      RETURN
C
      END
