C MEMBER PTINDX
C-----------------------------------------------------------------------
C
      SUBROUTINE PTINDX (IRESET,IWPRDC)
C
C  THIS ROUTINE PRINTS THE CONTENTS OF TIME SERIES INDEX
C  AND PRINT A FREQUENCE PLOT OF TIME SERIES IN RANGES OF RECORDS
C
      CHARACTER*1 ISTAR/'*'/
      CHARACTER*4 TSTYPE
      CHARACTER*8 TSID,TSIDF
C
      DIMENSION IXBUF(4),IFREQ(50)
C
      INCLUDE 'uiox'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptindx.f,v $
     . $',                                                             '
     .$Id: ptindx.f,v 1.2 2002/02/11 21:11:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK IF ANY TIME SERIES DEFINED
      IF (NUMTMS.EQ.0) GO TO 40
C
      WRITE (LP,50)
C
C  COMPUTE NUMBER OF INDEX RECORDS
      NUM=MAXTMS*2
C
C  COMPUTE WHERE OVERFLOW AREA STARTS
      IOVFL=FLOAT(NUM)*0.87225
C
      NUMTS=0
      NOVFL=0
      DO 10 I=1,50
         IFREQ(I)=0
10       CONTINUE
C
C  PROCESS EACH INDEX RECORD
      DO 20 I=1,NUM
         CALL UREADT (KINDEX,I,IXBUF,IERR)
C     CHECK IF UNUSED OR DELETED
         IF (IXBUF(1).EQ.0.OR.IXBUF(1).EQ.-1) GO TO 20
            NUMTS=NUMTS+1
            IF (I.GE.IOVFL) NOVFL=NOVFL+1
            LOC=((I-1)*50)/NUM+1
            IFREQ(LOC)=IFREQ(LOC)+1
            WRITE (LP,60) I,IXBUF
            CALL UMEMOV (IXBUF(1),TSID,2)
            CALL UMEMOV (IXBUF(3),TSTYPE,1)
            IREC=IXBUF(4)
            MAXX=0
            CALL RPRDH (TSID,TSTYPE,MAXX,IHEAD,NUMX,XBUF,TSIDF,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,65) IERR,TSTYPE,TSID,IREC
65    FORMAT (' **WARNING** STATUS CODE FROM ROUTINE RPRDH IS ',I2,
     *   ' READING ',A,' TIME SERIES FOR IDENTIFIER ',A,
     *   ' FOUND AT INDEX RECORD ',I5,'.')
               CALL UWARN (LP,0,-1)
               IF (IRESET.EQ.1) THEN
                  CALL UMEMST (-1,IXBUF,4)
                  CALL UWRITT (KINDEX,I,IXBUF,IERR)
                  WRITE (LP,67) I
67    FORMAT (' **NOTE** INDEX RECORD NUMBER ',I6,' SET TO DELETED.')
                  IWPRDC=1
                  ENDIF
               ENDIF
20       CONTINUE
C
C  PRINT FREQUENCY PLOT
      WRITE (LP,70) NUMTS,NUM,IOVFL,NOVFL
      WRITE (LP,75)
      WRITE (LP,80) (I,I=10,100,10)
      DO 30 LOC=1,50
         IBEG=((LOC-1)*NUM)/50+1
         IEND=(LOC*NUM)/50
         NPLOT=IFREQ(LOC)
         IF (NPLOT.EQ.0) WRITE (LP,90) IBEG,IEND
         IF (NPLOT.GT.0.AND.NPLOT.LT.101) WRITE (LP,90) IBEG,IEND,
     *      (ISTAR,I=1,NPLOT)
         IF (NPLOT.GT.100) WRITE (LP,100) IBEG,IEND,NPLOT
30       CONTINUE
C
40    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0CONTENTS OF INDEX RECORDS:' /
     *   '0INDEX               DATA  TS FILE' /
     *   ' RECORD  IDENTIFIER  TYPE  RECORD' /
     *   ' ------  ----------  ----  -------')
60    FORMAT (' ',I6,2X,2A4,2X,2X,A4,2X,I7)
70    FORMAT ('0THERE ARE ',I6,' TIME SERIES DISTRIBUTED OVER ',I6,
     *       ' RECORDS IN THE INDEX FILE' /
     *    ' THE OVERFLOW AREA BEGINS AT ','RECORD NUMBER ',I6,
     *       ' AND HAS ',I6,' TIME SERIES')
75    FORMAT ('0THE FOLLOWING IS A FREQUENCY PLOT OF ',
     *   'THE DISTRIBUTION OF THESE TIME SERIES:')
80    FORMAT ('0RECORDS',4X,10I10 /
     *   ' ',13('-'),2X,20('----+'))
90    FORMAT (' ',I5,' - ',I5,2X,100A1)
100   FORMAT (' ',I5,' - ',I5,2X,I5,' TIME SERIES IN THIS RANGE')
C
      END
