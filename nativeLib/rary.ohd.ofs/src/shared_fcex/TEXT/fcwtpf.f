C MEMBER FCWTPF
C  (from old member FCFCWTPF)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 09/22/95.11:08:42 BY $WC21DT
C
C @PROCESS LVL(77)
C
C  ROUTINE TO WRITE P, T, AND TS ARRAYS TO FILE FCPARAM.
C
      SUBROUTINE FCWTPF (NRECRD,SEGID,P,NP,T,NT,TS,NTS)
C
C
      CHARACTER*8 XSEGID
      DIMENSION SEGID(2),P(NP),T(NT),TS(NTS)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fciobf'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcwtpf.f,v $
     . $',                                                             '
     .$Id: fcwtpf.f,v 1.4 1999/01/19 21:46:01 page Exp $
     . $' /
C    ===================================================================
C
C
      LDEBUG=0
      IF (LDEBUG.GT.0) THEN
         CALL UMEMOV (SEGID,XSEGID,2)
         WRITE (IODBUG,*) 'XSEGID=',XSEGID
         IBUG1=0
         IF (XSEGID.EQ.'NCCNTY1') THEN
            ITRACE=1
            IBUG1=1
            ENDIF
         ENDIF
C
      IF (ITRACE.GT.0) WRITE (IODBUG,10)
10    FORMAT (' *** ENTER FCWTPF')
C
      IBUG1=IFBUG('FCIO')
      IBUG2=IFBUG('BUFW')
C
      IF (IBUG1.GT.0) THEN
         WRITE (IODBUG,'(1X,A,2A4,A)')
     *      ' SEGID=',SEGID,
     *      ' '
         WRITE (IODBUG,*)
     *      ' NRECRD=',NRECRD,
     *      ' NP=',NP,
     *      ' NT=',NT,
     *      ' NTS=',NTS,
     *      ' '
         WRITE (IODBUG,*) 'TS ARRAY:'
         I1=1
20       I2=I1+14
         IF (I2.GT.NTS) I2=NTS
         WRITE (IODBUG,30) I1,(TS(I),I=I1,I2)
30    FORMAT (' ',I5,': ',15(' ',F7.2))
         IF (I2.LT.NTS) THEN
            I1=I2+1
            GO TO 20
            ENDIF
         ENDIF
C
C  CHECK ARGUMENTS AND BUFFER SIZE AVAILABLE
      IF (NP.GE.0.AND.
     *    NT.GE.0.AND.
     *    NTS.GE.0.AND.
     *    NRECRD.GT.0.AND.
     *    NRECRD.LE.MRP) GO TO 50
         WRITE (IPR,40) SEGID,NP,NT,NTS,NRECRD
40    FORMAT ('0**ERROR** IN FCWTPF - INVALID ARGUMENTS FOR',
     *   'SEGMENT ',2A4,' :',
     *   ' NP=',I6,
     *   ' NT=',I6,
     *   ' NTS=',I6,
     *   ' NRECRD=',I6,
     *   ' ')
         CALL ERROR
         GO TO 170
50    IF (NP+NT+NTS.EQ.0) THEN
         WRITE (IPR,60)SEGID
60    FORMAT ('0**WARNING** IN FCWTPF - NO PARAMETERS TO BE WRITTEN ',
     *   'FOR SEGMENT ',2A4)
         CALL WARN
         ENDIF
      IF (NWRP.GT.MZZBUF) THEN
         WRITE (IPR,70)
70    FORMAT ('0**ERROR** IN FCWTPF - ARRAY TOO SMALL FOR ',
     *   'PARAMETER FILE RECORD SIZE.')
         CALL ERROR
         GO TO 170
         ENDIF
C
C  COMPUTE NUMBER OF PARAMETER RECORDS NEEDED
      NEEDRC=(NP+NT+NTS+2+NWRP-1)/NWRP
      INREC1=NRECRD
      INREC2=NRECRD+NEEDRC-1
      IF (INREC2.GT.MRP) THEN
         WRITE (IPR,80)SEGID
80    FORMAT('0**ERROR** IN FCWTPF - REQUESTED PARAMETER RECORD ',
     *   'EXTENDS BEYOND END OF FILE FOR SEGMENT ',2A4,'.')
         CALL ERROR
         GO TO 170
         ENDIF
C
C  EACH SEGMENT HAS 2+NP+NT+NTS WORDS ON THE PARAMETER FILE.
C   - EACH RECORD IS NWRP WORDS LONG.
C   - IWORD1 IS THE WORD COUNT FROM THE BEGINNING OF THE SEGMENT'S
C     RECORDS OF THE FIRST WORD CURRENTLY IN THE I/O BUFFER.
C   - IWORD2 IS THE WORD COUNT OF THE LAST WORD IN THE I/O BUFFER.
C   - IBUF1 AND IBUF2 ARE THE FIRST AND LAST WORDS IN THE I/O BUFFER
C     TO BE TRANSFERED TO A PARTICUALR ARRAY.
C
C  WRITE RECORDS
      DO 160 INREC=INREC1,INREC2
C     ICOUNT IS SEQUENTIAL COUNT OF RECORDS WRITE
         ICOUNT=INREC-INREC1+1
         IF (ICOUNT.GT.1) GO TO 90
            ZZZBUF(1)=SEGID(1)
            ZZZBUF(2)=SEGID(2)
C     P ARRAY TRANSFER
90       IWORD1=(ICOUNT-1)*NWRP+1
         IWORD2=IWORD1+NWRP-1
         IF (IWORD1.GT.NP+2) GO TO 110
         IF (NP.EQ.0) GO TO 110
         IBUF1=2-IWORD1+2
         IF (IWORD1.GT.2) IBUF1=1
         IBUF2=IBUF1+NP-1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         IP1=IWORD1-2
         IF (IP1.LT.1) IP1=1
         DO 100 I=IBUF1,IBUF2
            IP=I-IBUF1+IP1
            ZZZBUF(I)=P(IP)
100         CONTINUE
C     T ARRAY TRANSFER
110      IF (IWORD1.GT.NT+NP+2) GO TO 130
         IF (NT.EQ.0) GO TO 130
         IF (IWORD2.LT.NP+3) GO TO 150
         IBUF1=NP+2-IWORD1+2
         IF (IWORD1.GT.NP+2) IBUF1=1
         IBUF2=IBUF1+NT-1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         IT1=IWORD1-NP-2
         IF (IT1.LT.1)IT1=1
         DO 120 I=IBUF1,IBUF2
            IT=I-IBUF1+IT1
            ZZZBUF(I)=T(IT)
120         CONTINUE
C     TS ARRAY TRANSFER
130      IF (IWORD1.GT.NTS+NT+NP+2) GO TO 150
         IF (IWORD2.LT.NT+NP+3) GO TO 150
         IF (NTS.EQ.0) GO TO 150
         IBUF1=NT+NP+2-IWORD1+2
         IF (IWORD1.GT.NT+NP+2) IBUF1=1
         IBUF2=IBUF1+NTS-1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         ITS1=IWORD1-NT-NP-2
         IF (ITS1.LT.1)ITS1=1
         DO 140 I=IBUF1,IBUF2
            ITS=I-IBUF1+ITS1
            ZZZBUF(I)=TS(ITS)
140         CONTINUE
150      IF (IBUG2.GT.0) CALL FCDMP2 (INREC,NWRP)
         IF (IBUG1.GT.0) WRITE (IODBUG,*)
     *      ' KFPARM=',KFPARM,
     *      ' INREC=',INREC,
     *      ' '
C     WRITE TO FILE
         CALL UWRITT (KFPARM,INREC,ZZZBUF,IERR)
         IF (IERR.GT.0) THEN
             WRITE (IPR,155) INREC,KFPARM
155   FORMAT ('0**ERROR** IN FCWTPF - WRITING RECORD ',I6,
     *   ' TO UNIT ',I2,'.')
             CALL ERROR
             ENDIF
160      CONTINUE
C
      IF (LDEBUG.GT.0) ITRACE=0
C
170   RETURN
C
      END
