C MODULE FCRDPF
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ P, T AND TS ARRAYS FROM FILE FCPARAM.
C
      SUBROUTINE FCRDPF (NRECRD,SEGID,P,NP,T,NT,TS,NTS,IER)
C
      CHARACTER*8 RTNNAM,OPNOLD,SEGID
C
      DIMENSION P(NP),T(NT),TS(NTS)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fciobf'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcrdpf.f,v $
     . $',                                                             '
     .$Id: fcrdpf.f,v 1.4 2002/02/11 14:25:23 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='FCRDPF'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG1=IFBUG('FCIO')
      IBUG2=IFBUG('BUFW')
C
      LDEBUG=0
      IF (LDEBUG.GT.0) THEN
         IBUG1=0
         IF (SEGID.EQ.'NCCNTY1') THEN
            IBUG1=1
            ENDIF
         ENDIF
C
      IF (IBUG1.GT.0) THEN
         WRITE (IODBUG,*) 'SEGID=',SEGID
         WRITE (IODBUG,*)
     *      ' NRECRD=',NRECRD,
     *      ' NP=',NP,
     *      ' NT=',NT,
     *      ' NTS=',NTS
         ENDIF
C
      IER=0
C
C  CHECK ARGUMENTS AND BUFFER SIZE AVAILABLE
      IF (NP.GE.0.AND.
     *    NT.GE.0.AND.
     *    NTS.GE.0.AND.
     *    NRECRD.GT.0.AND.
     *    NRECRD.LE.MRP) GO TO 30
         WRITE (IPR,20) SEGID,NP,NT,NTS,NRECRD
20    FORMAT ('0**ERROR** IN FCRDPF - INVALID ARGUMENTS FOR',
     *   ' SEGMENT ',A,' :',
     *   ' NP=',I6,
     *   ' NT=',I6,
     *   ' NTS=',I6,
     *   ' NRECRD=',I6)
         CALL ERROR
         IER=1
         GO TO 160
30    IF (NP+NT+NTS.EQ.0) THEN
         WRITE (IPR,40) SEGID
40    FORMAT ('0** WARNING** IN FCRDPF - NO PARAMETERS TO BE READ ',
     *   'FOR SEGMENT ',A,'.')
         CALL WARN
         ENDIF
      IF (NWRP.GT.MZZBUF) THEN
         WRITE (IPR,50)
50    FORMAT ('0**ERROR** IN FCRDPF - ARRAY TOO SMALL FOR ',
     *   'PARAMETER FILE RECORD SIZE.')
         CALL ERROR
         IER=1
         GO TO 160
         ENDIF
C
C  COMPUTE NUMBER OF PARAMETER RECORDS NEEDED
      NEEDRC=(NP+NT+NTS+2+NWRP-1)/NWRP
      INREC1=NRECRD
      INREC2=NRECRD+NEEDRC-1
      IF (INREC2.GT.MRP) THEN
         WRITE (IPR,60) SEGID
60    FORMAT ('0**ERROR** IN FCRDPF - REQUESTED PARAMETER RECORD ',
     *   'EXTENDS BEYOND END OF FILE FOR SEGMENT ',A,'.')
         CALL ERROR
         IER=1
         GO TO 160
         ENDIF
C
C  EACH SEGMENT HAS 2+NP+NT+NTS WORDS ON THE PARAMETER FILE.
C   - EACH RECORD IS NWRP WORDS LONG.
C   - IWORD1 IS THE WORD COUNT FROM THE BEGINNING OF THE SEGMENT'S
C     RECORDS OF THE FIRST WORD CURRENTLY IN THE I/O BUFFER.
C   - IWORD2 IS THE WORD COUNT OF THE LAST WORD IN THE I/O BUFFER.
C   - IBUF1 AND IBUF2 ARE THE FIRST AND LAST WORDS IN THE I/O BUFFER
C     TO BE TRANSFERED TO A PARTICULAR ARRAY.
C
C  READ RECORDS
      DO 130 INREC=INREC1,INREC2
         IF (IBUG1.GT.0) WRITE (IODBUG,*)
     *      ' KFPARM=',KFPARM,
     *      ' INREC=',INREC
         CALL UREADT (KFPARM,INREC,ZZZBUF,IERR)
         IF (IERR.GT.0) THEN
            WRITE (IPR,65) INREC,KRPARM,SEGID
65    FORMAT ('0**ERROR** IN FCRDPF - READING RECORD ',I6,
     *   ' FROM UNIT ',I2,' FOR SEGMENT ',A,'.')            
            CALL ERROR
            IER=1
            ENDIF
         IF (IBUG2.GT.0) CALL FCDMP2 (INREC,NWRP)
C     ICOUNT IS SEQUENTIAL COUNT OF RECORDS READ
         ICOUNT=INREC-INREC1+1
CCC         IF (ICOUNT.EQ.1) CALL UMEMOV (ZZZBUF,SEGID,2)
C     P ARRAY TRANSFER
         IWORD1=(ICOUNT-1)*NWRP+1
         IWORD2=IWORD1+NWRP-1
         IF (IBUG2.GT.0) WRITE (IODBUG,*)
     *      ' IWORD1=',IWORD1,
     *      ' IWORD2=',IWORD2
         IF (NP.EQ.0) GO TO 90
         IBUF1=2-IWORD1+2
         IF (IWORD1.GT.2) IBUF1=1
         IBUF2=NP+2-IWORD1+1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         IP1=IWORD1-2
         IF (IP1.LT.1) IP1=1
         DO 80 I=IBUF1,IBUF2
            IP=I-IBUF1+IP1
            P(IP)=ZZZBUF(I)
80          CONTINUE
C     T ARRAY TRANSFER
90       IF (IWORD1.GT.NT+NP+2) GO TO 110
         IF (NT.EQ.0) GO TO 110
         IF (IWORD2.LT.NP+3) GO TO 130
         IBUF1=NP+2-IWORD1+2
         IF (IWORD1.GT.NP+2) IBUF1=1
         IBUF2=NP+NT+2-IWORD1+1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         IT1=IWORD1-NP-2
         IF (IT1.LT.1) IT1=1
         IF (IBUG2.GT.0) WRITE (IODBUG,*)
     *      ' IBUF1=',IBUF1,
     *      ' IBUF2=',IBUF2,
     *      ' IT1=',IT1
         DO 100 I=IBUF1,IBUF2
            IT=I-IBUF1+IT1
            T(IT)=ZZZBUF(I)
100         CONTINUE
C     TS ARRAY TRANSFER
110      IF (IWORD1.GT.NTS+NT+NP+2) GO TO 130
         IF (IWORD2.LT.NT+NP+3) GO TO 130
         IF (NTS.EQ.0) GO TO 130
         IBUF1=NT+NP+2-IWORD1+2
         IF (IWORD1.GT.NT+NP+2) IBUF1=1
         IBUF2=NP+NT+NTS+2-IWORD1+1
         IF (IBUF2.GT.NWRP) IBUF2=NWRP
         ITS1=IWORD1-NT-NP-2
         IF (ITS1.LT.1) ITS1=1
         IF (IBUG2.GT.0) WRITE (IODBUG,*)
     *      ' IBUF1=',IBUF1,
     *      ' IBUF2=',IBUF2,
     *      ' ITS1=',ITS1
         DO 120 I=IBUF1,IBUF2
            ITS=I-IBUF1+ITS1
            TS(ITS)=ZZZBUF(I)
120         CONTINUE
130      CONTINUE
C
      IF (IBUG1.GT.0) THEN
         WRITE (IODBUG,*) 'TS ARRAY:'
         I1=1
140      I2=I1+14
         IF (I2.GT.NTS) I2=NTS
         WRITE (IODBUG,150) I1,(TS(I),I=I1,I2)
150   FORMAT (' ',I4,': ',15(' ',F7.2))
         IF (I2.LT.NTS) THEN
            I1=I2+1
            GO TO 140
            ENDIF
         ENDIF
C
160   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
