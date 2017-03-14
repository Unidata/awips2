C MODULE FCWTRC
C-----------------------------------------------------------------------
C
C  ROUTNIE TO COPY COMMON BLOCK FRATNG TO RATING CURVE FILE.
C
      SUBROUTINE FCWTRC (IWREC,ZZZBUF,IRCPT)
C
C  ARGUMENT LIST:
C    IWREC  - RECORD ON FILE RATING CURVE IS TO BE WRITTEN
C    ZZZBUF - RATING CURVE IDENTIFIER
C    IRCPT  - POINTER FILE FLAG:
C               0=UPDATING (DO NOT CHANGE)
C               1=DELETING (CHANGE PARTIALLY)
C               2=ADDING (CHANGE POINTER FILE)
C
C  SUBROUTINE ORIGINALLY WRITTEN BY -- JOE OSTROWSKI -- HRL --800827
C
      CHARACTER*8 RTNNAM,OPNOLD
C
      DIMENSION OBSLET(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fratng'
      INCLUDE 'common/frcptr'
      INCLUDE 'common/fcunit'
C
      DIMENSION ZZZBUF(*),IRCZ(3,500)
      DIMENSION BUF(3),IBUF(3),JBUF(3)
C
      EQUIVALENCE (BUF(1),IBUF(1)),(RCZZ(1,1),IRCZ(1,1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcwtrc.f,v $
     . $',                                                             '
     .$Id: fcwtrc.f,v 1.4 2000/03/14 11:58:04 page Exp $
     . $' /
C    ===================================================================
C
      DATA OBSLET/4HOBSO,4HLETE/
C
C
      RTNNAM='FCWTRC'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('RTCV')
C
C  CHECK IF ID IN ZZZBUF AND RTCVID MATCH
      IF (ZZZBUF(1).NE.RTCVID(1).OR.ZZZBUF(2).NE.RTCVID(2)) THEN
         WRITE (IPR,10) RTNNAM(1:LENSTR(RTNNAM)),
     *      ZZZBUF(1),ZZZBUF(2),RTCVID
10    FORMAT ('0**ERROR** IN ',A,' - RATING CURVE TO BE WRITTEN (',2A4,
     *   ') DOES NOT MATCH THAT IN COMMON BLOCK FRATNG (',2A4,').')
         CALL ERROR
         GO TO 40
         ENDIF
C
C  UPDATE RATING CURVE FILE
      CALL UWRITT (KFRTCV,IWREC,RTCVID,IERR)
C
      IF (IRCPT.EQ.0) GO TO 20
         NREC=IWREC+1
         BUF(1)=ZZZBUF(1)
         BUF(2)=ZZZBUF(2)
         IBUF(3)=IWREC
C     UPDATE COMMON BLOCK FRCPTR
         RCZZ(1,IWREC)=ZZZBUF(1)
         RCZZ(2,IWREC)=ZZZBUF(2)
         IRCZ(3,IWREC)=IWREC
C     UPDATE POINTER FILE
         CALL UWRITT (KFRCPT,NREC,BUF,IERR)
         IF (IRCPT.EQ.1) GO TO 20
            NRC=NRC+1
            CALL UREADT (KFRCPT,1,JBUF,IERR)
            JBUF(1)=NRC
            CALL UWRITT (KFRCPT,1,JBUF,IERR)
C
20    IF (RTCVID(1).EQ.OBSLET(1).AND.RTCVID(2).EQ.OBSLET(2)) THEN
         ELSE
            WRITE (IPR,30) RTCVID
30    FORMAT ('0**NOTE** RATING CURVE FILE UPDATED FOR RATING CURVE ',
     *   2A4,'.')
         ENDIF
C
40    CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
