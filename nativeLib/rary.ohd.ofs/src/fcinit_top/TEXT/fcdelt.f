C MODULE FCDELT
C-----------------------------------------------------------------------
C
      SUBROUTINE FCDELT
C
C  PERFORMS THE DELETE COMMAND FOR THE FCINIT PROGRAM
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 29 NOV 1979
C
      CHARACTER*8 RTNNAM,CMDNAM,OPNOLD
      PARAMETER (NCOMD=15)
      CHARACTER*10 XCOMD(NCOMD)/
     *   'CGROUPS','CGROUP','CG','C',
     *   'FGROUPS','FGROUP','FG','F',
     *   'SEGMENTS','SEGMENT','SEGS','SEG','S',
     *   'RC','R'/
      PARAMETER (MCARDS=30)
      CHARACTER*80 IBUF,CARDS(MCARDS),WORD
      DIMENSION LIST(5,100),NUMC(100),LRC(2,100)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcdelt.f,v $
     . $',                                                             '
     .$Id: fcdelt.f,v 1.4 2001/06/13 13:17:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='FCDELT'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IBUG=IFBUG('DLTE')
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      WRITE (IPR,10)
10    FORMAT ('0',80('*') /
     *   ' ','*',32X,'DELETE COMMAND',32X,'*' /
     *   ' ',80('*'))
C
C  READ CARDS
      CMDNAM='DELETE'
      CALL CDINPT (CARDS,MCARDS,NCARDS,CMDNAM,IERR)
      IF (IERR.GT.0) GO TO 180
C
      ICARDS=0
C
20    ICARDS=ICARDS+1
      IF (ICARDS.GT.NCARDS) GO TO 180
C
C  PRINT CARD
      NBLINE=1
      IFORM=1
      NCARD=0
      CALL UPRCR2 (NBLINE,IFORM,IPR,CARDS(ICARDS),NCARD)
C
C  FIND FIRST FIELD ON CARD
      CALL UMEMOV (CARDS(ICARDS),IBUF,20)
      NSCAN=1
      CALL USCAN2 (IBUF,' ',NSCAN,WORD,LWORD,IERR)
C
C  CHECK FOR COMMENT
      IF (WORD(1:1).EQ.'$') GO TO 20
C
      DO 30 IDEST=1,NCOMD
         IF (WORD.EQ.XCOMD(IDEST)) GO TO 40
30       CONTINUE
      GO TO 50
40    GO TO (70,70,70,70,
     *   110,110,110,110,
     *   130,130,130,130,130,
     *   160,160),IDEST
50    WRITE (IPR,60) WORD(1:LENSTR(WORD)),IBUF
60    FORMAT ('0**ERROR** INVALID KEYWORD (',A,
     *      ') FOUND ON THE FOLLOWING CARD:' /
     *   ' ',A)
      CALL ERROR
      GO TO 180
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE CARRYOVER GROUPS
C
70    NWORDS=2
      CALL RDLIST (ICARDS,NCARDS,CARDS,LIST,NLIST,NWORDS,NUMC,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,80)
80    FORMAT ('0**ERROR** CANNOT READ NAMES ON ABOVE CARDS.')
         CALL ERROR
         GO TO 180
         ENDIF
      IF (NLIST.EQ.0) THEN
         WRITE (IPR,90) 'CARRYOVER GROUP'
90    FORMAT ('0**ERROR** NO ',A,'NAMES FOUND.')
         CALL ERROR
         GO TO 20
         ENDIF
C
      DO 100 I=1,NLIST
         CALL CGDEL (LIST(1,I))
100      CONTINUE
C
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE FORECAST GROUPS
C
110   NWORDS=2
      CALL RDLIST (ICARDS,NCARDS,CARDS,LIST,NLIST,NWORDS,NUMC,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,80)
         CALL ERROR
         GO TO 180
         ENDIF
      IF (NLIST.EQ.0) THEN
         WRITE (IPR,90) 'FORECAST GROUP'
         CALL ERROR
         GO TO 20
         ENDIF
C
      DO 120 I=1,NLIST
         CALL FGDEL (LIST(1,I))
120      CONTINUE
C
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE SEGMENTS
C
130   NWORDS=2
      CALL RDLIST (ICARDS,NCARDS,CARDS,LIST,NLIST,NWORDS,NUMC,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,80)
         CALL ERROR
         GO TO 180
         ENDIF
      IF (NLIST.EQ.0) THEN
         WRITE (IPR,90) 'SEGMENT'
         CALL ERROR
         GO TO 20
         ENDIF
C
      DO 150 I=1,NLIST
         CALL DELSEG (LIST(1,I),IERR)
150      CONTINUE
C
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DELETE RATING CURVES
C
160   NWORDS=2
      CALL RDLIST (ICARDS,NCARDS,CARDS,LIST,NLIST,NWORDS,NUMC,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,80)
         CALL ERROR
         GO TO 180
         ENDIF
      IF (NLIST.EQ.0) THEN
         WRITE (IPR,90) 'RATING CURVE'
         CALL ERROR
         GO TO 20
         ENDIF
C
      DO 170 I=1,NLIST
         LRC(1,I)=LIST(1,I)
         LRC(2,I)=LIST(2,I)
170      CONTINUE
      CALL FDELRC (LRC,NLIST)
C
      GO TO 20
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IPR,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
