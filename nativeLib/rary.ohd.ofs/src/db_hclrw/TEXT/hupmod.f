C MODULE HUPMOD
C-----------------------------------------------------------------------
C
      SUBROUTINE HUPMOD (TMPMOD,NXI,NCDS,IRANGE,IRECSV,IOPSAV)
C
C  THIS ROUTINE PUTS MOD CARDS INTO THE IMAGE ARRAY
C
      CHARACTER*80 TMPMOD
      CHARACTER*8 RTNNAM,RTNOLD
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hseg1'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hupmod.f,v $
     . $',                                                             '
     .$Id: hupmod.f,v 1.2 2001/06/13 13:43:33 dws Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='HUPMOD'
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      MIBUF=80
C
      NXI=NXI+1
      CALL UMEMST (IBLNK,IBUF(1),MIBUF)
      TMPMOD=' '
C
C  MAKE SURE COMPLETE PACKED STRING IS IN THIS RECORD
      CALL HINCNX
      NCDS=NCDS+1
      IRANGE=0
      IF (ISTAT.NE.0) GO TO 60
C
      IRECSV=NUMRED
      IOPSAV=NXOPT
      LENREC=IOPTRC(NXOPT)
      IF (LENREC.LT.0) THEN
         IRANGE=1
         IF (IHCLDB.GT.0) WRITE (IOGDB,*) 'IN HUPMOD - LENREC=',LENREC,
     *      ' IRANGE=',IRANGE
         LENREC=-LENREC
         ENDIF
C
      NMOV1=LENREC
      NMOV2=0
      NMOV3=0
      IF (LENREC+NXOPT.GT.LRECOP) THEN
         NMOV1=LRECOP-NXOPT
         NMOV2=LENREC-NMOV1
         IF (NMOV2.GT.LRECOP) THEN
            NMOV3=NMOV2-LRECOP
            NMOV2=LRECOP
            ENDIF
         ENDIF
C
C  MOVE THE PACKED STRING INTO A TEMP BUFFER
      IF (NMOV1.GT.0) THEN
         CALL UMEMOV (IOPTRC(NXOPT+1),IBUF(1),NMOV1)
         ENDIF
      IF (NMOV2.GT.0) THEN
         NXOPT=LRECOP
         LENREC=NMOV2-1
         CALL HINCNX
         IF (ISTAT.NE.0) GO TO 60
         CALL UMEMOV (IOPTRC(NXOPT),IBUF(NMOV1+1),NMOV2)
         IF (NMOV3.GT.0) THEN
            NXOPT=LRECOP
            LENREC=NMOV3-1
            CALL HINCNX
            IF (ISTAT.NE.0) GO TO 60
            CALL UMEMOV (IOPTRC(NXOPT),IBUF(NMOV1+NMOV2+1),NMOV3)
            ENDIF
         ENDIF
C
C  UNBLANK-SUPPRESS STRING
      MTMPMOD=LEN(TMPMOD)/4
      CALL HGTSTR (MTMPMOD,IBUF,TMPMOD,LENSTR,ISTAT)
      IF (ISTAT.NE.0) GO TO 100
      NXOPT=NXOPT+LENREC
      IF (IHCLDB.GT.1) WRITE (LP,50) NXI,NXOPT,NMOV1,NMOV2,NMOV3,TMPMOD
50    FORMAT (' IN HUPMOD - NXI=',I3,' NXOPT=',I3,' NMOV1=',I4,
     *   ' NMOV2=',I4,' NMOV3=',I4 /
     *   ' TMPMOD=',A)
C
C  UNPACK THE RECORD BACK INTO IBUF FOR PARSING
      NWORDS=(LENSTR+3)/4
      CALL UNPAKS (TMPMOD,IBUF,NWORDS,MIBUF,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,55) NWORDS,MIBUF
55    FORMAT ('0**ERROR** IN HUPMOD - NUMBER OF CHARACTERS ON MOD ',
     *   'CARD (',I3,') IS MORE THAN ',I3,' CHARACTERS.')
         CALL ERROR
         GO TO 100
         ENDIF
C
C  FIND FIELDS
      CALL UFREE (1,72)
      GO TO 100
C
60    WRITE (LP,70)
70    FORMAT ('0**ERROR** IN HUPMOD - UNEXPECTED EOF ON RUN OPTIONS.')
      CALL ERROR
      GO TO 100
C
100   CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
