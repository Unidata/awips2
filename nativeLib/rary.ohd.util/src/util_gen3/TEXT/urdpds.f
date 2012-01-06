C MEMBER URDPDS
C-----------------------------------------------------------------------
C  ROUTINE URDPDS READS A MEMBER OF A PARTITIONED DATASET ONE RECORD
C  AT A TIME.
C
C  Output Status: ISTAT = 0 for no error
C                 ISTAT = 2 for end-of-file
C                 ISTAT = other for error
C-----------------------------------------------------------------------
      SUBROUTINE URDPDS (DDN,MBR,IPRERR,REC,LRECL,NUMREC,IFLAG,ISTAT)

      CHARACTER*1 REC(1)
      CHARACTER*8 DDN,MBR,XMBR,TYPMSG

      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/urdpds.f,v $
     . $',                                                             '
     .$Id: urdpds.f,v 1.2 1999/04/22 13:57:05 page Exp $
     . $' /
C    ===================================================================
C


              IF (ICMTRC.GT.1) THEN
                 CALL ULINE (ICMPRU,1)
                 WRITE (ICMPRU,120)
              ENDIF

      ISTAT = 0

      IF (MBR.NE.' '.AND.NUMREC.EQ.0) IFLAG = 0

              IF (ICMDBG.GT.2.AND.NUMREC.EQ.0) THEN
                 CALL ULINE (ICMPRU,1)
                 WRITE (ICMPRU,130) DDN,MBR
              ENDIF

              IF (ICMDBG.GT.2) THEN
                 CALL ULINE (ICMPRU,1)
                 WRITE (ICMPRU,140) NUMREC,IFLAG
              ENDIF

C  Read one record

      XMBR = MBR
      CALL URRPDS (IFLAG,DDN,XMBR,REC,LRECL,IERR)

              IF (ICMDBG.GT.1) THEN
                 CALL ULINE (ICMPRU,1)
                 WRITE (ICMPRU,145) IERR
              ENDIF

C  If no error and file has a name (meaning it is open), increment rec

      IF (IERR .EQ. 0) THEN
        IF (XMBR.NE.' ') NUMREC=NUMREC+1

C  Else if end-of-file, set ISTAT to 2

      ELSEIF (IERR .EQ. 4) THEN
        ISTAT = 2

C  Else we have an error!

      ELSE
        TYPMSG='WARNING'
        IF (IPRERR.LT.0) TYPMSG='ERROR'
        CALL ULENTH (TYPMSG,LEN(TYPMSG),LMSG)

        IF (IPRERR.GT.0) CALL UWARN (LP,1,-1)
        IF (IPRERR.LT.0) CALL UEROR (LP,1,-1)

        IF (IERR.EQ. 8) THEN
          IF (IPRERR.NE.0) WRITE (LP,170) TYPMSG(1:LMSG),MBR,DDN
          ISTAT = 3
        ELSEIF (IERR.EQ.12) THEN
          IF (IPRERR.NE.0) WRITE (LP,180) TYPMSG(1:LMSG),DDN
          ISTAT = 4
        ELSEIF (IERR.EQ.20) THEN
          IF (IPRERR.NE.0) WRITE (LP,200) TYPMSG(1:LMSG),MBR,DDN
          ISTAT = 6
        ELSEIF (IERR.EQ.24) THEN
          IF (IPRERR.NE.0) WRITE (LP,210) TYPMSG(1:LMSG),DDN
          ISTAT = 7
        ELSEIF (IERR.EQ.28) THEN
          IF (IPRERR.NE.0) WRITE (LP,220) TYPMSG(1:LMSG),DDN
          ISTAT = 8
        ELSEIF (IERR.EQ.32) THEN
          IF (IPRERR.NE.0) WRITE (LP,230) TYPMSG(1:LMSG),DDN
          ISTAT = 9
        ELSE
          IF (IPRERR.NE.0) WRITE (LP,150) TYPMSG(1:LMSG),IERR
          ISTAT = 1
        ENDIF

      ENDIF

              IF (ICMTRC.GT.1) THEN
                 CALL ULINE (ICMPRU,1)
                 WRITE (ICMPRU,240) ISTAT
              ENDIF

      RETURN

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

120   FORMAT (' *** ENTER URDPDS')
130   FORMAT (' DDN=',A,3X,'MBR=',A)
140   FORMAT (' NUMREC=',I5,3X,'IFLAG=',I5)
145   FORMAT (' IERR=',I3)
150   FORMAT ('+*** ',A,' - IN URDPDS - UNEXPECTED RETURN CODE : ',I3)
170   FORMAT ('+*** ',A,' - IN URDPDS - MEMBER ',A,' NOT FOUND ',
     *   'IN DATASET ALLOCATED TO DDNAME ',A,'.')
180   FORMAT ('+*** ',A,' - IN URDPDS - NO DD STATEMENT OR BLANK ',
     *   'MEMBER FOR DDNAME ',A,'.')
200   FORMAT ('+*** ',A,' - IN URDPDS - FILE ACCESS ERROR OCCURED ',
     *   'FOR MEMBER ',A,' IN DATASET ALLOCATED TO DDNAME ',A,'.')
210   FORMAT ('+*** ',A,' - IN URDPDS - UNRECOGNIZED RECORD FORMAT ',
     *   'FOR DATASET ALLOCATED TO DDNAME ',A,'.')
220   FORMAT ('+*** ',A,' - IN URDPDS - UNSUCCESSFUL OPEN FOR ',
     *   'DATASET ALLOCATED TO DDNAME ',A,'.')
230   FORMAT ('+*** ',A,' - IN URDPDS - CANNOT CLOSE FILE FOR ',
     *   'DATASET ALLOCATED TO DDNAME ',A,'.')
240   FORMAT (' *** EXIT URDPDS : STATUS CODE=',I2)

      END
