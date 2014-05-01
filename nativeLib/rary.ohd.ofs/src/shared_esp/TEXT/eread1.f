C MODULE EREAD1
C-----------------------------------------------------------------------
C
      SUBROUTINE EREAD1 (IER)
C
C   THIS ROUTINE READS RECORD 1 FROM THE ESP PARAMETER FILE.
C
      CHARACTER*8 OLDOPN
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/eunit'
      INCLUDE 'common/esprec'
      INCLUDE 'common/espfle'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eread1.f,v $
     . $',                                                             '
     .$Id: eread1.f,v 1.2 2001/06/14 18:23:52 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IOPNUM=0
      CALL FSTWHR ('EREAD1  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE(IODBUG,*) 'ENTER EREAD1'
C
      IER=0
C
C  READ FIRST RECORD OF FILE ESPPARM
      IREC=1
      CALL UREADT (KEPARM,IREC,ESPDAT,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,10) IERR,IREC,KEPARM
10    FORMAT ('0**ERROR** STATUS CODE ',I2,' RETURNED FROM ROUTINE ',
     *   'UREADT READING RECORD ',I6,' FROM UNIT ',I2,'.')
         CALL ERROR
         IER=1
         ELSE
            MXREC=ESPDAT(1)
            NXREC=ESPDAT(2)
            LRECL=ESPDAT(3)
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
