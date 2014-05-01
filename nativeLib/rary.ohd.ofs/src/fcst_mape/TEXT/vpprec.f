C MODULE VPPREC
C-----------------------------------------------------------------------
C
C  THIS ROUTINE IS CALLED TO DEBUG THE READING OF THE PARAMETRIC RECORDS 
C
      SUBROUTINE VPPREC (PEPARM,IPTRNX,MXPDIM,NFIL,ISTAT,PARMID,JPIDX)
C
      CHARACTER*8 PARMID
      DIMENSION PEPARM(MXPDIM)
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vpprec.f,v $
     . $',                                                             '
     .$Id: vpprec.f,v 1.2 1999/07/06 16:19:23 page Exp $
     . $' /
C    ===================================================================
C
C
      JFIL=NFIL
      IF (NFIL.EQ.48) JFIL=58
      KEND=0
      IF (NFIL.EQ.48 ) KEND=10
      JPTR=(JPIDX-1)*JFIL+1
      JEND=JPIDX*JFIL-KEND
C
      WRITE (IOPDBG,10) ISTAT,PARMID,IPTRNX,MXPDIM,NFIL,JPIDX,JPTR,JEND
10    FORMAT(' IN VPPREC: ISTAT=',I2,' PARMID=',A,
     2 ' IPTRNX=',I5,' MXPDIM=',I5,' NFIL=',I8,' JPIDX=',I5,
     3 ' JPTR=',I4,' JEND=',I4)
C
      WRITE (IOPDBG,20) JPTR,JEND
20    FORMAT (' IN VPPREC: PEPARM(',I2.2,'...',I2.2,'):')
      WRITE (IOPDBG,30) (PEPARM(J),J=JPTR,JEND)
30    FORMAT (6(/8F10.4))
C
      JPEND=JPTR+9
      WRITE (IOPDBG,40) JPTR,JPEND,(PEPARM(J),J=JPTR,JPEND)
40    FORMAT(' IN VPPREC: PEPARM(',I2,'...',I2,') IN A FORMAT=',
     1 10(1X,A4))
      WRITE (IOPDBG,50) PEPARM(JPTR+47)
50    FORMAT (' IN VPPREC: JULIAN DATE OF LAST DAY IN SUMPE=',F10.0)
C
      RETURN
C
      END
