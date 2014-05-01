C MEMBER FGSPUN
C  (from old member FCFPUNFG)
C
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/21/95.14:07:19 BY $WC21DT
C
      SUBROUTINE FGSPUN(FGID,DESCR,NSEG,IREC)
      DIMENSION SEGID(2,100),FGID(2),DESCR(5)
C
C  COMMON
      INCLUDE 'common/fcunit'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgspun.f,v $
     . $',                                                             '
     .$Id: fgspun.f,v 1.1 1995/09/17 18:54:39 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLNK/4H    /,SEGS/4HSEGS/,ANDS/4H&   /
C....................
C
  420 DO 450 K=1,NSEG
      NOREC=IREC+K-1
      CALL UREADT(KFFGL,NOREC,SEGID(1,K),IERR)
  450 CONTINUE
C
C --- BEGINNING PUNCH
C
      WRITE(IPU,702) FGID
  702 FORMAT(3HID ,2A4)
      WRITE(IPU,703) DESCR
  703 FORMAT(7HTITLE ',5A4,1H')
      K2=0
      AMPS=ANDS
      HEAD=SEGS
  480 K1=K2+1
      K2=K1+6
      IF(K2.GT.NSEG) K2=NSEG
      IF(K2.GE.NSEG) AMPS=BLNK
      IF(K1.GT.1) HEAD=BLNK
      WRITE(IPU,704) HEAD,((SEGID(J1,J2),J1=1,2),J2=K1,K2),AMPS
  704 FORMAT(A4,1X,7(2A4,1X),A4)
      IF(K2.LT.NSEG) GOTO 480
C
      RETURN
      END
