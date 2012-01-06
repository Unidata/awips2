C MODULE FGSGLS
C-----------------------------------------------------------------------
C
C  ROUTINE TO LIST SEGMENTS IN A FORECAST GROUP.
C
      SUBROUTINE FGSGLS (IREC,NSEG)
C
C  IREC=RECORD TO START LIST ON FILE FCFGLIST
C  NSEG=NUMBER OF SEGMENTS
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON -- HRL -- 11/1979
C
      PARAMETER (MSEGZ=10)
      DIMENSION SEGZ(2,MSEGZ)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgsgls.f,v $
     . $',                                                             '
     .$Id: fgsgls.f,v 1.3 2000/03/14 11:55:37 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.2) WRITE (IODBUG,*) 'ENTER FGSGLS'
C
      WRITE (IPR,10)
10    FORMAT ('0',5X,'SEGMENTS IN COMPUTATIONAL ORDER ',
     *   '(READ ACROSS):')
C
      DO 40 I=1,NSEG,MSEGZ
         J1=I
         J2=I+MSEGZ-1
         IF (J2.GT.NSEG) J2=NSEG
         DO 20 J=J1,J2
            N=J-J1+1
            NREC=IREC+J-1
            CALL UREADT (KFFGL,NREC,SEGZ(1,N),IERR)
20          CONTINUE
         N2=J2-J1+1
         WRITE (IPR,30)((SEGZ(J,N),J=1,2),N=1,N2)
30    FORMAT (' ',5X,10(2A4,2X))
40       CONTINUE
C
      RETURN
C
      END
