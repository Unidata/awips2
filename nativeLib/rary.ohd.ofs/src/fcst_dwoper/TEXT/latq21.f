C MEMBER LATQ21
C  (from old member FCLATQ21)
C
      SUBROUTINE LATQ21(NQL,LQ,QL,LOQL,J,I1,TINP,IT1,IT2,QLU,QLV,QLI,TT)
C
C      THIS SUBROUTINE COMPUTES LATERAL INFLOW
C
C           THIS SUBROUTINE WAS WRITTEN ORIGINALLY BY:
C           DR. DANNY FREAD   HRL   APRIL 1978
C
C           THIS SUBROUTINE WAS MODIFIED TO MEET VER. NO. 5 STANDARDS
C           OF THE NWSRFS BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/M621/KTIME,DHF,J1,KX
C
      DIMENSION NQL(1),LQ(1),QL(1),LOQL(1),QLI(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/latq21.f,v $
     . $',                                                             '
     .$Id: latq21.f,v 1.1 1995/09/17 18:56:08 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HLATQ,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      NQL1=NQL(J)
      LQL=LCAT21(1,J,NQL)-1
      DO 10 I2=1,NQL1
      II=I2
      IF(I1-LQ(II+LQL)) 10,20,10
   10 CONTINUE
      GO TO 30
   20 LOQ=LOQL(II+LQL)-1
C      WRITE(6,998) I1,II,LQL,J,(LOQL(MX),MX=1,NQL1)
C  998 FORMAT(3X,' I1  I2 LQL   J LOQL='/1X,10I4,5X,' **** IN LATQ21')
      QLU=QL(IT2+LOQ)+TINP*(QL(IT1+LOQ)-QL(IT2+LOQ))
      IF(TT.LT.DHF) QLU=QLI(II+LQL)+(QL(1+LOQ)-QLI(II+LQL))*TT/DHF
C      WRITE(6,999)TT,QLU,IT1,IT2,LOQ,TINP,QL(IT2+LOQ),QL(IT1+LOQ),QL(1)
C  999 FORMAT(3X,'TT       QLU IT1 IT2 LOQ TINP  QL1  QL2   QL='/
C     1 F5.2,F10.0,3I4,F5.2,3F5.0,5X,'  **** IN LATQ21')
      QLV=0.
      GO TO 40
   30 QLU=0.
      QLV=0.
C
   40 IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
