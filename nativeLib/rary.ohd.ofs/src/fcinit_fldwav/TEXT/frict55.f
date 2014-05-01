      SUBROUTINE FRICT55(NCML,CM,YQCM,J,K,YQ,CMM,DCM,K1,K7,K8)
C
C      THIS SUBROUTINE SETS UP MANNING'S N VALUES
C      IN CASE NQCM<>0, YQ IS AVERAGE VALUES OF NCM(I,J) AND NCM(I+1,J)
C      AND ONLY DONE IN UNSTEADY COMPUTATION BECAUSE
C       1. IT IS APPLICABLE ONLY IN FLAT RIVERS
C       2. NO MIXED FLOW ALLOWED
C       3. SAME ASSUMPTION AS ORIGINAL DWOPER PROGRAM
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
C
      DIMENSION CM(K8,K7,K1),YQCM(K8,K7,K1)

      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/frict55.f,v $
     . $',                                                             '
     .$Id: frict55.f,v 1.2 2004/02/02 21:53:22 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'FRICT55 '/
C
      CALL FPRBUG(SNAME,1,55,IBUG)
C
      DCM=0.
      IF(YQ.LE.YQCM(1,K,J))CMM=CM(1,K,J)
      IF(YQ.LE.YQCM(1,K,J)) GO TO 15
      IF(YQ.GE.YQCM(NCML,K,J))CMM=CM(NCML,K,J)
      IF(YQ.GE.YQCM(NCML,K,J)) GO TO 15
      DO 5 L=2,NCML
      L2=L
      IF(YQ.LE.YQCM(L,K,J)) GO TO 10
    5 CONTINUE
   10 L1=L2-1
      HFAC=(YQCM(L2,K,J)-YQCM(L1,K,J))
      IF(HFAC.LE.0.0) HFAC=0.01
      DCM=(CM(L2,K,J)-CM(L1,K,J))/HFAC
      CMM=CM(L1,K,J)+DCM*(YQ-YQCM(L1,K,J))
   15 CONTINUE
      RETURN
      END
