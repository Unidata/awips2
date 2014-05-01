      SUBROUTINE DIRSOL4(IMA, IM, JM, AE, INIT1, DIFF, FI, R 
     *,                  WSV, ix, wrk1, wrk2, FM)
c
C     Warning!  Both input and output share the same location!!
c     ---------------------------------------------------------
c
      LOGICAL INIT1, DIFF
c
      DIMENSION FI(IMA,JM), WSV(1), IX(IM)
      DIMENSION WRK1(IM*JM,14), WRK2(IM,6), FM(JM)
c
      imb2 = im / 2
c
      CALL DIRSL4X(IMA, IM, JM, imb2, AE, INIT1, DIFF, FI, FI, R 
     *,            wsv(1),      wsv(jm+1),  wsv(jm*2+1)
     *,            wsv(jm*3+1), wsv(jm*4+im), IX
c
     *,            WRK1(1,11), WRK1(1,13)
c
     *,            WRK1(1,1),  WRK1(1,3), WRK1(1,5)
     *,            WRK1(1,7),  WRK1(1,8), WRK1(1,9)
     *,            WRK1(1,10), WRK1(1,11), WRK1(1,13), WRK1(1,14)
c
     *,            WRK1(1,1),  WRK1(1,2), WRK1(1,3), WRK1(1,4)
     *,            WRK1(1,5),  FM
c
     * ,           WRK2(1,1), WRK2(1,3), WRK2(1,5),   WRK2(1,6))
c
      RETURN
      END
      SUBROUTINE DIRSL4X(IMA,IM,JM,imb2,AE,INIT1,DIFF,FI,FO,R 
     *,                  cph, u, w, wvsq, tr, ix 
     *,                  A, B, AMK, BMK, CMK, DN1K,  DN2K
     *,                  FN1K, FN2K , ALPHA, BETA1, BETA2
     *,                  AM, BM, CM, DN1, FN1, FM
     *,                  WRK1, WRK2, VRK1,   VRK2)

c
c
C***********************************************************************
C                         Subroutine DIRSOL4
C***********************************************************************
c
C   This modified version Written by S. Moorthi of EMC/NCEP/NWS/NOAA
c
c   based on the code (developed by Y. Li) presented in the NASA
c   technical memorandum by Y. Li, S. Moorthi and R. Bates, (1994)
c
C
C
C PURPOSE:  This routine solves the following equation on the sphere
C
c             4
c        [(del) + R ] phi = F
c
c            2
c  where (del) is the horozontal Laplacian on a sphere
C
C
C The method follows Lindzen and Kuo (1969, Mon. Wea. Rev., p732-734)
c, which is summarized as:
C
C     1). Convert the \del^4-equation into a set of two \del^2 equations.
C         Now the model variables become a vector of
C         (\del^2(phi), (phi));
C
C     2). Since the coefficients are only latitude-dependent, FFT along
C         latitudes transforms the second-order vector PDE in to a series
C         second-order vector ODE's for the FFT coefficients.
C
C     3). Solve the set of second-order ODE's;
C
C     4). Perform inverse FFT to obtain the solution (phi)^(n+1)
C
C
C SYSTEM ROUTINES USED:
C          FFTFAX: Cray routine which initialize the FFT routines;
C          RFFTMLT: CRay FFT routines routine (Cray Manual: SR-2081 8.0)
C
C SUBROUTINES CALLED:
C     S2ODE: solve the set of the second-order ODE's for FFT
C            coefficients k > 0;
C     SOMEAN:solve the zonal means of the solution;
C
C VARIABLES:
C
C     INPUT:
C       AE    - radius of earth (meters);
C       DIFF  - logical variable - when .true. finite difference equivalent
c               of wavenumber square is used;
c       FI    - input forcing -- on output will contain the solution;
C       IM    - number of grid points along latitudes;
C       IMA   - leading dimension of array FI; IM .le. IMA;
C       INIT1 - logical variable -- when true constants are calculated
c               and returned 
C       XK    - diffusion coefficient (m^4/s).
C
C     Work Arrays:
C         A-array storing the FFT coefficinets;
C         AM-coefficient array for the 2nd-order ODE (for the zonal mean
C             at j-1 point;
C         AMK-coefficient array for the 2nd-order ODE for the FFT coeffi
C             at j-1 point;
C         B-working array for the FFT;
C         BM-coefficient array for the 2nd-order ODE (for the zonal mean
C             at j point;
C         BMK-coefficient array for the 2nd-order ODE for the FFT coeffi
C             at j point;
C         CM-coefficient array for the 2nd-order ODE (for the zonal mean
C             at j+1 point;
C         CMK-coefficient array for the 2nd-order ODE for the FFT coeffi
C             at j+1 point;
C         DN1-real part of the forcing term for the 2nd-order ODE or for
C             for the equation for the zonal mean;
C         DN1K-real part of the forcing term for the 2nd-order ODE or fo
C             to the equation for the FFT coefficients;
C         DN2K-imaginery part of the forcing term for the 2nd-order ODE
C             to the equation for the FFT coefficients;
C         FM-zonal mean of the input field (phi)^(n);
C         FN1-the solution to the 2nd-order ODE for the zonal mean;
C         FN1K-real part of the solution to the 2nd-order ODE for the FF
C             coefficients;
C         FN2K-imaginery part of the solution to the 2nd-order ODE for t
C             coefficients;
c
C    Extenal Arrays -- initialised when INIT1=.true.  Must be saved for
c             use with all subsequent calls to this routine with INIT1=
c             .false.
C         IX-working array for the FFT;
C         TR-working array for the FFT.
C         U-coeffcients at j+1/2;
C         W-coeffcients at j-1/2;
C         W-coeffcients at j;
C         CPH-coefficient at the mass point;
C         WVSQ-array storing  X-direction wavenumber square
C
C***********************************************************************
      INTEGER FORWARD, BACKWARD
      PARAMETER(FORWARD = -1, BACKWARD = 1)

      DIMENSION FI(IMA,JM), FO(IMA,JM)

      DIMENSION IX(IM), TR(3*(IM+2)+1)
     #,         CPH(JM), U(JM), W(JM), WVSQ(IMB2)

      DIMENSION A(IM+2,JM-2), B(IM*2,JM-2)


      DIMENSION AM(2,2,JM), BM(2,2,JM), CM(2,2,JM)
     #,        DN1(2,JM), FN1(2,JM), FM(JM)

      DIMENSION AMK(IMB2,2,2,JM), BMK(IMB2,2,2,JM), CMK(IMB2,2,2,JM)
     #,         DN1K(IMB2,2,JM),  DN2K(IMB2,2,JM)
     #,         FN1K(IMB2,2,JM),  FN2K(IMB2,2,JM)
c
      DIMENSION ALPHA(IMB2,2,2,JM), BETA1(IMB2,2,JM), BETA2(IMB2,2,JM)
      DIMENSION WRK1(IMB2,2,2), WRK2(IMB2,2,2)
     *,         VRK1(IMB2,2),   VRK2(IMB2,2)


      LOGICAL INIT1, DIFF
c
      save dysq
C
CFPP$ NOCONCUR R
c
      IMB2 = IM/2
      JMM1 = JM - 1
      JMM2 = JM - 2
      IMP1 = IM + 1
      IMP2 = IM + 2
      LEN  = IMB2 - 1
      FIM  = 1.0 / FLOAT(IM)
c
      IF (INIT1) THEN
         CALL FFTFAX(IM,IX,TR)
c
         PI    = 4.0*ATAN(1.0)
         TWOPI = 2.0 * PI
         PIO2  = PI/2.0
         DLM   = TWOPI*FIM
         DPH   = PI/FLOAT(JMM1)
C
         RDLM  = 1.0/DLM
C
         DY    = AE*DPH
         DYSQ  = DY * DY
C
         DO J = 1, JMM1
            TEM    = - PIO2 + (J-1)*DPH
            FM(J)  = COS(TEM+0.5*DPH)
            CPH(J) = 1.0 / COS(TEM)
         ENDDO
C
         DO J = 2, JMM1
            U(J) = FM(J)   * CPH(J)
            W(J) = FM(J-1) * CPH(J)
            tem  = DPH     * CPH(J)
            CPH(J) = tem * tem
         ENDDO

         IF (DIFF) THEN
            DO I = 1, IMB2
               WVSQ(I) = (SIN(0.5*I*DLM)*(RDLM*2.0))**2
            ENDDO
         ELSE
            DO I = 1, IMB2
               WVSQ(I) = I*I
            ENDDO
         ENDIF
         return
      ENDIF
C
      do i=1,imp2*jmm2
         A(i,1) = 0.0
      enddo
      do i=1,ima*jm
         fi(i,1) = dysq * fi(i,1)
      enddo

      DO J = 1, JM
         FM(J) = 0.0
         DO I = 1, IM
            FM(J) = FM(J) + FI(I,J)
         ENDDO
      ENDDO

      DO J = 1, JM
         FM(J) = FM(J)*FIM
      ENDDO

      DO J = 2, JMM1
         DO I = 1, IM
            A(I,J-1) = FI(I,J) - FM(J)
         ENDDO
      ENDDO
      DO J = 1, JMM2
         A(IM+1,J) = A(1,J)
         A(IM+2,J) = A(2,J)
      ENDDO
c
      CALL RFFTMLT(A,B,TR,IX,1,IMP2,IM,JMM2,FORWARD)
c
      DO J = 1, JMM2
         A(1,J) = 0.0
         A(2,J) = 0.0
         A(IMP2,J) = 0.0
      enddo


      DO J = 2, JMM1
         DO I = 1, IMB2
            AMK(I,1,1,J) = W(J)
            AMK(I,2,2,J) = W(J)
            AMK(I,1,2,J) = 0.0
            AMK(I,2,1,J) = 0.0

            CMK(I,1,1,J) = U(J)
            CMK(I,2,2,J) = U(J)
            CMK(I,1,2,J) = 0.0
            CMK(I,2,1,J) = 0.0

            BMK(I,1,1,J) = - CPH(J) * WVSQ(I) - U(J) - W(J) 
            BMK(I,2,2,J) =   BMK(I,1,1,J)
            BMK(I,1,2,J) =   R*DYSQ
            BMK(I,2,1,J) = - DYSQ

            DN1K(I,1,J) = A(I+I+1,J-1)
            DN1K(I,2,J) = 0.0

            DN2K(I,1,J) = A(I+I+2,J-1)
            DN2K(I,2,J) = 0.0
         ENDDO
      ENDDO

c     DN1K(:,:,1) = 0.0
c     DN2K(:,:,1) = 0.0
c     AMK(:,1,1,1) = 1.0
c     AMK(:,2,2,1) = 1.0
c     AMK(:,1,2,1) = 0.0
c     AMK(:,2,1,1) = 0.0
c     BMK(:,:,:,1) = 0.0
c     DN1K(:,:,JM) = 0.0
c     DN2K(:,:,JM) = 0.0
c     AMK(:,:,:,JM) = 0.0
c     BMK(:,1,1,JM) = 1.0
c     BMK(:,2,2,JM) = 1.0
c     BMK(:,1,2,JM) = 0.0
c     BMK(:,2,1,JM) = 0.0

      do ii = 1, IMB2
        DN1K(ii,1,1) = 0.0
        DN1K(ii,2,1) = 0.0
        DN2K(ii,1,1) = 0.0
        DN2K(ii,2,1) = 0.0

        AMK(ii,1,1,1) = 1.0
        AMK(ii,2,2,1) = 1.0
        AMK(ii,1,2,1) = 0.0
        AMK(ii,2,1,1) = 0.0

        BMK(ii,1,1,1) = 0.0
        BMK(ii,2,1,1) = 0.0
        BMK(ii,1,2,1) = 0.0
        BMK(ii,2,2,1) = 0.0

        DN1K(ii,1,JM) = 0.0
        DN1K(ii,2,JM) = 0.0
        DN2K(ii,1,JM) = 0.0
        DN2K(ii,2,JM) = 0.0

        AMK(ii,1,1,JM) = 0.0
        AMK(ii,2,1,JM) = 0.0
        AMK(ii,1,2,JM) = 0.0
        AMK(ii,2,2,JM) = 0.0

        BMK(ii,1,1,JM) = 1.0
        BMK(ii,2,2,JM) = 1.0
        BMK(ii,1,2,JM) = 0.0
        BMK(ii,2,1,JM) = 0.0
      enddo

      CALL S2ODEN(IMB2,DN1K,DN2K,FN1K,FN2K,AMK,BMK,CMK,JM 
     *,          ALPHA, BETA1, BETA2, WRK1, WRK2, VRK1, VRK2)

      DO J = 2, JMM1
         DO I = 1, IMB2
            A(I+I+1, J-1) = FN1K(I,2,J)
            A(I+I+2, J-1) = FN2K(I,2,J)
         ENDDO
      ENDDO
      DO J = 1, JMM2
         A(1,J) = 0.0
         A(2,J) = 0.0
         A(IMP2,J) = 0.0
      enddo



      DO J = 2, JMM1
         AM(1,1,J) = W(J)
         AM(2,2,J) = W(J)
         AM(1,2,J) = 0.0
         AM(2,1,J) = 0.0

         CM(1,1,J) = U(J)
         CM(2,2,J) = U(J)
         CM(1,2,J) = 0.0
         CM(2,1,J) = 0.0

         BM(1,1,J) = - U(J) - W(J)
         BM(1,2,J) = R*DYSQ
         BM(2,1,J) = - DYSQ
         BM(2,2,J) = BM(1,1,J)

         DN1(1,J) = FM(J)
         DN1(2,J) = 0.0
      ENDDO

      DN1(1,1) = FM(1)
      DN1(2,1) = 0.0

      AM(1,1,1) = - 4.0
      AM(1,2,1) =   R*DYSQ
      AM(2,1,1) = - DYSQ
      AM(2,2,1) = - 4.0

      BM(1,1,1) = 4.0
      BM(1,2,1) = 0.0
      BM(2,1,1) = 0.0
      BM(2,2,1) = 4.0


      DN1(1,JM) = FM(JM)
      DN1(2,JM) = 0.0

      AM(1,1,JM) = 4.0
      AM(1,2,JM) = 0.0
      AM(2,1,JM) = 0.0
      AM(2,2,JM) = 4.0

      BM(1,1,JM) = - 4.0
      BM(1,2,JM) =   R*DYSQ
      BM(2,1,JM) = - DYSQ
      BM(2,2,JM) = - 4.0

      do j=1,2*jm
         FN1(J,1) = 0.0
      enddo

      CALL SOMEAN(DN1, FN1, AM, BM, CM, JM, BETA1, BETA2)

      CALL RFFTMLT(A,B,TR,IX,1,IMP2,IM,JMM2,BACKWARD)

      DO J = 1, JMM2
         DO I = 1, IM
            FO(I,J+1) = A(I,J) + FN1(2,J+1)
         ENDDO
      ENDDO

      DO I = 1, IM
         FO(I,1) = FN1(2,1)
         FO(I,JM) = FN1(2,JM)
      ENDDO
C
      RETURN
      END

      SUBROUTINE S2ODEN(LEN, DN1, DN2, FN1, FN2, AM, BM, CM, JM 
     *,                ALPHA, BETA1, BETA2, WRK1, WRK2, VRK1, VRK2)

c
      parameter (N=2)
c
C******************************************************************
C                Subroutine S2ODE
C******************************************************************
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C Modified by S. Moorthi, NMC, May 1994
C
C PURPOSE: This routine solves the second-order ODE which originally res
C the \del^4 equation on the sphere.  But appplication is not so restric
C as far as the discretized equation is of the following format.
C The unknown to be solved is a 2x1 vector (Lindzen and Kuo, 1969, MWR).
C Therefore this routine is used here to solve for the FFT  coefficients
C in the \del^4 direct solver if the the B.C's are chosen to be zero for
C
C The discretized form of the ODE is:
C
C               A(j) X(j-1) + B(j) X(j) + C(j) X(j+1) = F(j) ......(1)
C                       (for j = 2, 3, 4, ....., JM-1)
C
C The boundary conditions are:
C
C                   A(1) X(1) + B(1) X(2)  = F(1)         ........(2.1)
C
C           and     A(JM) X(JM-1) + B(JM) X(JM) = F(JM)   ........(2.2)
C
C The coefficient A, B and C are all real.  The forcing F and hence the
C solution X have both real and imaginery parts.
C
C CALLED FROM:
C     DIRSOL4
C
C SUBROUTINES CALLED:
C     INVERSE1: inversion of one 2x2 matrix;
C     INVERSE2: inversion of a number of 2x2 matrices;
C     MULTABK:  multiplication of a number of two matrix set; (not used)
C     MULTAVK : multiplication of a number of a matrix-vector sets. (not used)
C
C VARIABLES:
C
C     INPUT:
C        AM-array storing the coefficient matrix A as in (1);
C        BM-array storing the coefficient matrix B as in (1);
C        CM-array storing the coefficient matrix C as in (1);
C        DN1-array storing the real part of the vector of the forcing
C            term F in (1);
C        DN2-array storing the imaginery part of the vector of the forci
C            term F in (1);
C        JM-number of total grid points including the two boundary point
C
C     OUTPUT:
C        FN1-array storing the real part of the solution vector X of (1)
C        FN2-array storing the imaginery part of the solution vector X o
C
C     LOCAL:
C        ALPHA-array storing intermediate coefficient matrix alpha
C              (Lindzen and Kuo, 1969);
C        BETA1-real part of vector beta (Lindzen and Kuo, 1969);
C        BETA2-imaginery part of vector beta (Lindzen and Kuo, 1969);
C        VRK1-working array;
C        VRK2-working array;
C        WRK1-working array;
C        WRK2-working array;
C
C***********************************************************************
C
      DIMENSION DN1(LEN,N,JM), DN2(LEN,N,JM), FN1(LEN,N,JM)
     #,         FN2(LEN,N,JM)

      DIMENSION AM(LEN,N,N,JM), BM(LEN,N,N,JM), CM(LEN,N,N,JM)

      DIMENSION ALPHA(LEN,N,N,JM), BETA1(LEN,N,JM), BETA2(LEN,N,JM)

      DIMENSION WRK1(LEN,N,N), WRK2(LEN,N,N), VRK1(LEN,N), VRK2(LEN,N)

c
CFPP$ NOCONCUR R
c
      JMM1 = JM - 1
c
c     CALL INVERSE2(LEN, AM(1,1,1,1), WRK1)
C
      DO I = 1, N
        DO J = 1, N
          do il=1,len
             ALPHA(il,I,J,1) = 0.0
          enddo
c         DO K = 1, N
c            DO il = 1, LEN
c              ALPHA(il,I,J,1) = ALPHA(il,I,J,1)
c    *                         - wrk1(il,I,K) * BM(il,K,J,1)
c            ENDDO
c         ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,  N
        do ii=1,len
           BETA1(ii,I,1) = 0.0
           BETA2(ii,I,1) = 0.0
        enddo
c       DO J = 1, N
c         do ii=1,len
c            BETA1(ii,I,1) = BETA1(ii,I,1) + WRK1(ii,I,J)*DN1(ii,J,1)
c            BETA2(ii,I,1) = BETA2(ii,I,1) + WRK1(ii,I,J)*DN2(ii,J,1)
c         enddo
c       ENDDO
      ENDDO
C
      DO JJ = 2, JMM1
C
        DO I = 1, N
          DO J = 1, N
            do il=1,len
               WRK1(il,I,J) =  BM(il,I,J,JJ)
            enddo
            DO K = 1, N
               DO il = 1, LEN
                 WRK1(il,I,J) = WRK1(il,I,J)
     *                           + AM(il,I,K,JJ) * ALPHA(il,K,J,JJ-1)
               ENDDO
            ENDDO  
          ENDDO 
        ENDDO 
C
         CALL INVERSE2(LEN,WRK1,WRK2)
C
        DO I = 1, N
          DO J = 1, N
            do il=1,len
               ALPHA(il,I,J,JJ) =  0.0
            enddo
            DO K = 1, N
               DO il = 1, LEN
                 ALPHA(il,I,J,JJ) = ALPHA(il,I,J,JJ)
     *                            - WRK2(il,I,K) * CM(il,K,J,JJ)
               ENDDO
            ENDDO
          ENDDO
        ENDDO
C
        DO I = 1,  N
          do ii=1,len
             VRK1(ii,I) = DN1(ii,I,JJ)
             VRK2(ii,I) = DN2(ii,I,JJ)
          enddo
          DO J = 1, N
            do ii=1,len
             VRK1(ii,I) = VRK1(ii,I) - AM(ii,I,J,JJ)*BETA1(ii,J,JJ-1)
             VRK2(ii,I) = VRK2(ii,I) - AM(ii,I,J,JJ)*BETA2(ii,J,JJ-1)
            enddo
          ENDDO
        ENDDO
C
        DO I = 1,  N
          do ii=1,len
             BETA1(ii,I,JJ) = 0.0
             BETA2(ii,I,JJ) = 0.0
          enddo
          DO J = 1, N
            do ii=1,len
             BETA1(ii,I,JJ) = BETA1(ii,I,JJ) + WRK2(ii,I,J)*VRK1(ii,J)
             BETA2(ii,I,JJ) = BETA2(ii,I,JJ) + WRK2(ii,I,J)*VRK2(ii,J)
            enddo
          ENDDO 
        ENDDO 
C
      ENDDO
C
c     DO I = 1,  N
c       do ii=1,len
c          VRK1(ii,I) = DN1(ii,I,JM)
c          VRK2(ii,I) = DN2(ii,I,JM)
c       enddo
c       DO J = 1, N
c         do ii=1,len
c            VRK1(ii,I) = VRK1(ii,I) - AM(ii,I,J,JM)*BETA1(ii,J,JMM1)
c            VRK2(ii,I) = VRK2(ii,I) - AM(ii,I,J,JM)*BETA2(ii,J,JMM1)
c         enddo
c       ENDDO
c     ENDDO
C
c     DO I = 1, N
c       DO J = 1, N
c         do il=1,len
c            WRK2(il,I,J) = BM(il,I,J,JM)
c         enddo
c         DO K = 1, N
c            DO il = 1, LEN
c              WRK2(il,I,J) = WRK2(il,I,J)
c    *                      + AM(il,I,K,JM) * ALPHA(il,K,J,JMM1)
c            ENDDO
c         ENDDO
c       ENDDO
c     ENDDO
C
c     CALL INVERSE2(LEN, WRK2, WRK1)
c
      DO I = 1,  N
        do ii=1,len
           FN1(ii,I,JM) = 0.0
           FN2(ii,I,JM) = 0.0
        enddo
c       DO J = 1, N
c         do ii=1,len
c            FN1(ii,I,JM) = FN1(ii,I,JM) + wrk1(ii,I,J)*VRK1(ii,J)
c            FN2(ii,I,JM) = FN2(ii,I,JM) + wrk1(ii,I,J)*VRK2(ii,J)
c         enddo
c       ENDDO
      ENDDO
c
      DO JJ = JMM1, 1, -1
        DO I = 1,  N 
          do ii=1,len 
           FN1(ii,I,JJ) = BETA1(ii,I,JJ)
           FN2(ii,I,JJ) = BETA2(ii,I,JJ)
          enddo 
          DO J = 1, N 
            do ii=1,len
             FN1(ii,I,JJ) = FN1(ii,I,JJ) 
     *                    + ALPHA(ii,I,J,JJ)*FN1(ii,J,JJ+1)
             FN2(ii,I,JJ) = FN2(ii,I,JJ)
     *                    + ALPHA(ii,I,J,JJ)*FN2(ii,J,JJ+1) 
            enddo 
          ENDDO 
        ENDDO 
C
      ENDDO

      RETURN
      END


      SUBROUTINE S2ODE(LEN, DN1, DN2, FN1, FN2, AM, BM, CM, JM 
     *,                ALPHA, BETA1, BETA2, WRK1, WRK2, VRK1, VRK2)

c
      parameter (N=2)
c
C******************************************************************
C                Subroutine S2ODE
C******************************************************************
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C Modified by S. Moorthi, NMC, May 1994
C
C PURPOSE: This routine solves the second-order ODE which originally res
C the \del^4 equation on the sphere.  But appplication is not so restric
C as far as the discretized equation is of the following format.
C The unknown to be solved is a 2x1 vector (Lindzen and Kuo, 1969, MWR).
C Therefore this routine is used here to solve for the FFT  coefficients
C in the \del^4 direct solver if the the B.C's are chosen to be zero for
C
C The discretized form of the ODE is:
C
C               A(j) X(j-1) + B(j) X(j) + C(j) X(j+1) = F(j) ......(1)
C                       (for j = 2, 3, 4, ....., JM-1)
C
C The boundary conditions are:
C
C                   A(1) X(1) + B(1) X(2)  = F(1)         ........(2.1)
C
C           and     A(JM) X(JM-1) + B(JM) X(JM) = F(JM)   ........(2.2)
C
C The coefficient A, B and C are all real.  The forcing F and hence the
C solution X have both real and imaginery parts.
C
C CALLED FROM:
C     DIRSOL4
C
C SUBROUTINES CALLED:
C     INVERSE1: inversion of one 2x2 matrix;
C     INVERSE2: inversion of a number of 2x2 matrices;
C     MULTABK:  multiplication of a number of two matrix set; (not used)
C     MULTAVK : multiplication of a number of a matrix-vector sets. (not used)
C
C VARIABLES:
C
C     INPUT:
C        AM-array storing the coefficient matrix A as in (1);
C        BM-array storing the coefficient matrix B as in (1);
C        CM-array storing the coefficient matrix C as in (1);
C        DN1-array storing the real part of the vector of the forcing
C            term F in (1);
C        DN2-array storing the imaginery part of the vector of the forci
C            term F in (1);
C        JM-number of total grid points including the two boundary point
C
C     OUTPUT:
C        FN1-array storing the real part of the solution vector X of (1)
C        FN2-array storing the imaginery part of the solution vector X o
C
C     LOCAL:
C        ALPHA-array storing intermediate coefficient matrix alpha
C              (Lindzen and Kuo, 1969);
C        BETA1-real part of vector beta (Lindzen and Kuo, 1969);
C        BETA2-imaginery part of vector beta (Lindzen and Kuo, 1969);
C        VRK1-working array;
C        VRK2-working array;
C        WRK1-working array;
C        WRK2-working array;
C
C***********************************************************************
C
      DIMENSION DN1(LEN,N,JM), DN2(LEN,N,JM), FN1(LEN,N,JM)
     #,         FN2(LEN,N,JM)

      DIMENSION AM(LEN,N,N,JM), BM(LEN,N,N,JM), CM(LEN,N,N,JM)

      DIMENSION ALPHA(LEN,N,N,JM), BETA1(LEN,N,JM), BETA2(LEN,N,JM)

      DIMENSION WRK1(LEN,N,N), WRK2(LEN,N,N), VRK1(LEN,N), VRK2(LEN,N)

c
CFPP$ NOCONCUR R
c
      JMM1 = JM - 1
c
      CALL INVERSE2(LEN, AM(1,1,1,1), WRK1)
C
      DO I = 1, N
        DO J = 1, N
          do il=1,len
             ALPHA(il,I,J,1) = 0.0
          enddo
          DO K = 1, N
             DO il = 1, LEN
               ALPHA(il,I,J,1) = ALPHA(il,I,J,1)
     *                         - wrk1(il,I,K) * BM(il,K,J,1)
             ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,  N
        do ii=1,len
           BETA1(ii,I,1) = 0.0
           BETA2(ii,I,1) = 0.0
        enddo
        DO J = 1, N
          do ii=1,len
             BETA1(ii,I,1) = BETA1(ii,I,1) + WRK1(ii,I,J)*DN1(ii,J,1)
             BETA2(ii,I,1) = BETA2(ii,I,1) + WRK1(ii,I,J)*DN2(ii,J,1)
          enddo
        ENDDO
      ENDDO
C
      DO JJ = 2, JMM1
C
        DO I = 1, N
          DO J = 1, N
            do il=1,len
               WRK1(il,I,J) =  BM(il,I,J,JJ)
            enddo
            DO K = 1, N
               DO il = 1, LEN
                 WRK1(il,I,J) = WRK1(il,I,J)
     *                           + AM(il,I,K,JJ) * ALPHA(il,K,J,JJ-1)
               ENDDO
            ENDDO  
          ENDDO 
        ENDDO 
C
         CALL INVERSE2(LEN,WRK1,WRK2)
C
        DO I = 1, N
          DO J = 1, N
            do il=1,len
               ALPHA(il,I,J,JJ) =  0.0
            enddo
            DO K = 1, N
               DO il = 1, LEN
                 ALPHA(il,I,J,JJ) = ALPHA(il,I,J,JJ)
     *                            - WRK2(il,I,K) * CM(il,K,J,JJ)
               ENDDO
            ENDDO
          ENDDO
        ENDDO
C
        DO I = 1,  N
          do ii=1,len
             VRK1(ii,I) = DN1(ii,I,JJ)
             VRK2(ii,I) = DN2(ii,I,JJ)
          enddo
          DO J = 1, N
            do ii=1,len
             VRK1(ii,I) = VRK1(ii,I) - AM(ii,I,J,JJ)*BETA1(ii,J,JJ-1)
             VRK2(ii,I) = VRK2(ii,I) - AM(ii,I,J,JJ)*BETA2(ii,J,JJ-1)
            enddo
          ENDDO
        ENDDO
C
        DO I = 1,  N
          do ii=1,len
             BETA1(ii,I,JJ) = 0.0
             BETA2(ii,I,JJ) = 0.0
          enddo
          DO J = 1, N
            do ii=1,len
             BETA1(ii,I,JJ) = BETA1(ii,I,JJ) + WRK2(ii,I,J)*VRK1(ii,J)
             BETA2(ii,I,JJ) = BETA2(ii,I,JJ) + WRK2(ii,I,J)*VRK2(ii,J)
            enddo
          ENDDO 
        ENDDO 
C
      ENDDO
C
      DO I = 1,  N
        do ii=1,len
           VRK1(ii,I) = DN1(ii,I,JM)
           VRK2(ii,I) = DN2(ii,I,JM)
        enddo
        DO J = 1, N
          do ii=1,len
             VRK1(ii,I) = VRK1(ii,I) - AM(ii,I,J,JM)*BETA1(ii,J,JMM1)
             VRK2(ii,I) = VRK2(ii,I) - AM(ii,I,J,JM)*BETA2(ii,J,JMM1)
          enddo
        ENDDO
      ENDDO
C
      DO I = 1, N
        DO J = 1, N
          do il=1,len
             WRK2(il,I,J) = BM(il,I,J,JM)
          enddo
          DO K = 1, N
             DO il = 1, LEN
               WRK2(il,I,J) = WRK2(il,I,J)
     *                      + AM(il,I,K,JM) * ALPHA(il,K,J,JMM1)
             ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      CALL INVERSE2(LEN, WRK2, WRK1)
c
      DO I = 1,  N
        do ii=1,len
           FN1(ii,I,JM) = 0.0
           FN2(ii,I,JM) = 0.0
        enddo
        DO J = 1, N
          do ii=1,len
             FN1(ii,I,JM) = FN1(ii,I,JM) + wrk1(ii,I,J)*VRK1(ii,J)
             FN2(ii,I,JM) = FN2(ii,I,JM) + wrk1(ii,I,J)*VRK2(ii,J)
          enddo
        ENDDO
      ENDDO
c
      DO JJ = JMM1, 1, -1
        DO I = 1,  N 
          do ii=1,len 
           FN1(ii,I,JJ) = BETA1(ii,I,JJ)
           FN2(ii,I,JJ) = BETA2(ii,I,JJ)
          enddo 
          DO J = 1, N 
            do ii=1,len
             FN1(ii,I,JJ) = FN1(ii,I,JJ) 
     *                    + ALPHA(ii,I,J,JJ)*FN1(ii,J,JJ+1)
             FN2(ii,I,JJ) = FN2(ii,I,JJ)
     *                    + ALPHA(ii,I,J,JJ)*FN2(ii,J,JJ+1) 
            enddo 
          ENDDO 
        ENDDO 
C
      ENDDO

      RETURN
      END

      SUBROUTINE SOMEAN(DN1, FN1, AM, BM, CM, JM, ALPHA, BETA1)
c
      parameter (N=2)
c
C******************************************************************
C                Subroutine SOMEAN
C******************************************************************
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: This routine solves the second-order ODE which originally res
C the \del^4 equation on the sphere.  But appplication is not so restric
C as far as the discretized equation is of the following format.
C The unknown to be solved is a 2x1 vector (Lindzen and Kuo, 1969, MWR).
C This routine is used here to solve for the zonal mean, equivalent to F
C coefficients (k = 0) in the \del^4 direct solver.
C
C The discretized form of the ODE is:
C
C               A(j) X(j-1) + B(j) X(j) + C(j) X(j+1) = F(j) ......(1)
C                       (for j = 2, 3, 4, ....., JM-1)
C
C The boundary conditions are:
C
C                   A(1) X(1) + B(1) X(2)  = F(1)         ........(2.1)
C
C           and     A(JM) X(JM-1) + B(JM) X(JM) = F(JM)   ........(2.2)
C
C The coefficient A, B and C are all real.  The forcing F and hence the
C solution X also real.
C
C CALLED FROM:
C     DIRSOL4
C
C SUBROUTINES CALLED:
C     INVERSE: matrix inversion;
C     MULTAB: multiplication of two matrix;            (not used)
C     MULTAV: multiplication of a matrix and a vector. (not used)
C
C VARIABLES:
C
C     INPUT:
C        AM-array storing the coefficient matrix A as in (1);
C        BM-array storing the coefficient matrix B as in (1);
C        CM-array storing the coefficient matrix C as in (1);
C        DN1-array storing the vector of the forcing term F in (1);
C        JM-number of total grid points including the two boundary point
C
C     OUTPUT:
C        FN1-array storing the solution vector X of (1);
C
C     LOCAL:
C        ALPHA-array storing intermediate coefficient matrix alpha
C              (Lindzen and Kuo, 1969);
C        BETA1-real part of vector beta (Lindzen and Kuo, 1969);
C        VRK1-working array;
C        WRK1-working array;
C        WRK2-working array;
C***********************************************************************
      DIMENSION AM(N,N,JM), BM(N,N,JM), CM(N,N,JM)
     #,         DN1(N,JM), FN1(N,JM), ALPHA(N,N,JM), BETA1(N,JM)

      DIMENSION WRK1(N,N), WRK2(N,N)

      DIMENSION VRK1(N)

c
CFPP$ NOCONCUR R
c
      CALL INVERSE1(AM(1,1,1), WRK1)
C
      DO I = 1, N
         DO J = 1, N
            ALPHA(I,J,1) = 0.0
            DO K = 1, N
               ALPHA(I,J,1) = ALPHA(I,J,1) - WRK1(I,K)*BM(K,J,1)
            ENDDO
         ENDDO
      ENDDO
C
      DO I = 1,  N
         BETA1(I,1) = 0.0
         DO J = 1, N
            BETA1(I,1) = BETA1(I,1) + WRK1(I,J)*DN1(J,1)
         ENDDO
      ENDDO
C
      DO JJ = 2, JM-1
C
        DO I = 1,  N
          DO J = 1, N
            WRK1(I,J) = BM(I,J,JJ)
            DO K = 1, N
              WRK1(I,J) = WRK1(I,J) + AM(I,K,JJ)*ALPHA(K,J,JJ-1)
            ENDDO   
          ENDDO   
        ENDDO
C
         CALL INVERSE1(WRK1,WRK2)
C
        DO I = 1,  N 
          DO J = 1, N
            ALPHA(I,J,JJ) = 0.0
            DO K = 1, N 
              ALPHA(I,J,JJ) = ALPHA(I,J,JJ) - WRK2(I,K)*CM(K,J,JJ) 
            ENDDO   
          ENDDO
        ENDDO
c
        DO I = 1,  N
          VRK1(I) = DN1(I,JJ)
          DO J = 1, N
            VRK1(I) = VRK1(I) - AM(I,J,JJ)*BETA1(J,JJ-1)
          ENDDO    
        ENDDO  
        DO I = 1,  N
          BETA1(I,JJ) = 0.0
          DO J = 1, N
            BETA1(I,JJ) = BETA1(I,JJ) + WRK2(I,J)*VRK1(J)
          ENDDO
        ENDDO
C
      ENDDO
      DO I = 1,  N
        VRK1(I) = DN1(I,JM)
        DO J = 1, N
          VRK1(I) = VRK1(I) - AM(I,J,JM)*BETA1(J,JM-1)
        ENDDO
      ENDDO

      DO I = 1,  N
        DO J = 1, N
          WRK2(I,J) = BM(I,J,JM)
          DO K = 1, N
            WRK2(I,J) = WRK2(I,J) + AM(I,K,JM)*ALPHA(K,J,JM-1)
          ENDDO
        ENDDO
      ENDDO
C
      CALL INVERSE1(WRK2, WRK1)
C
      DO I = 1,  N
        FN1(I,JM) = 0.0
        DO J = 1, N
          FN1(I,JM) = FN1(I,JM) + WRK1(I,J)*VRK1(J)
        ENDDO
      ENDDO 
C
      DO JJ = JM-1, 1, -1
        DO I = 1,  N
          FN1(I,JJ) = BETA1(I,JJ)
          DO J = 1, N
            FN1(I,JJ) = FN1(I,JJ) + ALPHA(I,J,JJ)*FN1(J,JJ+1)
          ENDDO
        ENDDO 
      ENDDO

      RETURN
      END

      SUBROUTINE INVERSE1(A,B)
C****************************************************************
C               Subroutine INVERSE1
C****************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Find the inverse of a 2x2 matrix
C
C VARIABLES:
C     INPUT:
C          A-input matrix to be inverted;
C     OUTPUT:
C          B-inverse of matrix A;
C****************************************************************
      DIMENSION A(2,2), B(2,2)
CFPP$ NOCONCUR R

      DD = 1.0 / (A(1,1)*A(2,2) - A(1,2)*A(2,1))

      B(1,1) =   A(2,2) * DD
      B(1,2) = - A(1,2) * DD
      B(2,1) = - A(2,1) * DD
      B(2,2) =   A(1,1) * DD

      RETURN
      END


      SUBROUTINE INVERSE2(LEN,A,B)
C****************************************************************
C               Subroutine INVERSE2
C****************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Find the inverse of a a number (LEN) of 2x2 matrix
C
C VARIABLES:
C     INPUT:
C          A-input of LEN 2x2 matrices to be inverted simultaneously;
C     OUTPUT:
C          B-inverse the LEN 2x2 matrices A;
C****************************************************************

      Dimension A(LEN,2,2), B(LEN,2,2)

CFPP$ NOCONCUR R
      DO K = 1, LEN
         DD = 1.0 / (A(K,1,1)*A(K,2,2) - A(K,1,2)*A(K,2,1))
c
         B(K,1,1) =   A(K,2,2) * DD
         B(K,1,2) = - A(K,1,2) * DD
         B(K,2,1) = - A(K,2,1) * DD
         B(K,2,2) =   A(K,1,1) * DD
      ENDDO

      RETURN
      END



      SUBROUTINE MULTAB(A,B,C,N)
C***************************************************************
C            Subroutine MULTAB
C***************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Multiplication of two NxN matrix C = A B
C
C VARIABLES:
C     INPUT:
C          A-input matrix;
C          B-input matrix;
C     OUTPUT:
C          C-output matrix.
C***************************************************************
      DIMENSION A(N,N), B(N,N), C(N,N)
CFPP$ NOCONCUR R
      DO I = 1, N
         DO J = 1, N
            C(I,J) = 0.0
            DO K = 1, N
               C(I,J) = C(I,J) + A(I,K)*B(K,J)
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE MULTABK(LEN,A,B,C,N)
C***************************************************************
C            Subroutine MULTABK
C***************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Multiplication of a series of two NxN matrix C = A B
C
C VARIABLES:
C     INPUT:
C          A-input of LEN 2x2 matrices;
C          B-input of LEN 2x2 matrixes;
C     OUTPUT:
C          C-output matrix.
C***************************************************************
      DIMENSION A(LEN,N,N), B(LEN,N,N), C(LEN,N,N)
c
CFPP$ NOCONCUR R
      DO I = 1, N
         DO J = 1, N
            do il=1,len
               C(il,I,J) = 0.0
            enddo
            DO K = 1, N
               DO il = 1, LEN
                  C(il,I,J) = C(il,I,J) + A(il,I,K)*B(il,K,J)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END


      SUBROUTINE MULTAV(A,V,W,N)
C***************************************************************
C            Subroutine MULTAV
C***************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Multiplication of an NxN matrix A and a vector V: W = A V
C
C VARIABLES:
C     INPUT:
C          A-input matrix;
C          B-input vector;
C     OUTPUT:
C          C-output vector.
C***************************************************************
      DIMENSION A(N,N), V(N), W(N)
CFPP$ NOCONCUR R
      DO I = 1,  N
         W(I) = 0.0
         DO J = 1, N
            W(I) = W(I) + A(I,J)*V(J)
         ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE MULTAVK(LEN,A,V,W,N)
C***************************************************************
C            Subroutine MULTAVK
C***************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Multiplication of an NxN matrix A and a vector V: W = A V
C
C VARIABLES:
C     INPUT:
C          A-input of LEN 2x2 matrices;
C          B-input of LEN vectors;
C     OUTPUT:
C          C-output vector.
C***************************************************************
c
      DIMENSION A(LEN,N,N), V(LEN,N), W(LEN,N)
c
CFPP$ NOCONCUR R
      DO I = 1,  N
         do ii=1,len
            W(ii,I) = 0.0
         enddo
         DO J = 1, N
            do ii=1,len
               W(ii,I) = W(ii,I) + A(ii,I,J)*V(ii,J)
            enddo
         ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE INVERSE(A,Y,NP)
C*******************************************************************
C              Subroutine INVERSE
C*******************************************************************
C
C WRITTEN BY: Yong Li, GSFC Code 910.3, Apr 1994
C
C PURPOSE: Invert an NPxNP matrix.
C
C SUBROUTINES CALLED:
C     LUDCMP: LU decomposition of matrix A;
C     LUBKSB: Solves the the set of NP linear equations after
C             LU decomposition of matrix A.
C VARIABLES:
C     INPUT:
C        A-matrix to be inverted;
C        NP-dimension size of the matrix (NPxNP).
C     OUTPUT:
C        Y-the inverse matrix of A.
C     LOCAL:
C        INDX-array storing the number of row pivoting in the LU
C             decomposition.
C*********************************************************************
      DIMENSION A(NP,NP), Y(NP,NP), INDX(NP)
CFPP$ NOCONCUR R

      N=NP

c     Y(:,:) = 0.0
      do jj = 1, NP
          do ii = 1, NP
             Y(ii,jj) = 0.0
          enddo
      enddo
      DO I = 1, N
         Y(I,I) = 1.0
      ENDDO

      CALL LUDCMP(A,N,NP,INDX,D)

      DO J = 1, N
         CALL LUBKSB(A,N,NP,INDX,Y(1,J))
      ENDDO

      RETURN
      END

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
C***********************************************************************
C              Subroutine LUBKSB
C***********************************************************************
C
C WRITTEN BY: Numerical Recipes (Page 36)
C
C PURPOSE: Solve the set of N linear equations A X  = B. Here A is input
C          as matrix A but rather as its LU decomposition.  Solutions ar
C          in B.
C***********************************************************************
      DIMENSION A(NP,NP),INDX(N),B(N)
CFPP$ NOCONCUR R
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
C**********************************************************************
C                 Subroutine LUDCMP
CC**********************************************************************
C
C WRITTEN BY: Numerical Recipes (Page 34)
C
C PURPOSE: Given an NxN matrix A, with physical dimension NP, this routi
C          replaces it by the LU decomposition of a rowwise permutation
C          itself.  See Numerical Recipes for further details.
C***********************************************************************
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
CFPP$ NOCONCUR R
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END
      SUBROUTINE DEL4(IMA,IM,JM,A,DA,AE,DIFF)
C************************************************************************
C             Subroutine DEL4
C************************************************************************
C WRITTEN BY: Yong Li, GSFC code 910.3, Apr 1994
C
C PUEPOSE: calculate the \del^4(A) by applying a conservative \del^2 twice.
C
C CALLED FROM:
C       FORCE: calculate the forcing term
C
C SUBROUTINES CALLED:
C       DEL2: \del^2 operator;
C       WRAPD: wrap up the ghost points in X-direction
C
C VARIABLES:
C
C     INPUT:
C       A-input field;
C       AE-radius of earth;
C       IM-number of total grid points in X-direction;
C       IMA-dimension size in X-direction;
C       JM-number of total grid points in Y-direction;
C
C     OUTPUT:
C       DA-\del^4(A)
C************************************************************************
      DIMENSION A(IMA,JM), DA(IMA,JM), B(IMA,JM)
CFPP$ NOCONCUR R
      logical DIFF

      CALL DEL2(IMA,IM,JM,A,B,AE, DIFF)
      CALL WRAPD(IMA,IM,JM,B)

      CALL DEL2(IMA,IM,JM,B,DA,AE, DIFF)
      CALL WRAPD(IMA,IM,JM,DA)

      RETURN
      END

      SUBROUTINE WRAPD(IMA,IM,JM,A)
      DIMENSION A(IMA,JM)

CFPP$ NOCONCUR R
      DO J = 1, JM
         A(1,J) = A(IM+1,J)
         DO I = IM+2, IMA
            A(I,J) = A(I-IM,J)
         ENDDO
      ENDDO

      RETURN
      END
      SUBROUTINE WRAP(IMA,IM,JM,A)
      DIMENSION A(IMA,JM)
      
CFPP$ NOCONCUR R
      DO J = 1, JM
         DO I = IM+1, IMA
            A(I,J) = A(I-IM,J)
         ENDDO
      ENDDO
      
      RETURN
      END
 
      SUBROUTINE DEL2(IMA,IM,JM,A,DA,AE, DIFF)
C*******************************************************************
C               Subroutine DEL2
C******************************************************************
C WRITTEN BY: Yong Li, GSFC code 910.3, Apr 1994
C    
C PURPOSE: calculate the \del^2(A) on the sphere.
C    
C CALLED BY:
C       DEL4
C    
C SUBROUTINES CALLED:    None
C     
C VARIABLES:
C    
C     INPUT:
C       A-input field;
C       AE-radius of earth;
C       IM-number of total grid points in X-direction;
C       IMA-dimension size in X-direction;
C       JM-number of total grid points in Y-direction;
C     
C     OUTPUT:
C       DA-\del^2 of A
C********************************************************************
      dimension a(ima,jm), da(ima,jm), dx(jm)
      dimension a1(jm), a3(jm), bp(jm), bm(jm)

      DIMENSION IX(IM), TR(3*(IM+2)+1)

      DIMENSION C(IM+2,JM-2), B(IM*2,JM)

      logical diff
 
CFPP$ NOCONCUR R
      imp1 = im + 1
      imp2 = im + 2
      pi = acos(-1.0)
      twopi = 2.0*pi
      dlm = twopi/float(im)
      dph = pi/float(jm-1)
      dy = ae*dph
 
      a2=1.0d0/dy**2
 
      do j = 2, jm-1
         theta = - pi*0.5 + float(j-1)*dph
         bp(j) = cos(theta+0.5*dph)/cos(theta)
         bm(j) = cos(theta-0.5*dph)/cos(theta)
         dx(j) = ae*dlm*cos(theta)
         a1(j) = 1.0/dx(j)**2
         a3(j) = 2.0*a1(j)+a2*(bp(j)+bm(j))
      enddo

      if (diff) then
         do 20 i=2,imp1
         do 30 j=2,jm-1
         da(i,j) = a1(j)*(a(i+1,j)+a(i-1,j)-a(i,j)-a(i,j))
     #           + a2 * (bp(j)*(a(i,j+1)-a(i,j))
     *           +       bm(j)*(a(i,j-1)-a(i,j)))
 30      continue
 20      continue
      else
         CALL FFTFAX(IM,IX,TR)
         jmm2 = jm - 2
         do j=1,jmm2
           do i=1,im
             c(i,j) = a(i,j+1)
           enddo
           c(im+1,j) = c(1,j)
           c(im+2,j) = c(2,j)
         enddo

         CALL RFFTMLT(C,B,TR,IX,1,IMP2,IM,JMM2,-1)
c
c        B(:,:) = 0.0
c        do i=1,imp2*jmm2
c        B(i,2) = C(i,1)
c        enddo
         do j=1,jmm2
            c(1,j) = 0.0
            c(2,j) = 0.0
            do i=1,im/2
              ii = 2*i+1
              tem = - i * i
c             tem = - i * i * dlm * dlm * a1(j)
              c(ii,j)   = tem  * c(ii,j)
c    #             + a2 * (bp(j)*(b(ii,j+2)-b(ii,j+1))
c    *             +       bm(j)*(b(ii,j)-b(ii,j+1)))
              c(ii+1,j) = tem  * c(ii+1,j)
c    #             + a2 * (bp(j)*(b(ii+1,j+2)-b(ii+1,j+1))
c    *             +       bm(j)*(b(ii+1,j)-b(ii+1,j+1)))
            enddo
c           c(1,j) =  a2 * (bp(j)*(b(1,j+2)-b(1,j+1))
c    *             +       bm(j)*(b(1,j)-b(1,j+1)))
         
         enddo
         CALL RFFTMLT(C,B,TR,IX,1,IMP2,IM,JMM2,1)
         do j=2,jm-1
           do i=1,im
             da(i,j) = a1(j)*c(i,j-1) * dlm * dlm
     #               + a2 * (bp(j)*(a(i,j+1)-a(i,j))
     *               +       bm(j)*(a(i,j-1)-a(i,j)))
c            da(i,j) = c(i,j)
           enddo
           da(im+1,j) = da(1,j)
        enddo
      endif
c
      do j = 2, jm-1
         da(1,j) = da(im+1,j)
         do i = im+2, ima
            da(i,j) = da(i-im,j)
         enddo
      enddo

C
C          Laplace at the poles
C
      tem = 4.0 * a2 / float(im)
      AACS = 0.0
      AACN = 0.0
      do 40 i = 2, imp1
         AACS = AACS + a(i,2) - a(1,1)
         AACN = AACN + a(i,jm-1) - a(1,jm)
 40   continue

      da(1,1)  = tem*AACS
      da(1,jm) = tem*AACN
C
      do i = 2, ima
        da(i,1)  = da(1,1)
        da(i,jm) = da(1,jm)
      enddo

      return
      end

      SUBROUTINE DELSQ(IM, JM, DXP, DY, CPHU, CPHV, FLD, DSQ
     *,                iw1, ie1, wk1, wk2, wk3)
C
      DIMENSION FLD(IM,JM), DSQ(IM,JM), CPHU(IM,JM), CPHV(IM,JM)
      DIMENSION WK1(IM,JM), WK2(IM,JM), WK3(IM,JM)
     *,         iw1(im*jm), ie1(im*jm)
C
CFPP$ NOCONCUR R
      ijm0m0 = im * jm
      ijm1m0 = ijm0m0 - im
      ijm2m0 = ijm1m0 - im
      i1p1   = im + 1
      tem1   = 1.0 / (dxp * dxp)
      tem2   = 1.0 / (dy * dy)
C
C
      DO 130 I=1,IJM0M0
      WK1(I,1) =  FLD(IE1(I),1) - FLD(I,1)
  130 CONTINUE
C
      DO 140 I=I1P1,IJM1M0
      WK3(I,1) = 1.0 / CPHU(I,1)
      WK1(I,1) = tem1 * WK1(I,1) * WK3(I,1)
  140 CONTINUE

      DO 150 I=I1P1,IJM1M0
      WK2(I,1) = WK1(I,1) - WK1(IW1(I),1)
  150 CONTINUE
C
      DO 170 I=1,IJM1M0
      WK1(I,1) = tem2 * (FLD(I,2) - FLD(I,1)) * CPHV(I,1)
  170 CONTINUE
C
      DO 260 I=1,IJM2M0
      DSQ(I,2)  = (WK2(I,2) + WK1(I,2) - WK1(I,1)) * WK3(I,1)
  260 CONTINUE
C
C          Laplace at the poles
C
      AAS = 0.0
      AAN = 0.0
      do 40 i = 1, im
         AAS = AAS + FLD(i,2)
         AAN = AAN + FLD(i,jm-1)
 40   continue

      dsq(1,1)  = 4.0*tem2*(AAS/float(im) - fld(1,1))
      dsq(1,jm) = 4.0*tem2*(AAN/float(im) - fld(1,jm))

      do 50 i = 2, im
        dsq(i,1)  = dsq(1,1)
        dsq(i,jm) = dsq(1,jm)
 50   continue
C
      RETURN
      END
