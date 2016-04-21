C MEMBER STEP22
C
C***********************************************************************
C
       SUBROUTINE STEP22(IYR,IMO,IDA,IHR,IYRF,IMOF,IDAF,IHRF,NHST,NS)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
       DIMENSION IN(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/step22.f,v $
     . $',                                                             '
     .$Id: step22.f,v 1.2 2002/05/15 13:53:20 hank Exp $
     . $' /
C    ===================================================================
C
       DATA IN/31,28,31,30,31,30,31,31,30,31,30,31/
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** STEP22 ENTERED.')
C
       IMOF=IMOF+(IYRF-IYR)*12
       ISS=0
       N2 = IMOF-1
       DO 100 I=IMO,N2
           IF(IMO.GT.IMOF-1)GOTO 101
           I1=I/12
           IF(I.EQ.I1*12)I1=I1-1
           IIYR=IYR+I1
           I3=IIYR/4
           IF(IIYR.EQ.I3*4)IN(2)=29
           IF(IIYR.NE.I3*4)IN(2)=28
           I2=I-12*I1
           ISS=ISS+IN(I2)
100    CONTINUE
C
101    IDDA=ISS*24+(IDAF-IDA)*24+IHRF-IHR
       NS=IDDA/NHST+1
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991    FORMAT(/10X,'** EXIT STEP22.')
C
       RETURN
       END
