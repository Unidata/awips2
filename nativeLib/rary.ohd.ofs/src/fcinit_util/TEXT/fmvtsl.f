C MEMBER FMVTSL
C  (from old member FCFMVTSL)
C
C     DESC - THIS SUBROUTINE WILL RETURN THE LOCATION OF A SPECIFIED
C     DESC - VARIABLE IN A MULTI-VALUED TIME SERIES GIVEN THE DATA
C     DESC - TYPE AND NAME OF THE PARTICULAR VARIABLE DESIRED.
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C               GEORGE F SMITH - 26 NOVEMBER 1980 - HRL
C
C...........................
C
      SUBROUTINE FMVTSL(DTYPE,NAME,LOC)
C...........................
C
C      THE ARGUMENT LIST CONTAINS --
C
C      1. DTYPE - AN A4 VARIABLE WITH THE DATA TYPE CODE FOR A
C                 MULTI-VALUED DATA TYPE
C      2. NAME  - A 2A4 VARIABLE WITH THE NAME OF THE PARTICULAR
C                 VARIABLE OF THE MULTIPLE VALUED TIME SERIES
C                 DESIRED
C      3. LOC   - THE LOCATION IN THE MULTI-VALUED TIME SERIES
C                 FOR THE VARIABLE NAMED ABOVE
C
C           ** IF THE DATA TYPE IS NOT A VALID MULTI-VALUED TYPE   **
C           ** OR THE NAME IS NOT VALID FOR THE DATA TYPE          **
C           ** SPECIFIED LOC WILL BE RETURNED WITH A VALUE OF ZERO **
C
      DIMENSION NAME(2),VALTYP(2),IVALNM(2,12),NVALNM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/fmvtsl.f,v $
     . $',                                                             '
     .$Id: fmvtsl.f,v 1.1 1995/09/17 18:54:41 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA VALTYP/4HSMZC,4HROCL/
      DATA IVALNM/4HUZTD,4HEF  ,4HUZFW,4HC   ,4HLZTD,4HEF  ,
     1            4HLZFS,4HC   ,4HLZFP,4HC   ,
     2            4HTCHA,4HNINF,4HIMP-,4HRO  ,4HDIR-,4HRO  ,
     3            4HSUR-,4HRO  ,4HINTE,4HRFLO,4HSUPB,4HASE ,
     4            4HPRIM,4HBASE/
      DATA NVALNM/5,7/
C
      NVALID=2
C
C     SET LOC TO ZERO
C
      LOC=0
C
C     CHECK FOR VALID MULTI-VALUED DATA TYPE
C
      DO 10 II=1,NVALID
      I=II
      IF(DTYPE.EQ.VALTYP(I))GO TO 20
   10 CONTINUE
C
C     DID NOT FIND VALID DATA TYPE - RETURN
C
      GO TO 9999
C
   20 IX=0
      IF(I.EQ.1)GO TO 40
      IM1=I-1
      DO 30 J=1,IM1
   30 IX=IX+NVALNM(J)
C
C     CHECK FOR VALID NAME WITHIN DATA TYPE
C
   40 JEND=NVALNM(I)
      DO 50 JJ=1,JEND
      J=JJ
      JX=J+IX
      IF(NAME(1).NE.IVALNM(1,JX))GO TO 50
      IF(NAME(2).EQ.IVALNM(2,JX))GO TO 60
   50 CONTINUE
C
C     DID NOT FIND VALID NAME - RETURN
C
      GO TO 9999
C
C     FOUND VALID NAME - SET LOC VALUE
C
   60 LOC=J
C
 9999 RETURN
      END
