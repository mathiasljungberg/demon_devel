      SUBROUTINE WFSTORE(CMO,DSTO,OCCN,OPTION1,OPTION2)
C
C     Purpose: Store wave function for analysis along BOMD trajectory
C
C     History: - Creation (10.07,15, LGMP)
C
C     ******************************************************************
C
C     List of local dimensions:
C
C     DSTO   : Dimension of Slater type orbital matrix.
C
C     List of local variables:
C
C     CMO   : MO coefficients.
C     OCCN  : Occupation numbers
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'fileio.h'
C
      CHARACTER*(*) OPTION1,OPTION2
      INTEGER DSTO,ISTO,JSTO
      REAL CMO(DSTO,DSTO),OCCN(DSTO)

      IF (OPTION2.EQ.'REWIND') REWIND(IOWF)
      IF (OPTION1.EQ.'WRITE') THEN
         WRITE(IOWF) (OCCN(ISTO),ISTO=1,DSTO)
         WRITE(IOWF) ((CMO(JSTO,ISTO),JSTO=1,DSTO),ISTO=1,DSTO)
      ELSE IF (OPTION1.EQ.'READ') THEN
         READ(IOWF) (OCCN(ISTO),ISTO=1,DSTO)
         READ(IOWF) ((CMO(JSTO,ISTO),JSTO=1,DSTO),ISTO=1,DSTO)
      END IF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE WFSTORE ***
C

      END
         
         
