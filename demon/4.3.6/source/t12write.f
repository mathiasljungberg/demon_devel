      SUBROUTINE T12WRITE(T12,DSTO)
C
C     Purpose: Store MO overlap matrix between BOMD time steps for
C              surface hopping analysis
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
C     T12   : Matrix elements
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'fileio.h'
C
      INTEGER DSTO,I,J
      REAL T12(DSTO,DSTO)
      write(*,*) 'writet12 routine ',DSTO
      WRITE(IT12) DSTO
      WRITE(IT12) ((T12(I,J),I=1,DSTO),J=1,DSTO)

      RETURN
      END
         
         
