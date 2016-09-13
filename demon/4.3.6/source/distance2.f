      REAL FUNCTION DISTANCE2(ATOMA,ATOMB,CCSET2)
C
C     Purpose: Calculation of atomic distance.
C
C     History: - Creation: (07.11.03, TH)
C
C     ******************************************************************
C
C     List of global variables:
C
C     ATOM{A/B}: Atoms between the distance has to be calculated.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'molecule.h'
C
      INTEGER ATOMA,ATOMB,CCSET2
C
C     ------------------------------------------------------------------
C
C     *** Calculate interatomic distance ***
C
      DISTANCE2 = SQRT((COORD(1,ATOMA,CCSET2) - COORD(1,ATOMB,CCSET))**2 
     $               +(COORD(2,ATOMA,CCSET2) - COORD(2,ATOMB,CCSET))**2 
     $               +(COORD(3,ATOMA,CCSET2) - COORD(3,ATOMB,CCSET))**2)
C
C     ------------------------------------------------------------------
C
C     *** End of REAL FUNCTION DISTANCE2 ***
C
      END
