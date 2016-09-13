      SUBROUTINE OVLT1T2(CMO1,CMO2,T12,DSTO,CCSET2)
C
C     Purpose: Calculate orbital overlaps between time steps
C
C     History: - Creation (10.07.15, LGMP)
C
C     ******************************************************************
C
C     List of local dimensions:
C
C     DSTO: Dimension of Slater type orbital matrix.
C
C     List of local variables:
C
C     List of local dynamical fields:
C
C     SMAT  : Overlap matrix
C     CMO1  : MO's from present time step
C     CMO2  : MO's from next time step
C     T12   : Final transformed overlap matrix
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER DSTO,CCSET2,ALLOCATION,I,J
      REAL CMO1(DSTO,DSTO,2),CMO2(DSTO,DSTO,2),T12(DSTO,DSTO,2)
C
      REAL,ALLOCATABLE :: SMAT(:,:)
C
C     ------------------------------------------------------------------
C
C     *** Allocate local fields ***
C
      ALLOCATE(SMAT(DSTO,DSTO),STAT=ALLOCATION)
      IF (ALLOCATION.GT.0) THEN
        CALL ERRMSG('OVLT1T2','ALLOCATION FAILED',1)
      END IF
      CALL BLDSMAT2(SMAT,DSTO,CCSET2)
      T12(:,:,1) = SMAT(:,:)
      T12(:,:,2) = SMAT(:,:)
C
C     *** Transform index for time t
C
      DO I = 1,2
C
C     *** Transform index for time t
C
         CALL MPMULMAT(CMO1(1,1,I),T12(1,1,I),SMAT,DSTO,DSTO,'TRANSA')
C
C     *** Transform index for time t+1
C
         CALL MPMULMAT(SMAT,CMO2(1,1,I),T12(1,1,I),DSTO,DSTO,'NORMAL')
      ENDDO
  
C
      DEALLOCATE(SMAT,STAT=ALLOCATION)
      IF (ALLOCATION.GT.0) THEN
        CALL ERRMSG('OVLT12','DEALLOCATION FAILED',1)
      END IF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE OVLT12 ***
C
      END
