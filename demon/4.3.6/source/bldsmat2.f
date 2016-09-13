      SUBROUTINE BLDSMAT2(S,DSTO,CCSET2)
C
C     Purpose: BuiLD overlap (S) MATrix between 2 geometries.
C
C     Lit.: S. Obara, A. Saika, J. Chem. Phys. 84, 3963 (1986)
C
C     History: - Creation from bldsmat (10.07.15, LGMP)
C
C     ******************************************************************
C
C     List of local dimensions:
C
C     DCOP: Dimension of Cartesian orbitals.
C     DSHL: Dimension of shell blocks.
C     DSPP: Dimension of scaling parameters.
C     DSTO: Dimension of Slater type orbital matrix.
C
C     List of local variables:
C
C     L{A/B}   : Orbital L quantum number.
C     NGTO{A/B}: Number of contractions.
C     SKIPSHL  : Skipping of shell combination, if true.
C
C     List of local dynamical fields:
C
C     HRRINT: Work field for horizontal recurrence relation.
C     SHLBLK: Diatomic integral shell block.
C     VRRINT: Work field for vertical recurrence relation.
C     WORK  : Work matrix.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'fileio.h'
      INCLUDE 'flags.h'
      INCLUDE 'molecule.h'
      INCLUDE 'pointer.h'
C
      INTEGER DSTO,CCSET2
      REAL S(DSTO,DSTO)
C
      LOGICAL SKIPSHL
      CHARACTER*80 PRTSTR
      INTEGER IATOM,ISHL,JATOM,JSHL,LA,LB,NGTOA,NGTOB
C
      INTEGER ALLOCATION,DCOP,DIMCOP,DIMSOP,DSHL,DSPP
      REAL,ALLOCATABLE :: HRRINT(:,:),SHLBLK(:,:),VRRINT(:,:),WORK(:,:)
C
C     ------------------------------------------------------------------
C
C     *** Allocate local fields ***
C
      DCOP = DIMCOP(2*LBASMAX(0))
      DSHL = DIMSOP(LBASMAX(0))
      DSPP = DIMSOP(2*LBASMAX(0))
C
      ALLOCATE(HRRINT(DCOP,DCOP),SHLBLK(DSHL,DSHL),
     $         VRRINT(DCOP,DSPP),STAT=ALLOCATION)
      IF (ALLOCATION.GT.0) THEN
        CALL ERRMSG('BLDSMAT2','ALLOCATION FAILED',1)
      END IF
C
C     *** Initialize overlap matrix ***
C
      S(:,:) = 0.0
C
C     *** Initialize integral recurrence fields ***
C
      SHLBLK(:,:) = 0.0
      VRRINT(:,:) = 0.0
C
C     *** Calculate overlap matrix ***
C
      DO 40 IATOM=1,NATOM
        DO 30 JATOM=1,NATOM
          CALL ATOMDIS2(IATOM,JATOM,RAB,RSQ,CCSET2)
          DO 20 ISHL=LLSHL(IATOM),ULSHL(IATOM)
            DO 10 JSHL=LLSHL(JATOM),ULSHL(JATOM)
              LA = SHLPTR(ISHL,3)
              LB = SHLPTR(JSHL,3)
              CALL OSETUP(LA,LB,NGTOA,NGTOB,ISHL,JSHL)
              CALL GTOPRO(NGTOA,NGTOB,SKIPSHL)
              IF (SKIPSHL) GO TO 10
              CALL OCONT(LA,LB,0,NGTOA,NGTOB,0,VRRINT,DCOP,DSPP)
              CALL OVRR(LA,LB,0,HRRINT,VRRINT,DCOP,DSPP)
              CALL HRR(LA,LB,HRRINT,SHLBLK,DCOP,DSHL,'TRANS')
              CALL BLDIMAT(LA,LB,ISHL,JSHL,LLSTO,NCSTO,S,SHLBLK,
     $                     DSTO,DSHL)
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
   40 CONTINUE
C
C     *** Deallocate local fields ***
C
      DEALLOCATE(HRRINT,VRRINT,SHLBLK,STAT=ALLOCATION)
      IF (ALLOCATION.GT.0) THEN
        CALL ERRMSG('BLDSMAT','DEALLOCATION FAILED',1)
      END IF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE BLDSMAT2 ***
C
      END
