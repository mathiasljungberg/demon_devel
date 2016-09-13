      SUBROUTINE TRJANA
C
C     Purpose: Molecular dynamics TRaJectory ANAlysis.
C
C     History: - Creation (01.11.05, AMK)
C                         (24.07.15, LGMP)
C
C     ******************************************************************
C
C     List of local variables:
C
C     MDCYC : Molecular dynamics cycle of analysis.
C     SIMINT: Step interval for trajectory analysis.
C     TRJMAX: Maximum trajectory entry marked.
C     TRJMIN: Minimum trajectory entry marked.
C
C     List of local dynamical fields:
C
C     LDMSUM: Linear and quadratic Lindemann sums.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'dynamics.h'
      INCLUDE 'fileio.h'
      INCLUDE 'flags.h'
      INCLUDE 'molecule.h'
C
      INTEGER CYCLE, CCSET2
ctest
      integer i,j,lgmp
ctest
C
      INTEGER ALLOCATION
      REAL,ALLOCATABLE :: LDMSUM(:,:,:),CMO1(:,:,:),CMO2(:,:,:),
     $     W(:,:,:),OCC(:,:),WOCC(:)
C
C     ------------------------------------------------------------------
C
C     *** Quick return, if no trajectory analysis is requested ***
C
      IF (TRJTYP.NE.'ANALYSIS') RETURN
C
C     *** Initialize cycle counter ***
C
      CYCLE = 0
C
C     *** Write trajectory header ***
C
      IF (PTRJ.NE.'XES') CALL TRJOUT('HEADER')
C
C     *** Initialize Lindemann parameter or RDF calculation ***
C
      IF (PTRJ.EQ.'LINDEMANN') THEN
        ALLOCATE(LDMSUM(2,NATOM_ALL,NATOM_ALL),STAT=ALLOCATION)
        CALL CHKERRSTAT(ALLOCATION,'TRJANA','ALLOCATION FAILED')
        CALL LINDEMANN(CYCLE,LDMSUM,'INITIALIZE')
      ELSE IF (PTRJ.EQ.'RADDISTR') THEN
        CALL GETRDF(CYCLE,'INITIALIZE')
      ELSE IF (PTRJ.EQ.'XES') THEN
        ALLOCATE(CMO1(NSTO,NSTO,2),CMO2(NSTO,NSTO,2),W(NSTO,NSTO,2),
     $        OCC(NSTO,2),WOCC(NSTO),STAT=ALLOCATION)
        CALL CHKERRSTAT(ALLOCATION,'TRJANA','ALLOCATION FAILED')
        DO MDCYC=1,TRJMIN
           CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
           CMO1(:,:,1) = W(:,:,1)
           OCC(:,1)    = WOCC(:)
           CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
           CMO1(:,:,2) = W(:,:,1)
           OCC(:,2)    = WOCC(:)
        ENDDO
      END IF
C
C     *** Loop over requested trajectory part ***
C
      DO MDCYC=TRJMIN,TRJMAX,SIMINT
C
C     *** Read trajectory step from trajectory tape ***
C
        CYCLE = CYCLE + 1
        RECNO(IOTRJ) = MDCYC
        CALL MDTRJ('READ')
C
C     *** Calculate property value for trajectory analysis ***
C
        IF (PTRJ.EQ.'DIPOLE') THEN
          CALL MDPRO('DIPOLE MOMENT')
        ELSE IF (PTRJ.EQ.'POLARIS') THEN
          CALL MDPRO('POLARIZABILITY')
        ELSE IF (PTRJ.EQ.'NMR') THEN
          CALL MDPRO('NMR SHIELDINGS')
          CALL MDPRO('NSR CONSTANTS')
        ELSE IF (PTRJ.EQ.'MAGNET') THEN
          CALL MDPRO('MAGNETIZABILITY')
        ELSE IF (PTRJ.EQ.'MEANDIS') THEN
          CALL MDPRO('MEAN DISTANCE')
        ELSE IF (PTRJ.EQ.'PROLATE') THEN
          CALL MDPRO('PROLATE')
        ELSE IF (PTRJ.EQ.'LENGTH') THEN
          CALL MDPRO('LENGTH')
        ELSE IF (PTRJ.EQ.'ANGLE') THEN
          CALL MDPRO('ANGLE')
        ELSE IF (PTRJ.EQ.'DIHEDRAL') THEN
          CALL MDPRO('DIHEDRAL')
        ELSE IF (PTRJ.EQ.'LINDEMANN') THEN
          CALL LINDEMANN(CYCLE,LDMSUM,'STEP')
        ELSE IF (PTRJ.EQ.'RADDISTR') THEN
          CALL GETRDF(CYCLE,'STEP')
        END IF
C
C     *** Calculate MO overlap between time steps
C     *** Requires preceding BOMD with option STOREWF
C
        IF (PTRJ.EQ.'XES'.AND.MDCYC.NE.TRJMAX) THEN
           IF (CCSET.EQ.1) THEN
              CCSET2 = 2
           ELSE
              CCSET2 = 1
           ENDIF
           IF (MDCYC.NE.TRJMIN) THEN
             CMO1(:,:,:) = CMO2(:,:,:)
           ENDIF
C
C     *** Store the present set of coordinates and get the 
C     *** coordinates and orbitals for the next geometry
C
           IF (MDCYC.NE.TRJMAX) THEN
              write(6,*) "CCSET, CCSET2", CCSET, CCSET2 
              COORD(:,:,CCSET2) = COORD(:,:,CCSET)
              RECNO(IOTRJ) = MDCYC + 1
              CALL MDTRJ('READ')
              
              write(6,*) "COORD(:,:,CCSET)", COORD(:,:,CCSET)
              write(6,*) "COORD(:,:,CCSET2)", COORD(:,:,CCSET2)

              CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
              CMO2(:,:,1) = W(:,:,1)
              OCC(:,1)    = WOCC(:)
              CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
              CMO2(:,:,2) = W(:,:,1)
              OCC(:,2)    = WOCC(:)

              write(6,*) "CMO1(:,:,1)", CMO1(:,:,1)
              write(6,*) "CMO1(:,:,2)", CMO1(:,:,2)
              write(6,*) "CMO2(:,:,1)", CMO2(:,:,1)
              write(6,*) "CMO2(:,:,2)", CMO2(:,:,2)

              CALL OVLT1T2(CMO1,CMO2,W,NSTO,CCSET2)

C MLJ
             CALL WFOVLAP(W,NSTO,OCC,XSPIN)
C              CALL WFOVLAP_NOHOLE(W,NSTO,OCC,XSPIN)
C              CALL WFOVLAP_GS_SINGLE_EXC(W,NSTO,OCC,XSPIN)
C end MLJ

           ENDIF
        ENDIF
        IF (PTRJ.NE.'XES') THEN
C
C     *** Update trajectory statistics ***
C
           CALL MDSTAT(CYCLE)
C
C     *** Write trajectory output ***
C
           CALL TRJOUT('STEP')
        ENDIF
C
      END DO
C
C     *** Calculate Lindemann parameter or RDF ***
C
      IF (PTRJ.EQ.'LINDEMANN') THEN
        CALL LINDEMANN(CYCLE,LDMSUM,'FINAL')
        DEALLOCATE(LDMSUM,STAT=ALLOCATION)
        CALL CHKERRSTAT(ALLOCATION,'TRJANA','DEALLOCATION FAILED')
      ELSE IF (PTRJ.EQ.'XES') THEN
        DEALLOCATE(CMO1,CMO2,W,OCC,WOCC,STAT=ALLOCATION)
        CALL CHKERRSTAT(ALLOCATION,'TRJANA','DEALLOCATION FAILED')
      ELSE IF (PTRJ.EQ.'RADDISTR') THEN
        CALL GETRDF(CYCLE,'FINAL')
      END IF
C
C     *** Write trajectory statistics ***
C
      IF (PTRJ.NE.'XES') CALL TRJOUT('FINAL')
C
C     *** Reset property flags ***
C
      MXI = .FALSE.
      NMR = .FALSE.
      PEM = .FALSE.
      POLARIS = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE TRJANA ***
C
      END
