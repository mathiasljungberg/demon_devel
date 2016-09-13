      SUBROUTINE WFOVLAP_NOHOLE(OVL,NSTO,OCC,XSPIN)
      IMPLICIT NONE
      CHARACTER*(*) XSPIN
      INTEGER ALLOCATION,I,J,INFO,NOCCA,NOCCB,NOCC,NOCCM,IJ,NSTO
      REAL DET,DET0,DET1,OVL(NSTO,NSTO,2),OCC(NSTO,2)
      REAL,ALLOCATABLE :: RES(:,:),W(:,:)
      INTEGER,ALLOCATABLE :: PIV(:)
      
      NOCCA = 0
      DO I = NSTO,1,-1
         IF (OCC(I,1).GT.0.9D0) THEN
            NOCCA = I
            GO TO 10
         ENDIF
      ENDDO
 10   NOCCB = 0
      DO I = NSTO,1,-1
         IF (OCC(I,2).GT.0.9D0) THEN
            NOCCB = I
            GO TO 20
         ENDIF
      ENDDO
 20   DET0 = 1.D0
      NOCCM = MAX(NOCCA,NOCCB)


C     ALFA SPIN
      XSPIN = 'ALFA'
      IF (XSPIN.EQ.'ALFA') THEN
         NOCC = NOCCB
         IJ   = 2
      ELSE
         NOCC = NOCCA
         IJ   = 1
      ENDIF

      ALLOCATE(PIV(NOCCM),STAT=ALLOCATION)
      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')

      IF (NOCC.GT.1) THEN
         ALLOCATE(W(NOCC,NOCC),STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
         W(:,:) = OVL(1:NOCC,1:NOCC,IJ)
ctest
         write(6,*) ' WFOVLAP - Orbital overlaps '
         do i = 1,nocc
            write(6,'(8f15.8)') (w(i,j),j=1,nocc)
         enddo
ctest
         CALL DGETRF(NOCC,NOCC,W,NOCC,PIV,INFO)
         CALL GETDET(NOCC,W,PIV,DET0,INFO)
         DEALLOCATE(W,STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
      ELSE
         DET0 = OVL(1,1,IJ)
      ENDIF
ctest
      if (xspin.eq.'ALFA') then
         write(6,*) ' BETA determinant = ',det0
      else
         write(6,*) ' ALFA determinant = ',det0
      endif
ctest

C     BETA SPIN
      IF (XSPIN.EQ.'ALFA') THEN
         NOCC = NOCCA
         IJ   = 1
      ELSE
         NOCC = NOCCB
         IJ   = 2
      ENDIF

      IF (NOCC.GT.1) THEN
         ALLOCATE(W(NOCC,NOCC),STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
         W(:,:) = OVL(1:NOCC,1:NOCC,IJ)
ctest
         write(6,*) ' WFOVLAP - Orbital overlaps '
         do i = 1,nocc
            write(6,'(8f15.8)') (w(i,j),j=1,nocc)
         enddo
ctest
         CALL DGETRF(NOCC,NOCC,W,NOCC,PIV,INFO)
         CALL GETDET(NOCC,W,PIV,DET1,INFO)
         DEALLOCATE(W,STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
      ELSE
         DET1 = OVL(1,1,IJ)
      ENDIF
ctest
      if (xspin.eq.'ALFA') then
         write(6,*) ' ALFA determinant = ',det1
      else
         write(6,*) ' BETA determinant = ',det1
      endif

      DET = DET0 * DET1

      write(6,*) ' DETERMINANTAL OVERLAP = ', DET

C      NOCCM = NOCC - 1
C      IF (NOCCM.GT.1) THEN
C         ALLOCATE(W(NOCCM,NOCCM),STAT=ALLOCATION)
C         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
C      ELSE
C         ALLOCATION = 1
C         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP',
C     $        'MINIMUM TWO STATES NEEDED')
C      ENDIF
C      ALLOCATE(RES(NOCC,NOCC),STAT=ALLOCATION)
C      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
C      DO I = 1,NOCC
C         DO J = 1,NOCC
C            CALL COMPRESS(I,J,NOCC,OVL(1,1,IJ),NSTO,W,NOCCM)
C            CALL DGETRF(NOCCM,NOCCM,W,NOCCM,PIV,INFO)
C            CALL GETDET(NOCCM,W,PIV,DET,INFO)
C            RES(I,J) = DET0*DET
C         ENDDO
C      ENDDO
ctest
C      write(6,*) ' DETERMINANTAL OVERLAPS '
C      do i = 1,nocc
C         write(6,'(6f15.8)') (res(i,j),j=1,nocc)
C      enddo
ctest
C      CALL T12WRITE(RES,NOCC)

C      DEALLOCATE(PIV,W,RES,STAT=ALLOCATION)
      DEALLOCATE(PIV,STAT=ALLOCATION)
      IF (ALLOCATION.NE.0) THEN
        CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
      ENDIF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE WFOVLAP ***
C
      END
