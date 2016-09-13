      SUBROUTINE WFOVLAP(OVL,NSTO,OCC,XSPIN)
      IMPLICIT NONE
      CHARACTER*(*) XSPIN
      INTEGER ALLOCATION,I,J,INFO,NOCCA,NOCCB,NOCC,NOCCM,IJ,NSTO
      REAL DET,DET0,OVL(NSTO,NSTO,2),OCC(NSTO,2)
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
      IF (XSPIN.EQ.'ALFA') THEN
         NOCC = NOCCB
         IJ   = 2
      ELSE
         NOCC = NOCCA
         IJ   = 1
      ENDIF
      IF (NOCC.GT.1) THEN
         ALLOCATE(PIV(NOCCM),W(NOCC,NOCC),STAT=ALLOCATION)
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
      IF (XSPIN.EQ.'ALFA') THEN
         NOCC = NOCCA
         IJ   = 1
      ELSE
         NOCC = NOCCB
         IJ   = 2
      ENDIF
      NOCCM = NOCC - 1
      IF (NOCCM.GT.1) THEN
         ALLOCATE(W(NOCCM,NOCCM),STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
      ELSE
         ALLOCATION = 1
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP',
     $        'MINIMUM TWO STATES NEEDED')
      ENDIF
      ALLOCATE(RES(NOCC,NOCC),STAT=ALLOCATION)
      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
      DO I = 1,NOCC
         DO J = 1,NOCC
            CALL COMPRESS(I,J,NOCC,OVL(1,1,IJ),NSTO,W,NOCCM)
            CALL DGETRF(NOCCM,NOCCM,W,NOCCM,PIV,INFO)
            CALL GETDET(NOCCM,W,PIV,DET,INFO)
            RES(I,J) = DET0*DET
         ENDDO
      ENDDO
ctest
      write(6,*) ' DETERMINANTAL OVERLAPS '
      do i = 1,nocc
         write(6,'(6f15.8)') (res(i,j),j=1,nocc)
      enddo
ctest
      CALL T12WRITE(RES,NOCC)

      DEALLOCATE(PIV,W,RES,STAT=ALLOCATION)
      IF (ALLOCATION.NE.0) THEN
        CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
      ENDIF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE WFOVLAP ***
C
      END
