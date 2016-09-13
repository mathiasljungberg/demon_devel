      SUBROUTINE WFOVLAP_GS_SINGLE_EXC(OVL,NSTO,OCC,XSPIN)
      IMPLICIT NONE
      CHARACTER*(*) XSPIN
      INTEGER ALLOCATION,I,J,INFO,NOCCA,NOCCB,NOCC,NOCCM,IJ,NSTO, NUNOCC
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

c determinantal overlap of the spin without excitation, that is gs to gs 
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
         DEALLOCATE(PIV,STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
      ELSE
         DET0 = OVL(1,1,IJ)
      ENDIF
ctest

c determinantal overlap of the spin with excitation, that is gs to singly excited state 
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

c      NOCCM = NOCC - 1
      NOCCM = NOCC 
      NUNOCC = NSTO - NOCC
      
c      IF (NOCCM.GT.1) THEN
      ALLOCATE(PIV(NOCCM),STAT=ALLOCATION)
      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
      ALLOCATE(W(NOCCM,NOCCM),STAT=ALLOCATION)
      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
c      ELSE
c         ALLOCATION = 1
c         CALL CHKERRSTAT(ALLOCATION,'WFOVLAP',
c     $        'MINIMUM TWO STATES NEEDED')
c      ENDIF
      ALLOCATE(RES(NOCC,NUNOCC),STAT=ALLOCATION)
      CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','ALLOCATION FAILED')
      DO I = 1,NOCC
         DO J = 1, NUNOCC
            write(6,*) "I,J", I,J
            CALL COMPRESS_SINGLE_EXC(I,J+NOCC,NOCC,OVL(1,1,IJ),NSTO,
     $           W,NOCCM)

            IF (NOCC.GT.1) THEN
               CALL DGETRF(NOCCM,NOCCM,W,NOCCM,PIV,INFO)
               CALL GETDET(NOCCM,W,PIV,DET,INFO)
               RES(I,J) = DET0*DET
            ELSE
                RES(I,J) = DET0 * W(1,1)
            ENDIF

         ENDDO
      ENDDO
ctest
      write(6,*) ' DETERMINANTAL OVERLAPS '
      do i = 1,nocc
         write(6,'(6f15.8)') (res(i,j),j=1,nunocc)
      enddo
      write(6,*) 'END '
ctest
c      CALL T12WRITE(RES,NOCC)

C         DEALLOCATE(PIV,W,RES,STAT=ALLOCATION)

      IF (ALLOCATED(PIV)) THEN
         DEALLOCATE(PIV,STAT=ALLOCATION)
         IF (ALLOCATION.NE.0) THEN
            CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
         ENDIF
      ENDIF
      
      IF (ALLOCATED(W)) THEN
         DEALLOCATE(W,STAT=ALLOCATION)
         IF (ALLOCATION.NE.0) THEN
            CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
         ENDIF
      ENDIF
      
      IF (ALLOCATED(RES)) THEN
         DEALLOCATE(RES,STAT=ALLOCATION)
         IF (ALLOCATION.NE.0) THEN
            CALL CHKERRSTAT(ALLOCATION,'WFOVLAP','DEALLOCATION FAILED')
         ENDIF
      ENDIF
      
C     
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE WFOVLAP ***
C
      END
