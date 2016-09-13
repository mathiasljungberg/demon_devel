      SUBROUTINE FILEIO(OPTION)
C
C     Purpose: Driver routine for file Input/Output.
C
C     History: - Creation (07.01.98, MK)
C                         (07.03.02, AMK)
C                         (09.09.08, GG)
C
C     ******************************************************************
C
C     List of local variables:
C
C     OPTION: a) OPEN PERMANENT FILES      = Open input/output files.
C             b) OPEN REACTION FILES       = Open educt/product files.
C             c) OPEN EXTERNAL INPUTS      = Open external input files.
C             d) OPEN EXTERNAL OUTPUTS     = Open external output files.
C             e) OPEN TEMPORARY FILES      = Open scratch files.
C             f) OPEN TEMPORARY SCF FILES  = Open SCF scratch files.
C             g) OPEN PLOT FILES           = Open plot files.
C             h) OPEN ISO FILES            = Open isosurface files.
C             i) CLOSE ISO FILES           = Close isosurface files.
C             j) CLOSE PLOT FILES          = Close plot files.
C             k) CLOSE PERMANENT FILES     = Close input/output files.
C             l) CLOSE TEMPORARY FILES     = Close scratch files.
C             m) CLOSE TEMPORARY SCF FILES = Close SCF scratch files.
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
      INCLUDE 'mp_demon.h'
      INCLUDE 'opt.h'
      INCLUDE 'plot.h'
C
      CHARACTER OPTION*(*)
C
      LOGICAL EMPTYIO,EX,NMD,OD
      CHARACTER*80 LTU,NAME*20,PRTSTR,STRCOMP
      CHARACTER*3 DIRECT,MYFILE,SEQUENTIAL
      INTEGER DIMTRJ,STREXT,TAPE
C
      REAL RDUMMY(1)
C
C     ------------------------------------------------------------------
C
C     *** Open input/output files ***
C
      IF (OPTION.EQ.'OPEN PERMANENT FILES') THEN
C
        OPEN (AUX,FILE='AUXIS',STATUS='UNKNOWN',POSITION='REWIND',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=99940)
        OPEN (BAS,FILE='BASIS',STATUS='UNKNOWN',POSITION='REWIND',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=99950)
        OPEN (ECP,FILE='ECPS',STATUS='UNKNOWN',POSITION='REWIND',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=99960)
        OPEN (MCP,FILE='MCPS',STATUS='UNKNOWN',POSITION='REWIND',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=99970)
        OPEN (PRM,FILE='FFDS',STATUS='UNKNOWN',POSITION='REWIND',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED',ERR=99980)
C
C     *** Multiple input/output files for parallel processing ***
C
        IF (MPP.AND.(ME.GT.0)) THEN
C
          OPEN (INP,FILE='deMon.inp'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
          OPEN (NEW,FILE='deMon.new'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
          OPEN (OUT,FILE='deMon.out'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
          OPEN (TRJ,FILE='deMon.trj'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
          OPEN (MEM,FILE='deMon.mem'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='UNFORMATTED')
          OPEN (RST,FILE='deMon.rst'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',
     $          FORM='UNFORMATTED')
C
        ELSE
C
          OPEN (INP,FILE='deMon.inp',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (NEW,FILE='deMon.new',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (OUT,FILE='deMon.out',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (TRJ,FILE='deMon.trj',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (MEM,FILE='deMon.mem',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
          OPEN (RST,FILE='deMon.rst',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
        END IF
C
C     *** Define temporary scratch file names ***
C
        IF (MPP) THEN
C
          TAPENAME(IOADP) = 'ioadp.'//MYFILE(ME)
          TAPENAME(IOAOA) = 'ioaoa.'//MYFILE(ME)
          TAPENAME(IOCDF) = 'iocdf.'//MYFILE(ME)
          TAPENAME(IODIS) = 'iodis.'//MYFILE(ME)
          TAPENAME(IODIR) = 'iodir.'//MYFILE(ME)
          TAPENAME(IOERI) = 'ioeri.'//MYFILE(ME)
          TAPENAME(IOFFP) = 'ioffp.'//MYFILE(ME)
          TAPENAME(IOGRD) = 'iogrd.'//MYFILE(ME)
          TAPENAME(IOISO) = 'ioiso.'//MYFILE(ME)
          TAPENAME(IOIRC) = 'ioirc.'//MYFILE(ME)
          TAPENAME(IOMIX) = 'iomix.'//MYFILE(ME)
          TAPENAME(IOMOA) = 'iomoa.'//MYFILE(ME)
          TAPENAME(IOOPT) = 'ioopt.'//MYFILE(ME)
          TAPENAME(IOPCG) = 'iopcg.'//MYFILE(ME)
          TAPENAME(IOPRO) = 'iopro.'//MYFILE(ME)
          TAPENAME(IORPA) = 'iorpa.'//MYFILE(ME)
          TAPENAME(IOSCF) = 'ioscf.'//MYFILE(ME)
          TAPENAME(IOTRJ) = 'iotrj.'//MYFILE(ME)
          TAPENAME(IOVEC) = 'iovec.'//MYFILE(ME)
          TAPENAME(IOFFD) = 'ioffd.'//MYFILE(ME)
C
        ELSE
C
          TAPENAME(IOADP) = 'ioadp.scr'
          TAPENAME(IOAOA) = 'ioaoa.scr'
          TAPENAME(IOCDF) = 'iocdf.scr'
          TAPENAME(IODIS) = 'iodis.scr'
          TAPENAME(IODIR) = 'iodir.scr'
          TAPENAME(IOERI) = 'ioeri.scr'
          TAPENAME(IOFFP) = 'ioffp.scr'
          TAPENAME(IOGRD) = 'iogrd.scr'
          TAPENAME(IOIRC) = 'ioirc.scr'
          TAPENAME(IOISO) = 'ioiso.scr'
          TAPENAME(IOMIX) = 'iomix.scr'
          TAPENAME(IOMOA) = 'iomoa.scr'
          TAPENAME(IOOPT) = 'ioopt.scr'
          TAPENAME(IOPCG) = 'iopcg.scr'
          TAPENAME(IOPRO) = 'iopro.scr'
          TAPENAME(IORPA) = 'iorpa.scr'
          TAPENAME(IOSCF) = 'ioscf.scr'
          TAPENAME(IOTRJ) = 'iotrj.scr'
          TAPENAME(IOVEC) = 'iovec.scr'
          TAPENAME(IOFFD) = 'ioffd.scr'
C
        END IF
C
C     *** Initialize file roots ***
C
        TAPEROOT(:) = 'UNDEFINED'
C
C     *** Define external file roots ***
C
        TAPEROOT(AUX) = 'EXTERNAL'
        TAPEROOT(BAS) = 'EXTERNAL'
        TAPEROOT(ECP) = 'EXTERNAL'
        TAPEROOT(MCP) = 'EXTERNAL'
        TAPEROOT(PRM) = 'EXTERNAL'
C
C     *** Define input/output file roots ***
C
        TAPEROOT(INP) = 'I/O'
        TAPEROOT(NEW) = 'I/O'
        TAPEROOT(OUT) = 'I/O'
        TAPEROOT(TRJ) = 'I/O'
        TAPEROOT(MEM) = 'I/O'
        TAPEROOT(RST) = 'I/O'
C
C     *** Define temporary scratch file roots ***
C
        TAPEROOT(IOADP) = 'SCP'
        TAPEROOT(IOAOA) = 'SCP'
        TAPEROOT(IOCDF) = 'SCF'
        TAPEROOT(IODIS) = 'SCF'
        TAPEROOT(IODIR) = 'SCF'
        TAPEROOT(IOERI) = 'SCF'
        TAPEROOT(IOFFP) = 'SCP'
        TAPEROOT(IOGRD) = 'SCF'
        TAPEROOT(IOIRC) = 'TRJ'
        TAPEROOT(IOISO) = 'ISO'
        TAPEROOT(IOMIX) = 'SCF'
        TAPEROOT(IOMOA) = 'SCP'
        TAPEROOT(IOOPT) = 'OPT'
        TAPEROOT(IOPCG) = 'SCF'
        TAPEROOT(IOPRO) = 'POP'
        TAPEROOT(IORPA) = 'SCP'
        TAPEROOT(IOSCF) = 'SCF'
        TAPEROOT(IOTRJ) = 'TRJ'
        TAPEROOT(IOVEC) = 'SCP'
        TAPEROOT(IOFFD) = 'PRM'
C
C     *** Initialize position indices ***
C
        ADPIDX(:) = 0
        FFPIDX(:) = 0
C
C       *** Open secondary parameter file ***
C
        OPEN (IOFFD,FILE=TAPENAME(IOFFD),STATUS='UNKNOWN',
     $        ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     *** Open educt/product files ***
C
      ELSE IF (OPTION.EQ.'OPEN REACTION FILES') THEN
C
        IF (MPP.AND.(ME.GT.0)) THEN
C
          OPEN (EDU,FILE='deMon.edu'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (PRO,FILE='deMon.pro'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
        ELSE
C
          OPEN (EDU,FILE='deMon.edu',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
          OPEN (PRO,FILE='deMon.pro',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
        END IF
C
C     *** Define educt/product file roots ***
C
        TAPEROOT(EDU) = 'REACTION'
        TAPEROOT(PRO) = 'REACTION'
C
C     *** Open external input files ***
C
      ELSE IF (OPTION.EQ.'OPEN EXTERNAL INPUTS') THEN
C
        INQUIRE (UNIT=LAT,OPENED=OD)
        IF (LATINP.EQ.'EXTERNAL BINARY') THEN
          TAPENAME(LAT) = 'LAT.bin'
          IF (.NOT.OD) CALL SCRDIR(RDUMMY,3,3,1,LAT,'OPEN')
        ELSE IF (LATINP.EQ.'EXTERNAL ASCII') THEN
          IF (OD) THEN
            REWIND (LAT)
          ELSE
            OPEN (LAT,FILE='LAT.asc',ACCESS='SEQUENTIAL',
     $            POSITION='REWIND',FORM='FORMATTED')
          END IF
        END IF
C
C     *** Open external output files ***
C
      ELSE IF (OPTION.EQ.'OPEN EXTERNAL OUTPUTS') THEN
C
C     *** Open external Molden/Molekel/WFN input file ***
C
        IF (MPP.AND.(ME.GT.0).AND.(MOLDEN.NE.'NONE')) THEN
          OPEN (MOL,FILE='deMon.mol'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (MPP.AND.(ME.GT.0).AND.(MOLEKEL.NE.'NONE')) THEN
          OPEN (MOL,FILE='deMon.mkl'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (MPP.AND.(ME.GT.0).AND.(WFNFILE.NE.'NONE')) THEN
          OPEN (MOL,FILE='deMon.wfn'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (MOLDEN.NE.'NONE') THEN
          OPEN (MOL,FILE='deMon.mol',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (MOLEKEL.NE.'NONE') THEN
          OPEN (MOL,FILE='deMon.mkl',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (WFNFILE.NE.'NONE') THEN
          OPEN (MOL,FILE='deMon.wfn',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
        END IF
C
C     *** Open external CHARMM QM/MM interface file ***
C
        IF (MPMASTER.AND.CHARMM) THEN
          OPEN (QMM,FILE='deMon.qmm',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
        END IF
C
C     *** Open external XAS/XES analysis file ***
C
        IF (MPMASTER.AND.(XAS.OR.XES)) THEN
          OPEN (XRY,FILE='deMon.xry',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED') 
        END IF
C
C     *** Open external file for orbital overlaps between time steps in BOMD ***
C
        IF (MPMASTER.AND.PTRJ.EQ.'XES') THEN
          OPEN (IT12,FILE='deMon.t12',STATUS='UNKNOWN',POSITION='REWIND'
     $          ,ACCESS='SEQUENTIAL',FORM='UNFORMATTED') 
        END IF
C
C     *** Open external file to store wave function along BOMD trajectory ***
C
        IF (MPMASTER.AND.STOREWF) THEN
          OPEN (IOWF,FILE='deMon.vec',STATUS='UNKNOWN',POSITION='REWIND'
     $          ,ACCESS='SEQUENTIAL',FORM='UNFORMATTED') 
        END IF
C
C     *** Open external NBO input file ***
C
        IF (MPP.AND.(ME.GT.0).AND.NPA) THEN
          OPEN (NBO,FILE='deMon.nbo'//MYFILE(ME),STATUS='UNKNOWN',
     $          POSITION='REWIND',ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (NPA) THEN
          OPEN (NBO,FILE='deMon.nbo',STATUS='UNKNOWN',POSITION='REWIND',
     $          ACCESS='SEQUENTIAL',FORM='FORMATTED')
        END IF
C
C     *** Open temporary scratch files ***
C
      ELSE IF (OPTION.EQ.'OPEN TEMPORARY FILES') THEN
C
        IF ((XTYP.NE.'NOFUN').OR.(CTYP.NE.'NOFUN')) THEN
          IF (.NOT.DIRGRD) CALL GRDIO(1,'OPEN')
        END IF
C
        IF (EXX.OR.(ERITYP.EQ.'DIRECT')) THEN
          CALL SCFIO(RDUMMY,NSTO,'OPEN','KSALPHA')
        END IF
C
        IF (OMA.OR.(.NOT.CDXC)) THEN
          CALL MIXIO(RDUMMY,NSTO,'OPEN','PALPHA')
        END IF
C
        IF (DIIS.AND.(.NOT.CDXC)) THEN
          CALL DIISIO(RDUMMY,NSTO**2,'OPEN','PALPHA',1)
        END IF
C
        IF (FITTING.EQ.'NUMERICAL') THEN
          CALL PCGIO(RDUMMY,1,1,'OPEN')
        END IF
C
        IF (OPT.OR.IRC.OR.SADTS) THEN
          CALL OPTIO(RDUMMY,NDEG**2,'OPEN','HESSIAN')
        END IF
C
        IF (IRC) THEN
          CALL IRCIO(RDUMMY,NDEG**2+3,'OPEN')
        END IF
C
        IF (MD) THEN
          CALL TRJIO(RDUMMY,DIMTRJ(TRJLEN),'OPEN')
        END IF
C
        IF (POLARIS.OR.TDDFT) THEN
          CALL ADPIO(RDUMMY,0,NAUX,'OPEN','RMATRIX','ALPHA')
        END IF
C
        IF (DEFORM.OR.USEDD) THEN
          CALL PROIO(RDUMMY,NSTO,'OPEN')
        END IF
C
        IF (.NOT.MMONLY) THEN
          CALL CDFIO(RDUMMY,NAUX,1,'OPEN','CDERI')
          CALL FFPIO(RDUMMY,NSTO,'OPEN','CORE')
          CALL SCFIO(RDUMMY,NSTO,'OPEN','OVERLAP')
        END IF
C
C     *** Open temporary SCF scratch files ***
C
      ELSE IF (OPTION.EQ.'OPEN TEMPORARY SCF FILES') THEN
C
        IF ((XTYP.NE.'NOFUN').OR.(CTYP.NE.'NOFUN')) THEN
          IF (.NOT.DIRGRD) CALL GRDIO(1,'OPEN')
        END IF
C
        IF (OMA.OR.(.NOT.CDXC)) THEN
          CALL MIXIO(RDUMMY,NSTO,'OPEN','PALPHA')
        END IF
C
        IF (DIIS.AND.(.NOT.CDXC)) THEN
          CALL DIISIO(RDUMMY,NSTO**2,'OPEN','PALPHA',1)
        END IF
C
        IF (EXX.OR.(ERITYP.EQ.'DIRECT')) THEN
          CALL SCFIO(RDUMMY,NSTO,'OPEN','KSALPHA')
        END IF
C
        IF (FITTING.EQ.'NUMERICAL') THEN
          CALL PCGIO(RDUMMY,1,1,'OPEN')
        END IF
C
        CALL CDFIO(RDUMMY,NAUX,1,'OPEN','CDERI')
        CALL SCFIO(RDUMMY,NSTO,'OPEN','OVERLAP')
C
C     *** Open plot files ***
C
      ELSE IF (OPTION.EQ.'OPEN PLOT FILES') THEN
C
        IF (PLOTTYP.EQ.'CPSEARCH') THEN
          IF (FITTING.EQ.'NUMERICAL') THEN
            REWIND(IOPCG)
          ELSE
            CALL PCGIO(RDUMMY,1,1,'OPEN')
          END IF
          CALL BINARY(RDUMMY,1,3,1,IOPLOT,PLOTNAME(0),'OPEN')
        ELSE
          CALL BINARY(RDUMMY,1,1,1,IOPLOT,PLOTNAME(0),'OPEN')
        END IF
C
        IF (PLOT.EQ.'ASCII') THEN
          PRTSTR = PLOTNAME(0)
          PRTSTR(STREXT(PRTSTR)-2:) = 'asc'
          OPEN (PIE,FILE=PRTSTR,ACCESS='SEQUENTIAL',FORM='FORMATTED')
        ELSE IF (PLOT.EQ.'BINARY') THEN
          OPEN (PIE,FILE='deMon.pie',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
        END IF
C
C     *** Open isosurface files ***
C
      ELSE IF (OPTION.EQ.'OPEN ISO FILES') THEN
C
        CALL BINARY(RDUMMY,1,1,1,IOPLOT,PLOTNAME(0),'OPEN')
        CALL ISOIO('OPEN')
C
        IF (PLOT.EQ.'ASCII') THEN
          OPEN (LAT,FILE='deMon.lat',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
        ELSE IF (PLOT.EQ.'BINARY') THEN
          OPEN (PIE,FILE='deMon.pie',ACCESS='SEQUENTIAL',
     $          FORM='FORMATTED')
          CALL BINARY(RDUMMY,3,3,1,LAT,'LAT.bin','OPEN')
        END IF
C
C     *** Close isosurface files ***
C
      ELSE IF (OPTION.EQ.'CLOSE ISO FILES') THEN
C
        CALL BINARY(RDUMMY,1,1,1,IOPLOT,PLOTNAME(0),'DELETE')
        CALL ISOIO('DELETE')
C
C     *** Close plot files ***
C
      ELSE IF (OPTION.EQ.'CLOSE PLOT FILES') THEN
C
        IF ((PLOT.EQ.'BINARY').AND.(PLOTTYP.NE.'CPSEARCH')) THEN
          CALL BINARY(RDUMMY,1,1,1,IOPLOT,PLOTNAME(0),'CLOSE')
        ELSE
          CALL BINARY(RDUMMY,1,1,1,IOPLOT,PLOTNAME(0),'DELETE')
        END IF
C
C     *** Close all input/output files ***
C
      ELSE IF (OPTION.EQ.'CLOSE PERMANENT FILES') THEN
C
        DO TAPE=MINSEQ,MAXSEQ
          INQUIRE (UNIT=TAPE,NAMED=NMD,OPENED=OD,SEQUENTIAL=SEQUENTIAL)
          IF (LTU(SEQUENTIAL).EQ.'YES') THEN
            IF (TAPEROOT(TAPE).EQ.'REACTION') THEN
              CLOSE (UNIT=TAPE,STATUS='DELETE')
            ELSE IF (NMD.AND.OD) THEN
              REWIND (TAPE,ERR=99990)
              IF (EMPTYIO(TAPE)) THEN
                CLOSE (UNIT=TAPE,STATUS='DELETE')
              ELSE
                CLOSE (UNIT=TAPE,STATUS='KEEP')
              END IF
            ELSE IF (OD) THEN
              CLOSE (UNIT=TAPE,STATUS='DELETE')
            END IF
          ELSE
            CLOSE (UNIT=TAPE,STATUS='KEEP')
          END IF
        END DO
C
C     *** Close all temporary scratch files ***
C
      ELSE IF (OPTION.EQ.'CLOSE TEMPORARY FILES') THEN
C
        DO TAPE=MINIO,MAXIO
          NAME = TAPENAME(TAPE)
          INQUIRE (FILE=NAME,EXIST=EX,OPENED=OD)
          INQUIRE (FILE=NAME,DIRECT=DIRECT,SEQUENTIAL=SEQUENTIAL)
          IF (OD) THEN
            CLOSE (UNIT=TAPE,STATUS='DELETE')
          ELSE IF (EX) THEN
            IF (LTU(DIRECT).EQ.'YES') THEN
              OPEN (UNIT=TAPE,FILE=NAME,STATUS='UNKNOWN',
     $             ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1)
            ELSE IF (LTU(SEQUENTIAL).EQ.'YES') THEN
              OPEN (UNIT=TAPE,FILE=NAME,STATUS='UNKNOWN',
     $              ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            END IF
            CLOSE (UNIT=TAPE,STATUS='DELETE')
          END IF
        END DO
C
C     *** Close temporary SCF scratch files ***
C
      ELSE IF (OPTION.EQ.'CLOSE TEMPORARY SCF FILES') THEN
C
        DO TAPE=MINIO,MAXIO
          IF (TAPEROOT(TAPE).EQ.'SCF') THEN
            NAME = TAPENAME(TAPE)
            INQUIRE (FILE=NAME,EXIST=EX,OPENED=OD)
            INQUIRE (FILE=NAME,DIRECT=DIRECT,SEQUENTIAL=SEQUENTIAL)
            IF (OD) THEN
              CLOSE (UNIT=TAPE,STATUS='DELETE')
            ELSE IF (EX) THEN
              IF (LTU(DIRECT).EQ.'YES') THEN
                OPEN (UNIT=TAPE,FILE=NAME,STATUS='UNKNOWN',
     $                ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1)
              ELSE IF (LTU(SEQUENTIAL).EQ.'YES') THEN
                OPEN (UNIT=TAPE,FILE=NAME,STATUS='UNKNOWN',
     $                ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
              END IF
              CLOSE (UNIT=TAPE,STATUS='DELETE')
            END IF
          END IF
        END DO
C
C     *** Error handling ***
C
      ELSE
C
        PRTSTR = 'UNKNOWN OPTION '//OPTION//' SPECIFIED'
        CALL ERRMSG('FILEIO',STRCOMP(PRTSTR),2)
C
      END IF
C
      RETURN
C
C     ------------------------------------------------------------------
C
C     *** Error handling ***
C
99940 CONTINUE
      CALL ERRMSG('FILEIO','OPEN FOR FILE AUXIS FAILED',1)
C
99950 CONTINUE
      CALL ERRMSG('FILEIO','OPEN FOR FILE BASIS FAILED',1)
C
99960 CONTINUE
      CALL ERRMSG('FILEIO','OPEN FOR FILE ECPS FAILED',1)
C
99970 CONTINUE
      CALL ERRMSG('FILEIO','OPEN FOR FILE MCPS FAILED',1)
C
99980 CONTINUE
      CALL ERRMSG('FILEIO','OPEN FOR FILE FFDS FAILED',1)
C
99990 CONTINUE
      CLOSE (UNIT=TAPE,STATUS='KEEP')
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE FILEIO ***
C
      END
