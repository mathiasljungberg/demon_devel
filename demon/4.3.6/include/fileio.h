C     FILEIO COMMON
C
C     Purpose: Common definition for FILE Input/Output routines.
C
C     History: - Creation (18.12.96, MK)
C                         (09.10.00, AMK)
C                         (08.09.08, GG)
C
C     ******************************************************************
C
C     List of tapes:
C
C     a) ACCESS = 'SEQUENTIAL', FORM = 'FORMATTED'
C
C     AUX: Auxiliary function input file (AUXIS).
C     BAS: Basis set input file (BASIS).
C     ECP: ECP input file (ECPS).
C     MCP: MCP input file (MCPS).
C     INP: Input file (deMon.inp).
C     OUT: Output file (deMon.out).
C     NEW: New input file (deMon.new).
C     TRJ: Trajectory file (deMon.trj).
C     MOL: Molden input file (deMon.mol).
C     QMM: QM/MM interface file (deMon.qmm).
C     XRY: XAS/XES analysis file (deMon.xry).
C     NBO: NBO analysis input file (deMon.nbo).
C     PIE: Plot import/export file (deMon.pie).
C     CUB: User defined embedding cube (deMon.cub).
C     LAT: User defined external lattice (deMon.lat).
C     EDU: Input file for educt structure (deMon.edu).
C     PRO: Input file for product structure (deMon.pro).
C
C     b) ACCESS = 'SEQUENTIAL', FORM = 'UNFORMATTED'
C
CTODO NMR11: NMR grid tape.
CTODO NMR70: Dimension informations.
CTODO NMR71: Pointer informations.
CTODO NMR72: Molecule informations.
C     MEM  : Memory file (deMon.mem).
C     RST  : Restart file (deMon.rst).
C     VEC  : Wave function after each BOMD step (deMon.vec).
C     T12  : Orbital overlaps between BOMD time steps.
C
C     c) ACCESS = 'DIRECT', STATUS = 'KEEP'
C
C     IOPLOT: Plot output file.
C
C     d) ACCESS = 'DIRECT', STATUS = 'DELETE'
C
C     IOADP: Auxiliary density perturbation scratch tape.
C     IOAOA: TDDFT AO scratch tape.
C     IOCDF: Charge Density Fit scratch tape.
C     IODIS: MO DIIS scratch tape.
C     IODIR: Direct SCF scratch tape.
C     IOERI: ERI scratch tape.
C     IOFFP: Finit field perturbation scratch tape.
C     IOGRD: Grid scratch tape.
C     IOIRC: IRC path scratch tape.
C     IOISO: Isosurface scratch tape.
C     IOMIX: Mixing scratch tape.
C     IOMOA: TDDFT MO scratch tape.
C     IOOPT: Optimization scratch tape.
C     IORPA: TDDFT scratch tape.
C     IOSCF: SCF scratch tape.
C     IOTRJ: Trajectory scratch tape.
CTODO IOVEC: TDDFT scratch tape.
C
C     e) ACCESS = 'SEQUENTIAL', STATUS = 'DELETE'
C
C     IOPCG: PCG and CP scratch tape.
C     IOPRO: Promolecular density scratch tape.
C
C     List of global input/output variables:
C
C     NREC(IO) : Number of records on tape IO.
C     RECNO(IO): Record number of tape IO.
C
C     List of tape position indices:
C
C     ADPIDX: Auxiliary density perturbation tape index.
C     FFPIDX: Finit field perturbation tape index.
C
C     ------------------------------------------------------------------
C
C     *** Input/Output parameters ***
C
      INTEGER AUX,BAS,CUB,ECP,EDU,INP,IOADP,IOAOA,IOCDF,IODIS,IODIR,
     $        IOERI,IOFFD,IOFFP,IOGRD,IOIRC,IOISO,IOMIX,IOMOA,IOOPT,
     $        IORPA,IOPCG,IOPLOT,IOPRO,IOSCF,IOTRJ,LAT,MAXIO,MAXIDX,
     $        MAXSEQ,MCP,MEM,MINIO,MINSEQ,MOL,NBO,NEW,OUT,PIE,QMM,PRM,
     $        PRO,RST,TRJ,XRY,IOVEC,NMR11,NMR70,NMR71,NMR72,IOWF,IT12
C
C     *** Input and output tapes ***
C
      PARAMETER (MINSEQ = 1,
     $           AUX    = 1,
     $           BAS    = 2,
     $           ECP    = 3,
     $           MCP    = 4,
     $           INP    = 5,
     $           OUT    = 6,
     $           NEW    = 7,
     $           TRJ    = 8,
     $           MOL    = 9,
     $           QMM    = 10,
     $           XRY    = 11,
     $           NBO    = 12,
     $           PIE    = 13,
     $           MEM    = 14,
     $           RST    = 15,
     $           CUB    = 16,
     $           LAT    = 17,
     $           EDU    = 18,
     $           PRO    = 19,
     $           PRM    = 20,
     $           MAXSEQ = 20,
     $           NMR11  = 21,
     $           NMR70  = 22,
     $           NMR71  = 23,
     $           NMR72  = 24,
     $           IOWF   = 25,
     $           IT12    = 26)
C
C     *** Scratch tapes ***
C
      PARAMETER (IOPLOT = 49,
     $           MINIO  = 50,
     $           IOADP  = 50,
     $           IOAOA  = 51,
     $           IOCDF  = 52,
     $           IODIS  = 53,
     $           IODIR  = 54,
     $           IOERI  = 55,
     $           IOFFP  = 56,
     $           IOGRD  = 57,
     $           IOIRC  = 58,
     $           IOISO  = 59,
     $           IOMIX  = 60,
     $           IOMOA  = 61,
     $           IOOPT  = 62,
     $           IOPCG  = 63,
     $           IOPRO  = 64,
     $           IORPA  = 65,
     $           IOSCF  = 66,
     $           IOTRJ  = 67,
     $           IOVEC  = 68,
     $           IOFFD  = 69,
     $           MAXIO  = 69)
C
C     *** Maximum tape index bound ***
C
      PARAMETER (MAXIDX = 25)
C
C     *** Common blocks for FILE Input/Output variables ***
C
      CHARACTER*20 TAPENAME(1:MAXIO),TAPEROOT(1:MAXIO)
C
      COMMON /CFILEIO/ TAPENAME,TAPEROOT
C
      INTEGER NREC(1:MAXIO),RECNO(1:MAXIO)
      INTEGER ADPIDX(0:MAXIDX),FFPIDX(0:MAXIDX)
C
      COMMON /IFILEIO/ ADPIDX,FFPIDX,NREC,RECNO
C
C     ------------------------------------------------------------------
