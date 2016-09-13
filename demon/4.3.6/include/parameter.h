C     PARAMETER DEFINITION FOR MEDIUM (4 Gbyte/rodio) VERSION
C
C     Purpose: Definition of parameters.
C
C     History: - Creation (16.01.96, AMK)
C                         (27.07.16, GG)
C
C     ******************************************************************
C
C     List of version parameters:
C
C     CREATION: Time stamp for version creation.
C     VERSION : Version number.
C
C     List of user defined work size parameters:
C
C     MAXDISK: Maximum disk size for deMon files in Mbytes.
C     MAXRAM : Maximum RAM size for deMon kernel in Mbytes.
C     PRORAM : Program RAM size without allocatable objects in Mbytes.
C
C     List of user defined parameters:
C
C     MAXATOM  : Maximum number of quantum mechanics atoms.
C     MAXATMM  : Maximum number of molecular mechanics atoms.
C     MAXAUX   : Maximum number of auxiliary functions.
C     MAXAUXSET: Maximum number of auxiliary function sets.
C     MAXAUXSHL: Maximum number of auxiliary function shells.
C     MAXCON   : Maximum degree of contraction.
C     MAXCUBE  : Maximum number of cube (embedding) points.
C     MAXECPGTO: Maximum number of ECP Gaussian functions.
C     MAXECPSHL: Maximum number of ECP shells.
C     MAXGTO   : Maximum number of Gaussian functions.
C     MAXLAUX  : Maximum L quantum number for auxiliary functions.
C     MAXLBAS  : Maximum L quantum number for orbital basis.
C     MAXLECP  : Maximum L quantum number for ECP functions.
C     MAXLINK  : Maximum number of molecular mechanics links.
C     MAXLMCP  : Maximum L quantum number for MCP functions.
C     MAXLSUM  : Maximum L order for double asymptotic expansion.
C     MAXMCPGTO: Maximum number of MCP Gaussian functions.
C     MAXMCPSHL: Maximum number of MCP shells.
C     MAXMCPSTO: Maximum number of contracted MCP STO orbitals.
C     MAXPAT   : Maximum number of geometry pattern.
C     MAXSHL   : Maximum number of shells.
C     MAXSTO   : Maximum number of contracted STO orbitals.
C     MAXTBSTO : Maximum number of tight-binding STO orbitals.
C
C     List of global parameters:
C
C     MAXDER : Maximum rank of analytic energy derivatives.
C     MAXDIIS: Maximum dimension of DIIS space.
C     MAXMOM : Maximum rank (>0) of electrostatic moments.
C
C     List of global depending parameters:
C
C     MAXATOM_ALL: Maximum number of all atoms.
C     MAXPRI     : Maximum number of primitive redundant coordinates.
C
C     List of global depending QM parameters:
C
C     MAXLAFN: Maximum L for auxiliary functions in molecular integrals.
C     MAXLBFN: Maximum L for basis functions in molecular integrals.
C     MAXLUL : Maximum L quantum number upper limit.
C     MAXLPTR: Maximum L quantum number for pointer definition.
C     MAXLTRA: Maximum L quantum number for transformation matrices.
C     MAXNCO : Maximum number of Cartesian orbitals in shell block.
C     MAXNCS : Maximum number of Cartesian sets for MAXLPTR.
C     MAXNSO : Maximum number of spherical orbitals in shell block.
C     MAXPOP : Maximum dimension of Product Orbital Pointer.
C     MAXSHP : Maximum dimension of Spherical Harmonics Pointer.
C
C     *** List of global depending MM parameters:
C
C     MAXBANG: Maximum number of molecular mechanics bond angles.
C     MAXBOND: Maximum number of molecular mechanics bond stretches.
C     MAXTANG: Maximum number of molecular mechanics torsion angles.
C
C     List of input and control parameters:
C
C     MAXCHAR : Maximum number of characters in input line.
C     MAXKEY  : Maximum number of input keywords.
C     MAXLINE : Maximum number of continuation lines.
C     MAXOPT  : Maximum number of options for input keywords.
C
C     ASSIGNS : Option assignment character.
C     COMBINE : Option combination character.
C     COMMENT : Comment character.
C     DELIMIT : Delimiter character.
C     ENDOFKEY: Optional end of keyword character.
C     FORBIDS : Forbids this character as element.
C     LBRACKET: Left hand side vector bracket.
C     LOLIMIT : Option lower limit character.
C     NEWLINE : New line character for continuation lines.
C     POWEROF : Mathematical power of character.
C     RBRACKET: Right hand side vector bracket.
C     UPLIMIT : Option upper limit character.
C
C     List of input character parameters:
C
C     MAXLAB: Maximum length of input labels.
C     MAXNUM: Maximum length of input numbers.
C     MAXVAR: Maximum length of input variables.
C
C     List of mathematical parameters:
C
C     MAXFAC : Maximum factorial function argument.
C     TABSCIS: Number of t abscissas for F(t) interpolation.
C     ZABSCIS: Number of z abscissas for K(z) interpolation.
C
C     List of symmetry parameters:
C
C     MAXCN : Maximum rotational degree of a C axis.
C     MAXORD: Maximum group order.
C     MAXSEC: Maximum number of C axes.
C     MAXSES: Maximum number of S axes.
C     MAXSIG: Maximum number of mirror planes.
C     MAXSN : Maximum rotational degree of a S axis.
C
C     List of grid parameters:
C
C     MAXGP_CFP: Maximum number of (radial) CFP grid points.
C     MAXGLB   : Number of grid points of largest Lebedev grid.
C     MAXGP    : Maximum number of grid points in grid buffer.
C     MAXGPB   : Maximum number of grid points per batch.
C     MAXLG    : Maximum number of stored Lebedev grids.
C     MAXLGP   : Sum of all stored Lebedev grid points.
C     MAXRAD   : Maximum number of radial quadrature points.
C
C     List of geometry optimization & molecular dynamics parameters:
C
C     MAXNHC: Maximum number of Nose-Hoover chain thermostats.
C     MAXUPD: Maximum number of DIIS/BERNY geometry optimization steps.
C
C     List of property parameters:
C
C     MAXMOL : Maximum number of MOLDEN SCF or optimization cycles.
C     MAXPLOT: Maximum number of plot functions.
C     MAXSTEP: Maximum number of gradient path steps.
C     MINTOP : Minimum topological attractor radius.
C
C     List of toolbox parameters:
C
C     MAXBIT: Maximum number of decimal bits.
C     MAXTIM: Maximum number of timer entries.
C
C     List of octree parameters:
C
C     MAXLEVEL: Maximum level of octree nodes.
C     MAXNODES: Maximum number of octree nodes.
C
C     ------------------------------------------------------------------
C     *** Creation and version number (use as reference) ***
C     ------------------------------------------------------------------
C
      CHARACTER CREATION*9,VERSION*5
C
      PARAMETER (CREATION = 'Aug. 2016',
     $           VERSION = '4.3.6')
C
C     ------------------------------------------------------------------
C     *** Begin of user defined parameter section ***
C     ------------------------------------------------------------------
C
C     *** User defined work size parameters ***
C
      REAL MAXDISK,MAXRAM,PRORAM
C
      PARAMETER (MAXDISK = 204800,
     $           MAXRAM = 4621,
     $           PRORAM = 256)
C
C     ------------------------------------------------------------------
C
C     *** User defined parameters ***
C
      INTEGER MAXATMM,MAXATOM,MAXAUX,MAXAUXSET,MAXAUXSHL,MAXCON,
     $        MAXCUBE,MAXECPGTO,MAXECPSHL,MAXGTO,MAXLAUX,MAXLBAS,
     $        MAXLECP,MAXLINK,MAXLMCP,MAXLSUM,MAXMCPGTO,MAXMCPSHL,
     $        MAXMCPSTO,MAXPAT,MAXSHL,MAXSTO,MAXTBSTO
C
      PARAMETER (MAXATMM = 15000,
     $           MAXATOM = 1500,
     $           MAXAUX = 100000,
     $           MAXAUXSET = 40000,
     $           MAXAUXSHL = 40000,
     $           MAXCON = 21,
     $           MAXCUBE = 500000,
     $           MAXECPGTO = 10000,
     $           MAXECPSHL = 3000,
     $           MAXGTO = 30000,
     $           MAXLAUX = 6,
     $           MAXLBAS = 6,
     $           MAXLECP = 5,
     $           MAXLINK = 4,
     $           MAXLMCP = 3,
     $           MAXLSUM = 8,
     $           MAXMCPGTO = 10000,
     $           MAXMCPSHL = 3000,
     $           MAXMCPSTO = 6000,
     $           MAXPAT = 10,
     $           MAXSHL = 10000,
     $           MAXSTO = 30000,
     $           MAXTBSTO = 30000)
CTODO
CTODO
CTODO
      INTEGER MAXCLASS,MAXTYPE
      PARAMETER (MAXCLASS = 100,
     $           MAXTYPE = 500)
CTODO
CTODO
CTODO
C
C     ------------------------------------------------------------------
C     *** End of user defined parameter section ***
C     *** Do not make changes beyond this line! ***
C     ------------------------------------------------------------------
C
C     *** Global parameters ***
C
      INTEGER MAXDER,MAXDIIS,MAXMOM
C
      PARAMETER (MAXDER = 1,
     $           MAXDIIS = 20,
     $           MAXMOM = 3)
C
C     ------------------------------------------------------------------
C
C     *** Global depending parameters ***
C
      INTEGER MAXATOM_ALL,MAXPRI
C
      PARAMETER (MAXATOM_ALL = MAXATOM + MAXATMM,
     $           MAXPRI = 10*MAXATOM_ALL)
C
C     *** Global depending QM parameters ***
C
      INTEGER MAXLAFN,MAXLBFN,MAXLUL,MAXLPTR,MAXLTRA,
     $        MAXNCO,MAXNCS,MAXNSO,MAXPOP,MAXSHP
C
      PARAMETER (MAXLAFN = MAXDER+MAXLAUX,
     $           MAXLBFN = MAXDER+MAXLBAS,
     $           MAXLUL = MAXLAUX+MAXDER+2*MAXLBAS,
     $           MAXLPTR = MAX(MAXLUL,MAXLSUM+MAXDER+2*MAXLBAS),
     $           MAXLTRA = MAX(MAXLECP,MAXLBAS)+MAXLBFN,
     $           MAXNCO = (MAXLPTR+1)*(MAXLPTR+2)/2,
     $           MAXNCS = (MAXLPTR**3-MAXLPTR)/6+MAXLPTR**2+2*MAXLPTR+1,
     $           MAXNSO = 2*MAXLPTR+1,
     $           MAXPOP = (4*MAXLBFN**3+11*MAXLBFN)/3+4*MAXLBFN**2+1,
     $           MAXSHP = (MAXLBAS+MAXLECP+MAXDER+1)**2)
C
C     *** Global depending MM parameters ***
C
      INTEGER MAXBANG,MAXBOND,MAXTANG
C
      PARAMETER (MAXBANG = 3*MAXATMM,
     $           MAXBOND = 2*MAXATMM,
     $           MAXTANG = 4*MAXATMM)
C
C     ------------------------------------------------------------------
C
C     *** Input and control parameters ***
C
      INTEGER MAXCHAR,MAXKEY,MAXLINE,MAXOPT
C
      PARAMETER (MAXCHAR = 78,
     $           MAXKEY = 84,
     $           MAXLINE = 5,
     $           MAXOPT = 35)
C
      CHARACTER*1 ASSIGNS,COMBINE,COMMENT,DELIMIT,ENDOFKEY*3,FORBIDS*2,
     $            LBRACKET,RBRACKET,LOLIMIT,UPLIMIT,NEWLINE,POWEROF
C
      PARAMETER (ASSIGNS = '=',
     $           LOLIMIT = '<',
     $           UPLIMIT = '>',
     $           COMBINE = '-',
     $           COMMENT = '#',
     $           DELIMIT = ',',
     $           NEWLINE = '&',
     $           POWEROF = '^',
     $           FORBIDS = 'XX',
     $           LBRACKET = '(',
     $           RBRACKET = ')',
     $           ENDOFKEY = 'END')
C
C     ------------------------------------------------------------------
C
C     *** Input character parameters ***
C
      INTEGER MAXLAB,MAXNUM,MAXVAR
C
      PARAMETER (MAXLAB = 10,
     $           MAXNUM = 15,
     $           MAXVAR = 10)
C
C     ------------------------------------------------------------------
C
C     *** Mathematical parameters ***
C
      INTEGER MAXFAC,TABSCIS,ZABSCIS
C
      PARAMETER (MAXFAC = MAX(2*MAXLTRA,30),
     $           TABSCIS = 120,
     $           ZABSCIS = 16*100)
C
C     ------------------------------------------------------------------
C
C     *** Symmetry analysis parameters ***
C
      INTEGER MAXCN,MAXORD,MAXSEC,MAXSES,MAXSIG,MAXSN
C
      PARAMETER (MAXCN = 20)
C
      PARAMETER (MAXORD = MAX(120,4*MAXCN),
     $           MAXSEC = MAXCN+1,
     $           MAXSES = 2*MAXCN+1,
     $           MAXSIG = MAXCN+1,
     $           MAXSN = 2*MAXCN)
C
C     ------------------------------------------------------------------
C
C     *** Grid parameters ***
C
      INTEGER MAXGP_CFP,MAXGLB,MAXGP,MAXGPB,MAXLG,MAXLGP,MAXRAD
C
      PARAMETER (MAXGP_CFP = 800,
     $           MAXGLB = 1202,
     $           MAXGPB = 128,
     $           MAXGP = 512*MAXGPB,
     $           MAXLG = 16,
     $           MAXLGP = 4943,
     $           MAXRAD = 500)
C
C     ------------------------------------------------------------------
C
C     *** Geometry optimization and molecular dynamics parameters ***
C
      INTEGER MAXNHC,MAXUPD
C
      PARAMETER (MAXNHC = 20,
     $           MAXUPD = 5)
C
C     ------------------------------------------------------------------
C
C     *** Property parameters ***
C
      INTEGER MAXBIN,MAXMOL,MAXPLOT,MAXSTEP
      REAL MINTOP
C
      PARAMETER (MAXBIN = 10000,
     $           MAXMOL = 9999,
     $           MAXPLOT = 20,
     $           MAXSTEP = 50,
     $           MINTOP = 0.01)
C
C     ------------------------------------------------------------------
C
C     *** Toolbox parameters ***
C
      INTEGER MAXBIT,MAXTIM
C
      PARAMETER (MAXBIT = 20,
     $           MAXTIM = 100)
C
C     ------------------------------------------------------------------
C
C     *** Octree parameters ***
C
      INTEGER MAXLEVEL,MAXNODES
C
      PARAMETER (MAXLEVEL = 20,
     $           MAXNODES = 40000)
C
C     ------------------------------------------------------------------
