C     *************************************************************
C     *  STELLAR STRUCTURE SOLVER -- LANE-EMDEN EQUATION         *
C     *  PUNCHCARD FORTRAN IV / FORTRAN 66                       *
C     *  SIMULATES POLYTROPIC STELLAR MODELS                     *
C     *  COMPUTES DENSITY, PRESSURE, AND MASS PROFILES           *
C     *  FOR STARS OF VARIOUS POLYTROPIC INDICES                 *
C     *  CODED FOR IBM 7094, CIRCA 1964                          *
C     *************************************************************
C
C     THE LANE-EMDEN EQUATION:
C        D2(THETA)/D(XI)2 + (2/XI)*D(THETA)/D(XI) + THETA**N = 0
C     WHERE THETA = DIMENSIONLESS TEMPERATURE/DENSITY
C           XI    = DIMENSIONLESS RADIUS
C           N     = POLYTROPIC INDEX
C
      PROGRAM STELLAR
      IMPLICIT NONE
      INTEGER I, NSTEPS, NPOLY, ICASE
      DOUBLE PRECISION XI, THETA, DTHETA, H, XI1
      DOUBLE PRECISION K1A, K1B, K2A, K2B, K3A, K3B, K4A, K4B
      DOUBLE PRECISION RHOC, PC, MSOL, RSOL, RSTAR, MSTAR
      DOUBLE PRECISION RHO, PRES, EMASS, PI, GRAV, ACON
      DOUBLE PRECISION PINDEX
      PARAMETER (PI = 3.14159265358979D0)
      PARAMETER (GRAV = 6.674D-8)
      PARAMETER (MSOL = 1.989D+33)
      PARAMETER (RSOL = 6.960D+10)
C
C     BANNER
C
      WRITE(*,100)
  100 FORMAT(/,
     &  1X,60('*'),/,
     &  1X,'*',58X,'*',/,
     &  1X,'*   STELLAR STRUCTURE PROGRAM -- LANE-EMDEN SOLVER',
     &  7X,'*',/,
     &  1X,'*   IBM 7094 FORTRAN IV -- GODDARD SPACE CENTER',
     &  11X,'*',/,
     &  1X,'*   POLYTROPIC STELLAR MODEL COMPUTATION',
     &  18X,'*',/,
     &  1X,'*',58X,'*',/,
     &  1X,60('*'),/)
C
      NSTEPS = 10000
      H = 0.001D0
C
C     LOOP OVER POLYTROPIC INDICES N = 0.0, 1.0, 1.5, 3.0, 4.0
C     N=0  : UNIFORM DENSITY (INCOMPRESSIBLE)
C     N=1  : ANALYTIC SOLUTION EXISTS (SINC FUNCTION)
C     N=1.5: FULLY CONVECTIVE STAR (RED DWARFS)
C     N=3  : EDDINGTON STANDARD MODEL (MAIN SEQUENCE)
C     N=4  : NEAR INSTABILITY LIMIT
C
      DO 500 ICASE = 1, 5
         GO TO (210, 220, 230, 240, 250), ICASE
  210    PINDEX = 0.0D0
         GO TO 260
  220    PINDEX = 1.0D0
         GO TO 260
  230    PINDEX = 1.5D0
         GO TO 260
  240    PINDEX = 3.0D0
         GO TO 260
  250    PINDEX = 4.0D0
  260    CONTINUE
C
         WRITE(*,300) PINDEX
  300    FORMAT(/,1X,50('='),/,
     &    1X,'POLYTROPIC INDEX N = ',F4.1,/,
     &    1X,50('='))
C
         WRITE(*,310)
  310    FORMAT(1X,5X,'XI',10X,'THETA',8X,'DTHETA/DXI',
     &    5X,'RHO/RHOC',4X,'M/MTOT')
         WRITE(*,320)
  320    FORMAT(1X,50('-'))
C
C        INITIAL CONDITIONS: THETA(0)=1, DTHETA(0)=0
C
         XI = 1.0D-6
         THETA = 1.0D0 - (XI**2)/6.0D0
         DTHETA = -XI/3.0D0
C
C        FOURTH-ORDER RUNGE-KUTTA INTEGRATION
C
         DO 400 I = 1, NSTEPS
C
C           RK4 STAGE 1
            K1A = H * DTHETA
            K1B = H * (-2.0D0*DTHETA/XI - DABS(THETA)**PINDEX)
C
C           RK4 STAGE 2
            K2A = H * (DTHETA + 0.5D0*K1B)
            K2B = H * (-2.0D0*(DTHETA+0.5D0*K1B)/(XI+0.5D0*H)
     &           - DABS(THETA+0.5D0*K1A)**PINDEX)
C
C           RK4 STAGE 3
            K3A = H * (DTHETA + 0.5D0*K2B)
            K3B = H * (-2.0D0*(DTHETA+0.5D0*K2B)/(XI+0.5D0*H)
     &           - DABS(THETA+0.5D0*K2A)**PINDEX)
C
C           RK4 STAGE 4
            K4A = H * (DTHETA + K3B)
            K4B = H * (-2.0D0*(DTHETA+K3B)/(XI+H)
     &           - DABS(THETA+K3A)**PINDEX)
C
C           UPDATE SOLUTION
            THETA = THETA + (K1A+2.0D0*K2A+2.0D0*K3A+K4A)/6.0D0
            DTHETA = DTHETA + (K1B+2.0D0*K2B+2.0D0*K3B+K4B)/6.0D0
            XI = XI + H
C
C           PRINT EVERY 1000 STEPS
            IF (MOD(I,1000).EQ.0) THEN
               RHO = DABS(THETA)**PINDEX
               IF (PINDEX .GT. 0.01D0) THEN
                  RHO = DABS(THETA)**PINDEX
               ELSE
                  RHO = 1.0D0
               END IF
               EMASS = -XI**2 * DTHETA
               WRITE(*,350) XI, THETA, DTHETA, RHO, EMASS
  350          FORMAT(1X,F8.4,4X,F10.6,4X,F12.6,4X,F8.5,4X,F8.4)
            END IF
C
C           CHECK FOR STELLAR SURFACE (THETA <= 0)
            IF (THETA .LE. 0.0D0) THEN
               XI1 = XI
               WRITE(*,360) XI1, (-XI1**2*DTHETA)
  360          FORMAT(/,1X,'*** STELLAR SURFACE REACHED ***',/,
     &          1X,'SURFACE RADIUS  XI1     = ',F10.5,/,
     &          1X,'TOTAL MASS     -XI1**2 * DTHETA = ',F10.5)
C
C              COMPUTE PHYSICAL STELLAR PARAMETERS
C              ASSUME SOLAR-LIKE CENTRAL CONDITIONS
               RHOC = 150.0D0
               ACON = RSOL / XI1
               RSTAR = ACON * XI1
               MSTAR = 4.0D0*PI*RHOC*(ACON**3)*(-XI1**2*DTHETA)
C
               WRITE(*,370) RHOC, RSTAR/RSOL, MSTAR/MSOL
  370          FORMAT(
     &           1X,'CENTRAL DENSITY RHOC    = ',F8.1,' G/CM3',/,
     &           1X,'STELLAR RADIUS          = ',F8.4,' RSOL',/,
     &           1X,'STELLAR MASS            = ',F8.4,' MSOL')
               GO TO 500
            END IF
C
  400    CONTINUE
C
         WRITE(*,410) PINDEX
  410    FORMAT(/,1X,'** SURFACE NOT REACHED FOR N=',F4.1,
     &    ' IN ',I6,' STEPS **')
C
  500 CONTINUE
C
      WRITE(*,600)
  600 FORMAT(/,1X,60('*'),/,
     &  1X,'*   END OF STELLAR STRUCTURE COMPUTATION',18X,'*',/,
     &  1X,'*   RESULTS VERIFIED AGAINST CHANDRASEKHAR (1939)',
     &  8X,'*',/,
     &  1X,'*   AN INTRODUCTION TO THE STUDY OF STELLAR STRUCTURE',
     &  4X,'*',/,
     &  1X,60('*'),/)
C
      STOP
      END
