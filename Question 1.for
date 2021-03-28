*************************************************************************
* Planet Formation
* Written by: Fritz and Christian
* CMS 495 - Dr. Fuse
*************************************************************************

       MODULE PLANETS                                                        ! Planets Module
*------------------------------------------------------------------------
              ! define the planet data structure
              TYPE PLANET
                     CHARACTER(8) :: name                                    ! name of the planet          
                     REAL(8) :: mass                                         ! mass of the planet in solar masses
                     REAL(8) :: a                                            ! axis in astronomical units
                     REAL(8) :: period                                       ! period of the planet                                       
                     REAL(8) :: x, y                                         ! position x and y
              END TYPE PLANET

              TYPE SUN                                                       ! sun is at position (0,0)
                     REAL(8) mass = 1.9891D+30                               ! sun's mass
              END TYPE SUN
       END
*------------------------------------------------------------------------
*------------------------------------------------------------------------

       PROGRAM PlanetFormation                                               ! Main Program
*------------------------------------------------------------------------
       ! import the planet data structure
       USE PLANETS                                                           ! import plants module    
       TYPE(PLANET) DIMENSION(7) :: solarsystem                              ! create array of planets (solar system)


       END PROGRAM
*------------------------------------------------------------------------

       ! initialize solar system
       SUBROUTINE INIT_SOLAR(solarsystem, sun)
       
       ! define variables
       USE PLANETS
       TYPE(SUN) :: sun
       TYPE(PLANET) DIMENSION(7) :: solarsystem

       ! VENUS
       solarsystem(1)%name = 'VENUS'                                  
       solarsystem(1)%mass = 4.867D+24
       solarsystem(1)%a = 0.72333199
       solarsystem(1)%period = CALL GETPERIOD(solarsystem(1), sun)

       ! EARTH

       ! MARS

       ! JUPITER

       ! SATURN

       ! UR ANUS

       ! NEPTUNE

       END SUBROUTINE
*------------------------------------------------------------------------

       ! find period of planet
       SUBROUTINE GETPERIOD(planet, sun)

       ! define variables
       USE PLANETS                                                           ! import planets module
       TYPE(PLANET) :: planet                                                ! define planet
       TYPE(SUN) :: sun                                                      ! define sun
       REAL(u)                                                               ! define u

       ! calculate period
       u = 9.81(sun%mass + planet%mass)/sun%mass                             ! u = GM
       planetT%period = 2 * 3.1614 * SQRT(((planetT%a)**3)/u)                ! calculate period

       END SUBROUTINE
*------------------------------------------------------------------------

       ! find position of planet
       SUBROUTINE GETPOSITION(planet, sun, t0, t1)
       
       ! define variables
       USE PLANETS                                                           ! import planets module
       TYPE(PLANET) :: planet                                                ! define planet
       TYPE(SUN) :: sun                                                      ! define sun
       INTEGER :: t0, t1                                                     ! define t0 and t1 (times)
       REAL(8) :: p                                                          ! define p (proportionality constant)
       
       ! calculate position
       p = 2 * 3.1416 / planet%period                                        ! calculate proportionality constant
       planetT%x = planet%a * COS(p * (t1 - t0)))                            ! x-position in relation to central body as function of time
       planet%y = planet%a * SIN(p * (t1 - t0)))                             ! y-position in relation to central body as function of time

       END SUBROUTINE
