*************************************************************************
* Planet Formation
* Written by: Fritz and Christian
* CMS 495 - Dr. Fuse
*************************************************************************

* planet information from : https://nssdc.gsfc.nasa.gov/

MODULE PLANETS                                                        ! Planets Module
       *------------------------------------------------------------------------
                     ! define the planet data structure
                     TYPE PLANET
                            CHARACTER(8) :: name                                    ! name of the planet          
                            REAL(8) :: a                                            ! axis in astronomical units
                            REAL(8) :: period                                       ! period of the planet                                       
                            REAL(8) :: x, y                                         ! position x and y
                     END TYPE PLANET
              END
       *------------------------------------------------------------------------
       *------------------------------------------------------------------------
       
              PROGRAM PlanetFormation                                               ! Main Program
       *------------------------------------------------------------------------
              ! import the planet data structure
              USE PLANETS                                                           ! import plants module    
              TYPE(PLANET), DIMENSION(7) :: ss                                       ! create array of planets (solar system)
              REAL :: in
              INTEGER :: pNum, datapoints, t
       
              ! initialize the solar system
              CALL INIT_SOLAR(ss)
              
              ! get user input
       1      WRITE(*,*) 'What planet do you want?'
              WRITE(*,*) '1. Venus'
              WRITE(*,*) '2. Earth'
              WRITE(*,*) '3. Mars'
              WRITE(*,*) '4. Jupiter'
              WRITE(*,*) '5. Saturn'
              WRITE(*,*) '6. Uranus'
              WRITE(*,*) '7. Neptune'
              READ(*,*) pNum
              IF((pNum.LT.1).or.(pNum.GT.7)) THEN
                     WRITE(*,*) 'Input invalid.'
                     GOTO 1
              ENDIF
       
              ! get amount of datapoints from user
              WRITE(*,*) 'How many datapoints do you want?'
              READ(*,*) datapoints
       
              ! calculate interval
              in = (3 * ss(pNum)%period) / datapoints
       
              DO t = 1, datapoints
                     CALL GETPOSITION(ss(pNum),in*(t-1), in*t)
                     WRITE(*,*) 'pos x', ss(pNum)%x
                     WRITE(*,*) 'pos y', ss(pNum)%y
              ENDDO
       
              READ(*,*) datapoints
       
              END PROGRAM
       *------------------------------------------------------------------------
       
              ! initialize solar system
              SUBROUTINE INIT_SOLAR(solarsystem)
              
              ! define variables
              USE PLANETS
              TYPE(PLANET), DIMENSION(7) :: solarsystem
       
              ! VENUS
              solarsystem(1)%name = 'VENUS'                                  
              solarsystem(1)%a = 0.72333199
              solarsystem(1)%period = CALL GETPERIOD(solarsystem(1))
       
              ! EARTH
              solarsystem(2)%name = 'EARTH'                                  
              solarsystem(2)%a = 1.0
              solarsystem(2)%period = CALL GETPERIOD(solarsystem(2))
       
              ! MARS
              solarsystem(3)%name = 'MARS'                                  
              solarsystem(3)%a = 1.52366231
              solarsystem(3)%period = CALL GETPERIOD(solarsystem(3))
       
              ! JUPITER
              solarsystem(4)%name = 'JUPITER'                                  
              solarsystem(4)%a = 5.20336301
              solarsystem(4)%period = CALL GETPERIOD(solarsystem(4))
       
              ! SATURN
              solarsystem(5)%name = 'SATURN'                                  
              solarsystem(5)%a = 9.53707032
              solarsystem(5)%period = CALL GETPERIOD(solarsystem(5))
       
              ! URANUS
              solarsystem(6)%name = 'URANUS'                                  
              solarsystem(6)%a = 19.19126393
              solarsystem(6)%period = CALL GETPERIOD(solarsystem(6))
       
              ! NEPTUNE
              solarsystem(7)%name = 'NEPTUNE'                                  
              solarsystem(7)%a = 30.06896348
              solarsystem(7)%period = CALL GETPERIOD(solarsystem(7))
       
              END SUBROUTINE
       *------------------------------------------------------------------------
       
              ! find period of planet
              SUBROUTINE GETPERIOD(p)
       
              ! define variables
              USE PLANETS                                                           ! import planets module
              TYPE(PLANET) :: p                                               ! define planet
       
              ! calculate period
              p%period = SQRT(((p%a)**3))                                 ! calculate period in earth years
       
              END SUBROUTINE
       *------------------------------------------------------------------------
       
              ! find position of planet
              SUBROUTINE GETPOSITION(p, t0, t1)
              
              ! define variables
              USE PLANETS                                                           ! import planets module
              TYPE(PLANET) :: p                                                     ! define planet
              REAL :: t0, t1                                                        ! define t0 and t1 (times)
              REAL(8) :: pc                                                         ! define p (proportionality constant)
              
              ! calculate position
              pc = 2 * 3.1416 / p%period                                            c! calculate proportionality constant
              p%x = p%a * COS(pc * (t1 - t0))                                      ! x-position in relation to central body as function of time
              p%y = p%a * SIN(pc * (t1 - t0))                                      ! y-position in relation to central body as function of time
       
              END SUBROUTINE
       