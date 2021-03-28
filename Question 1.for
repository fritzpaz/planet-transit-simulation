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
       TYPE(PLANET), DIMENSION(7) :: ss                                      ! create array of planets (solar system)
       REAL :: in                                                            ! interval
       INTEGER :: pNum, dp, t                                                ! planetNumber, datapoints, time
       CHARACTER :: exit                                                     ! exit to terminate program

       ! Formats
       CHARACTER(LEN=*), PARAMETER :: FMT1 = "(A12, A12, A12)"               ! format #1
       CHARACTER(LEN=*), PARAMETER :: FMT2 = "(F12.2, F12.6, F12.6)"         ! format #2


       ! initialize the solar system
       CALL Initsolar(ss)                                                    ! initialize the solar system
       
       ! print introduction
       WRITE(*,*) 'Welcome to the Planet Orbit Software'
       WRITE(*,*) 'I will predict the orbit of a planet in the solar'
       WRITE(*,*) 'system of your choice.'
       WRITE(*,*) 'Or all of them!'
       WRITE(*,*) ''

       ! get user input
1      WRITE(*,*) 'What planet do you want?'
       WRITE(*,*) '0. ALL'
       WRITE(*,*) '1. Venus'
       WRITE(*,*) '2. Earth'
       WRITE(*,*) '3. Mars'
       WRITE(*,*) '4. Jupiter'
       WRITE(*,*) '5. Saturn'
       WRITE(*,*) '6. Uranus'
       WRITE(*,*) '7. Neptune'
       READ(*,*) pNum                                                        ! read planet number
       IF((pNum.LT.0).or.(pNum.GT.7)) THEN                                   ! check if input is within bounds
              WRITE(*,*) 'Input invalid.'                                    ! if not in bounds ask for input again
              GOTO 1
       ENDIF

       ! get amount of datapoints from user
2      WRITE(*,*) 'How many datapoints do you want?'    
       READ(*,*) dp                                                          ! ask user for datapoints
       IF((dp.LT.4).or.(dp.GT.200)) THEN                                     ! check if input is reasonable
              WRITE(*,*) 'Input out of bounds.'                              ! if not ask for input again
              GOTO 2
       ENDIF

       IF(pNum.eq.0) THEN                                                    ! if user wants information on all planets
              DO i = 1, 7                                                    ! for each planet i in solar systems array
                     in = (3 * ss(i)%period) / dp                            ! calculate interval

                     WRITE(*,*) ''                                                  
                     WRITE(*,*) 'Planet:', ss(i)%name                        ! print planet name
                     WRITE(*,*) 'Period (earth years):', ss(i)%period        ! print planet period
                     WRITE(*,*) 'Interval (days):', 365 * in                 ! print interval (based on data points)
       
       
                     WRITE(*,FMT1) 'Time', ' X', '  Y'                       ! print for table format
                     DO t = 1, dp                                            ! loop through intervals
                            CALL getPosition(ss(i),in*(t))                   ! get position of the planet at time t
                            WRITE(*,FMT2) 365 *in*(t), ss(i)%x, ss(pi)%y     ! print position of the planet at time t
                     ENDDO   
                     WRITE(*,*) ''
                     WRITE(*,*) '--------------------------------------'
              ENDDO
       ELSE                                                                  ! if user wants information on a single planet
              in = (3 * ss(pNum)%period) / dp                                ! calculate interval

              WRITE(*,*) ''                                                  
              WRITE(*,*) 'Planet:', ss(pNum)%name                            ! print planet name
              WRITE(*,*) 'Period (earth years):', ss(pNum)%period            ! print planet period
              WRITE(*,*) 'Interval (days):', 365 * in                        ! print interval (based on data points)


              WRITE(*,FMT1) 'Time', ' X', '  Y'                              ! print for table format
              DO t = 1, dp                                                   ! loop through intervals
                     CALL getPosition(ss(pNum),in*(t))                       ! get position of the planet at time t
                     WRITE(*,FMT2) 365 *in*(t), ss(pNum)%x, ss(pNum)%y       ! print position of the planet at time t
              ENDDO
       ENDIF

       WRITE(*,*) 'Press any key to terminate the program.'                  ! terminate the program     
       READ(*,*) exit                                                        ! exit

       END
*------------------------------------------------------------------------

       ! initialize solar system
       SUBROUTINE Initsolar(solarsystem)
       
       ! define variables
       USE PLANETS
       TYPE(PLANET), DIMENSION(7) :: solarsystem

       ! VENUS
       solarsystem(1)%name = 'VENUS'                                  
       solarsystem(1)%a = 0.72333199
       CALL getPeriod(solarsystem(1))

       ! EARTH
       solarsystem(2)%name = 'EARTH'                                  
       solarsystem(2)%a = 1.0
       CALL getPeriod(solarsystem(2))

       ! MARS
       solarsystem(3)%name = 'MARS'                                  
       solarsystem(3)%a = 1.52366231
       CALL getPeriod(solarsystem(3))

       ! JUPITER
       solarsystem(4)%name = 'JUPITER'                                  
       solarsystem(4)%a = 5.20336301
       CALL getPeriod(solarsystem(4))

       ! SATURN
       solarsystem(5)%name = 'SATURN'                                  
       solarsystem(5)%a = 9.53707032
       CALL getPeriod(solarsystem(5))

       ! URANUS
       solarsystem(6)%name = 'URANUS'                                  
       solarsystem(6)%a = 19.19126393
       CALL getPeriod(solarsystem(6))

       ! NEPTUNE
       solarsystem(7)%name = 'NEPTUNE'                                  
       solarsystem(7)%a = 30.06896348
       CALL getPeriod(solarsystem(7))

       END
*------------------------------------------------------------------------

       ! find period of planet
       SUBROUTINE getPeriod(p)

       ! define variables
       USE PLANETS                                                           ! import planets module
       TYPE(PLANET) :: p                                               ! define planet

       ! calculate period
       p%period = SQRT(((p%a)**3))                                 ! calculate period in earth years

       END
*------------------------------------------------------------------------

       ! find position of planet
       SUBROUTINE getPosition(p, t)
       
       ! define variables
       USE PLANETS                                                           ! import planets module
       TYPE(PLANET) :: p                                                     ! define planet
       REAL :: t                                                             ! define t0 and t1 (times)
       REAL(8) :: pc                                                         ! define p (proportionality constant)
       
       ! calculate position
       pc = 2 * 3.1416 / p%period                                            c! calculate proportionality constant
       p%x = p%a * COS(pc * (t))                                             ! x-position in relation to central body as function of time
       p%y = p%a * SIN(pc * (t))                                             ! y-position in relation to central body as function of time

       END SUBROUTINE
