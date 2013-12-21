c     Finding PI using Monte Carlo Method in MPI environment
c     090185B Hettiarachchi H.A.C.
c     CS4552 Scientific Computing Assignment 1 Part 2

c     Take a uniformly distributed random point within the square with width and height of 1.   
c     Consider the circle encapsulated within the square with radius 0.5

c                                       Area of Circle        Pi * R * R
c     P(random point within circle) = ------------------  = --------------  = Pi/4
c                                     Area of the Square       4 * R * R

c     Pi = 4 * (No. of tries within circle) / (Total no. of Tries)

c     Termination condition: square of error between two iterations is less than 0.001 

	  PROGRAM FINDPI
	  	INCLUDE 'mpif.h'


		INTEGER*4 numtasks,rank,len,ierr
	    parameter(MASTER = 0)
		
	  	DOUBLE PRECISION PI_SUM
	  	DOUBLE PRECISION TRIES,SUCCESS
	  	DOUBLE PRECISION NEW_PI,OLD_PI,ERROR
	  	LOGICAL TRY
	  	LOGICAL TEST
	    INTEGER iseed
	    INTEGER SAMPLE


	    COMMON /CSEED/ ISEED
	  	
c       We take the mean value of the samples

	    PI_SUM = 0.0

	    iseed = 65243


	    
	    call MPI_INIT (ierr)
		if (ierr .ne. MPI_SUCCESS) then
    		print *,'Error starting MPI program. Terminating.'
      		call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
		end if

		call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   		call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
		
		if (rank .eq. MASTER) then
			PRINT *,"Calculating PI"
			print *, 'Number of tasks=',numtasks
	    end if

c	    DO SAMPLE = 1,20
		  	TRIES = 0.0
		  	SUCCESS = 0.0
			NEW_PI = 10.0
			OLD_PI = 0.0
	    	
			ERROR = 10


		  	DO WHILE ( (ERROR .GT. 0.000000001) .OR. (TRIES .LE. 10) )
		  	    OLD_PI =  NEW_PI
	            TRIES = TRIES + 1.0 
		  	    TEST = TRY()
		  	    IF (TEST .EQV. .TRUE.) THEN
		  	    	SUCCESS = SUCCESS + 1.0
		  	    END IF
		  	    NEW_PI = 4*SUCCESS/TRIES
	        	ERROR = ((NEW_PI - OLD_PI)**2)
		  	    
c		  	    PRINT *,SUCCESS,TRIES,NEW_PI ,ERROR
		  	END DO

		  	PRINT *,'Process ',rank,' PI Approximation: ', NEW_PI
c		  	PI_SUM = PI_SUM + NEW_PI

c		END DO
        call MPI_REDUCE( NEW_PI, PI_SUM, 1, MPI_DOUBLE_PRECISION, 
     &                   MPI_SUM, MASTER, MPI_COMM_WORLD, ierr )

     	if (rank .eq. MASTER) then
          PI_SUM = PI_SUM / numtasks
		  PRINT *,'Final mean value approximation for PI is: ', PI_SUM
          PRINT *,'Real value of PI is : 3.1415926535897'
  
        endif

	  	STOP
	  END

	  LOGICAL FUNCTION TRY()
	    REAL*8 RANF
	    REAL X,Y,RADIUS

        X = RANF()
        Y = RANF()
        X = X/2
        Y = Y/2

        RADIUS = (X * X) + (Y * Y)
c	    PRINT *,'Point', X,Y
	    IF (RADIUS .LE. 0.25) THEN
	       TRY = .TRUE.
	    ELSE 
           TRY = .FALSE.
        END IF
	    RETURN
	  END

c     Following random number generator is from 
c     http://www.physics.unlv.edu/~pang/cp_f77.html
c     Program 2.11: Uniform random number generator

c 	  And Modified according to
c     http://www.tek-tips.com/viewthread.cfm?qid=1538108

      FUNCTION RANF()
      REAL*8 RANF
      COMMON /CSEED/ ISEED      
      DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
        IH = ISEED/IQ
        IL = MOD(ISEED,IQ)
        IT = IA*IL-IR*IH
        IF(IT.GT.0) THEN
          ISEED = IT
        ELSE
          ISEED = IC+IT
        END IF
        RANF = ISEED/FLOAT(IC)
      RETURN
      END

