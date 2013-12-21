c     Finding PI using Monte Carlo Method Distributed Using MPI
c     Load Balanced Version
c     090185B Hettiarachchi H.A.C.
c     CS4552 Scientific Computing Assignment 1 Part B (ii) 


c     Each process is given two seconds to process.
c     When that interval  is finished, total and 
c     success counts are returned to the main process
c     in order. 
c     PI is calculated there.
c     Might overflow integers, as the limit is just
c     bounded by time, not from the count. But 
c     but here, time is hard coded for favourable number.

c     This assumed each process will handle LOCAL_TOTAL 
c     number of operations in similat time period
c     So load is equally distributed.

      program pi_parallel
      include 'mpif.h'

      DOUBLE PRECISION PI,TOTAL_SUCCESS_FP,TOTAL_FP
      integer   taskid, numtasks
      integer received_total,received_success
      integer total,total_success,status(MPI_STATUS_SIZE)
      integer task
      integer local_result
      REAL*8 RANF
      LOGICAL TRY

      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, taskid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numtasks, ierr )

c     ======================================================
c     ===============OTHER THREADS THAN MASTER==============

	    if(taskid .NE. 0) then

        call LOCAL_TRIES(taskid,LOCAL_TOTAL,LOCAL_SUCCESS)      
        print *, "taskid: ",taskid, "local_total:",LOCAL_TOTAL,
     & "local_success:",LOCAL_SUCCESS
        call MPI_SEND( LOCAL_TOTAL, 1, MPI_INT, 0, 0, 
     &                 MPI_COMM_WORLD, ierr )
        call MPI_SEND( LOCAL_SUCCESS, 1, MPI_INT, 0, 1, 
     &                 MPI_COMM_WORLD, ierr )
      endif

c     ======================================================
c     ===============MASTER CODE============================
      if(taskid .EQ. 0) then

c     ------Even the master does prosessing with others-----

        print *, "Processing. Please wait roughly 
     &   for two seconds"
        call LOCAL_TRIES(taskid,LOCAL_TOTAL,LOCAL_SUCCESS)   
        print *, "taskid:",taskid, "local_total:",LOCAL_TOTAL, 
     & "local_success:",LOCAL_SUCCESS
       
        total = LOCAL_TOTAL
        total_success = LOCAL_SUCCESS

c     -----------Receiving results from others--------------
c     --------and add them together to get a total----------
c     ------This is receiving in order, so no deadlocks-----

      	do task = 1,numtasks-1
          call MPI_RECV( received_total, 1, MPI_INT,
     &       task, 0, MPI_COMM_WORLD, status, ierr )

        total = total +  received_total


          call MPI_RECV( received_success, 1, MPI_INT,
     &       task, 1, MPI_COMM_WORLD, status, ierr )

        total_success = total_success + received_success


     		print *, "from:",task,"recv_total:",received_total,
     & " recv_success:",received_success
     		
     	end do

c     ------------------Final PI Calculation---------------
      TOTAL_SUCCESS_FP = total_success * 1.0
      TOTAL_FP = total * 1.0
      PI = 4 *  TOTAL_SUCCESS_FP / TOTAL_FP

      print *, "total:",total,"total_success:",total_success
      print *, "Estimated_PI:", PI

      print *, ' '
        print *,'Real value of PI: 3.1415926535897'
        print *, ' '
      endif

      call MPI_FINALIZE(ierr)
      end


c     ======================================================
c     ============Subroutine for each processor=============

      SUBROUTINE LOCAL_TRIES(taskid,LOCAL_TOTAL,LOCAL_SUCCESS)
        INCLUDE 'mpif.h'
        DOUBLE PRECISION start_time,interval,current_time
        DOUBLE PRECISION elapsed_time
        INTEGER taskid
        REAL*8 RANF
        LOGICAL TRY,TEST
        INTEGER LOCAL_TOTAL, LOCAL_SUCCESS
        INTEGER iseed
        INTEGER SAMPLE
        COMMON /CSEED/ ISEED

        LOCAL_TOTAL = 0
        LOCAL_SUCCESS = 0
        iseed = 65243 + taskid
        
        interval = 2.0
        elapsed_time = 0.0
        start_time = MPI_WTIME()
        current_time = start_time
        DO WHILE(elapsed_time .LE. interval)
            LOCAL_TOTAL = LOCAL_TOTAL + 1
            TEST = TRY()
c           print *, "Test:",TEST
            IF (TEST .EQV. .TRUE.) THEN
              LOCAL_SUCCESS = LOCAL_SUCCESS + 1
            END IF

            current_time =  MPI_WTIME()
            elapsed_time = current_time - start_time
        END DO    
        RETURN
      END SUBROUTINE


c     ======================================================
c     =======================EXPERIMENT=====================

      FUNCTION TRY()
        LOGICAL TRY
        REAL*8 RANF
        REAL X,Y,RADIUS

        X = RANF()
        Y = RANF()
        X = X/2
        Y = Y/2

        RADIUS = (X * X) + (Y * Y)
c     PRINT *,'Point', X,Y
        IF (RADIUS .LE. 0.25) THEN
          TRY = .TRUE.
        ELSE 
           TRY = .FALSE.
        END IF
      RETURN
      END FUNCTION


c     Following random number generator is from 
c     http://www.physics.unlv.edu/~pang/cp_f77.html
c     Program 2.11: Uniform random number generator

c     And Modified according to
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
      END FUNCTION



