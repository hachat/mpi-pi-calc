	  PROGRAM LOAD_BALANCER
	    INCLUDE 'mpif.h'
	    INTEGER*4 MAX_PROC_COUNT,I
	  	DOUBLE PRECISION START_TIME,END_TIME
		INTEGER*4 NUMTASKS,RANK,LEN,IERR,TASK

        
		INTEGER*4 CHUNK(10),ASSIGNED_CHUNK(10)
		INTEGER*4 STATUS_SEND(MPI_STATUS_SIZE)
        INTEGER*4 STATUS_RECV(MPI_STATUS_SIZE)
        INTEGER*4 REQUEST_SEND(9)
        INTEGER*4 REQUEST_RECV(1)

        INTEGER*4 STATUS_SEND_RES(MPI_STATUS_SIZE)
        INTEGER*4 STATUS_RECV_RES(MPI_STATUS_SIZE)
        INTEGER*4 REQUEST_SEND_RES(1)
        INTEGER*4 REQUEST_RECV_RES(9)

        INTEGER*4 RESULT(10)

c       keep idea about what to do with a given node
c       either send another request or waiting till a response comes
        INTEGER*1 NODE_STATUS(10)
        LOGICAL   SEND_TEST_FLAG,RECV_TEST_FLAG
		INTEGER*4 PENDING_SENDS,PENDING_RECVS;
        
        MAX_PROC_COUNT = 10
        PENDING_SENDS = 0;
        PENDING_RECVS = 0;

		ASSIGNED_CHUNK(1) = 1
		ASSIGNED_CHUNK(2) = 2
		ASSIGNED_CHUNK(3) = 3
		ASSIGNED_CHUNK(4) = 4
		ASSIGNED_CHUNK(5) = 5
		ASSIGNED_CHUNK(6) = 6
		ASSIGNED_CHUNK(7) = 8
		ASSIGNED_CHUNK(8) = 8
		ASSIGNED_CHUNK(9) = 9
		ASSIGNED_CHUNK(10) = 10

		DO I = 1,10
			NODE_STATUS(I) = 0
			REQUEST_RECV_RES(I) = I
		END DO

	  	CALL MPI_INIT (IERR)
		IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR STARTING MPI PROGRAM. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		END IF

   		CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERR)
   		IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN TAKING COMM_RANK. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		END IF
   		CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NUMTASKS, IERR)
		IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN TAKING COMM_SIZE. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		END IF

		IF (NUMTASKS .GE. MAX_PROC_COUNT) THEN
			NUMTASKS = MAX_PROC_COUNT
		END IF
C     ======================================================
C     ===============MASTER CODE============================
      IF(RANK .EQ. 0) THEN

      	START_TIME = MPI_WTIME()

		END_TIME = MPI_WTIME()
		PRINT *, 'START TIME:', START_TIME
		
		
c       initial processing request for each process

		DO TASK = 1, NUMTASKS-1
		  CALL MPI_ISEND(ASSIGNED_CHUNK(TASK), 1, MPI_INTEGER, TASK, 
     &     0, MPI_COMM_WORLD,
     &     REQUEST_SEND(TASK), IERR)
     	  IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN ISEND. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		  END IF
		  PENDING_SENDS = PENDING_SENDS +1
     	END DO

     	PRINT *, 'Pending Sends ',PENDING_SENDS
     	PRINT *, 'Waiting to clear another send buf..'
		 
		DO WHILE (.TRUE.)

c       test if last requests sent completely, and switch to
c       receiving mode

		  IF(PENDING_SENDS .GT. 0) THEN

c        given that we sent a request, confirm its sent

		    CALL MPI_TESTANY(NUMTASKS-1,REQUEST_SEND,TASK,
     &          SEND_TEST_FLAG,STATUS_SEND,IERR)
	        IF (IERR .NE. MPI_SUCCESS) THEN
    		  PRINT *,'ERROR IN WAIT FOR SEND. TERMINATING.'
      		  CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		    END IF
c           Undefined Task MPI_UNDEFINED -32766
		    IF (TASK .NE. MPI_UNDEFINED) THEN
		    
		    IF(NODE_STATUS(TASK) .EQ. 0) THEN
		     IF(SEND_TEST_FLAG) THEN
		      REQUEST_SEND(TASK) = MPI_REQUEST_NULL
		      PRINT *, 'Cleared send buf ',TASK
c             switching node status to receiving mode
		 	  NODE_STATUS(TASK) = 1
		 	  PENDING_SENDS = PENDING_SENDS -1

		 	  IF(ASSIGNED_CHUNK(TASK) .NE. -1) THEN
		 	   RESULT(TASK) = 0
		 	   CALL MPI_IRECV(RESULT(TASK), 1, MPI_INTEGER, TASK, 
     &	 	    0,  
     &          MPI_COMM_WORLD, REQUEST_RECV_RES(TASK), IERR)
	           IF (IERR .NE. MPI_SUCCESS) THEN
	    		PRINT *,'ERROR IN RECV RESPONSE. TERMINATING.'
	      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
			   END IF
		  		PENDING_RECVS = PENDING_RECVS + 1
		  		PRINT *, "Expecting response from ", TASK
		  	  ELSE
		  	  	PRINT *, "Finished work with ", TASK
		  	  	NODE_STATUS(TASK) = -1
		  	  END IF

		     END IF
		    END IF
		    END IF
		  END IF

		  IF(PENDING_RECVS .GT. 0) THEN
		  	CALL MPI_TESTANY(NUMTASKS-1,REQUEST_RECV_RES,TASK,
     &          RECV_TEST_FLAG,STATUS_RECV_RES,IERR)
	        IF (IERR .NE. MPI_SUCCESS) THEN
    		  PRINT *,'ERROR IN WAIT FOR RECV RESPONSE. TERMINATING.'
      		  CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		    END IF


c           Undefined Task MPI_UNDEFINED -32766
		    IF (TASK .NE. MPI_UNDEFINED) THEN
		    PRINT *, 'Got response from ',TASK
		    IF(NODE_STATUS(TASK) .EQ. 1) THEN
		    
		     IF(RECV_TEST_FLAG) THEN
		      REQUEST_RECV_RES(TASK) = MPI_REQUEST_NULL
		      
		      PENDING_RECVS = PENDING_RECVS -1
c             asses next chunk here..!!!
		      ASSIGNED_CHUNK(TASK) = -1
		      PRINT *, "Assigned ", ASSIGNED_CHUNK(TASK),' to ',TASK

			  CALL MPI_ISEND(ASSIGNED_CHUNK(TASK), 1, MPI_INTEGER, TASK,
     &           0, MPI_COMM_WORLD, REQUEST_SEND(TASK), IERR)
     	  	  IF (IERR .NE. MPI_SUCCESS) THEN
    		    PRINT *,'ERROR IN ISEND. TERMINATING.'
      		    CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		      END IF
		      PENDING_SENDS = PENDING_SENDS +1
		     
		     END IF
		    ELSE
		     PRINT *,'not in receiving mode'
		     DO I=1,NUMTASKS-1
		     	PRINT *, "task: ",TASK,' Status: ', NODE_STATUS(TASK)
		     END DO
		    
		    END IF
			END IF
		    

		  END IF

		  IF(PENDING_RECVS .EQ. 0 .AND. PENDING_SENDS .EQ. 0) THEN
		  	PRINT *, "Completed communications!"
		  	EXIT;
		  END IF
		END DO

C     ======================================================
C     ===============CLIENT CODE============================
	  ELSE

	  PRINT *, 'NUMBER OF TASKS=',NUMTASKS,' MY RANK=',RANK
	  DO WHILE (.TRUE.)
	    CALL MPI_IRECV(CHUNK(RANK), 1, MPI_INTEGER, 0,
     &   0,  
     &     MPI_COMM_WORLD, REQUEST_RECV(RANK), IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN RECV. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		 END IF
	  	PRINT *, 'Waiting to ready recv buf ',RANK
	    
	  	CALL MPI_WAIT(REQUEST_RECV(RANK),STATUS_RECV,IERR)
	  	PRINT *, 'Received buf ',RANK
	  	IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN WAIT FOR RECV. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		END IF
	  	PRINT *,'TASK=',RANK,' CHUNK=',CHUNK(RANK)
	  	
c       termination condition	  	
	  	IF( CHUNK(RANK) .EQ. -1) THEN
	  		PRINT *,'Terminating Process ', RANK
	  		EXIT
	  	END IF 

c       process the chunk here
	  	CALL SLEEP(CHUNK(RANK))


	    CALL MPI_ISEND(CHUNK(RANK), 1, MPI_INTEGER, 0, 
     &     0, MPI_COMM_WORLD, 
     &     REQUEST_SEND_RES(RANK), IERR)
     	  IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN ISEND. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		  END IF

		PRINT *, 'Waiting to clear response buf to master',RANK
		CALL MPI_WAIT(REQUEST_SEND_RES(RANK),STATUS_SEND_RES,IERR)
	  	IF (IERR .NE. MPI_SUCCESS) THEN
    		PRINT *,'ERROR IN WAIT FOR RECV. TERMINATING.'
      		CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
		END IF
c		IF(STATUS_SEND_RES)  ???

	  	PRINT *, 'Sent response buf to master',RANK
	  	
	  END DO
	  ENDIF
	  CALL MPI_FINALIZE(IERR)
	  END