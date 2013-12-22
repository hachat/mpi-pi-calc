	  program load_balancer
	    include 'mpif.h'
	  
		integer*4 numtasks,rank,len,ierr
		

	  	call MPI_INIT (ierr)
		if (ierr .ne. MPI_SUCCESS) then
    		print *,'Error starting MPI program. Terminating.'
      		call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
		end if

   		call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   		call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
		
		print *, 'Number of tasks=',numtasks,' My rank=',rank
		
		call MPI_FINALIZE(ierr)
	  end