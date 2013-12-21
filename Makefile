all:
	mpif77 -o bin/pi_parallel_lb source/pi_parallel_lb.f

pi_parallel_lb:
	mpif77 -o bin/pi_parallel_lb source/pi_parallel_lb.f

mpi_findpi:
	mpif77 -o bin/mpi_findpi source/mpi_findpi.f
	