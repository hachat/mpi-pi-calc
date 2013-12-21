
default:
	mkdir -p bin

all: default pi_parallel_lb mpi_findpi
	
pi_parallel_lb: default
	mpif77 -o bin/pi_parallel_lb source/pi_parallel_lb.f

mpi_findpi: default
	mpif77 -o bin/mpi_findpi source/mpi_findpi.f
	