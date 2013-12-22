
default:
	mkdir -p bin

all: default load_balancer pi_parallel_lb mpi_findpi
	
load_balancer: default
	mpif77 -o bin/load_balancer source/load_balancer.f

pi_parallel_lb: default
	mpif77 -o bin/pi_parallel_lb source/pi_parallel_lb.f

mpi_findpi: default
	mpif77 -o bin/mpi_findpi source/mpi_findpi.f
	