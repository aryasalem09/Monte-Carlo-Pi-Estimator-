
module rng_mod  ! namespace 
  implicit none ! forces a declaration of every variable 
  private       ! this is obvious
  public :: seed_rng ! seed_rng is public (also obvious)

contains ! start

  subroutine seed_rng() ! seeds RNG
    implicit none
    integer :: n, i ! n is the integer, i is the loop index
    integer, allocatable :: seed(:) ! allocates array once size is found

    call random_seed(size=n) ! checks runtime for the seed #, stored in n
    allocate(seed(n)) ! allocates seed as an array of length n 

    ! fills the seed array with a pattern
    do i = 1, n
      seed(i) = 104729 + 37 * i     ! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
    end do

    call random_seed(put=seed) ! seeds the value in seed
    deallocate(seed) ! finished with the array, deallocates it
  end subroutine seed_rng

end module rng_mod


module monte_carlo_pi_mod
  implicit none
  private
  public :: estimate_pi

contains
 
  pure logical function inside_unit_quarter_circle(x, y) result(is_in) ! pure depends only on inputs
    implicit none
    real, intent(in) :: x, y ! x,y cant be modified 

    is_in = (x * x + y * y <= 1.0) ! circle cond
  end function inside_unit_quarter_circle


  function estimate_pi(n_points) result(pi_est) ! 
    implicit none
    integer, intent(in) :: n_points
    real :: pi_est
    integer :: i, inside
    real :: x, y

    if (n_points <= 0) then
      pi_est = 0.0
      return
    end if

    inside = 0
    do i = 1, n_points
      call random_number(x)
      call random_number(y)

      if (inside_unit_quarter_circle(x, y)) then
        inside = inside + 1
      end if
    end do

    pi_est = 4.0 * real(inside) / real(n_points)
  end function estimate_pi

end module monte_carlo_pi_mod


program monte_carlo_pi
  use rng_mod, only: seed_rng
  use monte_carlo_pi_mod, only: estimate_pi
  implicit none

  integer :: n
  real :: pi_est

  print *, "number of random points?"
  read *, n

  call seed_rng()
  pi_est = estimate_pi(n)

  print *, "pi estimate =", pi_est
end program monte_carlo_pi
