program main
    use, intrinsic :: iso_fortran_env, only: int64
    use rngff, only: split_mix_t

    implicit none

    type(split_mix_t) :: RNG
    integer(int64) :: harvest
    integer :: i

    RNG = split_mix_t(123456890_int64)
    do i = 1, 10
        call RNG%next(harvest)
        print *, harvest
    end do
end program