! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
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