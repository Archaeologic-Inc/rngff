! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
program main
    use, intrinsic :: iso_fortran_env, only: int32
    use rngff, only: linear_congruential_t
    use write_bytes, only: write_32bits

    implicit none

    type(linear_congruential_t) :: RNG
    integer(int32) :: harvest

    RNG = linear_congruential_t(12345, 67890)

    do
        call RNG%next(harvest)
        call write_32bits(harvest)
    end do
end program