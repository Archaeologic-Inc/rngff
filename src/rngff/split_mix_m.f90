! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module rngff_split_mix_m
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use rngff_splitable_RNG_m, only: splitable_RNG_t

    implicit none
    private
    public :: split_mix_t

    type, extends(splitable_RNG_t) :: split_mix_t
        private
        integer(int64) :: seed, gamma
    contains
        procedure :: split
        procedure :: next_int32
        procedure :: next_int64
        procedure :: next_real32
        procedure :: next_real64
    end type

    interface split_mix_t
        module procedure seed_split_mix
        module procedure make_split_mix
    end interface
contains
    pure function seed_split_mix(seed, gamma) result(rng)
        integer(int64), intent(in) :: seed, gamma
        type(split_mix_t) :: rng

        rng%seed = seed
        rng%gamma = ior(gamma, 1_int64)
    end function

    pure function make_split_mix(seed) result(rng)
        integer(int64), intent(in) :: seed
        type(split_mix_t) :: rng

        integer(int64), parameter :: golden_gamma = int(z'9e3779b97f4a7c15', int64)

        rng = split_mix_t(mix64(seed), mix_gamma(seed + golden_gamma))
    end function

    pure function mix64(z0) result(z3)
        integer(int64), intent(in) :: z0
        integer(int64) :: z3

        integer(int64) :: z1, z2

        z1 = shift_xor_multiply(33, int(z'c4ceb9fe1a85ec53', int64), z0)
        z2 = shift_xor_multiply(33, int(z'ff51afd7ed558ccd', int64), z1)
        z3 = shift_xor(33, z2)
    end function

    pure function mix64_variant13(z0) result(z3)
        integer(int64), intent(in) :: z0
        integer(int64) :: z3

        integer(int64) :: z1, z2

        z1 = shift_xor_multiply(30, int(z'bf58476d1ce4e5b9', int64), z0)
        z2 = shift_xor_multiply(27, int(z'94d049bb133111eb', int64), z1)
        z3 = shift_xor(31, z2)
    end function

    pure function mix_gamma(z0) result(z1)
        integer(int64), intent(in) :: z0
        integer(int64) :: z1

        integer :: n

        z1 = mix64_variant13(ior(z0, 1_int64))
        n = popcnt(ieor(z1, shiftr(z1, 1_int64)))
        if (n >= 24) z1 = ieor(z1, int(z'aaaaaaaaaaaaaaaa', int64))
    end function

    pure function shift_xor_multiply(n, k, w)
        integer, intent(in) :: n
        integer(int64), intent(in) :: k, w
        integer(int64) :: shift_xor_multiply

        shift_xor_multiply = shift_xor(n, w * k)
    end function

    pure function shift_xor(n, w)
        integer, intent(in) :: n
        integer(int64), intent(in) :: w
        integer(int64) :: shift_xor

        shift_xor = ieor(w, shiftr(w, n))
    end function

    subroutine split(self, new)
        class(split_mix_t), intent(inout) :: self
        class(splitable_RNG_t), allocatable, intent(out) :: new

        integer(int64) :: seed_, seed__

        seed_ = self%seed + self%gamma
        seed__ = seed_ + self%gamma
        new = split_mix_t(mix64(seed_), mix_gamma(seed__))
        self%seed = seed__
    end subroutine

    elemental subroutine next_int32(self, harvest)
        class(split_mix_t), intent(inout) :: self
        integer(int32), intent(out) :: harvest

        integer(int64) :: harvest_

        call self%next(harvest_)
        harvest = int(harvest_, int32)
    end subroutine

    elemental subroutine next_int64(self, harvest)
        class(split_mix_t), intent(inout) :: self
        integer(int64), intent(out) :: harvest

        integer(int64) :: seed_

        seed_ = self%seed + self%gamma
        harvest = mix64(seed_)
        self%seed = seed_
    end subroutine

    elemental subroutine next_real32(self, harvest)
        class(split_mix_t), intent(inout) :: self
        real(real32), intent(out) :: harvest

        real(real64) :: harvest_

        call self%next(harvest_)
        harvest = real(harvest_, real32)
    end subroutine

    elemental subroutine next_real64(self, harvest)
        class(split_mix_t), intent(inout) :: self
        real(real64), intent(out) :: harvest

        real(real64), parameter :: double_ulp = 1.0_real64 / shiftl(1_int64, 53_int64)
        integer(int64) :: harvest_

        call self%next(harvest_)
        harvest = shiftr(harvest_, 11_int64) * double_ulp
    end subroutine
end module