! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module rngff_linear_congruential_m
    !! An implementation of the linear congruential algorithm
    !! from Numerical Recipes
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use rngff_RNG_m, only: RNG_t

    implicit none
    private
    public :: linear_congruential_t

    type, extends(RNG_t) :: linear_congruential_t
        private
        integer(int32) :: seeds_(2)
    contains
        procedure :: seeds
        procedure :: next_int32
        procedure :: next_int64
        procedure :: next_real32
        procedure :: next_real64
    end type

    interface linear_congruential_t
        module procedure construct
    end interface
contains
    pure function construct(seed1, seed2) result(new_rng)
        integer(int32), intent(in) :: seed1, seed2
        type(linear_congruential_t) :: new_rng

        new_rng%seeds_ = [seed1, seed2]
    end function

    pure function seeds(self)
        class(linear_congruential_t), intent(in) :: self
        integer(int32) :: seeds(2)

        seeds = self%seeds_
    end function

    elemental subroutine next_int32(self, harvest)
        class(linear_congruential_t), intent(inout) :: self
        integer(int32), intent(out) :: harvest

        logical :: negative
        real(real64) :: tmp

        call self%next(tmp)
        negative = tmp >= 0.5 ! Use the "first bit" of tmp as a sign
        tmp = 2*tmp - merge(1, 0, negative) ! Drop the "first bit" of tmp
        harvest = tmp * huge(harvest) * merge(-1, 1, negative)
    end subroutine

    elemental subroutine next_int64(self, harvest)
        class(linear_congruential_t), intent(inout) :: self
        integer(int64), intent(out) :: harvest

        logical :: negative
        real(real64) :: tmp

        call self%next(tmp)
        negative = tmp >= 0.5 ! Use the "first bit" of tmp as a sign
        tmp = 2*tmp - merge(1, 0, negative) ! Drop the "first bit" of tmp
        harvest = tmp * huge(harvest) * merge(-1, 1, negative)
    end subroutine

    elemental subroutine next_real32(self, harvest)
        class(linear_congruential_t), intent(inout) :: self
        real(real32), intent(out) :: harvest

        integer(int32), parameter, dimension(2) :: m  = [2147483563_int32, 2147483399_int32]
        real(real64), parameter :: over_m1 = 1._real64 / m(1)
        integer(int32), parameter :: a(2) = [40014_int32, 40692_int32]
        integer(int32), parameter :: b(2) = [53668_int32, 52774_int32]
        integer(int32), parameter :: c(2) = [12211_int32, 3791_int32]
        integer(int32) :: z

        associate(k => self%seeds_/b)
            self%seeds_ = a * (self%seeds_-k*b) - k*c
        end associate
        where (self%seeds_ < 0) self%seeds_ = self%seeds_ + m

        z = self%seeds_(1) - self%seeds_(2)
        if (z<1) z = z + m(1) - 1
        harvest = z * over_m1
        if (harvest >= 1) harvest = 0
    end subroutine

    elemental subroutine next_real64(self, harvest)
        class(linear_congruential_t), intent(inout) :: self
        real(real64), intent(out) :: harvest

        real(real32) :: tmp

        call self%next(tmp)
        harvest = tmp
    end subroutine
end module