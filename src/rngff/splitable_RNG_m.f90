! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module rngff_splitable_RNG_m
    !! Define an extension of a RNG that can be "split".
    !! I.e. a new, uncorrelated RNG can be produced from an existing RNG
    use rngff_RNG_m, only: RNG_t

    implicit none
    private
    public :: splitable_RNG_t

    type, abstract, extends(RNG_T) :: splitable_RNG_t
    contains
        procedure(split_i), deferred :: split
    end type

    abstract interface
        subroutine split_i(self, new)
            !! Should create a new random number generator from the current one.
            !! It should be done in such a way that the two generators are not correlated.
            import :: splitable_RNG_t
            implicit none
            class(splitable_RNG_t), intent(inout) :: self
            class(splitable_RNG_t), allocatable, intent(out) :: new
        end subroutine
    end interface
end module