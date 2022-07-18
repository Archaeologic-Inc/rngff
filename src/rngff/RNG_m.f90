module rngff_RNG_m
    !! Define the operations that should be available from a random number generator
    implicit none
    private
    public :: RNG_t

    type, abstract :: RNG_t
    contains
        procedure(split_i), deferred :: split
        procedure(next_int32_i), deferred :: next_int32
        procedure(next_int64_i), deferred :: next_int64
        procedure(next_real32_i), deferred :: next_real32
        procedure(next_real64_i), deferred :: next_real64
    end type

    abstract interface
        subroutine split_i(self, new)
            !! Should create a new random number generator from the current one.
            !! It should be done in such a way that the two generators are not correlated.
            !! If this is not possible, this procedure should error stop.
            import :: RNG_t
            implicit none
            class(RNG_t), intent(inout) :: self
            class(RNG_t), allocatable, intent(out) :: new
        end subroutine

        elemental subroutine next_int32_i(self, harvest)
            !! Produce a random 32 bit integer
            use, intrinsic :: iso_fortran_env, only: int32
            import :: RNG_t
            implicit none
            class(RNG_t), intent(inout) :: self
            integer(int32), intent(out) :: harvest
        end subroutine

        elemental subroutine next_int64_i(self, harvest)
            !! Produce a random 64 bit integer
            use, intrinsic :: iso_fortran_env, only: int64
            import :: RNG_t
            implicit none
            class(RNG_t), intent(inout) :: self
            integer(int64), intent(out) :: harvest
        end subroutine

        elemental subroutine next_real32_i(self, harvest)
            !! Produce a random 32 bit real
            use, intrinsic :: iso_fortran_env, only: real32
            import :: RNG_t
            implicit none
            class(RNG_t), intent(inout) :: self
            real(real32), intent(out) :: harvest
        end subroutine

        elemental subroutine next_real64_i(self, harvest)
            !! Produce a random 64 bit real
            use, intrinsic :: iso_fortran_env, only: real64
            import :: RNG_t
            implicit none
            class(RNG_t), intent(inout) :: self
            real(real64), intent(out) :: harvest
        end subroutine
    end interface
end module