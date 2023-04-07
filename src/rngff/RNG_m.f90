module rngff_RNG_m
    !! Define the operations that should be available from a random number generator
    implicit none
    private
    public :: RNG_t

    type, abstract :: RNG_t
    contains
        procedure(next_int32_i), deferred :: next_int32
        procedure(next_int64_i), deferred :: next_int64
        procedure(next_real32_i), deferred :: next_real32
        procedure(next_real64_i), deferred :: next_real64
        generic :: next => next_int32, next_int64, next_real32, next_real64
    end type

    abstract interface
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