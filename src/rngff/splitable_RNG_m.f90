module rngff_splitable_RNG_m
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