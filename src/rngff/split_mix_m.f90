! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module rngff_split_mix_m
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_double
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use rngff_splitable_RNG_m, only: splitable_RNG_t

    implicit none
    private
    public :: split_mix_t

    type, bind(C) :: SM_State
        integer(c_int64_t) :: seed
        integer(c_int64_t) :: gamma
    end type

    type, extends(splitable_RNG_t) :: split_mix_t
        private
        type(SM_State) :: state
    contains
        procedure :: set_state
        procedure :: get_state
        procedure :: split
        procedure :: next_int32
        procedure :: next_int64
        procedure :: next_real32
        procedure :: next_real64
    end type

    interface split_mix_t
        module procedure construct_from_seed
        module procedure construct_from_time
    end interface
contains
    pure function construct_from_seed(seed) result(rng)
        integer(int64), intent(in) :: seed
        type(split_mix_t) :: rng

        interface
            pure function construct_from_seed_c(seed) result(state) bind(C)
                import :: c_int64_t, SM_State
                implicit none
                integer(c_int64_t), intent(in), value :: seed
                type(SM_State) :: state
            end function
        end interface

        rng%state = construct_from_seed_c(seed)
    end function

    function construct_from_time() result(rng)
        type(split_mix_t) :: rng

        integer(int32) :: now(8), cpu
        integer(int64) :: seed

        call date_and_time(values = now)
        call system_clock(cpu)
        seed = transfer([sum(now), cpu], seed)
        rng = split_mix_t(seed)
    end function

    pure subroutine set_state(self, seed, gamma)
        class(split_mix_t), intent(inout) :: self
        integer(int64), intent(in) :: seed, gamma

        interface
            pure subroutine set_state_c(state, seed, gamma) bind(C)
                import :: c_int64_t, SM_State
                implicit none
                type(SM_State), intent(out) :: state
                integer(c_int64_t), intent(in), value :: seed, gamma
            end subroutine
        end interface

        call set_state_c(self%state, seed, gamma)
    end subroutine

    pure subroutine get_state(self, seed, gamma)
        class(split_mix_t), intent(in) :: self
        integer(int64), intent(out) :: seed, gamma

        seed = self%state%seed
        gamma = self%state%gamma
    end subroutine

    subroutine split(self, new)
        class(split_mix_t), intent(inout) :: self
        class(splitable_RNG_t), allocatable, intent(out) :: new

        interface
            function split_c(state) result(split_state) bind(C)
                import :: SM_State
                implicit none
                type(SM_State), intent(inout) :: state
                type(SM_State) :: split_state
            end function
        end interface

        type(split_mix_t) :: local_new

        local_new%state = split_c(self%state)
        new = local_new
    end subroutine

    elemental subroutine next_int32(self, harvest)
        class(split_mix_t), intent(inout) :: self
        integer(int32), intent(out) :: harvest

        integer(int64) :: whole
        integer(int32) :: parts(2)

        call self%next(whole)
        parts = transfer(whole, parts)
        harvest = parts(1)
    end subroutine

    elemental subroutine next_int64(self, harvest)
        class(split_mix_t), intent(inout) :: self
        integer(int64), intent(out) :: harvest

        interface
            pure subroutine next_word(state, harvest) bind(C)
                import :: SM_State, c_int64_t
                implicit none
                type(SM_State), intent(inout) :: state
                integer(c_int64_t), intent(out) :: harvest
            end subroutine
        end interface

        call next_word(self%state, harvest)
    end subroutine

    elemental subroutine next_real32(self, harvest)
        class(split_mix_t), intent(inout) :: self
        real(real32), intent(out) :: harvest

        real(real64) :: tmp

        call self%next(tmp)
        harvest = real(tmp, kind=kind(harvest))
    end subroutine

    elemental subroutine next_real64(self, harvest)
        class(split_mix_t), intent(inout) :: self
        real(real64), intent(out) :: harvest

        interface
            pure subroutine next_double(state, harvest) bind(C)
                import :: SM_State, c_double
                implicit none
                type(SM_State), intent(inout) :: state
                real(c_double), intent(out) :: harvest
            end subroutine
        end interface

        call next_double(self%state, harvest)
    end subroutine
end module