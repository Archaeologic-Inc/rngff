! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module sm_test
    use, intrinsic :: iso_fortran_env, only: int64
    use rngff, only: split_mix_t
    use sanity_checks_m, only: rng_input_t, sanity_checks, DISTRIBUTION_DESCRIPTION
    use veggies, only: test_item_t, describe

    implicit none
    private
    public :: test_sm
contains
    function test_sm() result(tests)
        type(test_item_t) :: tests

        type(rng_input_t) :: sm_rng

        sm_rng%rng = split_mix_t(1234567890_int64)
        tests = describe( &
                "the split mix generator " // DISTRIBUTION_DESCRIPTION, &
                sm_rng, &
                sanity_checks())
    end function
end module