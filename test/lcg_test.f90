! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module lcg_test
    use rngff, only: linear_congruential_t
    use sanity_checks_m, only: rng_input_t, sanity_checks
    use veggies, only: test_item_t

    implicit none
    private
    public :: test_lcg
contains
    function test_lcg() result(tests)
        type(test_item_t) :: tests

        type(rng_input_t) :: lcg_rng

        lcg_rng%rng = linear_congruential_t(12345, 67890)
        tests = sanity_checks("linear congruential", lcg_rng)
    end function
end module