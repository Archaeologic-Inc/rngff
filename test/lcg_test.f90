module lcg_test
    use rngff, only: linear_congruential_t
    use sanity_checks_m, only: rng_input_t, sanity_checks, DISTRIBUTION_DESCRIPTION
    use veggies, only: test_item_t, describe

    implicit none
    private
    public :: test_lcg
contains
    function test_lcg() result(tests)
        type(test_item_t) :: tests

        type(rng_input_t) :: lcg_rng

        lcg_rng%rng = linear_congruential_t(12345, 67890)
        tests = describe( &
                "the linear congruential generator " // DISTRIBUTION_DESCRIPTION, &
                lcg_rng, &
                sanity_checks())
    end function
end module