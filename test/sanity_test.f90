module sanity_test
    use, intrinsic :: iso_fortran_env, only: int32
    use rngff, only: linear_congruential_t, RNG_t
    use veggies, only: &
            input_t, &
            result_t, &
            test_item_t, &
            assert_equals_within_relative, &
            describe, &
            fail, &
            it_

    implicit none
    private
    public :: test_sanity

    type, extends(input_t) :: rng_input_t
        class(RNG_t), allocatable :: rng
    end type
contains
    function test_sanity() result(tests)
        type(test_item_t) :: tests

        type(rng_input_t) :: lcg_rng

        lcg_rng%rng = linear_congruential_t(12345, 67890)
        tests = describe( &
                "The linear congruential generator should produce a uniform distribution of numbers ", &
                lcg_rng, &
                [ it_("between -huge and huge for 32 bit integers", check_32bit_distribution) &
                ])
    end function

    function check_32bit_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
                integer, parameter :: sample_size = 1000000, distribution_size = 100
                double precision, parameter :: ideal_distribution(distribution_size) = 1.d0/distribution_size
                integer(int32), allocatable :: samples(:)
                double precision :: distribution(distribution_size)
                integer :: i
                class(RNG_t), allocatable :: the_rng

                allocate(samples(sample_size))
                the_rng = input%rng
                do i = 1, sample_size
                    call the_rng%next(samples(i))
                end do
                distribution = get_distribution(samples, distribution_size)
                result_ = assert_equals_within_relative(ideal_distribution, distribution, 0.05d0)
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    pure function get_distribution(samples, num_bins) result(distribution)
        integer(int32), intent(in) :: samples(:)
        integer, intent(in) :: num_bins
        double precision :: distribution(num_bins)

        double precision :: bin_width
        integer :: samples_in_bin(num_bins)
        integer :: i, bin

        samples_in_bin = 0
        bin_width = (real(huge(samples)) / num_bins) * 2
        do i = 1, size(samples)
            bin = floor(samples(i)/bin_width + huge(samples)/bin_width) + 1
            samples_in_bin(bin) = samples_in_bin(bin) + 1
        end do
        distribution = real(samples_in_bin, kind(distribution)) / size(samples)
    end function
end module