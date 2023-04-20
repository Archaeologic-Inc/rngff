module sanity_checks_m
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use iso_varying_string, only: varying_string, operator(==), operator(//)
    use prune, only: report_differences
    use rngff, only: RNG_t
    use strff, only: to_string
    use veggies, only: &
            input_t, &
            result_t, &
            test_item_t, &
            fail, &
            it_, &
            succeed

    implicit none
    private
    public :: rng_input_t, sanity_checks, DISTRIBUTION_DESCRIPTION

    type, extends(input_t) :: rng_input_t
        class(RNG_t), allocatable :: rng
    end type

    character(len=*), parameter :: DISTRIBUTION_DESCRIPTION = &
            "should produce a uniform distribution of numbers"

    interface get_distribution
        module procedure get_distribution_int32
        module procedure get_distribution_int64
    end interface
contains
    function sanity_checks() result(tests)
        type(test_item_t), allocatable :: tests(:)

        tests = &
                [ it_("between -huge and huge for 32 bit integers", check_32bit_distribution) &
                , it_("between -huge and huge for 64 bit integers", check_64bit_distribution) &
                ]
    end function

    function check_32bit_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
                integer, parameter :: sample_size = 1000000, distribution_size = 100
                real, parameter :: ideal_distribution(distribution_size) = 1./distribution_size
                integer(int32), allocatable :: samples(:)
                real :: distribution(distribution_size)
                integer :: i
                class(RNG_t), allocatable :: the_rng
                type(varying_string) :: differences

                allocate(samples(sample_size))
                the_rng = input%rng
                do i = 1, sample_size
                    call the_rng%next(samples(i))
                end do
                distribution = get_distribution(samples, distribution_size)
                differences = report_differences( &
                        expected = ideal_distribution, &
                        actual = distribution, &
                        absolute_tolerance = 0.0, &
                        relative_tolerance = 0.05, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(0.05) // "%")
                else
                    result_ = fail(differences)
                end if
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    function check_64bit_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
                integer, parameter :: sample_size = 1000000, distribution_size = 100
                real, parameter :: ideal_distribution(distribution_size) = 1./distribution_size
                integer(int64), allocatable :: samples(:)
                real :: distribution(distribution_size)
                integer :: i
                class(RNG_t), allocatable :: the_rng
                type(varying_string) :: differences

                allocate(samples(sample_size))
                the_rng = input%rng
                do i = 1, sample_size
                    call the_rng%next(samples(i))
                end do
                distribution = get_distribution(samples, distribution_size)
                differences = report_differences( &
                        expected = ideal_distribution, &
                        actual = distribution, &
                        absolute_tolerance = 0.0, &
                        relative_tolerance = 0.05, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(0.05) // "%")
                else
                    result_ = fail(differences)
                end if
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    pure function get_distribution_int32(samples, num_bins) result(distribution)
        integer(int32), intent(in) :: samples(:)
        integer, intent(in) :: num_bins
        real :: distribution(num_bins)

        real :: bin_width
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

    pure function get_distribution_int64(samples, num_bins) result(distribution)
        integer(int64), intent(in) :: samples(:)
        integer, intent(in) :: num_bins
        real :: distribution(num_bins)

        real :: bin_width
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