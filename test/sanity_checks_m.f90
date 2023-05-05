! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module sanity_checks_m
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use iso_varying_string, only: varying_string, operator(==), operator(//)
    use prune, only: report_differences
    use rngff, only: RNG_t
    use strff, only: to_string
    use veggies, only: &
            input_t, &
            result_t, &
            test_item_t, &
            assert_that, &
            describe, &
            fail, &
            it_, &
            succeed

    implicit none
    private
    public :: rng_input_t, sanity_checks

    type, extends(input_t) :: rng_input_t
        class(RNG_t), allocatable :: rng
    end type

    interface get_distribution
        module procedure get_distribution_int32
        module procedure get_distribution_int64
        module procedure get_distribution_real32
        module procedure get_distribution_real64
    end interface

    integer, parameter :: sample_size = 1000000, distribution_size = 100
    real, parameter :: ideal_distribution(distribution_size) = 1./distribution_size
    real, parameter :: absolute_tolerance = 0.0
    real, parameter :: relative_tolerance = 0.03
contains
    function sanity_checks(name, rng) result(tests)
        character(len=*), intent(in) :: name
        type(rng_input_t) :: rng
        type(test_item_t) :: tests

        tests = describe( &
            "The " // name // " generator", &
            rng, &
            [ it_("doesn't produce the same number in a row", check_no_repeat) &
            , describe("should produce a uniform distribution of numbers", &
                rng, &
                [ it_("between -huge and huge for 32 bit integers", check_32bit_int_distribution) &
                , it_("between -huge and huge for 64 bit integers", check_64bit_int_distribution) &
                , it_("between -huge and huge for 32 bit reals", check_32bit_real_distribution) &
                , it_("between -huge and huge for 64 bit reals", check_64bit_real_distribution) &
                ]) &
            ])
    end function

    function check_no_repeat(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
        block
            integer(int32) :: fst, snd
            class(RNG_t), allocatable :: the_rng

            the_rng = input%rng
            call the_rng%next(fst)
            call the_rng%next(snd)

            result_ = assert_that(fst /= snd, "Got " // to_string(fst) // " and " // to_string(snd))
        end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    function check_32bit_int_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
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
                        absolute_tolerance = absolute_tolerance, &
                        relative_tolerance = relative_tolerance, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(relative_tolerance) // "%")
                else
                    result_ = fail(differences)
                end if
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    function check_64bit_int_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
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
                        absolute_tolerance = absolute_tolerance, &
                        relative_tolerance = relative_tolerance, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(relative_tolerance) // "%")
                else
                    result_ = fail(differences)
                end if
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    function check_32bit_real_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
                real(real32), allocatable :: samples(:)
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
                        absolute_tolerance = absolute_tolerance, &
                        relative_tolerance = relative_tolerance, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(relative_tolerance) // "%")
                else
                    result_ = fail(differences)
                end if
            end block
        class default
            result_ = fail("Didn't get an RNG")
        end select
    end function

    function check_64bit_real_distribution(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (rng_input_t)
            block
                real(real64), allocatable :: samples(:)
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
                        absolute_tolerance = absolute_tolerance, &
                        relative_tolerance = relative_tolerance, &
                        name = "distribution", &
                        max_reported = 10)
                if (differences == "") then
                    result_ = succeed("distribution within tolerance: " // to_string(relative_tolerance) // "%")
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

    pure function get_distribution_real32(samples, num_bins) result(distribution)
        real(real32), intent(in) :: samples(:)
        integer, intent(in) :: num_bins
        real :: distribution(num_bins)

        real :: bin_width
        integer :: samples_in_bin(num_bins)
        integer :: i, bin

        samples_in_bin = 0
        bin_width = 1.0 / num_bins
        do i = 1, size(samples)
            bin = floor(samples(i)/bin_width) + 1
            samples_in_bin(bin) = samples_in_bin(bin) + 1
        end do
        distribution = real(samples_in_bin, kind(distribution)) / size(samples)
    end function

    pure function get_distribution_real64(samples, num_bins) result(distribution)
        real(real64), intent(in) :: samples(:)
        integer, intent(in) :: num_bins
        real :: distribution(num_bins)

        real :: bin_width
        integer :: samples_in_bin(num_bins)
        integer :: i, bin

        samples_in_bin = 0
        bin_width = 1.0 / num_bins
        do i = 1, size(samples)
            bin = floor(samples(i)/bin_width) + 1
            samples_in_bin(bin) = samples_in_bin(bin) + 1
        end do
        distribution = real(samples_in_bin, kind(distribution)) / size(samples)
    end function
end module