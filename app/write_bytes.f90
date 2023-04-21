! Copyright (c) 2022 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module write_bytes
    use, intrinsic :: iso_c_binding, only: c_int32_t, c_int64_t

    implicit none
    private
    public :: write_32bits, write_64bits

    interface
        subroutine write_32bits(bits) bind(C)
            import c_int32_t
            implicit none
            integer(c_int32_t), intent(in) :: bits
        end subroutine

        subroutine write_64bits(bits) bind(C)
            import c_int64_t
            implicit none
            integer(c_int64_t), intent(in) :: bits
        end subroutine
    end interface
end module