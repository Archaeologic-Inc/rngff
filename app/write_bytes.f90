module write_bytes
    use iso_c_binding, only: c_int32_t, c_int64_t

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