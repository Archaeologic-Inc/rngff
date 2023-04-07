module rngff
    use rngff_RNG_m, only: RNG_t
    use rngff_splitable_RNG_m, only: splitable_RNG_t
    use rngff_split_mix_m, only: split_mix_t

    implicit none
    private
    public :: RNG_t, splitable_RNG_t, split_mix_t
end module
