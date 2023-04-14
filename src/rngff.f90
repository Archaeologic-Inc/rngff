! Copyright (c) 2023 Archaeologic, Inc., Brad Richardson
! This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
! "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
module rngff
    use rngff_linear_congruential_m, only: linear_congruential_t
    use rngff_RNG_m, only: RNG_t
    use rngff_splitable_RNG_m, only: splitable_RNG_t
    use rngff_split_mix_m, only: split_mix_t
end module
