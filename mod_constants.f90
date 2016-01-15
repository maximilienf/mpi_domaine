module mod_constants
  
  implicit none
  integer      , parameter, public :: xp = selected_real_kind(8)
  real(kind=xp), parameter, public :: pi = 4.*datan(1.d0)
  real(kind=xp), parameter, public :: c = 2.99792458e8_xp

  integer, parameter               :: nb_particules = 7680
  integer, parameter               :: n_iteration = 500
  real(kind=xp), parameter         :: dt = 1.e-2_xp
  real(kind=xp), parameter         :: w = 1._xp, epsilon = 1.e-1_xp
  integer,parameter                :: s_count = 1000 
end module mod_constants
