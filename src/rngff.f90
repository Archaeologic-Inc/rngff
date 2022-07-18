module rngff
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, rngff!"
  end subroutine say_hello
end module rngff
