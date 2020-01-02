module serialport_kinds

    use, intrinsic :: iso_fortran_env, only : sp => real32
    use, intrinsic :: iso_fortran_env, only : dp => real64
    use, intrinsic :: iso_fortran_env, only : qp => real128
    use, intrinsic :: iso_fortran_env, only : i4 => int32
    use, intrinsic :: iso_fortran_env, only : i8 => int64

    implicit none
    private

    public rp, ip, dp , sp, i4, i8

    integer, parameter :: rp = dp
    integer, parameter :: ip = i8

end module serialport_kinds
