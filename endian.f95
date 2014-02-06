module endian
    implicit none

    interface byteswap
        module procedure byteswapInt
        module procedure byteswapdouble
    end interface byteswap

contains

    ! checks if this is a little endian machine
    logical function islittleendianmachine()
        integer*1:: j(2)
        integer*2:: i
        equivalence (i,j)

        i = 1
        if (j(1).eq.1) then
            islittleendianmachine = .true.
        else
            islittleendianmachine = .false.
        end if

    end function islittleendianmachine

  subroutine byteswapInt(val)
        integer, intent(inout) ::val
        integer*1, dimension(4):: bytes, bytes2
        integer:: i

        bytes = transfer(val, bytes)
        bytes2 = bytes
        do i = 1, 4
            bytes(i) = bytes2(5-i)
        end do

        val = transfer(bytes, val)
    end subroutine byteswapInt

    subroutine byteswapDouble(val)
        double precision, intent(inout) ::val
        integer*1, dimension(8):: bytes, bytes2
        integer:: i

        bytes = transfer(val, bytes)
        bytes2 = bytes
        do i = 1, 8
            bytes(i) = bytes2(9-i)
        end do

        val = transfer(bytes, val)
    end subroutine byteswapDouble

end module endian
