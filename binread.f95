module binread
    use endian
    implicit none

    private
    integer, parameter:: FILE_UNIT = 88

    public loadVector
    public loadMatrix

contains

    function loadVector(filename) result(v)
        character*(*), intent(in)::filename
        double precision, dimension(:),allocatable::v
        integer:: n, i, pos
        logical:: islittleendian

        islittleendian = islittleendianmachine()
        open(FILE_UNIT, file=filename, form='unformatted', status='old', action='read', access='direct', recl=1)
        ! Read the vector size
        n = readInteger(1, islittleendian)
        ! Allocate space for the vector
        allocate (v(n))
        pos = 5
        ! Read n double values
        do i=1,n
            v(i) = readDouble(pos, islittleendian)
            pos = pos + 8
        end do
        close(FILE_UNIT)
    end function loadVector

    function loadMatrix(filename) result(res)
        character*(*), intent(in)::filename
        double precision, dimension(:,:),allocatable::res
        integer:: n,m,i,j,pos
        logical:: islittleendian

        islittleendian = islittleendianmachine()

        open(FILE_UNIT, file=filename, form='unformatted', status='old', action='read', access='direct', recl=1)
        ! Read the matrix size
        n = readInteger(1, islittleendian)
        m = readInteger(5, islittleendian)
        print *,"n=",n,",m=",m
        ! Allocate space for the vector
        allocate (res(n,m))
        pos = 9
        ! Read n double values
        do i=1,n
            do j=1,n
                res(i,j) = readDouble(pos, islittleendian)
                pos = pos + 8
            end do
        end do
        close(FILE_UNIT)
    end function loadMatrix

    integer function readInteger(startpos, swap)
        integer, intent(in):: startpos
        logical, intent(in):: swap
        integer:: pos, i
        integer*1, dimension(4):: bytes

        do i = 0, 3
            pos = i+startpos-1
            if (swap) then
                pos = startpos+3-i
            end if
            read(FILE_UNIT, rec=pos) bytes(i+1)
        end do
        readInteger = transfer(bytes, readInteger)

    end function readInteger

    double precision function readDouble(startpos, swap)
        integer, intent(in):: startpos
        logical, intent(in):: swap
        integer:: pos, i
        integer*1, dimension(8):: bytes

        do i = 0, 7
            pos = i+startpos-1
            if (swap) then
                pos = startpos+7-i
            end if
            read(FILE_UNIT, rec=pos) bytes(i+1)
        end do
        readDouble = transfer(bytes, readDouble)

    end function readDouble

end module binread
