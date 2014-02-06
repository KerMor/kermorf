module kermor

    use binread

    implicit none

    integer, parameter :: KERNEL_GAUSSIAN  = 1, KERNEL_WENDLAND = 2

    type wendland_data
        integer :: d = 1, k = 1
    end type wendland_data

    type kernel
        integer :: type = 1
        double precision :: gamma = 1
        type(wendland_data) :: wdata
    end type kernel

    type expansion
        double precision, allocatable :: centers(:,:), coefficients(:,:)
        type(kernel) :: kernel
    end type expansion

contains

    function expansion_evaluate(this, x) result(fx)
        type(expansion), intent(in):: this
        double precision, intent(in):: x(:,:)
        double precision, dimension(size(this%coefficients,1),size(x,2)):: fx

        fx = matmul(this%coefficients, kernel_evaluate(this%kernel, this%centers, x))
    end

    function kernel_evaluate(this, x, y) result(K)
        type(kernel), intent(in):: this
        double precision:: x(:,:),y(:,:)
        double precision, dimension(size(x,2),size(y,2)):: K,r,xsq,ysq,p,const
        integer :: xones(size(x,2),1), yones(1,size(y,2)), l
        double precision:: xhlp(size(x,2),1), yhlp(1,size(y,2))

        xones = 1
        yones = 1
        xhlp(:,1) = sum(x*x,1)
        yhlp(1,:) = sum(y*y,1)
        xsq = matmul(xhlp,yones)
        ysq = matmul(xones,yhlp)

        ! Compute matrix of squared differences
        K = (xsq + ysq - 2*(transpose(x)*y))/(this%gamma*this%gamma)

        select case (this%type)
            case (KERNEL_GAUSSIAN)
                K = exp(-K)
            case (KERNEL_WENDLAND)
                const = 1
                r = sqrt(K)
                l = floor(this%wdata%d / 2.0) + this%wdata%k + 1;
                select case (this%wdata%k)
                    case (1)
                        p = (l + 1) * r + const;
                    case (2)
                        p = (l * l + 4 * l + 3) * K / 3 + (l + 2) * r + const;
                    case (3)
                        p = ((l * l * l + 9 * l * l + 23 * l + 15) * K*r &
                            + (6 * l * l + 36 * l + 45) * K) / 15 + (l + 3) * r + const;
                    case default

                end select
                const = 0
                K=(max(1-r, const)**(l+this%wdata%k))*p;
            case default

        end select

    end function kernel_evaluate

    subroutine expansion_load(this, directory)
        type(expansion):: this
        character*(*), intent(in):: directory

        this%centers = loadMatrix(directory//"centers.bin")
        this%coefficients = loadMatrix(directory//"coeffs.bin")
        call kernel_load(this%kernel, directory)

    end subroutine expansion_load

    subroutine kernel_load(this, dir)
        type(kernel):: this
        character*(*), intent(in):: dir
        double precision, dimension(:), allocatable:: kvals
        kvals = loadVector(dir//"kernel.bin")
        this%type = kvals(1)
        this%gamma = kvals(2)
        if (kvals(1) == 2) then
            this%wdata%d = kvals(3)
            this%wdata%k = kvals(4)
        end if
    end subroutine kernel_load

end module kermor
