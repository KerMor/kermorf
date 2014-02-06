program test
    use kermor

    implicit none

    type(expansion):: expa
    double precision:: x(3,40)
    double precision, allocatable :: fx(:,:)

    call random_seed
    call random_number(x)

    call expansion_load(expa, "examples/spine/")

    fx = expansion_evaluate(expa, x)

    print *, fx(:,:)

end program test
