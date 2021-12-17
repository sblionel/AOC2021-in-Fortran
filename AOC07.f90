    program AOC07
    use ifport
    implicit none

    integer :: vals(2000), nvals, fuel, fuel1, fuel2, i, pos1, pos2, goal, median, dist
    real :: mean
    
    open (unit=1,file='input.txt', form='formatted', status='old')
    vals = -1
    read (1,*,iostat=i) vals
    nvals = findloc(vals,-1,1) - 1
    call qsort(vals, nvals, 4, compar)
    !print *, vals(1:nvals)
    mean = real(sum(vals(1:nvals)))/real(nvals)
    pos1 = floor(mean)
    pos2 = ceiling(mean)
    print *, pos1, pos2
    print *
    !print *, vals(pos1), vals(pos2)
    
    fuel1 = 0
    do i=1,nvals
        dist = abs(vals(i) - pos1)
        if (dist < 2) then
            fuel = dist
        else if (mod(dist,2) == 0) then
            fuel = (dist+1)*(dist/2)
        else
            fuel = (dist * (1+(dist/2)))
        end if
        fuel1 = fuel1 + fuel
       ! print *, vals(i), dist, fuel
    end do
    print *
     fuel2 = 0
    do i=1,nvals
        dist = abs(vals(i) - pos2)
        if (dist < 2) then
            fuel = dist
        else if (mod(dist,2) == 0) then
            fuel = (dist+1)*(dist/2)
        else
            fuel = (dist * (1+(dist/2)))
        end if
        fuel2 = fuel2 + fuel
        !print *, vals(i), dist, fuel
    end do
    print *, fuel1, fuel2
    
    
    
    contains
    
    function compar (arg1, arg2)
    integer(2) :: compar
    integer(4) :: arg1,arg2
    if (arg1 < arg2) then
        compar = -1
    else if (arg1 > arg2) then
        compar = 1
    else
        compar = 0
    end if
    end function compar

    end program AOC07

