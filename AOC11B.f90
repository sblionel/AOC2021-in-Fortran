    program AOC11B

    implicit none

    integer :: grid(10,10)
    integer :: step
    integer :: xloc(10), x, y, i, j, flashes
    logical :: chain, mask(10,10)
    
    open (unit=1, file='input.txt', form='formatted', status='old')
    read (1,'(10I1)') grid
    flashes = 0
    
    do step=0,2000
        if (all(grid == 0)) then
            print *, "All flashed at step ", step
            stop
        end if
        grid = grid + 1
        mask = .true.
        do
            xloc = findloc (grid,10,dim=1,mask=mask)
            if (all((xloc==0),dim=1)) exit
            do y=1,10
                x = xloc(y)
                if (x == 0) cycle
                mask(x,y) = .false.
                grid(x,y) = 0
                flashes = flashes + 1
                do j = y-1,y+1
                    if (j < 1 .or. j > 10) cycle
                    do i = x-1,x+1
                        if (i < 1 .or. i > 10) cycle
                        if ((j == y) .and. (i == x)) cycle
                        if (.not. mask(i,j)) cycle
                        if (grid(i,j) < 10) grid(i,j) = grid(i,j) + 1
                    end do
                end do
            end do
        end do
    end do
    
    end program AOC11B

