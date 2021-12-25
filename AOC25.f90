    program AOC25

    implicit none

    integer, parameter :: xdim=139, ydim=137, xmax=xdim-1, ymax = ydim-1
    
    character, dimension(0:xmax,0:ymax) :: this
    integer :: x,y, step
    logical :: moved, wrap
    
    open (unit=1,file='input.txt', form='formatted', status='old')
    do y=0,ymax
        read (1,'(*(A1))') this(:,y)
    end do
    
    step = 0
    moved = .true.
    do while (moved)
        moved = .false.
        step = step + 1
        do y=0,ymax
            wrap = (this(xmax,y) == '>' .and. this(0,y) == '.')
            x = 0
            do while (x < xmax)
                if (this(x,y) == '>' .and. this(x+1,y) == '.') then
                    this(x,y) = '.'
                    this(x+1,y) = '>'
                    x = x + 1
                    moved = .true.
                end if
                x = x + 1
            end do
            if (wrap) then
                this(xmax,y) = '.'
                this(0,y) = '>'
                moved = .true.
            end if
        end do
            
        do x=0,xmax
            wrap = (this(x,ymax) == 'v' .and. this(x,0) == '.')
            y = 0
            do while (y < ymax)
                if (this(x,y) == 'v' .and. this(x,y+1) == '.') then
                    this(x,y) = '.'
                    this(x,y+1) = 'v'
                    y = y + 1
                    moved = .true.
                end if
                y = y + 1
            end do
            if (wrap) then
                this(x,ymax) = '.'
                this(x,0) = 'v'
                moved = .true.
            end if
        end do
    end do
    write (*,*) step

    end program AOC25

