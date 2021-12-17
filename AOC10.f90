    program AOC10

    implicit none
    
    type delim
        character(1) :: schar, echar
        integer(2) :: sval
    end type delim
    type(delim), parameter :: delims(4) = [ &
        delim('(',')',3), &
        delim('[',']',57), &
        delim('{','}',1197), &
        delim('<','>',25137)]
    integer :: score
    logical :: eof, res
    
    open (unit=1,file='input.txt', form='unformatted', access='stream')
    
    score = 0
    eof = .false.
    do while (.not. eof)
        res = parse()
 !       write (*,'(A1)') 'X'
    end do
    print *, score
    
    contains
    
    recursive function parse (curdel) result(res)
    use, intrinsic :: iso_fortran_env
    logical :: res
    type(delim), optional, intent(in) :: curdel
    type(delim) :: newdel
    character(1) :: c
    integer :: ios, i
    logical :: echar
    
    res = .true.
    do while (res)
        read (1,iostat=ios) c
        if (ios == iostat_end) then
            res = .false.
            eof = .true.
            return
        else if (c == char(13)) then
            ! end of line
       !     write (*,'(Z2.2)',advance='no') C
            read (1,iostat=ios) c
      !      write (*,'(Z2.2)',advance='no') C
            eof = (ios == iostat_end)
            res = .false.
            return
        end if
       ! write (*,'(A1)',advance='no') c
        do i=1,4
            if (c == delims(i)%schar) then
                newdel = delims(i)
                echar = .false.
                exit
            else if (c == delims(i)%echar) then
                newdel = delims(i)
                echar = .true.
                exit
            end if 
        end do
        if (i > 4) then
            print *, "Invalid character ", ichar(c)
            error stop
        end if
        if (.not. present(curdel)) then
            res = parse(newdel)
            if (.not. res) return
        else if (echar) then
            if (c == curdel%echar) then
                return
            else ! mismatched end char
                score = score + newdel%sval
                ios = 0
                do while ((ios >= 0) .and. (c >= char(13)))
                    read (1,iostat=ios) c
                end do
                eof = (ios == iostat_end)
                res = .false.
            end if
        else
            res = parse(newdel)
        end if
    end do
    
    return
    end function parse
    
    end program AOC10

