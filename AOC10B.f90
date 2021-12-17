    program AOC10B
    use ifport
    implicit none
    
    type delim
        character(1) :: schar, echar
        integer(2) :: sval
    end type delim
    type(delim), parameter :: delims(4) = [ &
        delim('(',')',1), &
        delim('[',']',2), &
        delim('{','}',3), &
        delim('<','>',4)]
    integer :: nscores, res
    logical :: eof
    integer(8) :: score, scores(102)
    
    open (unit=1,file='input.txt', form='unformatted', access='stream')
    
    eof = .false.
    nscores = 0
    do while (.not. eof)
        score = 0
        res = parse()
       ! write (*,'(A1)') 'X'
        print *, score
        if (score > 0) then
            nscores = nscores + 1
            scores(nscores) = score       
            end if
    end do
    call qsort(scores,nscores,8,compar)
    print *,scores((nscores/2)+1)
    
    contains
    
    recursive function parse (curdel) result(res)
    use, intrinsic :: iso_fortran_env
    integer :: res
    type(delim), optional, intent(in) :: curdel
    type(delim) :: newdel
    character(1) :: c
    integer :: ios, i
    logical :: echar
    
    res = 0
    mainloop: do while (res == 0)
        read (1,iostat=ios) c
        if (ios == iostat_end) then
            res = 1
            eof = .true.
            exit mainloop
        else if (c == char(13)) then
            ! end of line
            !write (*,'(Z2.2)',advance='no') C
            read (1,iostat=ios) c
            !write (*,'(Z2.2)',advance='no') C
            eof = (ios == iostat_end)
            res = 1
            exit mainloop
        end if
        !write (*,'(A1)',advance='no') c
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
                ios = 0
                do while ((ios >= 0) .and. (c >= char(13)))
                    read (1,iostat=ios) c
                end do
                eof = (ios == iostat_end)
                res = -1
            end if
        else
            res = parse(newdel)
        end if
    end do mainloop
    if ((res == 1) .and. present(curdel)) then
        score = (score*5) + curdel%sval
    end if
    
    return
    end function parse
    function compar (arg1,arg2)
    integer(2) :: compar
    integer(8) :: arg1, arg2
    if (arg1 < arg2) then
        compar = -1
    else if (arg1 > arg2) then
        compar = 1
    else
        compar = 0
    end if
    end function compar
    
    end program AOC10B

