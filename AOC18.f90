    program AOC18

    implicit none
    
    integer, parameter :: I8 = selected_int_kind(15)
    character(*), parameter :: inpfile = 'input.txt'

    type sn_t
        type(sn_t), pointer :: left => null()
        type(sn_t), pointer :: right => null()
        type(sn_t), pointer :: up => null()
        integer(I8) :: val = 0, depth = 0, curnum = 0
    end type sn_t
    type(sn_t), pointer :: curval, newval
    type nums_t
        type(sn_t), pointer :: num
    end type nums_t
    type(nums_t) :: nums(500)
    integer :: curnum, lastnum
    character(100) :: line
    integer :: curpos, ios   
    logical :: action_taken
    
    open (unit=1,file=inpfile,form='formatted',status='old')

    curval => null()
    do
        read (1,'(A)',iostat=ios) line
        if (ios /= 0) exit
        curpos = 0
        newval => read_nums()
        if (.not. associated(curval)) then
            curval => newval
        else
            curval => add (curval,newval)
        end if
        action_taken = .true.
        do while (action_taken)
            action_taken = .false.
            lastnum = 0
            call traverse(curval,1)
            call explode(curval)
            if (action_taken) cycle
            call split(curval)
        end do
    end do
                
    print *, magnitude(curval)
    
    contains
    
    recursive function read_nums() result(res)
    type(sn_t), pointer :: res
    
    allocate (res)
    curpos = curpos + 1
    select case (line(curpos:curpos))
    case ('[')
        res%left => read_nums()
        curpos = curpos + 1
        call errcheck(',')
        res%right => read_nums()
        curpos = curpos + 1
        call errcheck(']')
        res%left%up => res
        res%right%up => res
    case ('0':'9')
        res%val = ichar(line(curpos:curpos)) - ichar('0')
        !call errcheck(']')
    case default
        call errcheck(' ')
    end select
    return
    end function read_nums
    
    subroutine errcheck (expected)
    character, intent(in) :: expected
    if (expected == ' ') then
        print *, "Unexpected character '",line(curpos:curpos),"' at position ", curpos
    else if (line(curpos:curpos) /= expected) then
        print *, "Expected '",expected,"' but found '",line(curpos:curpos),"' at position ", curpos
    else
        return
    end if
    error stop
    end subroutine errcheck
    
    function add (left,right) result (res)
    type(sn_t), pointer :: res
    type(sn_t), pointer, intent(in) :: left, right
    
    allocate (res)
    res%left => left
    res%right => right
    end function add
    
    recursive subroutine traverse (val,depth)
    type(sn_t), pointer, intent(inout) :: val
    integer, intent(in) :: depth
    val%depth = depth
    if (.not. associated(val%left)) then
        lastnum = lastnum + 1
        nums(lastnum)%num => val
        val%curnum = lastnum
    else
        call traverse(val%left,depth+1)
        call traverse(val%right,depth+1)
    end if
    end subroutine traverse
    
    recursive subroutine explode (pair)
    type(sn_t), pointer, intent(in) :: pair
    type(sn_t), pointer :: left,right
    integer :: pos
    
    left => pair%left
    right => pair%right
    if (.not. associated(left)) return
    if ((pair%depth > 4) .and. (.not. associated(left%left)) .and. (.not. associated(right%right))) then
        ! Explode this pair
        pos = left%curnum - 1
        if (pos > 0) then
            nums(pos)%num%val = nums(pos)%num%val + left%val
        end if
        pos = right%curnum + 1
        if (pos <= lastnum) then
            nums(pos)%num%val = nums(pos)%num%val + right%val
        end if
        deallocate (pair%left)
        deallocate (pair%right)
        pair%val = 0
        action_taken = .true.
    else
        if (.not. action_taken) call explode(left)
        if (.not. action_taken) call explode(right)
    end if
    end subroutine explode
    
    subroutine split (cv)
    type(sn_t), pointer, intent(inout) :: cv
    type(sn_t), pointer :: sn
    integer :: i
    
    lastnum = 0
    call traverse(cv,1)
    do i=1,lastnum
        sn => nums(i)%num
        if (sn%val >= 10) then
            allocate (sn%left)
            allocate (sn%right)
            sn%left%val = sn%val/2
            sn%right%val = ceiling(real(sn%val)/2.0)
            sn%val = 0
            action_taken = .true.
            return
        end if
    end do
    end subroutine split
    
    recursive function magnitude (pair)
    integer :: magnitude
    type (sn_t), pointer, intent(in) :: pair
    
    if (.not. associated(pair%left)) then
        magnitude = pair%val
    else
        magnitude = (3*magnitude(pair%left)) + (2*magnitude(pair%right))
    end if
    end function magnitude
    
    recursive subroutine printval (val)
    type(sn_t), intent(in) :: val
    if (.not. associated(val%left)) then
        write (*,'(I0)',advance='no') val%val
    else
        write (*,'(A)',advance='no') '['
        call printval(val%left)
        write (*,'(A)',advance='no') ','
        call printval(val%right)
        write (*,'(A)',advance='no') ']'
    end if
    end subroutine printval
    
    

    end program AOC18

