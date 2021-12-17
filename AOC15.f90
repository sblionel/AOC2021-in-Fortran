    program AOC15

    implicit none

    integer, parameter :: xysize = 100
    character(*), parameter :: file='input.txt'
    
    logical :: notinset(xysize,xysize)
    integer :: risks(xysize,xysize), costs(xysize,xysize)
    integer :: i,j,curx,cury,candidate(2),newcost
    integer, parameter :: adjx(4) = [-1,1,0,0], adjy(4)=[0,0,1,-1]
    
    open (unit=1, file=file, form='formatted', status='old')
    do i=1,xysize
        read (1,'(*(I1))') (risks(i,j),j=1,xysize)
    end do
    costs = huge(0)
    costs(1,1) = 0
    notinset = .true.
    
    do
       ! write (*,'(10(10I3/)/)') (costs(i,:),i=1,10)
        candidate = minloc(costs,mask=notinset)
       !write (*,*) candidate
        cury = candidate(1)
        curx = candidate(2)
        if ((curx == xysize) .and. (cury == xysize)) exit
        notinset(cury,curx) = .false.
        do i=1,4
            if ((curx+adjx(i) < 1) .or. (curx+adjx(i) > xysize)) cycle
            if ((cury+adjy(i) < 1) .or. (cury+adjy(i) > xysize)) cycle
            if (notinset(cury+adjy(i),curx+adjx(i))) then
                newcost = costs(cury,curx) + risks(cury+adjy(i),curx+adjx(i))
                if (newcost < costs(cury+adjy(i),curx+adjx(i))) &
                    costs(cury+adjy(i),curx+adjx(i)) = costs(cury,curx) + risks(cury+adjy(i),curx+adjx(i))
            end if
        end do
    end do
    
    print *, costs(xysize,xysize) 
    

    end program AOC15

