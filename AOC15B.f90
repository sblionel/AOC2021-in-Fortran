    program AOC15B

    implicit none

    integer, parameter :: inpsize = 100
    integer, parameter :: xysize = 5*inpsize
    character(*), parameter :: file='input.txt'
    
    logical :: notinset(xysize,xysize)
    integer :: risks(xysize/5,xysize/5), costs(xysize,xysize)
    integer :: i,j,curx,cury,candidate(2)
    integer, parameter :: adjx(4) = [-1,1,0,0], adjy(4)=[0,0,1,-1]
    
    open (unit=1, file=file, form='formatted', status='old')
    do i=1,xysize/5
        read (1,'(*(I1))') (risks(i,j),j=1,inpsize)
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
        !print *, candidate
        if ((curx == xysize) .and. (cury == xysize)) exit
        notinset(cury,curx) = .false.
        do i=1,4
            if ((curx+adjx(i) < 1) .or. (curx+adjx(i) > xysize)) cycle
            if ((cury+adjy(i) < 1) .or. (cury+adjy(i) > xysize)) cycle
            if (notinset(cury+adjy(i),curx+adjx(i))) then
                block
                    integer :: newcost, newrisk
                    newrisk = getrisk(cury+adjy(i),curx+adjx(i))
                    newcost = costs(cury,curx) + newrisk
                    if (newcost < costs(cury+adjy(i),curx+adjx(i))) &
                        costs(cury+adjy(i),curx+adjx(i)) = costs(cury,curx) + newrisk
                end block
            end if
        end do
    end do
    
    print *, costs(xysize,xysize) 
    
    contains
    
    function getrisk (x,y)
    integer :: getrisk
    integer, intent(in) :: x,y
    integer :: origx,origy
    origx = mod(x-1,inpsize)+1
    origy = mod(y-1,inpsize)+1
    getrisk = risks(origy,origx) + ((x-1)/inpsize) + ((y-1)/inpsize)
    if (getrisk > 9) getrisk = getrisk - 9
    end function getrisk

    end program AOC15B

