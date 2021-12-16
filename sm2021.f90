module sm2021
    implicit none
    
    character(:), allocatable :: bitstream
    integer :: bspos, bslen
    
    type packet_t
        integer :: version
        integer :: type
        integer(8) :: value
    end type packet_t
    enum, bind(C)
        enumerator :: ptype_end = -1
        enumerator :: ptype_sum, ptype_product, ptype_min, ptype_max
        enumerator :: ptype_literal, ptype_gt, ptype_lt, ptype_eq
    end enum
    integer(8), parameter :: initvals(0:7) = [0,1,huge(0),-1,0,-1,-1,-1]
    
    contains
    
    subroutine get_bitstream (input)
    character(*), intent(in) :: input
    
    character(1500) :: line
    character(:), allocatable :: bstemp
    integer :: linelen, i,b
    open (unit=1,file=input,form='formatted',status='old')
    read (1,'(A)') line
    close (1)
    
    linelen = len_trim(line)
    bslen = linelen*4
    allocate (character(bslen)::bitstream)
    
    bspos = 1
    do i=1,linelen
        read (line(i:i),'(Z1)') b
        write (bitstream(bspos:bspos+3),'(B4.4)') b
        bspos = bspos + 4
    end do
    bspos = 1
    end subroutine get_bitstream
    
    function get_packet ()
    type(packet_t) :: get_packet
    integer :: bits
    
    if (bspos+6 > bslen) then
        get_packet%type = ptype_end
        return
    end if
     if (index(bitstream(bspos:),'1') == 0) then
        get_packet%type = ptype_end
        return
    end if   
    
    read (bitstream(bspos:),'(2B3)') get_packet%version, get_packet%type
    
    bspos = bspos + 6
    
    select case (get_packet%type)
    case (ptype_literal)
        block
            integer(8) :: pval, i,f
            pval = 0
            do
                if (bspos+4 > bslen) then
                    print *, "Past EOF for literal value at pos ", bspos
                    error stop
                end if
                read (bitstream(bspos:),'(B1,B4)') f,i
                pval = ior(shiftl(pval,4),i)
                bspos = bspos + 5
                if (f == 0) exit
            end do
            get_packet%value = pval
        end block
        
    case default ! operator
        block
            integer :: ltype,val,valbits,sublen,nsub,oldpos,i
            integer(8) :: initval
            type(packet_t) :: subpacket
            read (bitstream(bspos:),'(B1)') ltype
            val = 0; sublen=0; nsub = 0
            initval = initvals(get_packet%type)
            select case (ltype)
            case (0)
                if (bspos+15 > bslen) error stop 'sublen err'
                read (bitstream(bspos:),'(1X,B15)') sublen
                bspos = bspos + 16            
                do while (sublen > 0)
                    oldpos = bspos
                    subpacket = get_packet()
                    call do_op(get_packet%type,subpacket,initval)
                    sublen = sublen - (bspos - oldpos)
                    end do
            case (1)
                if (bspos+11 > bslen) error stop 'nsub error'
                read (bitstream(bspos:),'(1X,B11)') nsub
                bspos = bspos + 12
                do i=1,nsub
                    subpacket = get_packet()
                    call do_op(get_packet%type,subpacket,initval)
                end do
                
            end select
            get_packet%value = initval
        end block
    end select

    end function get_packet
    
    subroutine do_op (optype, subpacket, val)
    integer, intent(in) :: optype
    type(packet_t), intent(in) :: subpacket
    integer(8), intent(inout) :: val
    
    select case (optype)
    case(ptype_sum)
        val = val + subpacket%value
    case(ptype_product)
        val = val * subpacket%value
    case(ptype_min)
        val = min(val,subpacket%value)
    case(ptype_max)
        val = max(val,subpacket%value)
    case(ptype_gt)
        if (val == -1) then
            val = subpacket%value
        else if (val > subpacket%value) then
            val = 1
        else
            val = 0
        end if
     case(ptype_lt)
        if (val == -1) then
            val = subpacket%value
        else if (val < subpacket%value) then
            val = 1
        else
            val = 0
        end if
     case(ptype_eq)
        if (val == -1) then
            val = subpacket%value
        else if (val == subpacket%value) then
            val = 1
        else
            val = 0
        end if
     case default
        error stop "Invalid op"
     end select
    end subroutine do_op
    
end module sm2021
    