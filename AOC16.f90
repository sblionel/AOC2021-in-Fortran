    program AOC16

    use sm2021
    implicit none
    
    type(packet_t) :: packet
    call get_bitstream('input.txt')
    do
        packet = get_packet()
        if (packet%type == ptype_end) exit
        end do
    print *, packet%value


    end program AOC16

