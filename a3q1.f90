program Newtons_Forward_Method
    implicit none
    integer::i,j,n
    real::x(10),y(10,10),u,h,x_target,term,result_y

    open(unit=21,file="in_a3q1.txt")
    open(unit=22,file="out_a3q1.txt")

    read(21,*)n
    do i=1,n
        read(21,*) x(i),y(i,1)
    end do
    read(21,*)x_target

    do j=2,n
        do i=1,n-j+1
            y(i,j)=y(i+1,j-1)-y(i,j-1)
        end do
    end do

    h=x(2)-x(1)
    u=(x_target-x(1))/h
    result_y=y(1,1)
    term=1.0

    do i=1,n-1
        term=term*(u-(i-1))/real(i)
        result_y=result_y+term*y(1,i+1)
    end do
        write(22,'(A)') '--------Forward Difference Table---------'

    do i=1,n
            write(22,'(F8.1,10F12.4)') x(i),(y(i,j),j=1,n-i+1)
        end do

        write(22,'(A,F10.4)')'Estimated GDP (biliions USD):',result_y
        close (21)
        close(22)
end program Newtons_Forward_Method
