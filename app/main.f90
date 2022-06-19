program main
  use day20, only: image_t
  implicit none
  type(image_t) :: img
  integer, parameter :: NREP = 50
  integer :: i, ans1, ans2

! call img % read_file('sample.txt')
  call img % read_file('input.txt')
  call img % print()
  
  ! Part One
  call img % enhance()
  call img % print()
  call img % enhance()
  call img % print()
  ans1 = count(img % img)

  ! Part Two
  do i=1,NREP-2
    call img % enhance()
  enddo
  call img % print()
  ans2 = count(img % img)

  print '("Answer 1 is ",i0,1x,l1)', ans1, ans1==35 .or. ans1==5563
  print '("Answer 2 is ",i0,1x,l1)', ans2, ans2==3351 .or. ans2==19743
end program main
