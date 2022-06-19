  module day20
    implicit none
    private
    public image_t

    integer, parameter :: L2 = selected_int_kind(1)

    type image_t
      logical(kind=L2), allocatable :: img(:,:)
      logical(kind=L2) :: bgr
      logical(kind=L2) :: filter(2**9)
    contains
      procedure :: read_file, print => image_print
      procedure :: enhance => image_enhance
    end type

  contains

    subroutine image_enhance(this)
      class(image_t), intent(inout) :: this

      logical(kind=L2), allocatable :: mir(:,:)
      integer :: nx0, ny0, nx, ny, i, j

      ! image to mirror
      nx0 = size(this % img, dim=1)
      ny0 = size(this % img, dim=2)
      nx = nx0 + 2
      ny = ny0 + 2
      allocate(mir(0:nx+1, 0:ny+1))
      mir = this % bgr
      mir(2:nx-1,2:ny-1) = this % img

      ! mirror to enhanced image
      deallocate(this % img)
      allocate(this % img(1:nx,1:ny))
      do i=1,nx
      do j=1,ny
        this % img(i,j) = read_ngb(mir(i-1:i+1,j-1:j+1), this%filter)
      end do
      end do

      ! enhance the surrounding
      if (this % bgr) then
        this % bgr = this % filter(size(this%filter))
      else
        this % bgr = this % filter(1)
      end if
    end subroutine


    pure function read_ngb(ngb, filter) result(res)
      logical(kind=L2), intent(in) :: ngb(:,:)
      logical(kind=L2), intent(in) :: filter(:)
      logical(kind=L2) :: res

      integer :: ngbrow(9), i, ind

      where (reshape(transpose(ngb),[9]))
        ngbrow = 1
      else where
        ngbrow = 0
      end where
      ind = 0
      do i=0,8
        ind = ind + ngbrow(9-i)*(2**i)
      end do
      if (ind < 0 .or. ind > size(filter)-1) &
        error stop 'read_ngb - invalid value'
      res = filter(ind+1)
    end function


    subroutine read_file(this, filename)
      class(image_t), intent(out) :: this
      character(len=*), intent(in) :: filename

      integer :: fid, rows, cols, ios, ilen, i, j
      character(len=5000) :: line
      character(len=size(this%filter)) :: filter

      ! count rows and cols
      open(newunit=fid, file=filename, status='old')
      read(fid,'(a)') line
      if (len(trim(line)) /= 2**9) &
        error stop 'read_file - first line invalid length'
      read(fid,*)
      rows = 0
      cols = -1
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        ilen = len(trim(line))
        if (cols == -1) cols = ilen
        if (cols /= ilen) &
          error stop 'read_file - lines are not same length'
        rows = rows + 1
      end do
      print '("Img size: ",i0," x ",i0)', rows, cols
      rewind(fid)

      ! read filter
      read(fid,'(a)') filter
      do i=1,size(this%filter)
        this%filter(i) = convert(filter(i:i))
      end do
      read(fid,*)

      ! read image
      allocate(this % img(rows, cols))
      do i=1,rows
        read(fid,'(a)') line
        do j=1,cols
          this % img(i,j) = convert(line(j:j))
        end do
      end do
      this % bgr = .false.
      close(fid)

    contains
      logical function convert(ch)
        character(len=1), intent(in) :: ch
        select case (ch)
        case('#')
          convert = .true.
        case(".")
          convert = .false.
        case default
          error stop 'convert - invalid char'
        end select
      end function
    end subroutine


    subroutine image_print(this)
      class(image_t), intent(in) :: this

      integer :: i
      do i=1,size(this % img, dim=1)
        write(*,'(a)') back_convert(this % img(i,:))
      end do
      print *,'BGR = ', this % bgr
      print *
    contains
      function back_convert(larr) result(line)
        logical(kind=L2), intent(in) :: larr(:)
        character(len=size(larr)) :: line
        integer :: i
        do i=1,size(larr)
          if (larr(i)) then
            line(i:i) = '#'
          else
            line(i:i) = '.'
          end if
        end do
      end function
    end subroutine

  end module day20
