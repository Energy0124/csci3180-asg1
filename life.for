c     CSCI3180 Principles of Programming Languages
c
c     --- Declaration ---
c
c     I declare that the assignment here submitted is original except for source
c     material explicitly acknowledged. I also acknowledge that I am aware of
c     University policy and regulations on honesty in academic work, and of the
c     disciplinary guidelines and procedures applicable to breaches of such policy
c     and regulations, as contained in the website
c     http://www.cuhk.edu.hk/policy/academichonesty/
c
c     Assignment 1
c     Name : Ling Leong
c     Student ID : 1155062557
c     Email Addr : alanalan0124@yahoo.com.hk
      program life
c       force explicit type declarations
        implicit none
c       variable declaration
        character arg*80
        character name*80
        integer file, ios, gen, row, col
        call getarg(1, arg)
        write(*,*) arg
        file=1
        open(unit=file, iostat= ios, file=arg, status='old')
        if ( ios .neqv. 0 ) stop 'error opening file '
        read(file, *, iostat= ios) name
        if ( ios .neqv. 0 ) stop 'error reading name '
        read(file, *, iostat= ios) gen
        if ( ios .neqv. 0 ) stop 'error reading gen '
        read(file, *, iostat= ios) row, col
        if ( ios .neqv. 0 ) stop 'error reading row and col '
        write(*,*) name
        write(*,*) gen
        write(*,*) row, col
        stop 'quit normally'
      end
