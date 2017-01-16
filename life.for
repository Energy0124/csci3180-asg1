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
c
c     Language:
c     fortran 77
      program life
c       force explicit type declarations
        implicit none
c       variable declaration
        character arg*80, name*80
        integer ir, jr, ic, jc, rowc, colc
        integer file, ios, gen, row, col
        character pat1(1:100)*80, pat2(1:100)*80
        call getarg(1, arg)
        write(*,*) arg
        file=1
        open(unit=file, iostat= ios, file=arg, status='old')
        if ( ios .neqv. 0 ) stop 'error opening file '
        read(file, "(A)", iostat= ios) name
        if ( ios .neqv. 0 ) stop 'error reading name '
        read(file, *, iostat= ios) gen
        if ( ios .neqv. 0 ) stop 'error reading gen '
        read(file, *, iostat= ios) row, col
        if ( ios .neqv. 0 ) stop 'error reading row and col '
        write(*,"(A)") name
        write(*,*) gen
        write(*,*) row, col
c       read all line of pattern
        ir=1
        jr=1
        ic=1
        jc=1
        rowc=1
        colc=1
        write(*, *) " reading start", ir
 210    if ( ir.gt.row ) goto 200
        write(*, *) "start reading line ", ir
        read(file, "(A)") pat1(ir)
        if ( ios .neqv. 0 ) stop "error reading pattern"
        write(*, *) "end reading line ", ir
        ir=ir+1
        goto 210
 200    ir = 1
 201    if ( ir.gt.row ) goto 220
        write(*,"(A)") pat1(ir)
        pat2(ir)=pat1(ir)
        ir=ir+1
        goto 201
 220    write(*, *) "end reading file"
        stop 'quit normally'
      end
