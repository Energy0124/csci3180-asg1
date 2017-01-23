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
c
      subroutine printp(pat1,row,col)
        character pat1(1:100)*80
        integer row, col
        integer ir
        ir=1
 201    if ( ir.gt.row ) goto 220
          write(*,"(A)") pat1(ir)
          ir=ir+1
        goto 201
c 220    write(*, *) "end printing pattern"
 220    return
      end
c     function
c     check if two patter are the same
      logical function issame(s1,s2,row,col)
        character s1(1:100)*80, s2(1:100)*80
        integer row, col
        integer ir
        ir=1
        issame=.true.
 300    if ( ir.gt.row ) goto 310
c          write(*, *) "start comparing line ", ir
          if(s1(ir).ne.s2(ir)) issame=.false.
c          write(*, *) "end comparing line ", ir
          ir=ir+1
        goto 300
c 310    write(*, *) "done comparing lines"
 310    return
      end
      subroutine copypa(pat1,pat2,row,col)
        character pat1(1:100)*80, pat2(1:100)*80
        integer row, col
        integer ir
        ir=1
c        write(*, *) "start copying pattern"
 501    if ( ir.gt.row ) goto 520
          pat2(ir)=pat1(ir)
          ir=ir+1
        goto 501
c 520    write(*, *) "end copying pattern"
 520    return
      end
      integer function countc(pat1,pat2,row,col,crow,ccol)
        character pat1(1:100)*80, pat2(1:100)*80
        integer row, col,crow,ccol
        integer ir, ic
        ir=-1
        ic=-1
        countc=0
 600    if ( ir.gt.1 ) goto 610
c          write(*, *) "start counting line ", ir
 601      if ( ic.gt.1 ) goto 611
            if(crow+ir.lt.1 .or. crow+ir.gt.row)goto 620
            if(ccol+ic.lt.1 .or. ccol+ic.gt.col)goto 620
            if(ir.eq.0.and.ic.eq.0)goto 620
            if(pat2(crow+ir)(ccol+ic:ccol+ic).eq.'*') countc=countc+1
 620        ic=ic+1
          goto 601
c 611      write(*, *) "end counting line ", ir
  611     ic=-1
          ir=ir+1
        goto 600
c 610    write(*, *) "done counting row ", crow, " col ", ccol,
c     +    " ,count: ", countc
 610    return
      end
c     subroutine
c     simulate pattern
      subroutine sim(pat1,pat2,row,col)
        character pat1(1:100)*80, pat2(1:100)*80
        integer row, col , countc, celsum
        integer ir, ic
c       copy the pattern to first
        call copypa(pat1,pat2,row,col)
        ir=1
        ic=1
 400    if ( ir.gt.row ) goto 410
c          write(*, *) "start simulating line ", ir
 401      if ( ic.gt.col ) goto 411
            celsum=0
            celsum=countc(pat1,pat2,row,col,ir,ic)
c            write(*,*) "cell: ", ir, ", ", ic  ,"=",celsum
            if(celsum.eq.3) pat1(ir)(ic:ic)='*'
            if(celsum.eq.3) goto 420
            if(celsum.eq.2) goto 420
            pat1(ir)(ic:ic)='0'
c 420        write(*,*) "end sim for cell ", ir, ", ", ic
 420        ic=ic+1
          goto 401
c 411      write(*, *) "end simulating line ", ir
c 411      write(*,"(A)") pat1(ir)
 411      ic=1
          ir=ir+1
        goto 400
c 410    write(*, *) "done simulating"
 410    return
      end
      program life
c       force explicit type declarations
        implicit none
c       variable declaration
        character arg*80, name*80
        integer ir, jr, ic, jc, rowc, colc
        integer file, ios, gen, row, col, dgen, cgen, fgen
        character pat1(1:100)*80, pat2(1:100)*80
        logical eqsame,issame, stilll
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
        cgen=1
        fgen=gen+1
        write(*, *) " reading start", ir
 210    if ( ir.gt.row ) goto 200
c          write(*, *) "start reading line ", ir
          read(file, "(A)") pat1(ir)
          if ( ios .neqv. 0 ) stop "error reading pattern"
c          write(*, *) "end reading line ", ir
          ir=ir+1
        goto 210
 200    ir = 1
        call printp(pat1,row,col)
        write(*,*) "init pattern"
c       copy pattern
c       start simulating
c       compare 2 pattern
c        call copypa(pat1,pat2,row,col)
 710    if(cgen.gt.fgen) goto 700
          call sim(pat1,pat2,row,col)
          call printp(pat1,row,col)
          eqsame = issame(pat1,pat2,row,col)
          if(eqsame) write(*,*) "gen ",cgen," is same as before"
          if(eqsame) dgen=cgen-1
          if(eqsame) stilll=.true.
          if(eqsame) goto 700
          if(.not.eqsame) write(*,*) "gen ",cgen," is NOT same"
          cgen=cgen+1
        goto 710
 700    write(*,*) "simulated all ",fgen," generations"
        if(stilll) write(*,*) "Still life after ", dgen, " steps"
        if(.not.stilll) write(*,*) "Not Still after ", gen, " steps"
        call printp(pat1,row,col)
        write(*,*) "Final pattern 1 (",fgen," generations)"
        call printp(pat2,row,col)
        write(*,*) "Final pattern 2 (",gen," generations)"
        stop 'quit normally'
      end
