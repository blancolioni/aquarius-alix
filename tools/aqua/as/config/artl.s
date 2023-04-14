puts = 1

@ = 256

start:
    put rS, 4096
    put rO, 4096
    geta %2, 1f, 0
    put rT, %2
    or %3, %0, 0
    or %4, %1, 0
    pushj %2, main
    trap 0, 0, 0

1   pushj %255, 2f
    put rJ, %255
    get %255, rBB
    resume 1

2   get %1, rBB
    seth %2, 65535
    incl %2, 61443
    setl %0, 0

3   ldb %3, %1, %0
    bz %3, 4f
    stb %3, %2, 0
    add %0, %0, 1
    jmp 3b

4   put rBB, %0
    pop 1, 0
