ch = %0
last = %1
chrout = %2

start:  setl %0, 32
        setl last, 127
        setl chrout, 65490
_1:     stb %0, chrout, 0
        add  %0, %0, 1
        cmp %3, last, %0
_2:     bnz %3, _1
        setl %0, 10
        stb %0, chrout, 0
        trap 0, 0, 0

