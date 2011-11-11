procedure Put (Text : String) is
begin
   for I in Text'Range loop
      Put (Text (I));
   end loop;
end Put;

sub put__2
   arg ptr
   var count
   var addr
   count := *ptr
   addr  := ptr + 4
   loop count
     call put__1 (*addr/1)
     ++addr


1  put__2:
1	frame 2
2       push 4(fp)
3	deref
4       pop -4(fp)
5       push 4(fp)
6       push #4
7       add
8       pop -8(fp)
9  L1:	push -4(fp)
10	jz   L2
11	push -8(fp)
12	deref.1
13	call	put__1
14	push -8(fp)
15	push 1
16	add
17	pop  -8(fp)
18	jump L1
19 L2:	frame -2
20	ret

[1-8]   simple
[9-18]  loopback
[19-20]

4(fp):  0 -- 5  := R0 (simple single word arg)
-4(fp): 4 -- 9
-8(fp): 8 -- 17

-4(fp): 4 -- 18 := R1
-8(fp): 8 -- 17 := R0

cancel frame
push 4(fp)   st := r0
deref        st := (r0)
pop -4(fp)   r1 := st   => r1 := (r0) => mov (r0), r1
push 4(fp)   st := r0
push #4      st := r0:4
add          st := r0 + 4
pop -8(fp)   r0 := st => r0 := r0 + 3 => addq #4, r0
L1:push -4(fp)  st := r1
jz L2	     if st = 0 goto L2 => if r1 = 0 goto L2 => tst r1; beq L2
push -8(fp)  st := r0
deref.1      st := (r0).1
call put__1  
push 4(fp); deref --> push r0; deref --> (r0) 
pop -4(fp) --> pop r1 --> mov (r0), r1mov (R0), R1
add #4, (R0)



