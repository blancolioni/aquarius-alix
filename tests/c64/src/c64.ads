package C64 is

   type Word_16 is mod 65536;
   type Word_8  is mod 256;
   type Word_4  is mod 16;
   type Word_3  is mod 8;
   type Word_2  is mod 4;
   type Word_1  is mod 2;

   subtype Bit is Word_1;
   subtype Nybble is Word_4;

   subtype Byte is Word_8;
   subtype Address is Word_16;

end C64;

