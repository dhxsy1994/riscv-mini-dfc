.text
.align 2

.globl write_table_a
.type  write_table_a,@function

write_table_a:
  mv t0, a0
  mv t1, a1
  .long 0x00530074
  ret

