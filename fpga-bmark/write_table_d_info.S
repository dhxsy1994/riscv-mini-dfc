.text
.align 2

.globl write_table_d_info
.type  write_table_d_info,@function

write_table_d_info:
  mv t0, a0
  mv t1, a1
  .long 0x00532074
  ret

