.text
.align 2

.globl write_table_d_addr
.type  write_table_d_addr,@function

write_table_d_addr:
  mv t0, a0
  mv t1, a1
  .long 0x00531074
  ret

