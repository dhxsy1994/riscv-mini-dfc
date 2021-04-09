.text
.align 2

.globl get_pid
.type  get_pid,@function

get_pid:
  .long 0x80002573
  ret
