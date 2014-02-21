.data 
 .balign 4 
 var_a: .word 0 
 .balign 4 
 var_b: .word 0 
 .balign 4 
 var_c: .word 0 
 .balign 4 
 var_t: .word 0 
 .balign 4 
 nr: .asciz "%d \n" 
 .balign 4 
 .text 
 .global printf 
 .balign 4 
 .global main 
 main: 
 	 push {lr}  
  
 	 mov r1, #1 
 	 push {r1} 
 	 ldr r1, addr_a 
 	 pop {r2} 
 	 str r2, [r1] 
 	 mov r1, #1 
 	 push {r1} 
 	 ldr r1, addr_b 
 	 pop {r2} 
 	 str r2, [r1] 
 	 mov r1, #20 
 	 push {r1} 
 	 mov r1, #2 
 	 push {r1} 
 	 pop {r1} 
 	 pop {r2} 
 	 sub r1, r2, r1 
 	 push {r1} 
 	 ldr r1, addr_c 
 	 pop {r2} 
 	 str r2, [r1] 
 label0: 
 	 ldr r1, addr_c 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 pop {r1} 
 	 cmp r1, #0 
 	 beq label1 
 	 ldr r1, addr_b 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 ldr r1, addr_t 
 	 pop {r2} 
 	 str r2, [r1] 
 	 ldr r1, addr_a 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 ldr r1, addr_b 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 pop {r1} 
 	 pop {r2} 
 	 add r1, r2, r1 
 	 push {r1} 
 	 ldr r1, addr_b 
 	 pop {r2} 
 	 str r2, [r1] 
 	 ldr r1, addr_t 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 ldr r1, addr_a 
 	 pop {r2} 
 	 str r2, [r1] 
 	 ldr r1, addr_c 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 mov r1, #1 
 	 push {r1} 
 	 pop {r1} 
 	 pop {r2} 
 	 sub r1, r2, r1 
 	 push {r1} 
 	 ldr r1, addr_c 
 	 pop {r2} 
 	 str r2, [r1] 
 	 b label0 
 label1: 
 	 ldr r1, addr_b 
 	 ldr r1, [r1] 
 	 push {r1} 
 	 pop {r1} 
 	 ldr r0, addr_of_nr 
 	 bl printf 
  
 pop {lr}  
 	 bx lr 
  
 addr_of_nr : .word nr 
  
 addr_t : .word var_t 
 addr_c : .word var_c 
 addr_b : .word var_b 
 addr_a : .word var_a 
  
