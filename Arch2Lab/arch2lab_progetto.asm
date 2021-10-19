
.data

.align 4
str_header:    	 				.asciiz "\n=======  BIG MATH CALCULATOR  : Welcome!  =======\n( type 'c' to reset the calculator, 'q' to exit )\n\n"
str_input1:   					.asciiz "\nInsert first operand ( non-negative, maximum value : 2^"
str_input2:   					.asciiz "\nInsert second operand ( non-negative, maxium value : 2^"
str_prompt:						.asciiz "-1 ):\n=> "
str_op:			   				.asciiz "\nInsert the operator ( valid operators are +, -, *, /, % ) :\n"
str_result: 	    			.asciiz "\nResult:\n== "
str_bytes:  					.asciiz " Bytes.\n"
str_reset:						.asciiz "\nResetting the calculator...\n"
str_exit:    					.asciiz "\n===  Bye!  ===\n\n"   
str_parserr:   					.asciiz "\nError! Failed to parse input. Try again ...\n"
str_overflow:   				.asciiz "\nError! Max value exceeded. Try again...\n"
str_negative:   				.asciiz "\nError! Result is negative. Try again...\n"
str_divzero:   					.asciiz "\nError! Division by zero. Try again...\n"
.align 4


.text

main: 							# int main() {
									# BACKUP CALLER SAVED REGS
	subu $sp, $sp, 20 				# $sp -= 20
	sw $ra, 16($sp)					# $16($sp) = old_$ra
	sw $fp, 12($sp)					# $12($sp) = old_$fp
	sw $a2, 8($sp)					# $8($sp) = old_$a2
	sw $a1, 4($sp)					# $4($sp) = old_$a1
	sw $a0, 0($sp)					# $0($sp) = old_$a0
									# CALL MAIN FUNCTION
	li $a0, 16						# $a0 =16
	jal big_math_calculator			# call big_math_calculator(16)
									# RESTORE CALLER SAVED REGS
	lw $a0, 0($sp)					# $a0 = old_$a0
	lw $a1, 4($sp)					# $a1 = old_$a1
	lw $a2, 8($sp)					# $a2 = old_$a2
	lw $fp, 12($sp)					# $fp = old_$fp
	lw $ra, 16($sp)					# $ra = old_$ra
	addu $sp, $sp, 20				# $sp += 20
									# RETURN TO KERNEL CODE
	move $v0, $zero					# $v0 = 0
	jr $ra							# return 0 }


big_math_calculator: 				# big_math_calculator(max_input_len) {
									# CHECK INPUT PARAMETER
	li $v0, -1							# $v0 = -1
	andi $t0, $a0, 0x03					# $t0 = max_input_len % 4
	bne	$t0, $zero, bmc_end				# if (max_input_Len % 4 != 0) goto bmc_end and return -1;
									# BACKUP CALLEE SAVED REGS
	subu $sp, $sp, 28					# $sp -= 28
	sw $ra, 24($sp)						# $24($sp) = old_$ra
	sw $fp, 20($sp)						# $20($sp) = old_$fp
	sw $s4, 16($sp)						# $16($sp) = old_$s4
	sw $s3, 12($sp)						# $12($sp) = old_$s3
	sw $s2, 8($sp)						# $8($sp) = old_$s2
	sw $s1, 4($sp)						# $4($sp) = old_$s1
	sw $s0, 0($sp)						# $0($sp) = old_$s0
									# INIT REGS & MEMORY
	move $fp, $sp						# $fp = $sp
	move $s0, $a0						# $s0 = max_input_len
	sll $s1, $a0, 2						# $s1 = max_input_len * 4 = max_str_len
	ori $v0, $zero, 9					# load sbrk opcode
	syscall								# call sbrk(max_input_len)
	move $s2, $v0						# $s2 = loc(op1)
	ori $v0, $zero, 9					# load sbrk opcode
	syscall								# call sbrk(max_input_len)
	move $s3, $v0						# $s3 = loc(op2)
	ori $v0, $zero, 9					# load sbrk opcode
	sll $a0,$a0, 1						# $a0 = max_input_len * 2
	syscall								# call sbrk(max_input_len * 2)
	move $s4, $v0						# $s4 = loc(res)
									# PRINT HEADER
	ori $v0, $zero, 4					# load print_string code
	la $a0, str_header					# load address of the header string
	syscall								# call print_string(str_header)
bmc_input1:							# PARSE 1ST INPUT
	move $a0, $s2						# $a0 = loc(op1)
	move $a1, $s0						# $a1 = max_input_len
	jal memzero							# call memzero(loc(op1),max_input_len)
	ori $v0, $zero, 4					# load print_string code
	la $a0, str_input1					# load address of the 1st input message
	syscall								# call print_string(str_input1)
	ori $v0, $zero, 1					# load print_int code
	sll $a0, $s0, 3						# $a0 = max_input_len * 8
	syscall								# call print_int(max_input_len*4)
	ori $v0, $zero, 4					# load print_string code
	la $a0, str_prompt					# load address of the prompt string
	syscall								# call print_string(str_prompt)
	subu $sp, $sp, $s1					# $sp -= max_str_len
    li $v0, 8							# load read_string code
	move $a0, $sp						# $a0 = $sp = loc(str)
    move $a1, $s1						# $a1 = max_str_len
    syscall								# call str = read_string($sp, max_str_len)
	lb $t0, 0($sp)						# $t0 = str[0]
	lb $t1, 1($sp)						# $t1 = str[1]
	seq $t2, $t1, '\n'					# $t2 = str[1] = '\n'? 1 : 0
	seq $t3, $t0, 'c'					# $t3 = str[0] = 'c'? 1 : 0
	and $t3, $t3, $t2					# $t3 = str[0] = 'c' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_reset			# if (str[0] = 'c' && str[1] = '\n') goto bmc_reset
	seq $t3, $t0, 'q'					# $t3 = str[0] = 'q'? 1 : 0
	and $t3, $t3, $t2					# $t3 = str[0] = 'q' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_quit			# if (str[0] = 'q' && str[1] = '\n') goto bmc_quit
	move $a0, $sp						# $a0 = $sp = loc(str)
	move $a1, $s1						# $a1 = max_str_len
	move $a2, $s2						# $a2 = loc(op1)
	move $a3, $s0 						# $a3 = max_input_len
	jal read_big_int					# call read_big_int($sp,max_str_len,loc(op1),max_input_len)
	addu $sp, $sp, $s1					# $sp += max_str_len
	blt $v0, $zero, bmc_err1			# if ($v0 < 0) goto bmc_err1
	move $a0, $v0						# load the result length as arg
	li $v0, 1							# load print_int code
	syscall								# call print_int($v0)
	li $v0, 4							# load print_string code
	la $a0, str_bytes					# load message string address
	syscall								# call print_string(str_bytes)
	j bmc_input2						# goto bmc_input2
bmc_err1:							# 1ST INPUT ERROR HANDLING
	beq $v1, $zero, bmc_oflow1 			# if ($v1 != 0) {
	li $v0, 4								# load print_string code
	la $a0, str_parserr						# load message string address
	syscall									# call print_string(str_parserr)
	j bmc_input1						# } else {
bmc_oflow1:
	li $v0, 4								# load print_string code
	la $a0, str_overflow					# load message string address
	syscall									# call print_string(str_overflow)
	j bmc_input1						# }
bmc_input2:							# PARSE 2ND INPUT
	move $a0, $s3						# $a0 = loc(op2)
	move $a1, $s0						# $a1 = max_input_len
	jal memzero							# call memzero(loc(op2),max_input_len)
	ori $v0, $zero, 4					# load print_string code
	la $a0, str_input2					# load address of the 2nd input message
	syscall								# call print_string(str_input2)
	ori $v0, $zero, 1					# load print_int code
	sll $a0, $s0, 3						# $a0 = max_input_len * 8
	syscall								# call print_int(max_input_len*4)
	ori $v0, $zero, 4					# load print_string code
	la $a0, str_prompt					# load address of the prompt string
	syscall								# call print_string(str_prompt)
	subu $sp, $sp, $s1					# $sp -= max_str_len
    li $v0, 8							# load read_string code
	move $a0, $sp						# $a0 = $sp = loc(str)
    move $a1, $s1						# $a1 = max_str_len
    syscall								# call str = read_string($sp, max_str_len)
	lb $t0, 0($sp)						# $t0 = str[0]
	lb $t1, 1($sp)						# $t1 = str[1]
	seq $t2, $t1, '\n'					# $t2 = str[1] = '\n'? 1 : 0
	seq $t3, $t0, 'c'					# $t3 = str[0] = 'c'? 1 : 0
	and $t3, $t3, $t2					# $t3 = str[0] = 'c' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_reset			# if (str[0] = 'c' && str[1] = '\n') goto bmc_reset
	seq $t3, $t0, 'q'					# $t3 = str[0] = 'q'? 1 : 0
	and $t3, $t3, $t2					# $t3 = str[0] = 'q' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_quit			# if (str[0] = 'q' && str[1] = '\n') goto bmc_quit
	move $a0, $sp						# $a0 = $sp = loc(str)
	move $a1, $s1						# $a1 = max_str_len
	move $a2, $s3						# $a2 = loc(op2)
	move $a3, $s0 						# $a3 = max_input_len
	jal read_big_int					# call read_big_int($sp,max_str_len,loc(op2),max_input_len)
	addu $sp, $sp, $s1					# $sp += max_str_len
	blt $v0, $zero, bmc_err2			# if ($v0 < 0) goto bmc_err1
	move $a0, $v0						# load the result length as arg
	li $v0, 1							# load print_int code
	syscall								# call print_int($v0)
	li $v0, 4							# load print_string code
	la $a0, str_bytes					# load message string address
	syscall								# call print_string(str_bytes)   
	j bmc_op							# goto bmc_op
bmc_err2:							# 2ND INPUT ERROR HANDLING
	beq $v1, $zero, bmc_oflow2 			# if ($v1 != 0) {
	li $v0, 4								# load print_string code
	la $a0, str_parserr						# load message string address
	syscall									# call print_string(str_parserr)
	j bmc_input2						# } else {
bmc_oflow2:
	li $v0, 4								# load print_string code
	la $a0, str_overflow					# load message string address
	syscall									# call print_string(str_overflow)
	j bmc_input2						# }
bmc_op:								# PARSE OPERATOR
	move $a0, $s4						# $a0 = loc(op1)
	sll $a1, $s0, 1						# $a1 = max_input_len*2
	jal memzero							# call memzero(loc(res),max_input_len*2)
	li $v0, 4							# load print_string code
	la $a0, str_op						# load message string address
	syscall								# print_string(str_op)
	subu $sp, $sp, 4					# $sp -= 4
    ori $v0, $zero, 8					# load print_string code
	move $a0, $sp						# $a0 = loc(str)
    ori $a1, $zero, 4					# $a1 = len(str) = 4
    syscall								# call read_string($sp,4)
	lb $t0, 0($sp)						# $t0 = str[0]
	lb $t1, 1($sp)						# $t1 = str[1]
	addu $sp, $sp, 4					# $sp += 4
	seq $t1, $t1, '\n'					# $t1 = str[1] = '\n'? 1 : 0
	seq $t2, $t0, '+'					# $t2 = str[0] = '+'? 1 : 0
	and $t3, $t1, $t2					# $t3 =  str[0] = '+' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_add				# if (str[0] = '+' && str[1] = '\n') goto bmc_add
	seq $t2, $t0, '-'					# $t2 = str[0] = '-'? 1 : 0
	and $t3, $t1, $t2					# $t3 = str[0] = '-' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_sub				# if (str[0] = '-' && str[1] = '\n') goto bmc_sub
	seq $t2, $t0, '*'					# $t2 = str[0] = '*'? 1 : 0
	and $t3, $t1, $t2					# $t3 = str[0] = '*' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_mul				# if (str[0] = '*' && str[1] = '\n') goto bmc_mul
	seq $t2, $t0, '/'					# $t2 = str[0] = '/'? 1 : 0
	and $t3, $t1, $t2					# $t3 = str[0] = '/' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_div				# if (str[0] = '/' && str[1] = '\n') goto bmc_div
	seq $t2, $t0, '%'					# $t2 = str[0] = '%'? 1 : 0
	and $t3, $t1, $t2					# $t3 = str[0] = '%' && str[1] = '\n'? 1 : 0
	bne $t3, $zero, bmc_mod				# if (str[0] = '%' && str[1] = '\n') goto bmc_mod
	li $v0, 4							# load print_string code
	la $a0, str_parserr					# load message string address
	syscall								# call print_string(str_parserr)
	j bmc_op							# goto bmc_op
bmc_add:							# ADD OPERATION
	move $a0, $s2 						# $a0 = loc(op1)
	move $a1, $s3 						# $a1 = loc(op2)
	move $a2, $s4 						# $a2 = loc(res)
	move $a3, $s0 						# $a3 = max_input_len
	jal add_big_int						# call add_big_int(loc(op1),loc(op2),loc(res),max_input_len)
	move $t0, $s4						# $t0 = loc(res)
	move $t1, $s0						# $t1 = max_input_len
	j bmc_result						# goto bmc_result
bmc_sub:							# SUB OPERATION
	move $a0, $s2 						# $a0 = loc(op1)
	move $a1, $s3 						# $a1 = loc(op2)
	move $a2, $s4 						# $a2 = loc(res)
	move $a3, $s0 						# $a3 = max_input_len
	jal sub_big_int						# call sub_big_int(loc(op1),loc(op2),loc(res),max_input_len)
	beq $v0, $zero, bmc_suberr			# if ($v0 != 0) {
	li $v0, 4								# load print_string code
	la $a0, str_negative					# load message string address
	syscall									# call print_string(str_negative)
	j bmc_input1						# }
bmc_suberr:								#
	move $t0, $s4						# $t0 = loc(res)
	move $t1, $s0						# $t1 = max_input_len
	j bmc_result						# goto bmc_result
bmc_mul:							# MUL OPERATION
	subu $sp, $sp, 4					# $sp -= 4
	move $a0, $s2 						# $a0 = loc(op1)
	move $a1, $s3 						# $a1 = loc(op2)
	move $a2, $s4 						# $a2 = loc(res)
	move $a3, $s0 						# $a3 = max_input_len
	sw $s0, 0($sp)						# 0($sp) = max_input_len
	jal mul_big_int						# call mul_big_int(loc(op1),loc(op2),loc(res),max_input_len, max_input_len)
	move $t0, $s4						# $t0 = loc(res)
	sll $t1, $s0, 1						# $t1 = max_input_len * 2
	addi $sp, $sp, 4					# $sp += 4
	j bmc_result						# goto bmc_result
bmc_div:							# DIV OPERATION
	subu $sp, $sp, 8					# $sp -= 8
	move $a0, $s2 						# $a0 = loc(op1)
	move $a1, $s3 						# $a1 = loc(op2)
	move $a2, $s4 						# $a2 = loc(quot) = loc(res)
	add $a3, $s4, $s0					# $a3 = loc(rem) = loc(res) + max_input_len
	sw $s0, 4($sp)						# 4($sp) = max_input_len
	sw $s0, 0($sp)						# 0($sp) = max_input_len
	jal div_big_int						# call div_big_int(loc(op1),loc(op2),loc(res),loc(res)+max_input_len,max_input_len,max_input_len)
beq $v0, $zero, bmc_diverr				# if ($v0 != 0) {
	li $v0, 4								# load print_string code
	la $a0, str_divzero						# load message string address
	syscall									# call print_string(str_divzero)
	j bmc_input1						# }
bmc_diverr:								#
	move $t0, $s4						# $t0 = loc(res)
	move $t1, $s0						# $t1 = max_input_len
	addi $sp, $sp, 8					# $sp += 8
	j bmc_result						# goto bmc_result
bmc_mod:							# MOD OPERATION
	subu $sp, $sp, 8					# $sp -= 8
	move $a0, $s2 						# $a0 = loc(op1)
	move $a1, $s3 						# $a1 = loc(op2)
	move $a2, $s4 						# $a2 = loc(quot) = loc(res)
	add $a3, $s4, $s0					# $a3 = loc(rem) = loc(res) + max_input_len
	sw $s0, 4($sp)						# 4($sp) = max_input_len
	sw $s0, 0($sp)						# 0($sp) = max_input_len
	jal div_big_int						# call div_big_int(loc(op1),loc(op2),loc(res),loc(res)+max_input_len,max_input_len,max_input_len)
beq $v0, $zero, bmc_moderr				# if ($v0 != 0) {
	li $v0, 4								# load print_string code
	la $a0, str_divzero						# load message string address
	syscall									# call print_string(str_divzero)
	j bmc_input1						# }
bmc_moderr:								#
	add $t0, $s4, $s0					# $t0 = loc(res) + max_input_len
	move $t1, $s0						# $t1 = max_input_len
	addi $sp, $sp, 8					# $sp += 8
	j bmc_result						# goto bmc_result
bmc_result:							# PRINT RESULT
	li $v0, 4							# load print_string code
	la $a0, str_result					# load message string address
	syscall								# call print_string(str_result)
	move $a0, $t0						# $a0 = $t0
	move $a1, $t1						# $a1 = $t1
	jal write_big_int					# call write_big_int($t0, $t1)
	move $t0, $v0						# backup return value
	li $v0, 11							# load write_char code
	li $a0, '\n'						# load newline character
	syscall								# call write_char('\n')
	ori $v0, $zero, 1					# load print_int code
	move $a0, $t0						# load result length 
	syscall								# call print_int($v0)
	li $v0, 4							# load print_string code
	la $a0, str_bytes					# load message string address
	syscall								# call print_string(str_bytes)
	j bmc_input1						# LOOP BACK
bmc_reset:							# CLEAR HANDLING
	move $sp, $fp						# $sp = $fp
	li $v0, 4							# load print_string code
	la $a0, str_reset					# load message string address
	syscall								# call print_string(str_reset)
	j bmc_input1						# goto bmc_start
bmc_quit:							# QUIT HANDLING
	move $sp, $fp						# $sp = $fp
	li $v0, 4							# load print_string code
	la $a0, str_exit					# load address of quit message
	syscall								# call print_string(str_quit)
	lw $s0, 0($sp)						# $s0 = $old_s0	
	lw $s1, 4($sp)						# $s1 = $old_s1
	lw $s2, 8($sp)						# $s2 = $old_s2
	lw $s3, 12($sp)						# $s3 = $o ld_s3
	lw $s4, 16($sp)						# $s4 = $old_s4
	lw $fp, 20($sp)						# $fp = $old_fp
	lw $ra, 24($sp)						# $ra = $old_ra
	addi $sp, $sp, 28					# $sp += 28
	move $v0, $zero						# $v0 = 0
bmc_end:								# BACK TO CALLER
	jr $ra						# return $v0 } 



memzero:						# memzero(loc(buf), len(buf)) {
	move $t0, $zero					# $t0 = i = 0
mz_loop:							# FILL BUFFER OF ZEROS
	bge $t0, $a1, mz_end			# while (i < len(buf)) { 
	addu $t1, $a0, $t0					# $t1 = loc(buf) + i = loc(buf[i])
	sw $zero, 0($t1)					# loc(buf[i]) = 0
	addu $t0, $t0, 4					# $t0 = i += 4
	j mz_loop						# }
mz_end:								#
	jr $ra						# }



write_big_int: 					# write_big_int(loc(buf), len(buf)) {
	li $t0, 10						# $t0 = 10
	subu $v0, $a1, 4               	# $v0 = res_len = len(buf) - 4
	move $t7, $zero					# $t7 = dec_len = 0
wbi_loop0:							# DISCARD TRAILING ZEROS
	blt $v0, $zero, wbi_zero		# while (res_len >= 0) { 
	addu $t1, $a0, $v0					# $t1 = loc(buf) + res_len = loc(buf[res_len])
	lw $t2, 0($t1)						# $t1 = buf[res_len]
	bne $t2, $zero, wbi_zero			# if (buf[res_len] != 0) break
	subu $v0, $v0, 4					# res_len -= 4
	j wbi_loop0						# }
wbi_zero:							# HANDLE INPUT EQUALS TO ZERO
	bge $v0, $zero, wbi_notzero		# if (res_len < 0) {
	li $v0, 11							# load write_char code
	li $a0, '0'							# $a0 = '0' WRITES 0 DIGIT
	syscall								# call write_char(c)
	li $v0, 4							# $v0 = res_len = 4
	jr $ra								# return 4;
wbi_notzero:						# }
	addu $t2, $v0, 2				# $t2 = res_len + 2
	addu $t2, $a0, $t2				# $t2 = loc(buf)+res_len+2 = loc(buf[res_len+2])
	lhu $t2, 0($t2)					# $t2 = buf[res_len+2]
	sne $t2, $t2, $zero				# $t2 = buf[res_len+2] != 0
	sll $t2, $t2, 1					# $t2 = buf[res_len+2] != 0 ? 2 : 0
	addu $t2, $v0, $t2				# $t2 = i = buf[res_len+2]!=0 ? res_len+2 : res_len
wbi_loopi:			 				# do { EUCLIDEAN ALGORITHM
	move $t3, $t2						# $t3 = j = i
    move $t4, $zero                 	# $t4 = r = 0
wbi_loopj:								# do { SHORT DIVISION ALGORITHM
	addu $t5, $a0, $t3						# $t5 = loc(buf) + j = loc(buf[j])
	lhu $t5, 0($t5)							# $t6 = buf[j]
	sll $t6, $t4, 16						# $t6 = r << 16 =  r*2^16
	or $t6, $t5, $t6						# $t6 = r <<2^16 || buf[j] = r*2^16 + buf[j]
	divu $t6, $t0							# (r*2^16 + buf[j]) / 10
	mflo $t6								# $t2 = floor(r*2^16 + buf[j]) / 10)
	addu $t5, $a0, $t3						# $t5 = loc(buf) + j = loc(buf[j])
	sh $t6, 0($t5)							# buf[j] = floor(r*2^16 + buf[j]) / 10)
	mfhi $t4								# r = r*2^16 + buf[j] % 10
	subu $t3, $t3, 2						# j -= 2
	bge $t3, $zero, wbi_loopj			# } while ( j >= 0 )
	ori $t6, $t4, 0x30					# $t6 = r || 0x30 = c[j]
	subu $sp, $sp, 4					# $sp -= 4 	#  WRITE DECIMAL DIGIT ON THE STACK
	sw $t6, 0($sp)						# 0($sp) = c[j]
	addu $t7, $t7, 4					# $t7 = dec_len += 4
	addu $t6, $a0, $t2					# $t6 = loc(buf) + i = loc(buf[i])
	lhu $t6, 0($t6)						# $t6 = buf[j]
	bne $t6, $zero, wbi_decri			# if (buf[j] == 0) {
	subu $t2, $t2, 2						# $t2 = i -= 2
wbi_decri:								#}
	bge $t2, $zero, wbi_loopi 		# } while (i >= 0)
	move $t1, $zero					# $t1 = i = 0
	move $t2, $v0					# $t2 = res_len
wbi_endloop:						# PRINT DIGITS ON SCREEN
		bge $t1, $t7, wbi_end		# while (i >= 0) {
		addu $t0, $sp, $t1				# $t0 = loc(c[i])
		lw $a0, 0($t0)					# $a0 = c[i]
		li $v0, 11						# load write_char code
		syscall							# call write_char(c)
		addu $t1, $t1, 4				# $t1 = i +ss= 4
		j wbi_endloop				# }
wbi_end:
	addu $sp, $sp, $t7				# $sp += dec_len
	addu $v0, $t2, 4				# $v0 = res_len + 4
	jr $ra						# return i+4 }


read_big_int:					# read_big_int(loc(str), len(str), loc(buf), len(buf)) {
	li $t0, 10						# $t0 = 10
	move $t1, $zero					# $t1 = res_len = 0;
	move $t2, $zero					# $t2 = i = 0
rbi_loopi:							# LOOP ON STRING CHARACTERS
	beq $t1, $a1, rbi_end			# while (i < len(str)) {
	addu $t3, $a0, $t2					# $t3 = loc(str) + i = loc(str[i])
	lb $t3, 0($t3)						# $s3 = c = str[i]
	beq $t3, $t0, rbi_end				# if (c == '\n') break
	srl $t4, $t3, 4						# $t4 = c >> 4
	sne	$t4,$t4, 0x3					# $t4 = c >> 4 != 0x3
	andi $t5, $t3, 0xf					# $t5 = c && 0xf
	sge $t6, $t5, $t0					# $t6 = c && 0xf >= 10
	or $t6, $t4, $t6					# $t7 = c >> 4 != 0x3 || c && 0xf >= 10
	beq	$t6, $zero, rbi_badchar			# if (c >> 4 != 0x3 || c && 0xf >= 10) {
	not $v0, $zero							# $v0 = -1
	li $v1, 1						 		# $v1 = 1 
	jr $ra									# return (-1, 1) 
rbi_badchar:							# } BAD CHARACTER READ
    move $t3, $zero                 	# $t3 = j = 0
	mthi $zero							# $hi = 0
    mtlo $t5 	                		# $lo = c && 0xf
rbi_loopj:								# MULTIPY BY 10 AND ADD c
    bge $t3, $t1, rbi_carry           	# while ( j < res_len ) { 
	mthi $zero								# $hi = 0
    addu $t4, $a2, $t3               		# $t4 = loc(buf) + j = loc(buf[j]) 
    lw $t5, 0($t4)                	    	# $t5 = buf[j]
	maddu $t0, $t5							# $hi$lo += 10 * buf[j]
	mflo $t5								# $t5 = $lo
	sw $t5, 0($t4)							# buf[j] = $lo
	mfhi $t5								# $t5 = $hi
	mtlo $t5								# $lo = $hi
    addu $t3, $t3, 4	                	# j += 4
    j rbi_loopj                       	# } 
rbi_carry:								# WRITES CARRY ON BUFFER IF != 0
	mflo $t3							# $t3 = k = $lo
    beq $t3, $zero, rbi_inci       		# if ( k != 0 ) {
    addi $t1, $t1, 4                		# res_len += 4
    ble $t1, $a3, rbi_overflow        		# if ( res_len > len(buf) ) {
	not $v0, $zero								# $v0 = -1
	move $v1,$zero					 			# $v1 = 1 
	jr $ra										# return (-1, 0) 
rbi_overflow:								# } BUFFER OVERFLOW AVOIDANCE
	add $t4, $a2, $t1						# $t0 = loc(buf) + res_len = loc(buf[res_len])
    sw $t3, -4($t4)                  		# buff[res_len-4] = k
rbi_inci:                          		# }
	addu $t2, $t2, 1					# $t2 = i += 1
	j rbi_loopi						# } 
rbi_end:							# i == 0 MEANS EMPTY STRING
	bne $t2, $zero, rbi_empty		# if (i == 0) {
	not $v0, $zero						# $v0 = -1
	li $v1, 1							# $v1 = 1 
	jr $ra								# return (-1, 1) 
rbi_empty:							# }
									# res_len == 0 MEANS 0 AS INPUT
	bne $t1, $zero, rbi_zero		# if (res_len == 0) {
	li $v0, 4							# $v0 = 4
	li $v1, 1							# $v1 = 0 
	jr $ra								# return (4, 0) 
rbi_zero:							# }
	move $v0, $t1					# $v0 = res_len
	move $v1, $zero					# $v1 = 0
	jr $ra						# return (res_len,0) } 




add_big_int:					# add_big_int(loc(op1[0]), loc(op2[0]), loc(res[0]), len) {
    move $t0, $zero             	# $t0 = j = 0
    move $t1, $zero                 # $t1 = k = 0
abi_loop:							# LOOP ON J
    bge $t0, $a3, abi_end           # while ( j < len ) { 
    add $t2, $a0, $t0                   # $t2 = loc(op1) + j = loc(op1[j]) 
    lw $t2, 0($t2)                      # $t2 = op1[j]
    add $t3, $a1, $t0                   # $t3 = loc(op2) + j = loc(op2[j]) 
    lw $t3, 0($t3)                      # $t3 = op2[j]
    addu $t4, $t2, $t3                  # $t4 = op1[j] + op2[j]
    addu $t5, $t4, $t1                  # $t5 = op1[j] + op2[j] + k
    add $t6, $a2, $t0                   # $t6 = loc(res) + j = loc(res[j])
    sw $t5, 0($t6)                      # res[j] = op1[j] + op2[j] + k 
    sltu $t6, $t4, $t2                  # $t6 = op1[j]+op2[j] < op1[j] (<- overflow)
    sltu $t7, $t5, $t4                  # $t7 = op1[j]+op2[j]+k < op1[j]+op2[j] (<- overflow)  
    or $t1, $t6, $t7                    # $t1 = k = overflow? 1 : 0
    addi $t0, $t0, 4                    # $t0 = j = j + 4 
    j abi_loop                      # }
abi_end:
    add $t6, $a2, $t0               # $t6 = loc(res) + len = loc(res[len])
    sw $t1, 0($t6)                  # res[len] = k
    jr $ra                      # }




sub_big_int:                    # sub_big_int(loc(op1), loc(op2), loc(res), len) {
    move $t0, $zero                 # $t0 = j = 0
    move $t1, $zero                 # $t1 = k = 0
sbi_loop:							# LOOP ON J
    bge $t0, $a3, sbi_end           # while ( j < len ) { 
    add $t2, $a0, $t0                   # $t2 = loc(op1) + j = loc(op1[j]) 
    lw $t2, 0($t2)                      # $t2 = op1[j]
    add $t3, $a1, $t0                   # $t3 = loc(op2) + j = loc(op2[j]) 
    lw $t3, 0($t3)                      # $t3 = op2[j]
    subu $t4, $t2, $t3                  # $t4 = op1[j] - op2[j]
    subu $t5, $t4, $t1                  # $t5 = op1[j] - op2[j] - k
    add $t6, $a2, $t0                   # $t6 = loc(res) - j = loc(res[j])
    sw $t5, 0($t6)                      # res[j] = op1[j] - op2[j] - k 
    sgtu $t6, $t4, $t2                  # $t6 = op1[j]-op2[j]) > op1[j] (<- overflow)
    sgtu $t7, $t5, $t4                  # $t7 = op1[j]-op2[j]-k > op1[j]-op2[j] (<- overflow)  
    or $t1, $t6, $t7                    # $t1 = k = overflow? 1 : 0
    addi $t0, $t0, 4                    # $t0 = j = j + 4 
    j sbi_loop                      # }
sbi_end:							# TEST OVERFLOW
	move $v0, $zero					# $v0 = 0
	beq $t1, $zero, sbi_err			# if (k != 0) {
	not $v0, $v0						# $v0 = -1
sbi_err:							# }
    jr $ra                      # return $v0 }




mul_big_int:                    # mul_big_int(loc(op1), loc(op2), loc(res), len(op1), len(op2)) {
    move $t0, $zero                 # k = 0
    move $t1, $zero                 # i = 0
    move $t2, $zero                 # j = 0
    li $t3, 1                       # $t3 = 1
    lw $t4, 0($sp)                  # $t4 = arg4 = len(op2)
m_loop0:							# INITIALIZE 
    bge $t1, $a3, m_loopj           # while ( i < len(op1) ) {
    add $t5, $a2, $t1                   # $t5 = loc(res[0]) + i = loc(res[i])
    sw $zero, 0($t5)                    # res[i] = 0
    addi $t1, $t1, 4                    # i += 4
    j m_loop0                       # }
m_loopj:    						# LOOP ON J
    bge $t2, $t4, m_end             # while ( j < len(op2) )
	move $t0, $zero						# k = 0
    move $t1, $zero                     # i = 0
    add $t5, $a1, $t2                   # $t5 = loc(op2[0]) + j = loc(op2[j])
    lw $t5, 0($t5)                      # $t5 = op2[j]
mlj_loopi:								# LOOP ON I
    bge $t1, $a3, mlj_end               # while ( i < len(op1) ) {
    multu $t0, $t3                          # acc = k
    add $t6, $a0, $t1                       # $t6 = loc(op1[0]) + i = loc(op1[i])
    lw $t6, 0($t6)                          # $t6 = op1[i]
    maddu $t6, $t5                          # acc = k + op1[i]*op2[j]
    add $t6, $t1, $t2                       # $t6 = i + j
    add $t6, $a2, $t6                       # $t6 = loc(res[0]) + i + j = loc(res[i+j])
    lw $t7, 0($t6)                          # $t7 = res[i+j]
    maddu $t7, $t3                          # acc = k + op1[i]*op2[j] + res[i+j]
    mflo $t7                                # $t7 = $lo
    sw $t7, 0($t6)                          # res[i+j] = $lo    
    mfhi $t0                                # k = $hi
    addi $t1, $t1, 4                        # i += 4
    j mlj_loopi                         # }
mlj_end:
    add $t6, $t2, $t1                   # $t6 = j + m
    add $t6, $a2, $t6                   # $t6 = loc(res[0]) + j + m = loc(res[j+m])
    sw $t0, 0($t6)                      # res[j+m] = k
    addi $t2, $t2, 4                    # j += 4
    j m_loopj                       # }
m_end:
    jr $ra                      # }	


div_big_int:					# div_big_int(loc(op1), loc(op2), loc(quot), loc(rem), len(op1), len(op2)) {
    subu $sp, $sp, 24           	# $sp -= 24
    sw $s5, 20($sp)             	# 20($sp) = old_$s5
    sw $s4, 16($sp)             	# 16($sp) = old_$s4
    sw $s3, 12($sp)             	# 12($sp) = old_$s3
    sw $s2, 8($sp)              	# 8($sp) = old_$s2
    sw $s1, 4($sp)              	# 4($sp) = old_$s1
    sw $s0, 0($sp)              	# 0($sp) = old_$s0
	lw $s1, 24($sp)					# $s1 = len(op2)
	subu $s2, $s1, 4				# $s2 = n = len(op2) - 4
dbi_divlen0:						# DISCARD TRAILING ZEROS
	blt	$s2, $zero, dbi_divlen1		# while ( n >= 0 ) { 
	add $t0, $a1, $s2					# $t0 = loc(op2) + n = loc(op2[n])
	lw $t0, 0($t0)						# $t0 = op2[n]
	bne $t0, $zero, dbi_divlen1 		# if (op2[n] != 0) break
	subu $s2, $s2, 4					# $s2 = n -= 4
	j dbi_divlen0					# } 
dbi_divlen1:						# FIND NORMALIZATION FACTOR x
	not $v0, $zero					# $v0 = -1
	blt $s2, $zero, dbi_end			# if ( n < 0) return -1;
	move $v0, $zero					# $v0 = 0
	clz $s3, $t0					# $s3 = x = leading_zeroes(op2[n])
	ori $t0, $zero, 32				# $t0 = 32
	subu $t0, $t0, $s3		 		# $t0 = 32 - x
	move $t1, $zero					# $t1 = i = 0
	move $t2, $zero					# $t2 = k = 0
	subu $t3, $s1, $s2				# $t3 = len(op2) - n
dbi_normdiv0:						# NORMALIZE DIVISOR IN UPPER QUOT BUFFER
	bge $t1, $s2, dbi_normdiv1		# while ( i < n ) ) {
	addu $t4, $a1, $t1					# $t4 = loc(op2[0]) + i = loc(op2[i])
	lw $t5, 0($t4)						# $t5 = op2[i]
	sll $t6, $t5, $s3					# $t6 = op2[i] << x
	or $t6, $t6, $t2					# $t6 = op2[i] << x || k
	addu $t4, $t3, $t1					# $t4 = len(op2) - n + i
	addu $t4, $a2, $t4					# $t4 = loc(quot[len(op2)-n+i])
	sw $t6, 0($t4)						# quot[len(op2)-n+i] = op2[i] << x || k
	srl $t2, $t5, $t0					# $t7 = k = op2[i] >> 32 - x
	addu $t1, $t1, 4					# $t3 = i += 4
	j dbi_normdiv0					# }
dbi_normdiv1:						# SAVE DIVISOR MSWORD IN REGISTER
	addu $t4, $a1, $t1				# $s4 = loc(op2[n])
	lw $s4, 0($t4)					# $s4 = op2[n]
	sll $s4, $s4, $s3				# $s4 = op2[n] << x
	or $s4, $s4, $t2				# $s4 = op2[n] << x || k = v'v''
	lw $s0, 24($sp)					# $s0 = len(op1)
    move $t1, $zero                 # $t1 = i = 0
	move $s5, $zero					# $s5 = k = 0
dbi_normrem0:						# NORMALIZES DIVIDEND IN REM BUFFER
	bge $t1, $s0, dbi_normrem1		# while ( i < len(op1) ) {
	addu $t2, $a0, $t1					# $t2 = loc(op1) + i = loc(op1[i])
	lw $t2, 0($t2)						# $t2 = op1[i]
	sll $t3, $t2, $s3					# $t3 = op1[i] << x
	or $t3, $t3, $s5					# $t3 = (op1[i] << x )|| k
	addu $t4, $a3, $t1					# $t4 = loc(rem) + i = loc(rem[i])
	sw $t3, 0($t4)						# rem[i] = (op1[i] << x )|| k
	srl $s5, $t2, $t0					# $s5 = k = op1[i] >> 32 - x
	addu $t1, $t1, 4					# $t1 = i += 4
	j dbi_normrem0					# } ( $s5 = u'u'' )
dbi_normrem1:						# SARTS LOOP ON J
	subu $t2, $s0, 2				# $t2 = j = len(op1) - 2
dbi_loop:							# do { FINDS Q
	srl $t4, $s4, 16					# $t4 = op2[n] >> 16 = v'
	divu $s5, $t4						# u'u'' (=k) / v' = q
	addu $t1, $a3, $t2					# $t1 = loc(rem[0]) + j = loc(rem[j])
	lhu $t1, 0($t1)						# $t1 = op1[j] = u'''
	mflo $t4							# $t4 = q 
	mfhi $t5							# $t5 = r
dbi_testq:								# while(true){ TEST Q
	andi $t7, $s4, 0xffff					# $t7 = op2[n] && 0xffff = v''
	multu $t4, $t7							# q * v''
	mflo $t6								# $t6 = q * v''
	sll $t7, $t5, 16						# $t7 = r << 16 = r * 2^16
	or $t7, $t7, $t1						# $t7 = r * 2^16 + u'''
	sgtu $t6, $t6, $t7						# $t6 = q*v'' > r*2^16+u'''
	srl $t7, $t4, 16						# $t7 = q >> 16 = q / 2^16
	or $t6, $t6, $t7						# $t6 = q >= 2^16 || q*v'' > r*2^16+u'''
	beq $t6, $zero, dbi_mulsub				# if (q >= 2^16 || q*v'' > r*2^16+u''') {
	subu $t4, $t4, 1							# $t4 = q -= 1
	srl $t6, $s4, 16							# $t6 = v'
	addu $t5, $t5, $t6							# $t5 = r += v'
	srl $t7, $t5, 16							# $t7 = r >> 16
	beq $t7, $zero, dbi_testq					# if ( r >= 2^16) break 
dbi_mulsub:									# } else break 
										# }
	mtlo $zero							# $lo = 0
	move $t0, $zero						# $t0 = i = 0
	move $t3, $zero						# $t3 = k = 0
	subu $t5, $s1, $s2					# $t5 = len(op2) - n
	subu $t6, $t2, $s2					# $t6 = j - n
dbi_mulsub0:							# MULTIPLY AND SUBTRACT
	bge $t0, $s2, dbi_mulsub1			# while ( i < n ) ) {
	mthi $zero								# $hi = 0
	addu $t7, $t5, $t0						# $t7 = len(op2) - n + i
	addu $t7, $a2, $t7						# $t7 = loc(quot[len(op2)-n+i]
	lw $t7, 0($t7)							# $t7 = quot[len(op2)-n+i]
	maddu $t4, $t7							# q * quot[len(op2)-n+i]
	addu $t7, $t6, $t0						# $t7 = j - n + i
	addu $v0, $a3, $t7						# $v0 = loc(rem[j-n+i])
	ulw $t7, -2($v0)						# $t7 = rem[j-n+i]
	subu $v0, $t7, $t3						# $v0 = rem[j-n+i] - k 
	sltu $v1, $t7, $v0						# $v1 = rem[j-n+i]-k > rem[j-n+i] (<-overflow)
	mflo $t7								# $t7 = $lo
	subu $t7, $v0, $t7						# $t7 = rem[j-n+i] - k - $lo
	sltu $v0, $v0, $t7						# $v0 = rem[j-n+i]-k-$lo > rem[j-n+i]-k (<-overflow)
	or $t3, $v0, $v1						# $t3 = k = overflow? 1 : 0 
	addu $v0, $t6, $t0						# $v0 = j - n + i
	addu $v0, $a3, $v0						# $v0 = loc(rem[j-n+i])
	usw $t7, -2($v0)						# rem[j+n-i] = rem[j-n+i]-k-$lo
	mfhi $t7								# $t7 = $hi
	mtlo $t7								# $lo = $hi
	addu $t0, $t0, 4						# $t0 = i += 4
	j dbi_mulsub0						# }
dbi_mulsub1:							# FIND MSWORD OF PARTIAL REMAINDER
	maddu $t4, $s4						# q * v'v''
	sll $t7, $s5, 16					# $t7 = u'u'' << 16
	or $t7, $t7, $t1 					# $t7 = (u'u'' << 16 )|| u''' =  u''u'''
	subu $v0, $t7, $t3					# $v0 = u''u''' - k
	sltu $t7, $t7, $v0					# $t7 = u''u'''-k > u''u''' (<-overflow)
	mflo $v1							# $v1 = lsw(q*v'v'')
	subu $v1, $v0, $v1					# $v1 = u''u'''-k -lsw(q*v'v'')
	sltu $v0, $v0, $v1					# $v0 = u''u'''-k-lsw(q*v'v'') > u''u'''-k (<-overflow)
	or $t3, $v0, $v1					# $t3 = k = overflow? 1 : 0
	srl $t7, $s5 , 16					# $t7 = u'u'' >> 16 = u'
	move $s5, $v1						# $s5 = lsw(u''u'''-k-lsw(q*v'v''))
	j dbi_addback						# sorry, addback is broken and never executed
	subu $v0, $t7, $t3					# $v0 = u' - k
	mfhi $v1							# $v1 = msw(q*v'v'')
	beq $v1, $v0, dbi_addback			# if (u'-k- msw(q*v'v'') < 0) { (<-overflow)
	subu $t4, $t4, 1						# $t4 = q -= 1
	move $t0, $zero							# $t0 = i = 0
	move $t3, $zero							# $t3 = k = 0
	subu $t5, $s1, $s2						# $t5 = len(op2) - n
	subu $t6, $t2, $s2						# $t6 = j - n
dbi_addloop0:								#  ADD BACK
	bge $t0, $s2, dbi_addloop1				# while (i < n) {
	addu $t7, $t6, $t0							# $t7 = j - n + i
	addu $v0, $a3, $t7							# $v0 = loc(rem[j-n+i])
	ulw $t7, -2($v0)							# $t7 = rem[j-n+i]
	addu $v0, $t7, $t3							# $v0 = rem[j-n+i] + k 
	sltu $v1, $v0, $t7							# $v1 = rem[j-n+i]+k < rem[j-n+i] (<-overflow)
	addu $t7, $t5, $t0							# $t7 = len(op2) - n + i
	addu $t7, $a2, $t7							# $t7 = loc(quot[len(op2)-n+i]
	lw $t7, 0($t7)								# $t7 = quot[len(op2)-n+i]
	addu $t7, $v0, $t7							# $t7 = rem[j-n+i] + k + quot[len(op2)-n+i]
	sltu $v0, $t7, $v0							# $v0 = rem[j-n+i]+k+quot[len(op2)-n+i] < rem[j-n+i]+k (<-overflow)
	or $t3, $v0, $v1							# $t3 = k = overflow? 1 : 0
	addu $v0, $t6, $t0							# $v0 = j - n + i
	addu $v0, $a3, $v0							# $v0 = loc(rem[j-n+i])
	usw $t7, -2($v0)							# rem[j+n-i] = rem[j-n+i]-k-$lo
	addu $t0, $t0, 4							# $t0 = i += 4
	j dbi_addloop0							# }
dbi_addloop1:
	addu $s5, $s5, $s4						# $s5 = u''u''' += v'v''
	addu $s5, $s5, $t3						# $s5 = u''u''' += k
dbi_addback:							# }
	addu $t5, $a3, $t2					# $t5 = loc(rem[0]) + j = loc(rem[j])
	sh $zero 0($t5)						# rem[j] = 0
	subu $t5, $t2, $s2					# $t5 = j - n
	addu $t5, $a2, $t5					# $t5 = loc(quot)+j-n = loc(quot[j-n])
	sh $t4, 0($t5)						# quot[j-n] = q 
	subu $t2, $t2, 2					# j -= 2
	bge $t2, $s2, dbi_loop			# } while (j >= n)
	addu $t0, $a3, $s2				# $t0 = loc(rem) + n = loc(rem[n])
	sw $s5, 0($t0)					# rem[n] = u'u''
	ori $t0, $zero, 32				# $t0 = 32
	subu $t0, $t0, $s3		 		# $t0 = 32 - x
	move $t1, $s2					# $t1 = i = n
	move $t2, $zero					# $t2 = k = 0
dbi_unormrem0:						#  UNNORMALIZE REMAINDER
	blt $t1, $zero, dbi_unormrem1	# while ( i >= 0 ) ) {
	addu $t4, $a3, $t1					# $t4 = loc(op1) + i = loc(op1[i])
	lw $t4, 0($t4)						# $t4 = op1[i]
	srl $t5, $t4, $s3					# $t5 = op[i] >> x
	or $t5, $t5, $t2					# $t5 = (op[i] >> x)|| k
	addu $t6, $a3, $t1					# $t6 = loc(rem[0]) + i = loc(rem[i])
	sw $t5, 0($t6)						# res[j] = (op[i] >> x )|| k
	sll $t2, $t4, $t0					# $t2 = k = op[i] << 32 - x
	subu $t1, $t1, 4					# $t1 = i -= 4
	j dbi_unormrem0						# } 
dbi_unormrem1:						#
	move $t0, $zero					# $t0 = i = 0
	subu $t1, $s1, $s2				# $t1 = len(op2) - n
dbi_end0:							# CLEAR UPPER QUOT BUFFER
	bge $t0, $s2, dbi_end			# while ( i < n ) ) {
	addu $t2, $t1, $t0					# $t2 = len(op2) - n + i
	addu $t2, $a2, $t2					# $t2 = loc(quot[len(op2)-n+i])
	sw $zero, 0($t2)					# quot[len(op2)-n+i] = 0
	addu $t0, $t0, 4					# $t0 = i += 4
	j dbi_end0						# }
dbi_end:							#
    lw $s5, 20($sp)             	# $s5 = old_$s5
    lw $s4, 16($sp)             	# $s4 = old_$s4
    lw $s3, 12($sp)             	# $s3 = old_$s3
    lw $s2, 8($sp)              	# $s2 = old_$s2
    lw $s1, 4($sp)              	# $s1 = old_$s1
    lw $s0, 0($sp)              	# $s0 = old_$s0
	addiu $sp, $sp,	24				# $sp += 24
	jr $ra						# } 


##
## PROCEDURA add_reg(loc(op1[0]), len(op1), op2, loc(res))
##
## Calcola la somma tra un big_int come array di indirizzo loc(op1) e lunghezza di len(op1) bytes e un intero (op2), 
## salvandone il risultato nell' array di indirizzo loc(res) e lunghezza di len(op1) bytes, ritornando in $v0 il  
## resto dell' operazione
##

add_reg:						# add_reg(loc(op1[0]), len(op1), op2, loc(res[0]) {
    move $t0, $zero                 # $t0 = j = 0
    move $v0, $a2                   # $v0 = k = op2
ar_loop:							# LOOP ON J
    bge $t0, $a1, ar_end           	# while ( j < len(op1) ) { 
    add $t1, $a0, $t0               	# $t1 = loc(op1) + j = loc(op1[j])
    lw $t1, 0($t1)                	  	# $t1 = op1[j]
    addu $t1, $t1, $v0                	# $t1 = op1[j]+ k
    add $t2, $a3, $t0               	# $t2 = loc(res) + j = loc(res[j])
    sw $t1, 0($t2)                    	# res[j] = op1[j] + k 
    sltu $v0, $t1, $v0                  # k = op1[j] + k < k  (<-overflow)
    addiu $t0, $t0, 4	                # j += 4
    j ar_loop                       # }
ar_end:
    jr $ra                      # return k }

##
## PROCEDURA mul_reg(loc(op1), len(op1), op2, loc(res))
##
## Calcola la moltiplicazione tra un big_int come array di indirizzo loc(op1[0]) e lunghezza di len(op1) bytes e un  
## intero (op2), salvandone il risultato nell' array di indirizzo loc(res) e lunghezza di len(op1) bytes, 
## ritornando in $v0 il resto dell' operazione
##

mul_reg:						# mul_reg(loc(op1[0]), len(op1), op2, loc(res[0]) {
	addi $t0, $zero, 1				# $t0 = 1
    move $t1, $zero                 # $t1 = j = 0
    move $v0, $zero                 # $v0 = k = 0
mr_loop:							# LOOP ON J
    bge $t1, $a1, mr_end            # while ( j < len(op1) ) { 
	multu $v0, $t0					    # acc = k
    add $t2, $a0, $t1               	# $t2 = loc(op1[j]) = loc(op1[0]) + j 
    lw $t2, 0($t2)                	    # $t2 = op1[j]
	maddu $t2, $a2						# acc += op1[j] * op2
	mflo $t2							# $t2 = $lo
    add $t3, $a3, $t1               	# $t3 = loc(res[j]) = loc(res[0])+ j
	usw $t2, 0($t3)						# res[j] = $lo
	mfhi $v0							# k = $hi
    addiu $t1, $t1, 4	                # j += 4
    j mr_loop                       # }
mr_end:
    jr $ra                      # return k }    

##
## PROCEDURA div_reg(loc(op1), len(op1), op2, loc(res))
##
## Calcola la divisione tra un big_int come array di indirizzo loc(op1) e lunghezza di len(op1) bytes e un  
## intero (op2) e salvandone il quoziente nell' array di indirizzo loc(res) e lunghezza di len(op1) bytes, 
## ritornando in $v0 il resto dell' operazione
##

dv_reg:							# div_reg(loc(op1[0]), len(op1), op2, loc(res[0]) {
	ori $t1, $zero, 32				# $t1 = 32
	clz $t0, $a2					# $t0 = x = leading_zeroes(op2)
	subu $t1, $t1, $t0				# $t1 = 32 - x
	sll $a2, $a2, $t0 				# $a2 = op2 = op2 << x
    move $t2, $zero                 # $t2 = i = 0
	move $t3, $zero					# $t3 = k = 0
dv_norm0:							# NORMALIZE DIVIDEND
	bge $t2, $a1, dv_norm1			# while ( i < len(op1) ) {
	addu $t4, $a0, $t2					# $t4 = loc(op1[0]) + i = loc(op1[i])
	lw $t4, 0($t4)						# $t4 = op1[i]
	sll $t5, $t4, $t0					# $t5 = op[i] << x
	or $t5, $t5, $t3					# $t5 = (op[i] << x )|| k
	addu $t6, $a3, $t2					# $t6 = loc(res[0]) + i = loc(res[i])
	sw $t5, 0($t6)						# res[j] = (op[i] << x )|| k
	srl $t3, $t4, $t1					# $t3 = k = op[i] >> 32 - x
	addu $t2, $t2, 4					# $t2 = i += 4
	j dv_norm0						# }
dv_norm1:							# START LOOP ON J
	subu $t2, $a1, 2				# $t2 = j = len(op1) - 2
dv_loop:							# do { FIND Q
	srl $t4, $a2, 16					# $t4 = op2 >> 16 = v'
	divu $t3, $t4						# u'u'' (=k) / v' = q
	addu $t1, $a3, $t2					# $t1 = loc(res[0]) + j = loc(res[j])
	lhu $t1, 0($t1)						# $t1 = op1[j] = u'''
	mflo $t4							# $t4 = q 
	mfhi $t5							# $t5 = r
dv_testq:								# while(true){ TEST Q
	andi $t7, $a2, 0xffff					# $t7 = op2 && 0xffff=  v''
	multu $t4, $t7							# q * v''
	mflo $t6								# $t6 = q * v''
	sll $t7, $t5, 16						# $t7 = r << 16 = r * 2^16
	or $t7, $t7, $t1						# $t7 = r * 2^16 + u'''
	sgtu $t6, $t6, $t7						# $t6 = q*v'' > r*2^16+u'''
	srl $t7, $t4, 16						# $t7 = q >> 16 = q / 2^16
	or $t6, $t6, $t7						# $t6 = q >= 2^16 || q*v'' > r*2^16+u'''
	beq $t6, $zero, dv_mulsub				# if (q >= 2^16 || q*v'' > r*2^16+u''') {
	subu $t4, $t4, 1							# $t4 = q -= 1
	srl $t6, $a2, 16							# $t6 = v'
	addu $t5, $t5, $t6							# $t5 = r += v'
	srl $t7, $t5, 16							# $t7 = r >> 16
	beq $t7, $zero, dv_testq					# if ( r >= 2^16) break 
dv_mulsub:									# } else break 
										# } MULTIPLY AND SUBTRACT
	srl $t5, $t3, 16					# $t5 = u'u'' >> 16 = u'
	mthi $t5							# $hi = u'
	sll $t5, $t3, 16					# $t1 = u'u'' << 16
	or $t5, $t5, $t1 					# $t5 = (u'u'' << 16 )|| u''' =  u''u'''
	mtlo $t5							# $lo = u''u'''
	msubu $t4, $a2						# $hi$lo = u'u''u''' - q * v
	mflo $t3							# $t3 = k = u'u''u''' - q * v
	mfhi $t5							# $t5 = overflow ? 1 : 0
	beq $zero, $t5, dv_addback			# if (overflow) { ADD BACK
	addu $t3, $t3, $a2						# $t3 = k += op2
	subu $t4, $t4, 1						# $t4 = q -= 1
dv_addback:								# }
	addu $t5, $a3, $t2					# $t5 = loc(res[0]) + j = loc(res[j])
	sh $t4, 0($t5)						# res[j] = q 
	subu $t2, $t2, 2					# j -= 2
	bge $t2, $zero, dv_loop			# } while (j >= 0)
dv_end:								# UNNORMALIZE REMAINDER
	sll $v0, $t3, $t0				# $v0 = k >> x
	jr $ra						# return k }  


