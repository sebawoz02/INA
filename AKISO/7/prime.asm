section .data:
	format: db "%d", 10, 0

section .text:
	global main
	extern printf


;nasm -f elf32 -o z2.o zad2.asm
;gcc -o z2 z2.o
main:
	mov ecx, 1
	next_number:
		inc ecx		;ebx += 1
		cmp ecx, 100000
		;if equal jump
		je end
		;else
		mov edi, ecx

		push ecx
	
		mov eax, edi
		call ceil_sqrt
		mov ebx, eax
		mov ecx, 2	;counter, divider
	
		loop:
			mov eax, edi
			mov edx, 0
			div ecx
			cmp edx, 0	;edx = eax % ecx
			;if edx = 0
			je fail
			inc ecx
			cmp ecx, ebx
			;if counter<= ebx
			jle loop
			;else no divider found
			jmp loop_end

		fail:
			mov eax, 0 	;edi is not prime
			pop ecx
			jmp print_or_next
		loop_end:
			mov eax, 1	;edi is prime
			pop ecx
			jmp print_or_next



print_or_next:
		cmp eax, 0
		je next_number
		
		;print if prime
		push ecx
		push dword format
		call printf
		add esp, 4
		pop ecx
		jmp next_number
		

end:
	mov ebx, 0
	mov eax, 1	;sys_exit
	int 0x80


ceil_sqrt:
	push ebx
	push ecx
	push edx
	
	mov ebx, eax
	mov ecx, 1	
	search:		;search for ecx^2>=ebx
		mov eax, ecx
		mul ecx
		cmp eax, ebx
		jg found
		inc ecx
		jmp search
	
	found:
		mov eax, ecx
		pop edx
		pop ecx
		pop ebx
		ret
