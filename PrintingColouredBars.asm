; multitasking and dynamic thread registration


[org 0x0100]
		jmp start


; PCB layout:
; ax,bx,cx,dx,si,di,bp,sp,ip,cs,ds,ss,es,flags,next,dummy
; 0, 2, 4, 6, 8,10,12,14,16,18,20,22,24, 26 , 28 , 30

oldisr:		dd 0
pcb: 		times 32*16 dw 0 ; space for 32 PCBs
stack: 		times 32*256 dw 0 ; space for 32 512 byte stacks
nextpcb: 	dw 1 ; index of next free pcb
current: 	dw 0 ; index of current pcb
lineno: 	dw 0 ; line number for next thread
chars:		db '\|/-'

strb:  		times 255 db 0 ; actual string
m1:		db 'start', 0
m2:		db 'quit', 0
c1:	 	db 'white',0
c2:		db 'green',0
c3:		db 'red',0

; mytask subroutine to be run as a thread
; takes line number as parameter


mytask: 	push bp
		mov bp, sp
		sub sp, 2

;counter for deleting a process
		mov word[bp-2], 0		
		push ax
		push bx
		push di
		push es
		
		mov di, [bp+6]
		

		mov ax, 0xb800
		mov es, ax	
		mov ah, [bp+4]


taskone: 	mov al, [cs:chars+bx] ; read the next shape
		mov [es:di], ax ; write at top left of screen
		inc bx ; increment to next shape
		and bx,3
		jmp taskone
	
		pop es
		pop di
		pop bx
		pop ax
		mov sp, bp
		pop bp
		ret


; subroutine to register a new thread
; takes the segment, offset, of the thread routine and a parameter
; for the target thread subroutine

initpcb: 	push bp
		mov bp, sp
		push ax
		push bx
		push cx
		push si

		mov bx , [nextpcb]
		cmp bx, 32
		je exit

		mov cl, 5
		shl bx, cl

		mov ax, [bp+8] ; read segment parameter
		mov [pcb+bx+18], ax ; save in pcb space for cs
		mov ax, [bp+6] ; read offset parameter
		mov [pcb+bx+16], ax ; save in pcb space for ip
	
		mov [pcb+bx+22], ds ; set stack to our segment
		mov si, [nextpcb]
		mov cl, 9
		shl si, cl ; multiply by 512
		add si, 256*2+stack ; end of stack for this thread
		
		mov ax, [lineno]

		sub si, 2 				; decrement thread stack pointer 
		mov [si], ax 

		mov ax, [bp+10]
		sub si, 2 ; space for attribute
		mov [si], ax

		sub si, 2 ; return address
		mov [pcb+bx+14], si ; save si in pcb space for sp
	
		mov word [pcb+bx+26], 0x0200 ; initialize thread flags
		mov ax, [pcb+28] ; read next of 0th thread in ax
		mov [pcb+bx+28], ax ; set as next of new thread
		mov ax, [nextpcb] ; read new thread index
		mov [pcb+28], ax ; set as next of 0th thread

		inc word[nextpcb]

exit: 		pop si
		pop cx
		pop bx
		pop ax
		pop bp
		ret 8


; timer interrupt service routine
timer: 		push ds
		push bx
		push cs
		pop ds ; initialize ds to data segment
	
		
		mov bx, [current] ; read index of current in bx
		shl bx, 1
		shl bx, 1
		shl bx, 1
		shl bx, 1
		shl bx, 1 ; multiply by 32 for pcb start
	
		mov [pcb+bx+0], ax ; save ax in current pcb
		mov [pcb+bx+4], cx ; save cx in current pcb
		mov [pcb+bx+6], dx ; save dx in current pcb
		mov [pcb+bx+8], si ; save si in current pcb
		mov [pcb+bx+10], di ; save di in current pcb
		mov [pcb+bx+12], bp ; save bp in current pcb
		mov [pcb+bx+24], es ; save es in current pcb


		pop ax ; read original bx from stack
		mov [pcb+bx+2], ax ; save bx in current pcb
		pop ax ; read original ds from stack
		mov [pcb+bx+20], ax ; save ds in current pcb
		pop ax ; read original ip from stack
		mov [pcb+bx+16], ax ; save ip in current pcb
		pop ax ; read original cs from stack
		mov [pcb+bx+18], ax ; save cs in current pcb
		pop ax ; read original flags from stack


		mov [pcb+bx+26], ax ; save cs in current pcb
		mov [pcb+bx+22], ss ; save ss in current pcb
		mov [pcb+bx+14], sp ; save sp in current pcb

		mov bx, [pcb+bx+28] ; read next pcb of this pcb
		mov [current], bx ; update current to new pcb
		mov cl, 5
		shl bx, cl ; multiply by 32 for pcb start


proceed:	mov cx, [pcb+bx+4] ; read cx of new process
		mov dx, [pcb+bx+6] ; read dx of new process
		mov si, [pcb+bx+8] ; read si of new process
		mov di, [pcb+bx+10] ; read diof new process
		mov bp, [pcb+bx+12] ; read bp of new process
		mov es, [pcb+bx+24] ; read es of new process
		mov ss, [pcb+bx+22] ; read ss of new process
		mov sp, [pcb+bx+14] ; read sp of new process


		push word [pcb+bx+26] ; push flags of new process
		push word [pcb+bx+18] ; push cs of new process
		push word [pcb+bx+16] ; push ip of new process
		push word [pcb+bx+20] ; push ds of new process

		mov al, 0x20
		out 0x20, al ; send EOI to PIC
	

		mov ax, [pcb+bx+0] ; read ax of new process
		mov bx, [pcb+bx+2] ; read bx of new process
		pop ds ; read ds of new process
		iret ; return to new process


strlen: 	push bp
		mov bp,sp
		push es
		push cx
		push di
		les di, [bp+4] ; point es:di to string
		mov cx, 0xffff ; load maximum number in cx
		xor al, al ; load a zero in al
		repne scasb ; find zero in the string
		mov ax, 0xffff ; load maximum number in ax
		sub ax, cx ; find change in cx
		dec ax ; exclude null from length
		pop di
		pop cx
		pop es
		pop bp
		ret 4

strcmp: 	push bp
		mov bp,sp
		push cx
		push si
		push di
		push es
		push ds
	
		lds si, [bp+4] ; point ds:si to first string
		les di, [bp+8] ; point es:di to second string
		push ds ; push segment of first string
		push si ; push offset of first string
		call strlen ; calculate string length
		mov cx, ax ; save length in cx
		push es ; push segment of second string
		push di ; push offset of second string
		call strlen ; calculate string length
		cmp cx, ax ; compare length of both strings
		jne exitfalse ; return 0 if they are unequal
		inc cx
		mov ax, 1 ; store 1 in ax to be returned
		repe cmpsb ; compare both strings
		jcxz exitsimple ; are they successfully compared

exitfalse: 	mov ax, 0 ; store 0 to mark unequal
exitsimple: 	pop ds
		pop es
		pop di
		pop si
		pop cx
		pop bp
		ret 8

start: 		xor ax, ax
		mov es, ax ; point es to IVT base

		mov ax, 1100
		out 0x40, al
		mov al, ah
		out 0x40, al
	
		cli
		mov ax, [es:8*4]
		mov [oldisr], ax
		
		mov ax, [es:8*4+2]
		mov [oldisr+2], ax
		
		mov word [es:8*4], timer
		mov [es:8*4+2], cs ; hook timer interrupt
		sti

nextkey:	mov bx, strb
		mov si, 0
		mov cx, 0
	
		mov ax, 0xb800
		mov es, ax
		mov di, 3840

take:   	xor ah, ah
		int 0x16 
		cmp al, 13
		je stop
		mov [bx+si], al
		mov ah, 0x07
		stosw
		inc si
		inc cx
		jmp take

stop:		mov bx, cx
		mov ax, 0x0720
		inc cx
		std
	
l1:		stosw
		loop l1	
	
		cld

		push ds			;check for start
		mov ax, m1
		push ax
		push ds
		mov ax, strb
		push ax
		call strcmp

		mov bx, 0x0007
		cmp ax, 1
		je callinitpcb

		push ds			;check for quit
		mov ax, m2
		push ax
		push ds
		mov ax, strb
		push ax
		call strcmp

		cmp ax, 1
		je terminate

		push ds			;check for white
		mov ax, c1
		push ax
		push ds
		mov ax, strb
		push ax
		call strcmp

		mov bx, 0x0007
		cmp ax, 1
		je callinitpcb


		push ds			;check for green
		mov ax, c2
		push ax
		push ds
		mov ax, strb
		push ax
		call strcmp

		mov bx, 0x0002
		cmp ax, 1
		je callinitpcb

		
		push ds			;check for red
		mov ax, c3
		push ax
		push ds
		mov ax, strb
		push ax
		call strcmp

		mov bx, 0x0004
		cmp ax, 1
		je callinitpcb
		
		jmp askagain
		
callinitpcb:	push bx
		push cs ; use current code segment
		mov ax, mytask
		push ax ; use mytask as offset
		push word [lineno]			;dummy parameter, no actual need
				
		call initpcb ; register the thread
		inc word[lineno]
		inc word[lineno]

askagain:	mov cx, bx
		mov ax, ds
		mov es, ax
		mov di, strb
		mov ax, 0
		rep stosw
		jmp nextkey


terminate:	cli
		xor ax,ax
		mov es, ax
		mov ax, [oldisr]
		mov bx, [oldisr+2]
		mov [es:8*4], ax
		mov [es:8*4+2], bx
		sti
		call clrscr
		mov ax, 0x4c00
	        int 0x21


clrscr: 	push es
		push ax
		push cx
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		xor di, di ; point di to top left column
		mov ax, 0x0720 ; space char in normal attribute
		mov cx, 2000 ; number of screen locations
		cld ; auto increment mode
		rep stosw ; clear the whole screen
		pop di

		pop cx
		pop ax
		pop es
		ret