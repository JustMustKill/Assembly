.model small

.stack 100h

.data    
readedTotal           dw  0
writedTotal           dw  0
stepReadedTotal       dw  0
stepWritedTotal       dw  0
reader                dw  0

maxCMDSize equ 127
cmd_size              db  ?
cmd_text              db  maxCMDSize + 2 dup(0)
sourcePath            db  maxCMDSize + 2 dup(0) 
               
;sourcePath            db  "text.txt", 0

sourceID              dw  0
                      
maxWordSize           equ 50
buffer                db  maxWordSize + 2 dup(0)
line   				  db  maxWordSize + 2 dup(0)                           
spaceSym              db  " ","$"                            
deleteLine		      dw  0
numberOfLine          dw  1
clearMode			  dw  0


zero 				  equ '0'
nine 				  equ '9'                            
spaceSymbol           equ ' '
newLineSymbol         equ 0Dh
returnSymbol          equ 0Ah
tabulation            equ 9
endl                  equ 0

clearBoard            db  "                                                  "
startText             db  "Program is started",                                               '$'
badCMDArgsMessage     db  "Bad command-line arguments",'$'
badSourceText         db  "Cannot open source file",                                          '$'
fileNotFoundText      db  "File not found",                                                   '$'
errorClosingSource    db  "Cannot close source file",                                         '$' 
errorWritingText      db  "Error writing to file",                                            '$'
endText               db  "Program is ended",                                                 '$'
errorReadSourceText   db  "Error reading from source file",                                   '$'
EOF                   db  0

period                equ 2
currWordStartingValue equ 0
currWordIndex         db  currWordStartingValue	;


.code  
.386
println MACRO info          ;
	push ax                 ;
	push dx                 ;
                            ;
	mov ah, 09h             
	lea dx, info           
	int 21h                 
                            ;
	mov dl, 0Ah             
	mov ah, 02h           
	int 21h                 
                            
	mov dl, 0Dh             
	mov ah, 02h             
	int 21h                 
                            
	pop dx                  
	pop ax                  
ENDM

main:
	mov ax, @data           
	mov es, ax              
                            
	xor ch, ch              
	mov cl, ds:[80h]		; Смещеие для дальнейшей работы с командой строкой
	mov cmd_size, cl 		; В cmd_size загружаем длину командной строки
	mov si, 81h             ;
	lea di, cmd_text        ; Загружаем в di смещение текста переданного через командную строку
	rep movsb               ; Записать в ячейку адресом ES:DI байт из ячейки DS:SI
    						;
	mov ds, ax              
                            
	println startText       
                            
	call parseCMD           
	cmp ax, 0               
	jne endMain				

    call convertInNumber

	call openFiles         
	cmp ax, 0               
	jne endMain				
    
	call convertInNumber
							
	call mainProc           
	cmp ax, 0               
	jne endMain				
                            
	call closeFiles         
	cmp ax, 0               
	jne endMain				
                            
endMain:                    
	println endText         
                            
	mov ah, 4Ch             
	int 21h                 


cmpWordLenWith0 MACRO textline, is0Marker       
	push si                                     
	                                            
	lea si, textline                            
	call    strlen                              
	                                            
	pop si                                      
	cmp ax, 0                                   
	je is0Marker                                
ENDM                                                                         ;
                                                
                                                                                        ;                               ;
                                                        
                                                
;**************************************************************************************************************************
parseCMD PROC                                   
	push bx                                     
	push cx                                     
	push dx                                     
                                                
	mov cl, cmd_size                            
	xor ch, ch                                  
                                                
	lea si, cmd_text                            
	                                                                                                 
	lea di, buffer                                 
	call rewriteAsWord                                                                                    
    	
	lea di, sourcePath                        
	call rewriteAsWord                          
                                                
	cmpWordLenWith0 sourcePath, badCMDArgs      
	;checkTxt        sourcePath, badCMDArgs
    
	lea di, line
	call rewriteAsNumber
	
	push si
	
	lea si, line
	call strlen
	
	pop si
	
	cmp ax, 0
	je badCMDArgs
												 
	lea di, buffer                              
	call rewriteAsWord                          
                                                
	cmpWordLenWith0 buffer, argsIsGood          
                                                
badCMDArgs:                                     
	println badCMDArgsMessage                  
	mov ax, 1                                   
                                               
	jmp endproc                                
                                                
argsIsGood:                                     
	mov ax, 0                                   
endproc:                                        
	pop dx                                      
	pop cx                                      
	pop bx                                      
	ret	                                        
ENDP
;*************************************************************************************************************************  

;*************************************************************************************************************************
;cx - Длина командной строки
;Результат - переписывает параметры от 0 до 9 из коммандной строки  
rewriteAsNumber PROC
	push ax                     
	push cx                     
	push di                     

	
loopParseNumber:	
	mov al, ds:[si]
	
	cmp al, spaceSymbol         
    je isStoppedNumber          
	cmp al, newLineSymbol       
	je isStoppedNumber         
	cmp al, tabulation          
	je isStoppedNumber          
	cmp al, returnSymbol       
	je isStoppedNumber          
	cmp al, endl               
	je isStoppedNumber          
	
	cmp al, zero
	jb badCMDArgs
	cmp al, nine
	ja badCMDArgs
	
	mov es:[di], al            
                                
	inc di                     
	inc si  					
								
loop loopParseNumber            
isStoppedNumber:  
								
	mov al, endl                
	mov es:[di], al             
	inc si                      
	

	pop di                      
	pop cx                      
	pop ax                      
	ret   
ENDP  
;*************************************************************************************************************************  

;*************************************************************************************************************************
;cx - Длина командной строки
;Результат - переписывает параметр из коммандной строки 
rewriteAsWord PROC              
	push ax                     
	push cx                     
	push di                     
	                            
loopParseWord:                  
	mov al, ds:[si]            
	
	cmp al, spaceSymbol         
    je isStoppedSymbol          
	cmp al, newLineSymbol       
	je isStoppedSymbol          
	cmp al, tabulation          
	je isStoppedSymbol          
	cmp al, returnSymbol        
	je isStoppedSymbol         
	cmp al, endl                
	je isStoppedSymbol          
                                
	mov es:[di], al             
                                
	inc di                      
	inc si                      
                                
	loop loopParseWord          
isStoppedSymbol:                
	mov al, endl          
	mov es:[di], al           
	inc si                      
 
								
	pop di                      
	pop cx                      
	pop ax                      
	ret                         
ENDP  
;*************************************************************************************************************************  
  
;*************************************************************************************************************************
convertInNumber PROC

	push ax
	push cx
	push bx
	push dx
	push si

	lea si, line
	call strlen
	mov cx, ax 
	
	xor ax, ax
	
newDeletLineSymbol:
	
	mov dx, 0Ah
	mul dx
	
	mov bl, ds:[si]
	sub bl, zero
	add ax, bx
	
	inc si
	loop newDeletLineSymbol
	
	mov deleteLine, ax
	
	pop si
	pop dx
	pop bx
	pop cx
	pop ax 

ENDP
;**************************************************************************************************************************  

;*************************************************************************************************************************
;ds:si - смещение, в котором находится начало строки
;Результат - в ax помещается длина строки 
strlen PROC                     
	push bx                     
	push si                     
	                            
	xor ax, ax                  
                                
    startCalc:                  
	    mov bl, ds:[si]         ;  Загружаем очередной символ строки из ds со смещением si
	    cmp bl, endl            ;  Сравниваем этот символ с символом конца строки
	    je endCalc              ;  Если это символ конца строки - прыгаем в endCalc и заканчиваем вычисления
                                
	    inc si                  
	    inc ax                                                                      
	    jmp startCalc           
	                            
    endCalc:                    
	pop si                      
	pop bx                      
	ret                         
ENDP  
;*************************************************************************************************************************

;**************************************************************************************************************************
skipLine PROC

	xor dx, dx
	dec si
	
skipSymbol:
	
	
	cmp al, 0 
	je endSkipLine 
	dec al
	dec reader
	inc si
	
	mov bl, ds:[si]
	cmp bl, 0Ah
	je newLine

clearSymbol:	
	cmp clearMode, 1
	je skipSymbol
	
enterSymbol:
    
	mov byte ptr ds:[di], bl
	inc reader
	inc di
	jmp skipSymbol

newLine:
    inc dx
	inc si
    cmp clearMode, 1
	je ggg
	mov byte ptr ds:[di], bl
	inc reader
	inc di
ggg:
    mov clearMode, 0
	
	
endSkipLine:
	
	ret
ENDP 
;*************************************************************************************************************************

;**************************************************************************************************************************
; переместить указатель чтения/записи      
setPosInFileTo MACRO symbolsInt, symbols;
	push ax                     
	push bx                     
	push cx                     
	push dx                     
                                
	mov ah, 42h                 
	xor al ,al 			        ; Обнуляем al, т.к. al=0 - код перемещения указателя в начало файла, al=1 текущая позиция, al=2 конец файла
	mov cx, symbolsInt          ; Обнуляем cx, 
	mov dx, symbols			    ; Обнуляем dx, т.е премещаем указатель на 0 символов от начала файла (cx*2^16)+dx 
	int 21h                     
                                
	pop dx                      
	pop cx                      
	pop bx                      
	pop ax                      
ENDM 
;******************************************************************************************************************************
 
;*************************************************************************************************************************  

mainProc PROC
    push ax                             
	push bx                             
	push cx                             
	push dx                             
	push si                             
	push di                             
                                        
	mov bx, sourceID                    ; Загружаем в bx ID файла-источника
	setPosInFileTo 0,0                  
 
readFromFileInBuffer:

	mov bx, sourceID                    
	setPosInFileTo stepReadedTotal, readedTotal                  
gg: 
    call readFromFile                   
	                                    
	cmp ax, 0                           ; Сравнивание ah с 0 для проверки на конец файла
	je finishProcessing                 ; Если ah == 0, то буффер пуст и мы дошли до конца файла
	
	mov reader, ax
	
	
	add readedTotal, ax
	adc stepReadedTotal, 0
	
	lea si, buffer                      
	lea di, buffer
	
checkDeleteLine:	
	mov cx, deleteLine
	cmp cx, numberOfLine
	je activationClearMode

skipLineProc:
	call skipLine
	
	cmp dx, 0
	je printBuffer
	
	add numberOfLine, dx
	jmp checkDeleteLine

activationClearMode:
	
	mov clearMode, 1					; Перевод в режим чистки
	mov numberOfLine, 0					; Сброс счетчика строк
	jmp skipLineProc

printBuffer:
	

	
	xor ax, ax
	xor dx, dx 
	
	cmp reader, 0
	je gg
	
	mov bx, sourceID                    
	setPosInFileTo stepWritedTotal, writedTotal                 
	
	
	
	
	mov ah, 40h
	mov bx, sourceID
	mov cx, reader
	lea dx, buffer
	int 21h
	
	mov ax, reader
	add writedTotal, ax
	adc stepWritedTotal, 0
	

	
	jmp readFromFileInBuffer
	
finishProcessing:  
                     
    mov bx, sourceID                                         
    setPosInFileTo stepWritedTotal, writedTotal         
                                        
    xor ax,ax                           
    mov ah, 40h                         
    mov bx, sourceID                    
    mov cx, 0h                          
    int 21h                             
                                        
	pop di                              
	pop si                              
	pop dx                              
	pop cx                              
	pop ax                              
	pop bx                              
	ret   

ENDP 
;*************************************************************************************************************************

;**************************************************************************************************************************
;Результат в ax - 0 если все хорошо
openFiles PROC                  
	push bx                     
	push dx                             
                                
	mov ah, 3Dh			        ; Функция 3Dh - открыть существующий файл 		00H - открыть файл только для ввода (чтение из файла);
	mov al, 02h			        ; Режим открытия файла							01H - открыть файл только для вывода (запись в файл);
	lea dx, sourcePath          ; Загружаем в dx название исходного файла		02H - открыть файл для ввода и вывода.
	mov cx, 00h			         
	int 21h                     
                                
	jb badOpenSource	        
                                
	mov sourceID, ax	        
                                
	mov ax, 0			        
	jmp endOpenProc		        
                                
badOpenSource:                  
	println badSourceText       
	cmp ax, 02h                 
	jne errorFound              
                                
	println fileNotFoundText    
                                
	jmp errorFound              
                                                         
errorFound:                     
	mov ax, 1                   
endOpenProc:                    
	pop dx                      
	pop bx                      
	ret                         
ENDP                            
;******************************************************************************************************************************        
 
;**************************************************************************************************************************
closeFiles PROC                 
	push bx                     
	push cx                     
                                
	xor cx, cx                  
                                
	mov ah, 3Eh                 
	mov bx, sourceID            ; В bx загружаем ID файла, подлежащего закрытию
	int 21h                     
                                
	jnb goodCloseOfSource		
                                
	println errorClosingSource        
	                            
	inc cx 			            
                                
goodCloseOfSource:                             
	mov ax, cx 		            
                                
	pop cx                      
	pop bx                      
	ret                         
ENDP                            

;*****************************************************************************************************************************
   
;*****************************************************************************************************************************   
;reads to buffer maxWordSize symbols
;Результат: в ax помещается колв-во считанных из файла символов
readFromFile PROC                   
	push bx                         
	push cx                         
	push dx                         
                                    
	mov ah, 3Fh                     
	mov bx, sourceID                ; В bx загружаем ID файла, из которого собираемся считывать
	mov cx, maxWordSize             ; В cx загружаем максимальный размер слов
	lea dx, buffer                  ; В dx загружаем смещения буффера, в который будет считывать данные из файла
	int 21h                         
                                    
	jnb goodRead					
                                    
	println errorReadSourceText    
	mov ax, 0                       
                                    
goodRead:                              
                                    
	pop dx                          
	pop cx                          
	pop bx                          
	ret                             
ENDP                                

;*******************************************************************************************************************************

end main