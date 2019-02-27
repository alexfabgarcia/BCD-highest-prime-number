; Projeto 2: Programa��o com PIC - Laborat�rio de Microcontroladores
; Escrever um programa que leia 3 d�gitos BCD da porta A, convertendo-os para
; um �nico n�mero bin�rio de 8 bits. Como sa�da o programa deve apresentar na
; porta B, em sequ�ncia, 1 d�gito BCD (para centena), seguido de 2 d�gitos BCD
; (dezenae unidade), correspondendo ao maior n�mero primo que seja menor ou
; igual ao valor de entrada. O programa deve ser desenvolvido e executado na
; plataforma MPlab (vers�o IDE X), usando o PIC16F873A.
;
; Autores: Alex Fabiano Garcia e Thales Marcel
; Data: 28/02/2019

    list   P=PIC16F873A	    ; Microcontrolador utilizado: PIC16F873A

; Arquivos inclu�dos no c�digo
    #include <p16f873a.inc>

; FUSE bits: Cristal, sem watchdog timer, com Power Up timer (72 ms) e 
; sem Code protection
	    __config _XT_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF

; Troca de banco de mem�ria
bank0	    macro ; Selecionar o banco 0 da mem�ria
	    bcf		STATUS, RP0
	    bcf		STATUS, RP1
	    endm

bank1	    macro ; Selecionar o banco 1 da mem�ria
	    bsf		STATUS, RP0
	    bcf		STATUS, RP1
	    endm

; Entradas
;    #define	

; Vetor de RESET
	    org		H'0000'	; Origem no endere�o de mem�ria 0000h
	    goto	inicio	; V� para label que indica inicio

; Vetor de Interrup��o
	    org		H'0004'	; Endere�o do vetor de interrup��o
	    retfie		; Retorna da interrup��o

; Programa principal
inicio:
	    ; Definir entrada e sa�da via TRISA e TRISB
	    bank1
	    movlw	H'FF'	    ; W = B'11111111', pois 1 = input
	    movwf	TRISA	    ; Porta A servir� como input	    
	    movlw	H'00'	    ; W = B'00000000', pois 0 = output
	    movwf	TRISB	    ; Porta B servir� como outinput
	    bank0
	    
	    movlw	H'FF'
	    movwf	PORTA

loop:
	    
	    ;goto	$	    ; Loop infinito (vai para posi��o atual $)

	    end