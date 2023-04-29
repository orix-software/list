; vim: set ft=asm6502-2 ts=8:

.feature labels_without_colons
.feature string_escapes
.macpack longbranch

;----------------------------------------------------------------------
;			cc65 includes
;----------------------------------------------------------------------
.include "telestrat.inc"
.include "fcntl.inc"
.include "stdio.inc"

;----------------------------------------------------------------------
;			Orix SDK includes
;----------------------------------------------------------------------
.include "SDK.mac"
.include "types.mac"
.include "errors.inc"

;----------------------------------------------------------------------
;				Imports
;----------------------------------------------------------------------
; From debug
.import PrintHexByte

; From sopt
.import spar1, sopt1, incr
.import calposp
.import inbuf
.importzp cbp
spar := spar1
sopt := sopt1

; From ermes
.import ermes

; From ermtb
.import ermtb

; From stop-or-cont
.import StopOrCont

.import __BSS_SIZE__, __BSS_LOAD__, __RAMEND__

;----------------------------------------------------------------------
;				Exports
;----------------------------------------------------------------------
.export _main
;.export _argc
;.export _argv

; Pour ermes
.export crlf1, out1, seter1
.export prfild, prnamd

.export drive
.exportzp xtrk, psec

;.export dskname

;----------------------------------------------------------------------
;			Librairies
;----------------------------------------------------------------------

;----------------------------------------------------------------------
; Defines / Constants
;----------------------------------------------------------------------
VERSION_MAJ = 1
VERSION_MIN = 0

TOKEN_REM := $9D
TOKEN_BANG := $C0

KERNEL_MAX_PATH_LENGTH = 49
max_path := KERNEL_MAX_PATH_LENGTH

; COMAL = 210
BASIC = 1
DEBUG = 1

;----------------------------------------------------------------------
;				Page Zéro
;----------------------------------------------------------------------
.pushseg
	.zeropage
		unsigned char xtrk
		unsigned char psec

		unsigned short extractptr
		unsigned short token_ptr

		; Pour les options
		unsigned long offset
	.if 0
		unsigned short line_start
		unsigned short line_end
	.endif
.popseg

;----------------------------------------------------------------------
;				Variables
;----------------------------------------------------------------------
.pushseg
	.segment "DATA"
		unsigned char drive
		unsigned char dskname[max_path]

		unsigned short fp

		unsigned short extractcnt

		unsigned char buffer[512]

		unsigned char xio
		unsigned char yio

		unsigned short token_tbl[256]

		unsigned char OPTIONS
		unsigned char token_code

		unsigned char xtokens

		; findline: poids fort pour la relocation
		;unsigned char reloc_offset
.popseg

;----------------------------------------------------------------------
; Variables et buffers
;----------------------------------------------------------------------
.segment "CODE"

;----------------------------------------------------------------------
;			Segments vides
;----------------------------------------------------------------------
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"

;----------------------------------------------------------------------
;				Programme
;----------------------------------------------------------------------
.segment "CODE"

.proc _main

		crlf

		; Adresse de la ligne de commande
		ldy	#<BUFEDT
		lda	#>BUFEDT

		; Saute le nom du programme
		sty	cbp
		sta	cbp+1

		ldy	#$ff
	loop:
		iny
		lda	(cbp),y
		clc
		beq	eol

		cmp	#' '
		bne	loop

	eol:
		; Ici si on a trouvé un ' ' => C=1
		tya
		ldy	cbp+1
		adc	cbp
		sta	cbp
		bcc	loop_end

		iny
	loop_end:
		sty	cbp+1


		; Initialise la table des pointeurs token
		jsr	loadtokens

		; Saute au premier paramètre
		ldy	#$00
		jsr	calposp

	getopt:
		jsr	sopt
		.asciiz	"CHT"
		bcs	error

		; -h?
		cpx	#%01000000
		bne	opt_token
		jmp	cmnd_help

	opt_token:
		stx	OPTIONS
		cpx	#%00100000
		bne	cmnd_exec
		jsr	cmnd_tokens
		bcs	error
		bcc	next

	cmnd_exec:
		ldy	#$00
		lda	(cbp),y
		cmp	#'@'
		bne	go
		; Mode batch activé
		jsr	openbatch
		; Décommenter la ligne suivante si on veut pouvoir utiliser les options
		; dans le fichier batch
		; Remarque: signifie qu'il faut préciser -c pour chaque fichier et non
		;           globalement
		;bcc	getopt
		bcc	cmnd_exec
		bcs	error

	go:
		ldy	cbp
		lda	cbp+1
		ldx	#%11000000
		jsr	spar
		.byte	offset, 0

		jsr	getfname
		bcs	error

		; Listing
		jsr	cmnd_list
		bcc	next
		; Si "No more entries" on a appuyer sur CTRL+C => fin
		cmp	#e20
		beq	end
		sec
		bcs	error

	next:
		;
		ldy	#$00
	skipcr:
		lda	(cbp),y
		beq	end
		cmp	#$0d
		bne	_calposp
		iny
		bne	skipcr
		; Si débordement de Y
		beq	errEOF

	_calposp:
		jsr	calposp
		; Décommenter la ligne suivante si on veut pouvoir utiliser les options
		; dans le fichier batch
		; Remarque: signifie qu'il faut préciser -c pour chaque fichier et non
		;           globalement
		;bne	getopt
		bne	cmnd_exec

	end:
		crlf
		rts

	errEOF:
		lda	#e4
		.byte	$2c

	errOpt:
	 	lda	#e15
		;sec

	error:
		pha
		jsr	cmnd_version
		pla
		sec
		jsr	ermes
		crlf
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;	diskname doit être rensigné
;
; Sortie:
;	A: Code erreur
;	C:0 -> Ok, 1-> Erreur
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		dskname
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_list

		crlf

		; Ouverture du fichier
		fopen	dskname, O_RDONLY
		sta	fp
		stx	fp+1

		eor	fp+1
		jeq	errFopen

		; Calcul de l'adresse du tampon pour l'extraction
		lda	#$00
		sta	extractptr
		sta	extractcnt
		ldy	#>( (__BSS_LOAD__ + __BSS_SIZE__) & $ff00)
		lda	#<( (__BSS_LOAD__ + __BSS_SIZE__) )
		beq	*+3
		iny
		sty	extractptr+1

		; Calcul de la taille du tampon pour l'extraction
		sec
		lda	#>__RAMEND__
		sbc	extractptr+1
		sta	extractcnt+1

		; ldx	#SEEK_CUR
		; lda	offset
		; ldy	offset+1
		; BRK_KERNEL	XFSEEK
		; cmp	#CH376_USB_INT_SUCCESS
		; beq	seek_ok
		lda	#$00
		sta	offset+2
		sta	offset+3
		fseek	fp, offset, #SEEK_CUR
		beq	seek_ok

	eof:
		; EOF Error
		lda	#e4
		sec
		jcs	error

	seek_ok:
		; Chargement du fichier
		fread	(extractptr), (extractcnt), 1, fp
		fclose	(fp)

		; Affiche le nom du fichier
		bit	OPTIONS
		bpl	no_color
		print	title_color

	no_color:
		print	dskname
		crlf
		crlf

		; On ne prend que l'octet faible du déplacement
		; Saute le premier octet pour les programmes Basic sur FTDOS
		; Initialiser Y=$FF pour Sedoric
	.if 0
		lda	#$64
		sta	line_start
		lda	#$00
		sta	line_start+1
		jsr	findline
		jcs	error
	.endif
		; Offset +xxx?
	;	ldy	offset
	;	bne	ligne
		; Non, premier octet du programme nul? (fichier basic ftdos)
		ldy	#$00
		lda	(extractptr),y
		beq	ligne
		; Non, il s'agit d'un programme basic sedoric
		dey

	ligne:
		; Début d'une ligne
		; Saute le poids faible du lien
		iny

		; Poids fort du lien nul?
		iny
		lda	(extractptr),y
		jeq	end

		; Changement de couleur pour les n° de ligne
		bit	OPTIONS
		bpl	disp_line_number
		sty	yio
		print	linenumber_color
		ldy	yio

		; Affiche le n° de ligne
	disp_line_number:
		iny
		lda	(extractptr),y
		pha
		iny
		lda	(extractptr),y
	;	iny
		sty	yio
		tay
		pla
		ldx	#$03
		print_int

		; -C?
		bit	OPTIONS
		bmi	end_color
		; Ajoute un espace après le n° de ligne
		print	#' '
		jmp	get_line
	end_color:
		; Remet la couleur line_color
		print	line_color

	get_line:
		ldy	yio
		ldx	#$00

	loop:
		iny
		lda	(extractptr),y
		sta	buffer,x
		beq	affiche
		bmi	token
		inx
		bne	loop

	errOOM:
		lda	#e1
		sec
		rts

	token:
		stx	xio
		sty	yio
	;	and	#$7f
		; Conserve le token pour -C
		sta	token_code

		; [ -C?
		bit	OPTIONS
		bpl	addtoken
		; On veut la couleur AVANT le token
		clc
		jsr	addcolor
		beq	errOOM

	addtoken:
		; Offset dans la table
		asl
		tax
		lda	token_tbl,x
		sta	token_ptr
		inx
		lda	token_tbl,x
		sta	token_ptr+1

		ldx	xio
		ldy	#$ff
	token_loop:
		iny
		lda	(token_ptr),y
		bmi	token_end
		sta	buffer,x
		inx
		bne	token_loop
		beq	errOOM

	token_end:
		and	#$7f
		sta	buffer,x
		ldy	yio
	;	iny
		inx


		; [ -C?
		bit	OPTIONS
		bpl	loop
		lda	token_code
		; On veut la couleur APRES le token
		sec
		jsr	addcolor
		bne	loop
		; -]
		beq	errOOM

	affiche:
		clc
		tya
		adc	extractptr
		sta	extractptr
		lda	#$00
		adc	extractptr+1
		sta	extractptr+1

	;	ldy	#<buffer
	;	lda	#>buffer
		print	buffer
		crlf
		jsr	StopOrCont
		bcs	abort
		ldy	#$00
		jeq	ligne

	end:
		clc
		rts

	abort:
		; No more entries
		lda	#e20
		sec
		rts

	errFopen:
		lda	#e13
		sec

	error:
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_help
		print	helpmsg
		clc
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_version
        prints  "list version 1.1 - 2023.2\r\n"
        rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		token_ptr
;		xtokens
;		buffer
;
;	Utilisées:
;		-keywords
; Sous-routines:
;	PrintHexByte
;	StopOrCount
;----------------------------------------------------------------------
.ifdef DEBUG
	.proc cmnd_tokens

			print	tokens_title

			lda	#<keywords
			ldy	#>keywords
			sta	token_ptr
			sty	token_ptr+1

			lda	#':'
			sta	buffer
			lda	#' '
			sta	buffer+1

			; Code premier token-1 <=> $80-1
			lda	#$7F
			sta	xtokens

		next:
			inc	xtokens
			ldx	#$02
			ldy	#$00

			; Ajoute la couleur avant le token
			clc
			lda	xtokens
			jsr	addcolor

		loop:
			lda	(token_ptr),y
			sta	buffer,x
			beq	end
			bmi	display
			iny
			inx
			bne	loop
			beq		error

		display:
			and	#$7f
			sta	buffer,x

			; Ajoute la couleur après le token
			lda	xtokens
			inx
			sec
			jsr	addcolor
			; Ajuste X à cause du inx suivant
			dex

			; AJoute un nul à la fin du buffer
			lda	#$00
			inx
			sta	buffer,x

			iny
			tya
			clc
			adc	token_ptr
			sta	token_ptr
			lda	#$00
			adc	token_ptr+1
			sta	token_ptr+1

			print	#' '

			; AJoute la couleur apres le token
			lda	xtokens
		;	sec
		;	jsr	addcolor
			jsr	PrintHexByte

			print	buffer
			crlf

			jsr	StopOrCont
			bcc	next

		end:
			clc
			rts

		error:
			lda	#e4
			sec
			rts
	.endproc

.else
	.proc cmnd_tokens
			lda	#e15
			sec
			rts
	.endproc

.endif


;----------------------------------------------------------------------
;
; Entrée:
;	YA: Adresse de la table des tokens
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	inittable
;----------------------------------------------------------------------
.proc loadtokens
		; Chargement de la table des tokens
		;sta token_ptr
		;sty token_ptr+1
		; ...

		; [ simulation
		ldx	#(TOKEN_REM & $7f)
		; lda #(('A'-'@') | $08) + (('D'-'@') | $08)<<4
		lda	#(('C'-'@')<<4)
		; lda #('C'-'@')
		sta	color_tbl,x

		;ldx #(TOKEN_BANG & $7f)
		;lda #( ('A'-'@') | (('G'-'@') << 4) )
		;sta color_tbl,x
		; ]

		; Initialisation de token_tbl
		jsr	inittable

		clc
		rts
.endproc


;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
; Créé une table avec l'adresse de début de chaque token dans la table
; keywords (pour éviter de faire une recherche et un comptage à chaque
; fois qu'on veut afficher une instruction)
;----------------------------------------------------------------------
; TODO: Charger la table keywords depuis un fichier
;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;	A,X,Y: Modifiés
;
; Variables:
;	Modifiées:
;		token_tbl
;		token_ptr
;
;	Utilisées:
;		keywords
;
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc inittable

		lda	#<keywords
		ldy	#>keywords
		sta	token_ptr
		sty	token_ptr+1

		ldx	#$00
		sta	token_tbl,x
		inx
		tya
		sta	token_tbl,x

	next:
		ldy	#$00
	loop:
		lda	(token_ptr),y
		beq	end
		bmi	update
		iny
		bne	loop

	update:
		iny
		inx
		tya
		clc
		adc	token_ptr
		sta	token_ptr
		sta	token_tbl,x
		lda	#$00
		adc	token_ptr+1
		sta	token_ptr+1
		inx
		sta	token_tbl,x
		bne	next

	end:
		clc
		rts

.endproc


;----------------------------------------------------------------------
;
; Entrée:
;	A: Token
;	C: 0->Quartet faible, 1->Quartet fort
;
; Sortie:
;	A,X,Y: inchangés
;	C: 0
;
; Variables:
;	Modifiées:
;		xio
;		buffer
;
;	Utilisées:
;		color_tbl
;
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc addcolor
		; Sauvegarde A, X
		pha
		stx	xio

		and	#$7f
		tax
		lda	color_tbl,x
		bcc	color
		lsr
		lsr
		lsr
		lsr

	color:
		; Couleur == 0?
		and	#$0f
		beq	end

		ldx	xio
		pha
		lda	#$1b
		sta	buffer,x
		inx
		pla
		; Paper ou Ink?
		cmp	#$08
		bcc	ink
		adc	#('P'-'@'-8-1)
	ink:
		adc	#'@'
		sta	buffer,x
		inx
		stx	xio

	end:
		; Restaure A,X
		pla
		ldx	xio
		cpx	#$ff
		clc
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;	extractptr: pointeur vers le début du programme
; Sortie:
;
; Variables:
;	Modifiées:
;		TMP
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.if 0
	.proc findline
			ldy	#$01

			; Poids du lien = 0?
			lda	(extractptr),y
			beq	errEOF

			; Sauvegarde l'octet de poids fort du lien pour l'ajustement
			sta	reloc_offset

			iny
			iny
			; Poids fort du numéro de la ligne
			lda	line_start+1
			cmp	(extractptr),y
			bcc	errEOF
			beq	L2635

			dey
			bne	L263E

			; Vérification du poids faible
		L2635:
			lda	line_start
			dey
			cmp	(extractptr),y
			bcc	errEOF
			beq	found

			; Passe à la ligne suivante
		L263E:
			dey
			lda	(extractptr),y
			pha
			; Poids faible
			dey
			lda	(extractptr),y
			; Mise à jour du pointeur
			sta	extractptr
			pla
			sta	extractptr+1
			bne	findline

		found:
			clc
			rts

		errEOF:
			lda	#e4
			sec
			rts
	.endproc
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc openbatch
		; Mode batch activé
		ldx	#cbp
		jsr	incr
		ldy	cbp
		lda	cbp+1
		jsr	getfname
		bcs	errFopen

		; rempli de buffer de $00 au cas où...
		lda	#$00
		ldx	#80
	loop:
		sta	inbuf,x
		dex
		bpl	loop

		fopen	dskname, O_RDONLY
		sta	fp
		stx	fp+1

		eor	fp+1
		beq	errFopen

		; TODO: Tester le code de retour de fread
		fread	inbuf, #79, 1, fp
		fclose	(fp)

		; Remplace les $0a par des $0d
		ldx	#81
	loop1:
	 	dex
		beq	go_batch
		lda	inbuf,x
		cmp	#$0a
		bne	loop1
		lda	#$0d
		sta	inbuf,x
		bne	loop1

	go_batch:
		ldy	#<inbuf
		lda	#>inbuf
		;bne	getopt			; >inbuf ne peut pas être nul

		; Met à jour cbp
		sty	cbp
		sta	cbp+1
		clc
		rts

	errFopen:
		lda	#e13
		sec

	error:
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc getfname
		; AY : adresse du paramètre suivant
		; cbp:   ''          ''
		;sty dskname
		;sta dskname+1

		ldy	#$ff
	loop:
		iny
		lda	(cbp),y
		sta	dskname,y
		beq	endloop
		cmp	#$0d
		beq	endloop
		cmp	#' '
		bne	loop

	endloop:
		cpy	#00
		beq	error_no_filename

		; Termine la chaîne par un nul
		;	cmp	#$00
		;	beq	ajuste

		lda	#$00
		;sta	(cbp),y
		sta	dskname,y
		;iny

		; Ajuste cbp
		;ajuste:
		;	clc
		;	tya
		;	adc	cbp
		;	sta	cbp
		;	bcc	skip
		;	inc	cbp+1
		;
		;skip:
		;	clc
		jsr	calposp
		rts

	error_no_filename:
		lda	#e12
		sec
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;	AY: Position (A=LSB)
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
;.proc seek
;	ldx	#CH376_BYTE_LOCATE
;	stx	CH376_COMMAND
;	sta	CH376_DATA
;	sty	CH376_DATA
;	lda	#$00
;	sta	CH376_DATA
;	sta	CH376_DATA
;
;	jsr	WaitResponse
;.endproc

;===========================================================================
;		Gestion des erreurs
;===========================================================================
.segment "CODE"

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc crlf1
		crlf
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc out1
		cputc
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prfild
		print dskname
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prnamd
		print dskname
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
seter:
seter1:
		rts

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc svzp
		rts
.endproc

;**********************************************************************
; Fin du programme
;**********************************************************************

;----------------------------------------------------------------------
;				DATAS
;----------------------------------------------------------------------
.segment "RODATA"

	helpmsg:
		.byte "\r\n"
		;         123456789.123456789.123456789.123456789
		.byte $1b,"C           List utility\r\n\n"
		.byte " ",$1b,"TSyntax:",$1b,"P\r\n"
		.byte "    list",$1b,"A-h",$1b,"G\r\n"

		.ifdef DEBUG
			.byte "    list",$1b,"A-t",$1b,"G\r\n"
		.endif

		.byte "    list",$1b,"B[-c] [+offset]",$1b,"Afile\x1bB[file...]\r\n"
		.byte "    list",$1b,"B[-c]\x1bA@file\r\n"
		.byte "\n"
		.byte $00

	.ifdef DEBUG
		tokens_title:
			;             123456789.123456789.123456789.123456789.
			.byte "\r\n"
			.byte "             \x1bTToken list\x1bP"
			.byte "\r\n\n"
			.byte $00
	.endif

;----------------------------------------------------------------------
;				TABLES
;----------------------------------------------------------------------


;----------------------------------------------------------------------
;			Tables par défaut
; Peut être écrasé par le chargement d'un fichier
; A mettre dans un segment BSS à charger depuis un fichier
;----------------------------------------------------------------------
.segment "DATA"

	.macro string80 arg
		; Place la chaine avec le bit7 du dernier caratère à 1
		.repeat .strlen(arg)-1, I
			.byte .strat(arg,I)
		.endrepeat
		.byte .strat(arg,.strlen(arg)-1) ^ $80
	.endmacro


	title_color:
		.asciiz "\x1bT"

	linenumber_color:
		 .asciiz "\x1bB"
		;.asciiz "\x1bR"

	line_color:
		 .asciiz "\x1bG"
		;.asciiz "\x1bP"

	color_tbl:
		; Quartet faible: couleur avant le token
		; Quartet fort  : couleur après le token
		.res $80,0

	keywords:
	.if .defined(BASIC)
		string80 "END"
		string80 "EDIT"
		string80 "STORE"
		;	string80 "INVERSE"
		string80 "RECALL"
		;	string80 "NORMAL"
		string80 "TRON"
		;	string80 "QUIT"
		string80 "TROFF"
		;	string80 "ERROR"
		string80 "POP"
		string80 "PLOT"
		string80 "PULL"
		string80 "LORES"
		string80 "DOKE"
		string80 "REPEAT"
		string80 "UNTIL"
		string80 "FOR"
		string80 "LLIST"
		string80 "LPRINT"
		string80 "NEXT"
		string80 "DATA"
		string80 "INPUT"
		string80 "DIM"
		string80 "CLS"
		string80 "READ"
		string80 "LET"
		string80 "GOTO"
		string80 "RUN"
		string80 "IF"
		string80 "RESTORE"
		string80 "GOSUB"
		string80 "RETURN"
		string80 "REM"
		string80 "HIMEM"
		string80 "GRAB"
		string80 "RELEASE"
		string80 "TEXT"
		string80 "HIRES"
		string80 "SHOOT"
		string80 "EXPLODE"
		string80 "ZAP"
		string80 "PING"
		string80 "SOUND"
		string80 "MUSIC"
		string80 "PLAY"
		string80 "CURSET"
		string80 "CURMOV"
		string80 "DRAW"
		string80 "CIRCLE"
		string80 "PATTERN"
		string80 "FILL"
		string80 "CHAR"
		string80 "PAPER"
		string80 "INK"
		string80 "STOP"
		string80 "ON"
		string80 "WAIT"
		string80 "CLOAD"
		string80 "CSAVE"
		string80 "DEF"
		string80 "POKE"
		string80 "PRINT"
		string80 "CONT"
		string80 "LIST"
		string80 "CLEAR"
		string80 "GET"
		string80 "CALL"
		string80 "!"
		string80 "NEW"

		; Instructions secondaires
		string80 "TAB("
		string80 "TO"
		string80 "FN"
		string80 "SPC("
		string80 "@"
		string80 "AUTO"
		string80 "ELSE"
		string80 "THEN"
		string80 "NOT"
		string80 "STEP"

		; Opérateurs mathématiques et logiques
		string80 "+"
		string80 "-"
		string80 "*"
		string80 "/"
		string80 "^"
		string80 "AND"
		string80 "OR"
		string80 ">"
		string80 "="
		string80 "<"

		; Fonctions
		string80 "SGN"
		string80 "INT"
		string80 "ABS"
		string80 "USR"
		string80 "FRE"
		string80 "POS"
		string80 "HEX$"
		string80 "&"
		string80 "SQR"
		string80 "RND"
		string80 "LN"
		string80 "EXP"
		string80 "COS"
		string80 "SIN"
		string80 "TAN"
		string80 "ATN"
		string80 "PEEK"
		string80 "DEEK"
		string80 "LOG"
		string80 "LEN"
		string80 "STR$"
		string80 "VAL"
		string80 "ASC"
		string80 "CHR$"
		string80 "PI"
		string80 "TRUE"
		string80 "FALSE"
		string80 "KEY$"
		string80 "SCRN"
		string80 "POINT"
		string80 "LEFT$"
		string80 "RIGHT$"
		string80 "MID$"

		.byte $00

	.elseif .defined(BASIC_lower)
		string80 "end"
		string80 "edit"
		string80 "store"
		string80 "recall"
		string80 "tron"
		;	string80 "quit"
		string80 "troff"
		;	string80 "error"
		string80 "pop"
		string80 "plot"
		string80 "pull"
		string80 "lores"
		string80 "doke"
		string80 "repeat"
		string80 "until"
		string80 "for"
		string80 "llist"
		string80 "lprint"
		string80 "next"
		string80 "data"
		string80 "input"
		string80 "dim"
		string80 "cls"
		string80 "read"
		string80 "let"
		string80 "goto"
		string80 "run"
		string80 "if"
		string80 "restore"
		string80 "gosub"
		string80 "return"
		string80 "rem"
		string80 "himem"
		string80 "grab"
		string80 "release"
		string80 "text"
		string80 "hires"
		string80 "shoot"
		string80 "explode"
		string80 "zap"
		string80 "ping"
		string80 "sound"
		string80 "music"
		string80 "play"
		string80 "curset"
		string80 "curmov"
		string80 "draw"
		string80 "circle"
		string80 "pattern"
		string80 "fill"
		string80 "char"
		string80 "paper"
		string80 "ink"
		string80 "stop"
		string80 "on"
		string80 "wait"
		string80 "cload"
		string80 "csave"
		string80 "def"
		string80 "poke"
		string80 "print"
		string80 "cont"
		string80 "list"
		string80 "clear"
		string80 "get"
		string80 "call"
		string80 "!"
		string80 "new"

		; instructions secondaires
		string80 "tab("
		string80 "to"
		string80 "fn"
		string80 "spc("
		string80 "@"
		string80 "auto"
		string80 "else"
		string80 "then"
		string80 "not"
		string80 "step"

		; opérateurs mathématiques et logiques
		string80 "+"
		string80 "-"
		string80 "*"
		string80 "/"
		string80 "^"
		string80 "and"
		string80 "or"
		string80 ">"
		string80 "="
		string80 "<"

		; fonctions
		string80 "sgn"
		string80 "int"
		string80 "abs"
		string80 "usr"
		string80 "fre"
		string80 "pos"
		string80 "hex$"
		string80 "&"
		string80 "sqr"
		string80 "rnd"
		string80 "ln"
		string80 "exp"
		string80 "cos"
		string80 "sin"
		string80 "tan"
		string80 "atn"
		string80 "peek"
		string80 "deek"
		string80 "log"
		string80 "len"
		string80 "str$"
		string80 "val"
		string80 "asc"
		string80 "chr$"
		string80 "pi"
		string80 "true"
		string80 "false"
		string80 "key$"
		string80 "scrn"
		string80 "point"
		string80 "left$"
		string80 "right$"
		string80 "mid$"

		.byte $00

	.elseif .defined(COMAL)


		string80 "END." ; Token: $80
		string80 "FOR"
		string80 "ENDFOR"
		string80 "DATA"
		string80 "INPUT"
		string80 "DELETE"
		string80 "DIM"
		string80 "READ"
		string80 "RENUMBER"
		.if COMAL < 2
			.byte $81
		;	string80 "QUIT"
			.byte $81
			.byte $81
		.else
			string80 "INVERSE"
			string80 "CLS"
			string80 "ON:"
		.endif
		string80 "CALL"
		.byte $81
		.byte $81
		.byte $81

		string80 "ELSE" ; Token: $90
		.byte $81
		string80 "ENDWHILE"
		string80 "WHILE"
		string80 "DO:"
		string80 "UNTIL"
		.if COMAL < 2
			.byte $81
		.else
			string80 "DOS"
		.endif
		.byte $81
		string80 "PROC"
		string80 "EXEC:"
		string80 "ENDPROC"
		.if COMAL < 2
			.byt $81
			.byt $81
			.byt $81
			.byt $81
			.byt $81
		.else
			string80 "CREATE"
			string80 "OPEN"
			string80 "CLOSE"
			string80 "DEL"
			string80 "CHAIN"
		.endif
		.byte $81				; Token: $A0
		string80 ":="
		.byte $81
		.if COMAL < 2
			.byte $81
		.else
			string80 "AUTO"
		.endif
		string80 "LABEL:"
		string80 "ONERR"
		string80 "RESUME"
		string80 "REPEAT"
		string80 "ENDIF"
		.byte $81
		string80 "ENDCASE"
		string80 "GOTO"
		string80 "RUN"
		string80 "IF"
		string80 "RESTORE"
		.byte $81

		string80 "CASE"		; Token: $B0
		string80 "OTHERWISE"
		string80 "//"		; Token: $B2
		string80 "STOP"
		string80 "WHEN"
		string80 "WAIT"
		string80 "LOAD"
		string80 "SAVE"
		string80 "DEF"
		string80 "POKE"
		string80 "PRINT"
		string80 "CONT"
		string80 "LIST"
		string80 "CLEAR"
		string80 "GET"
		string80 "NEW"
		;
		; Table des mots clé secondaires
		;
		string80 "TAB("		; Token: $C0
		string80 "TO"
		string80 "FN"
		string80 "SPC("
		string80 "THEN"
		.byte $81
		string80 "NOT"
		string80 "STEP"
		;
		; Table des opérateurs
		;
		string80 "+"
		string80 "-"
		string80 "*"
		string80 "/"
		string80 "^"		; "^"
		string80 "AND"
		string80 "OR"
		string80 ">"

		string80 "="		; Token: $D0
		string80 "<"
		;
		; Table des fonctions
		;
		string80 "SGN"
		string80 "INT"
		string80 "ABS"
		string80 "USR"
		string80 "FRE"
		.byte $81
		;	string80 "IN$"	; Instruction on présente
		string80 "POS"
		string80 "RES"
		string80 "SQR"
		string80 "RND"
		string80 "LOG"
		string80 "EXP"
		string80 "COS"
		string80 "SIN"

		string80 "TAN"		; Token: $E0
		string80 "ATN"
		string80 "PEEK"
		string80 "LEN"
		string80 "STR$"
		string80 "VAL"
		string80 "ASC"
		string80 "CHR$"
		string80 "LEFT$"
		string80 "RIGHT$"
		string80 "MID$"		; Token: $EA
		.byte $00		; Fin de la liste des mots clé
	.endif
