; upkr dos unpacker - the no-repeated-offsets version
; add --x86b to command line when packing
; put the packed intro into data.bin
max_len        equ 16384
prog_start     equ (0x100+max_len+510+relocation-upkr_unpack)
probs          equ (((prog_start+max_len+510)+255)/256)*256

org 0x100

relocation:    ; this is will be loaded at 0x100, but relocates the code and data to prog_start
     push si   ; si points to 0x100 = start of code, save it and return to it later with ret
     pusha
     push si
     mov  di, prog_start
     mov  ch, max_len/512
     rep  movsw
     jmp si

upkr_unpack:
     xchg ax, bp                 ; position in bitstream = 0
     cwd                     ; upkr_state = 0;
     xchg cx, ax                 ; cx > 0x0200
     mov  al, 128                ; for(int i = 0; i < sizeof(upkr_probs); ++i) upkr_probs[i] = 128;
     rep  stosb
     pop  di                     ; u8* write_ptr = (u8*)destination;
     .mainloop:
      mov  bx, probs
      call upkr_decode_bit
      jnc  .else                 ; if(upkr_decode_bit(0)) {
           inc  bh
           call upkr_decode_length       ;  offset = upkr_decode_length(258) - 1;
           loop .notdone             ; if(offset == 0)
            popa
            clc
            ret
           .notdone:
           mov  si, di
           .sub:
            dec  si
           loop .sub
           mov  bl, 128          ; int length = upkr_decode_length(384);
           call upkr_decode_length
           rep  movsb            ; *write_ptr = write_ptr[-offset];
           jmp  .mainloop
      .else:
           inc  bx
           .byteloop:
            call upkr_decode_bit     ; int bit = upkr_decode_bit(byte);
            adc  bl, bl          ; byte = (byte << 1) + bit;
            jnc  .byteloop
           xchg ax, bx
           stosb
           jmp  .mainloop            ;  prev_was_match = 0;


; parameters:
;    bx = context_index
;    dx = state
;    bp = bit position in input stream
; returns:
;    bx = context_index+1
;    dx = new state
;    bp = new bit position in input stream
;    carry = bit
upkr_load_bit:
     bt   [compressed_data-relocation+prog_start], bp
     inc  bp
     adc  dx, dx
upkr_decode_bit:
     inc  dx
     dec  dx ; or whatever other test for the top bit there is
     jns  upkr_load_bit
     movzx ax, byte [bx]             ; int prob = upkr_probs[context_index]
     push ax                     ; save prob
     cmp  dl, al                 ; int bit = (upkr_state & 255) < prob ? 1 : 0; (carry = bit)
     pushf                   ; save bit flags
     jc   .bit                   ; (skip if bit)
      neg  al                ;   tmp = 256 - tmp;
     .bit:
     mov  [bx], al               ; tmp += (256 - tmp + 8) >> 4;
     neg  byte [bx]
     shr  byte [bx],4
     adc  [bx], al               ; upkr_probs[context_index] = tmp;
     mul  dh                     ; upkr_state = tmp * (upkr_state >> 8) + (upkr_state & 255);
     mov  dh, 0
     add  dx, ax
     popf
     pop  ax
     jc   .bit2                  ; (skip if bit)
      neg  byte [bx]             ;    tmp = 256 - tmp;
      sub  dx, ax                ;    upkr_state -= prob; note that this will also leave carry always unset, which is what we want
     .bit2:
     ret                     ; flags = bit

; parameters:
;    bx = context_index
;    bp = bit position in input stream
; returns:
;    cx = length
; trashes bl, ax
upkr_decode_length_loop:
      inc  bx
      call upkr_decode_bit
upkr_decode_length:
      rcr  cx, 1
      inc  bx
      call  upkr_decode_bit
      jnc  upkr_decode_length_loop       ; while(upkr_decode_bit(context_index)) {
     .loop2:
      rcr  cx, 1
      jnc  .loop2
     ret

compressed_data:
    incbin "data.bin"                ; nasm syntax