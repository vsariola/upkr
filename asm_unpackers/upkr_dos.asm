; supports only the bitstream data (add -b to command line when packing)
; put the packed intro into data.bin
prog_start     equ 0xC000
prog_len       equ 0x30FF ; must end 0xFF
probs          equ prog_start+prog_len

org prog_start

entrypoint:    ; this is will be loaded at 0x100, but relocates the code and data to prog_start
     push si   ; si points to 0x100 = start of code, save it and return to it later with ret
     pusha
     push si
     mov  di, prog_start
     mov  ch, prog_len >> 8
     rep  movsb
     push upkr_unpack
     ret

upkr_unpack:
     xchg ax, dx                             ; upkr_state = 0;
     mov  al, 128                            ; for(int i = 0; i < sizeof(upkr_probs); ++i) upkr_probs[i] = 128;
     mov  ch, 2                              ; cx = 0x0200
     rep  stosb
     pop  di                                 ; u8* write_ptr = (u8*)destination;
     xor  si, si                             ; upkr_data_ptr = (u8*)compressed_data;
     .mainloop:
     xor  bx, bx
     call upkr_decode_bit
     jnc  .else                              ; if(upkr_decode_bit(0)) {
     mov  bh, 1
     test bp, bp                             ; if(prev_was_match || upkr_decode_bit(256)) {
     jnz  .skip_call
     call upkr_decode_bit
     jnc  .skipoffset
     .skip_call:
     mov  bl, 1
     call upkr_decode_length                 ;  offset = upkr_decode_length(257) - 1;
     dec  cx
     jnz  .notdone                           ; if(offset == 0)
     popa
     ret
     .notdone:
     mov  [.mutant], cx
     .skipoffset:
     mov  bl, 257+64-256                     ; int length = upkr_decode_length(257 + 64);
     call upkr_decode_length
     push si
     mov  si, di
     sub  si, 0x4242
     .mutant equ $-2
     rep  movsb                              ; *write_ptr = write_ptr[-offset];
     pop  si
     jmp  .mainloop
     .else:
     mov  bx, 1                              ; int byte = 1;
     .byteloop:
     call upkr_decode_bit                    ; int bit = upkr_decode_bit(byte);
     adc  bl, bl                             ; byte = (byte << 1) + bit;
     jnc  .byteloop
     xchg ax, bx
     stosb
     xor  bp, bp                             ;  prev_was_match = 0;
     jmp  .mainloop


; parameters:
;    bx = context_index
;    dx = state
;    si = bit position in input stream
; returns:
;    bx = context_index
;    dx = new state
;    si = new bit position in input stream
;    carry = bit
;    trashes ax
upkr_decode_bit:
     push cx
     cmp  dx, 0x8000
     jae  .skiploop                          ; TODO: get rid of this extremely stupid loop formulation
     .bitloop:                               ; while(upkr_state < 32768)
     bt   [data], si
     inc  si
     adc  dx, dx
     jns  .bitloop
.skiploop:
     movzx ax, byte [probs+bx]               ; int prob = upkr_probs[context_index]; TODO: can we assume ch = 0?
     push ax                                 ; save prob
     cmp  dl, al                             ; int bit = (upkr_state & 255) < prob ? 1 : 0; (carry = bit)
     pushf                                   ; save bit flags
     jc   .bit                               ; (skip if bit)
          neg  al                            ;   tmp = 256 - tmp;
     .bit:
     push ax
     mul  dh                                 ; upkr_state = tmp * (upkr_state >> 8) + (upkr_state & 255);
     mov  dh, 0
     add  dx, ax
     mov  ax, 256+8                          ; tmp += (256 - tmp + 8) >> 4;
     pop  cx
     sub  ax, cx
     shr  ax, 4
     add  ax, cx
     popf
     pop  cx
     jc   .bit2                              ; (skip if bit)
          pushf
          sub  dx, cx                        ;    upkr_state -= prob;
          neg  al                            ;    tmp = 256 - tmp;
          popf
     .bit2:
     mov  [probs+bx], al                     ; upkr_probs[context_index] = tmp;
     pop  cx
     ret                                     ; flags = bit

; parameters:
;    bx = context_index
; returns:
;    cx = length
;    bp > 0
; trashes bl
upkr_decode_length:
     xor  cx, cx                             ; int length = 0;
     mov  bp, 1                              ; int bit_pos = 1;
     .loop:
     call upkr_decode_bit
     jnc  .end                               ; while(upkr_decode_bit(context_index)) {
          inc  bx                            ; context_index++
          call upkr_decode_bit
          jnc  .notlengthbit
          add  cx, bp                        ; length |= bit_pos
          .notlengthbit:
          add  bp, bp                        ; bit_pos <<= 1
          inc  bx                            ; context_index++
          jmp  .loop
     .end:
     add  cx, bp                             ; length |= bitpos (highest bit)
     ret

data:
     incbin "data.bin"