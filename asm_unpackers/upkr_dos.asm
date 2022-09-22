; supports only the bitstream data (add -b to command line when packing)
; put the packed intro into data.bin
prog_start     equ 0xC001 ; must end with 0x01
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
     inc  si                                 ; upkr_data_ptr = (u8*)compressed_data;
     .mainloop:
          mov  bx, probs
          call upkr_decode_bit
          jnc  .else                         ; if(upkr_decode_bit(0)) {
               mov  bh, probs+256 >> 8
               test cx, cx                   ; if(prev_was_match || upkr_decode_bit(257)) {
               jz   .skip_call
               call upkr_decode_bit
               jnc  .skipoffset
                    .skip_call:
                    call upkr_decode_length  ;  offset = upkr_decode_length(258) - 1;
                    dec  cx
                    jnz  .notdone            ; if(offset == 0)
                         popa
                         ret
                    .notdone:
                    mov  bp, cx
               .skipoffset:
               mov  bl, 128                  ; int length = upkr_decode_length(384);
               call upkr_decode_length
               push si
               mov  si, di
               sub  si, bp
               rep  movsb                    ; *write_ptr = write_ptr[-offset];
               pop  si
               jmp  .mainloop
          .else:
               inc bx
               .byteloop:
                    call upkr_decode_bit     ; int bit = upkr_decode_bit(byte);
                    adc  bl, bl              ; byte = (byte << 1) + bit;
                    jnc  .byteloop
               xchg ax, bx
               stosb
               inc  cx                       ;  prev_was_match = 0;
               jmp  .mainloop


; parameters:
;    bx = context_index
;    dx = state
;    si = bit position in input stream
; returns:
;    bx = context_index+1
;    dx = new state
;    si = new bit position in input stream
;    carry = bit
;    trashes ax
upkr_decode_bit_bxplus1:
     inc  bx
upkr_decode_bit:
     push cx
     shr  dx, 1                              ; for the first round, the shr cancels the adc dx, dx and we just check the sign of dx
     jmp  .looptest
     .bitloop:                               ; while(upkr_state < 32768)
          bt   [data-(prog_len+1+0x100)/8], si
          inc  si
          .looptest:
          adc  dx, dx
          jns  .bitloop
     movzx ax, byte [bx]                     ; int prob = upkr_probs[context_index]
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
          neg  al                            ;    tmp = 256 - tmp;
          sub  dx, cx                        ;    upkr_state -= prob; note that this will also leave carry always unset, which is what we want
     .bit2:
     mov  [bx], al                           ; upkr_probs[context_index] = tmp;
     pop  cx
     ret                                     ; flags = bit

; parameters:
;    bx = context_index
; returns:
;    cx = length
;    bp > 0
; trashes bl
upkr_decode_length:
     push bp
     xor  cx, cx                             ; int length = 0;
     mov  bp, 1                              ; int bit_pos = 1;
     .loop:
          call upkr_decode_bit_bxplus1
          jnc  .end                          ; while(upkr_decode_bit(context_index)) {
          call upkr_decode_bit_bxplus1
          jnc  .notlengthbit
          add  cx, bp                        ; length |= bit_pos
          .notlengthbit:
          add  bp, bp                        ; bit_pos <<= 1
          jmp  .loop
     .end:
     add  cx, bp                             ; length |= bitpos (highest bit)
     pop  bp
     ret

data:
    incbin "data.bin"