{==============================================================================

Datei            :   x320x200.PAS

Zweck            :   1. erm봥licht ruckelfreie Animationen durch virtuale
                        Seiten im Speicher

                     2. verschiedene Grafikbefehle stehen zum Zeichnen,
                        f걊 Speicherverschiebungen und Seitenbehandlung
                        zur Verf갾ung

                     3. aus Geschwindigkeitsgr걆den sind verschiedene
                        Programmsegmente in ASSEMBLER geschrieben

Datum            :   13.08.1996

Version          :   V2.12

Autor            :   Michael Schulze (c)

Compiler         :   TURBO PASCAL 6.0, Turbo ASSEMBLER

letzte Aenderung :   15.06.1997
==============================================================================
}
{$X+}                                   { erweiterte Syntax nutzen          }
{$D-}                                   { keine Informationen des Debuggers }
{$I-}                                   { keine I/O-Pr갽ung                 }
{$S-}                                   { keine Pr갽ung des Stacks          }

{$IFDEF VER70}                          { falls Turbo/Borland Pascal 7.0    }
{$Q-}                                   { keine Pr갽ung von Integer-Operationen auf 쉇erlauf }
{$R-}                                   { keine Bereichspr갽ung             }
{$ENDIF}

UNIT x320x200;                          { Unit f걊 Modus 13h mit virtualen Seiten }

INTERFACE

Uses Crt, Dos;

{ diese Konstanten und Typen sind modulextern, }
{ d.h. anderen Programmen zug꼗glich           }

CONST

  { Korrekturfaktor f걊 das H봦en-/Seiten- }
  { verh꼕tnis des Bildschirms             }

  Korrekturfaktor : Real = 0.83;

  max_anz= 4;                       { 4 virtuelle Bildschirmseiten 0 bis 3  }
  x_Aufl = 320;                     { horizontale Aufl봲ung des Bildschirms }
  y_Aufl = 200;                     { vertikale Aufl봲ung des Bildschirms   }
  visiblepage     : word=$a000;     { aktuelle Seite                        }
  hiddenpage      : word=$a000;     { muss mit wert belegt sein, sie bekommt
                                      spaeter eine andere Adresse nach init
                                      hiddenpage ist immer die erste
                                      virtuelle Seite im Speicher (man kann
                                      das aber auch spaeter im Programm noch
                                      veraendern )                          }

  rot   = 1;
  gruen = 2;
  blau  = 3;

TYPE
    String_80       = String[80];
    Bit_Muster_Typ  = ARRAY[0..7] OF Byte;
    Zeichensatz_Typ = ARRAY[0..255] OF Bit_Muster_Typ;

    RGB_Palette = rot..blau;
    DAC_Block   = ARRAY[0..255, RGB_Palette] OF Byte;

VAR
    page_adr    : array[1..max_anz] of word;
    Zeichensatz : Zeichensatz_Typ;

PROCEDURE InitVGA(a : byte);    { INITIALIZE VGA CARD MODE 13H + virtuale Seiten}
PROCEDURE CloseVGA;             { CLOSE VGA MODE AND SET TEXT + loeschen von v. S. }
PROCEDURE VRET;                 { works For CGA,EGA and VGA cards}
PROCEDURE Bildschirm_ein;       { Schaltet Bildwiederholung ein }
PROCEDURE Bildschirm_aus;       { Schaltet Bildwiederholung aus }

PROCEDURE Punkt_setzen(X,Y : INTEGER; C: BYTE; where : word);         { PLOT PIXEL AT (X,Y) }
PROCEDURE Punkt_setzen_clip(X,Y : Integer; C : Byte; where:word);
FUNCTION  PunktFarbe(X,Y : INTEGER; where : word): BYTE;              { GET A PIXEL FROM (X,Y) }
PROCEDURE Bereich_fuellen(X1,Y1,X2,Y2: INTEGER; COLOR: BYTE; where : word);   { FILLED RECTANGLE }
PROCEDURE Bereich_loeschen(x1, y1, x2, y2 : Integer; where : word);
PROCEDURE Ellipse_zeichnen(x, y : Integer; x_Radius, y_Radius : Word; Farbe : Byte; where : word);
PROCEDURE Ellipse_fuellen(xc,yc,a,b : Integer; Farbe:byte; where : word);
PROCEDURE Kreis_zeichnen(x, y : Integer; Radius : Word; Farbe : Byte; where : word);
PROCEDURE Kreis_fuellen(x,y,r : Integer; Farbe:byte; where : word);
PROCEDURE Linie_zeichnen(x,y,x2,y2:word; Farbe: byte; where : word);
PROCEDURE Rechteck_zeichnen(x1, y1, x2, y2 : Integer; Farbe : Byte; where : word);
PROCEDURE FILL(x, y : Integer; Farbe : Byte; where : word);
PROCEDURE Standard_Zeichensatz_laden;
PROCEDURE Zeichensatz_laden(Dateiname : PathStr; VAR Fehlercode : Byte);
PROCEDURE Zeichen_ausgeben(x, y : Integer; Anzahl : Word; Zeichen : Char; Farbe : Byte; where : word);
PROCEDURE Text_ausgeben(x, y : Integer; s : String_80; Farbe : Byte; where : word);


PROCEDURE In_Puffer_kopieren(x1, y1, x2, y2 : Integer; x_Laenge,
  y_Laenge : Word; VAR Puffer : Pointer; where : word);   { eine Art GETIMAGE }

PROCEDURE In_VRAM_kopieren(x, y : Integer; x_Laenge, y_Laenge : Word;
  Puffer : Pointer; where : word);       { eine Art Putimage mit Speicher freimachen }

PROCEDURE In_VRAM_kopieren_new(x, y : Integer; x_Laenge, y_Laenge : Word;
  Puffer : Pointer; where : word);       { eine Art Putimage }

PROCEDURE Screen_to_Page(a : word); { kopiert Bildschirm auf virtuale Seite }
PROCEDURE Page_to_Screen(a : word); { kopiert virtuale Seite auf Bildschirm }
PROCEDURE Page_to_Page(a,b : word); { kopiert Seite auf andere Seite }
PROCEDURE Flip;                     { schaltet zwischen beiden virtuallen
                                      Seiten um und bringt die zuvor be-
                                      arbeitete Seite auf den Bildschirm }
PROCEDURE Loesche_Page(a:word);     { Loescht virt. Bild. mit Farbe 0}
PROCEDURE Loesche_Screen;           { Loescht Bildschirm mit Farbe 0}
PROCEDURE LOESCHE_PAGE_COLOR(a:word;col:byte);     { Loescht Bildschirm mit Farbe }
PROCEDURE LOESCHE_SCREEN_COLOR(col:byte);          { Loescht Bildschirm mit Farbe }

PROCEDURE DAC_Reg_setzen(Reg_Nr, Rotanteil, Gruenanteil, Blauanteil : Byte);

PROCEDURE DAC_Reg_lesen(Reg_Nr : Byte; VAR Rotanteil, Gruenanteil,
          Blauanteil : Byte);

PROCEDURE DAC_Block_setzen(Startindex : Byte; Anzahl_Reg : Word;
          Block : DAC_Block);

PROCEDURE DAC_Block_lesen(Startindex : Byte; Anzahl_Reg : Word;
          VAR Block : DAC_Block);

PROCEDURE Graustufen(Startindex : Byte; Anzahl_Reg : Word;
          VAR Block : DAC_Block);

PROCEDURE Load_Palette(Fname : String; VAR Block : Dac_Block);

PROCEDURE Save_Palette(Fname : String; Block : Dac_Block);

PROCEDURE Farb_Rotation(Start, Ende : Byte; hin : Boolean;
          VAR Block :  Dac_Block; Zeit : Integer);

Procedure Get_Image(ImgPtr : pointer; XOfs,YOfs,XSize,YSize : Word;
                    where : Word );
Procedure Put_Image( ImgPtr : pointer; XOfs,YOfs  : Word; where : Word);
Procedure PutTransparent(XOfs, YOfs : Word; ImgPtr : pointer;
                         tcolor : byte; where : Word);


IMPLEMENTATION

uses Memmove;

TYPE
 TSCREEN        = ARRAY [0..63999] OF BYTE;
 PSCREEN        = ^TSCREEN;                           { Aufbau der v. S. }


VAR  Bitmap      : Array[1..4] of PSCREEN;              { Anzahl der v. S. }
     Color       : Byte;                                { farbe f걊 Befehle }
     Fillval     : Byte;                                { f걊 Fillroutine }
     Scr_Ofs     : Array[0..199] of Word;
     anz_page    : byte;

PROCEDURE CLS32_COLOR (Where:word;Col : Byte); assembler;
   { Dieses loescht den Bildschirm  mit einer bestimmten Farbe }
asm
   push    es
   mov     cx, 16000;
   mov     es,[where]
   xor     di,di
   mov     al,[col]
   mov     ah,al
   mov     dx, ax
   db      $66, $C1, $E0, $10         {shl eax, 16}
   mov     ax, dx
   db      $F3, $66, $AB              {rep stosd}
   pop     es
End;

PROCEDURE CLS32 (Where:word); assembler;
   { Dieses loescht den Bildschirm  mit einer bestimmten Farbe }
asm
   mov     cx, 16000;
   mov     es,[where]
   xor     di,di
   db $66 ; xor ax,ax
   db      $F3, $66, $AB              {rep stosd}
End;

PROCEDURE LOESCHE_PAGE_COLOR(a:word;col:byte);    { Loescht virt. Bild. mit Farbe }
begin
     cls32_COLOR(a,col);
end;

PROCEDURE LOESCHE_PAGE(a:word);    { Loescht virt. Bild. mit Farbe 0}
begin
     cls32(a);
end;

PROCEDURE LOESCHE_SCREEN_COLOR(col:byte);  { Loescht Bildschirm mit Farbe }
begin
     cls32_COLOR($a000,col);
end;

PROCEDURE LOESCHE_SCREEN;  { Loescht Bildschirm mit Farbe }
begin
     cls32($a000);
end;

PROCEDURE CREATEVIRTUAL(a :byte); { CREATE VIRTUAL SCREEN IN MEMORY }
BEGIN
     GetMem (Bitmap[a],64000);
     cls32(Seg(Bitmap[a]^));
END;

PROCEDURE INITVGA(a : byte);
var i: byte;
BEGIN
     if a>5 then
     begin
          sound(440);
          delay(100);
          nosound;
          writeln(' Error Error Nicht genuegend Speicher vorhanden.');
          halt;
     end;
     for i:=1 to 4 do
         page_adr[i]:=$a000;
     ASM
        MOV AX,0013H
        INT 10H
     END;
     cls32($A000);
     if a<>0 then
     begin
        for i:=1 to a do
        begin
            Createvirtual(i);
            page_adr[i]:=seg(Bitmap[i]^);
        end;
        hiddenpage:=Seg(Bitmap[1]^);
     end;
     anz_page:=a;
end;

PROCEDURE RELEASEVIRTUAL(a:byte); { REMOVE VIRTUAL SCREEN IN MEMORY }
BEGIN
     FreeMem(BITMAP[a],64000);
END;

PROCEDURE CLOSEVGA;
var i : byte;
BEGIN
     ASM
        MOV AX,0003H
        INT 10H
     end;
     if anz_page<>0 then
        for i:=1 to anz_page do
            Releasevirtual(i);

END;

PROCEDURE VRET;Assembler; {works For CGA,EGA and VGA cards}
Asm
  MOV  DX, $03DA
  MOV  AH, 8
@Wau: in   AL, DX
  TEST AL, AH
  JNZ  @Wau     { wait Until out of retrace }
@Wai: in   AL, DX
  TEST AL, AH
  JZ   @Wai     { wait Until inside retrace }
end;

PROCEDURE Bildschirm_ein;
begin
     port[$3c4]:=1;
     port[$3c5]:=port[$3c5] and not $20;
end;

PROCEDURE Bildschirm_aus;
begin
     port[$3c4]:=1;
     port[$3c5]:=port[$3c5] or $20;
end;

PROCEDURE FLIP32(source,dest:Word); assembler;
  { Dieses kopiert den virtuellen Screen zum Bildschirm }
asm
  push    ds
  mov     ax, [Dest]
  mov     es, ax
  mov     ax, [Source]
  mov     ds, ax
  xor     si, si
  xor     di, di
  mov     cx, 16000
  db      $F3, $66, $A5
  pop     ds
end;

PROCEDURE PAGE_TO_SCREEN(a : word); { COPY v. S. TO ADDRESS 0A000:0 }
BEGIN
     Flip32(a,$A000);
END;

PROCEDURE PAGE_TO_PAGE(a,b : word);  { Copy v. S. to v. S. }
BEGIN
     Flip32(a,b);
END;
PROCEDURE SCREEN_TO_PAGE(a : word); { kopiert Bildschirm auf virtuale Seite }
begin
     flip32($A000,a);
end;
PROCEDURE FLIP;
BEGIN
     PAGE_TO_SCREEN(hiddenpage);
END;

PROCEDURE Punkt_setzen(X,Y : Integer; C : Byte; where:word); assembler;
  { This puts a pixel on the screen by writing directly to memory. }
asm
   mov  ax,where
   mov  es,ax
   mov  bx,[y]
   shl  bx,1
   mov  di,word ptr [Scr_Ofs + bx]
   add  di,[x]
   mov  al,[c]
   mov  es:[di],al
end;

PROCEDURE Punkt_setzen_clip(X,Y : Integer; C : Byte; where:word); assembler;
  { This puts a pixel on the screen by writing directly to memory. }
asm
   mov ax,y
   cmp ax,0
   jb @exit
   cmp ax,199
   ja @exit
   mov ax,x
   cmp ax,0
   jb @exit
   cmp ax,319
   ja @exit
   mov  ax,where
   mov  es,ax
   mov  bx,[y]
   shl  bx,1
   mov  di,word ptr [Scr_Ofs + bx]
   add  di,[x]
   mov  al,[c]
   mov  es:[di],al
@exit:
end;

{컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴}
Function PunktFarbe (X,Y : Integer; where:word):byte; assembler;
  { This puts a pixel on the screen by writing directly to memory. }
asm
   mov  ax,where
   mov  es,ax
   mov  bx,[y]
   shl  bx,1
   mov  di,word ptr [Scr_Ofs + bx]
   add  di,[x]
   mov  al,es:[di]
end;

PROCEDURE BEREICH_FUELLEN(X1,Y1,X2,Y2: INTEGER; COLOR: BYTE; where : word); ASSEMBLER; { FILLED RECTANGLE }
VAR I,H,ENDE : INTEGER;
ASM
 MOV   AX, X2
 CMP   AX, X1
 JAE   @L1
 XCHG  X1, AX
 XCHG  X2, AX
 XCHG  X1, AX
 @L1:
 MOV   AX, Y2
 CMP   AX, Y1
 JAE   @L2
 XCHG  Y1, AX
 XCHG  Y2, AX
 XCHG  Y1, AX
 @L2:
 MOV   AX, X2
 MOV   ENDE, AX
 MOV   CX, Y2
 SUB   CX, Y1
 INC   CX
 MOV   AX, [where]
 MOV   ES, AX
 MOV   AX, 320
 MUL   Y1
 ADD   AX, X1
 MOV   DI, AX
 MOV   DX, X2
 SUB   DX, X1
 INC   DX
 MOV   AH, COLOR
 MOV   AL, COLOR
 @FORSCHLEIFE:
 MOV  BX, X1
 CMP  DX, 1
 JE   @WHILE2
 @WHILE1:
 STOSW
 ADD   BX, 2
 CMP   BX, ENDE
 JB @WHILE1
 MOV  H, DX
 AND  H, 1
 CMP  H, 1
 JNE  @GERADE
 @WHILE2:
 STOSB
 @GERADE:
 ADD   DI, 320
 SUB   DI, DX
 LOOP @FORSCHLEIFE
END;

PROCEDURE Ellipse_zeichnen(x, y : Integer; x_Radius, y_Radius : Word; Farbe : Byte; where : word);

{ Kreisalgorithmus }

VAR
  hilf1, hilf2, hilf3 : Integer;
  ix, iy              : Integer;                    { rel. Punktkoordinaten }
  Faktor              : Real;                { Stauchungs-/Streckungsfaktor }

BEGIN
  IF (x_Radius > 0) AND (y_Radius > 0) THEN
    BEGIN

      { Anfangswerte setzen und Stauchungs-/Streckungsfaktor ermitteln }

      ix := 0;
      IF x_Radius > y_Radius THEN
        BEGIN
          iy := x_Radius;
          hilf1 := x_Radius;
          Faktor := y_Radius/x_Radius;
        END
      ELSE
        BEGIN
          iy := y_Radius;
          hilf1 := y_Radius;
          Faktor := x_Radius/y_Radius;
        END;

      { solange wiederholen, bis ein Achtelkreis berechnet ist }

      WHILE ix <= iy DO
        IF hilf1 < 0 THEN
          BEGIN
            Dec(iy);
            Inc(hilf1, 2*iy);
          END
        ELSE
          BEGIN

            { zum Mittelpunkt des Kreises rel. Punktkoordinaten   }
            { mit dem Stauchungs-/Streckungsfaktor multiplizieren }
            { und runden                                          }

            hilf2 := Round(ix*Faktor);
            hilf3 := Round(iy*Faktor);

            { Ellipsenform durch Spiegelung, Drehung und Stauchung }
            { bzw. Streckung des Kreises berechnen und darstellen  }

            IF x_Radius > y_Radius THEN
              BEGIN
                Punkt_setzen(x-ix, y-hilf3, Farbe, where);
                Punkt_setzen(x-ix, y+hilf3, Farbe, where);
                Punkt_setzen(x+ix, y-hilf3, Farbe, where);
                Punkt_setzen(x+ix, y+hilf3, Farbe, where);
                Punkt_setzen(x-iy, y-hilf2, Farbe, where);
                Punkt_setzen(x-iy, y+hilf2, Farbe, where);
                Punkt_setzen(x+iy, y-hilf2, Farbe, where);
                Punkt_setzen(x+iy, y+hilf2, Farbe, where);
              END
            ELSE
              BEGIN
                Punkt_setzen(x-hilf2, y-iy, Farbe, where);
                Punkt_setzen(x-hilf2, y+iy, Farbe, where);
                Punkt_setzen(x+hilf2, y-iy, Farbe, where);
                Punkt_setzen(x+hilf2, y+iy, Farbe, where);
                Punkt_setzen(x-hilf3, y-ix, Farbe, where);
                Punkt_setzen(x-hilf3, y+ix, Farbe, where);
                Punkt_setzen(x+hilf3, y-ix, Farbe, where);
                Punkt_setzen(x+hilf3, y+ix, Farbe, where);
              END;

            Dec(hilf1, 2*ix-1);
            Inc(ix);
          END;
    END;
END;


PROCEDURE Kreis_zeichnen(x, y : Integer; Radius : Word; Farbe : Byte; where : word);
BEGIN
  Ellipse_zeichnen(x, y, Radius, Round(Radius*Korrekturfaktor), Farbe, where);
END;

PROCEDURE ELLIPSE_FUELLEN(xc, yc, a, b:Integer;Farbe:Byte; where : word);
Var
  x, y, i   : Integer;
  aa, aa2,
  bb, bb2,
  d, dx, dy : LongInt;
begin
  x   := 0;
  y   := b;
  aa  := LongInt(a) * a;
  aa2 := 2 * aa;
  bb  := LongInt(b) * b;
  bb2 := 2 * bb;
  d   := bb - aa * b + aa div 4;
  dx  := 0;
  dy  := aa2 * b;

  linie_zeichnen(xc, yc-y,xc,yc+y, farbe, where);

  While (dx < dy) do
  begin
    if (d > 0) then
    begin
      dec(y);
      dec(dy, aa2);
      dec(d, dy);
    end;
    inc(x);
    inc(dx, bb2);
    inc(d, bb + dx);
    linie_zeichnen(xc-x, yc-y,xc-x,yc+y,farbe, where);
    linie_zeichnen(xc+x, yc-y,xc+x,yc+y,farbe, where);

  end;

  inc(d, (3 * (aa - bb) div 2 - (dx + dy)) div 2);

  While (y >= 0) do
  begin
    if (d < 0) then
    begin
      inc(x);
      inc(dx, bb2);
      inc(d, bb + dx);
      linie_zeichnen(xc-x, yc-y,xc-x,yc+y,farbe, where);
      linie_zeichnen(xc+x, yc-y,xc+x,yc+y,farbe, where);

    end;
    dec(y);
    dec(dy, aa2);
    inc(d, aa - dy);
  end;
end;


PROCEDURE KREIS_FUELLEN(x,y,r : Integer; Farbe:byte; where : word);
BEGIN
     Ellipse_fuellen(x,y,r,r,farbe,where);
END;

PROCEDURE Line(x,y,x2,y2:word; where : word);assembler;
asm
          mov ax,[where]      { Adresse rein }
          mov es,ax
          mov bx,x
          mov ax,y
          mov cx,x2
          mov si,y2
          cmp ax,si
          jbe @NO_SWAP   {always draw downwards}
          xchg bx,cx
          xchg ax,si
 @NO_SWAP:
          sub si,ax         {yd (pos)}
          sub cx,bx         {xd (+/-)}
          cld               {set up direction flag}
          jns @H_ABS
          neg cx      {make x positive}
          std
 @H_ABS:
          mov di,320
          mul di
          mov di,ax
          add di,bx   {di:adr}
          or si,si
          jnz @NOT_H
          {horizontal line}
          mov al,color
          inc cx
          rep stosb
          jmp @EXIT
 @NOT_H:
          or cx,cx
          jnz @NOT_V
          {vertical line}
          cld
          mov al,color
          mov cx,si
          inc cx
          mov bx,320-1
 @VLINE_LOOP:
          stosb
          add di,bx
          loop @VLINE_LOOP
          jmp @EXIT
 @NOT_V:
          cmp cx,si    {which is greater distance?}
          lahf         {then store flags}
          ja @H_IND
          xchg cx,si   {swap for redundant calcs}
 @H_IND:
          mov dx,si    {inc2 (adjustment when decision var rolls over)}
          sub dx,cx
          shl dx,1
          shl si,1     {inc1 (step for decision var)}
          mov bx,si    {decision var, tells when we need to go secondary direction}
          sub bx,cx
          inc cx
          push bp      {need another register to hold often-used constant}
          mov bp,320
          mov al,color
          sahf         {restore flags}
          jb @DIAG_V
          {mostly-horizontal diagonal line}
          or bx,bx     {set flags initially, set at end of loop for other iterations}
 @LH:
          stosb        {plot and move x, doesn't affect flags}
          jns @SH      {decision var rollover in bx?}
          add bx,si
          loop @LH   {doesn't affect flags}
          jmp @X
 @SH:
          add di,bp
          add bx,dx
          loop @LH   {doesn't affect flags}
          jmp @X
 @DIAG_V:
          {mostly-vertical diagonal line}
          or bx,bx    {set flags initially, set at end of loop for other iterations}
 @LV:
          mov es:[di],al   {plot, doesn't affect flags}
          jns @SV          {decision var rollover in bx?}
          add di,bp        {update y coord}
          add bx,si
          loop @LV         {doesn't affect flags}
          jmp @X
 @SV:
          scasb   {sure this is superfluous but it's a quick way to inc/dec x coord!}
          add di,bp        {update y coord}
          add bx,dx
          loop @LV         {doesn't affect flags}
 @X:
          pop bp
 @EXIT:
end;

PROCEDURE Linie_zeichnen(x,y,x2,y2:word; Farbe: byte; where : word);
BEGIN
     color:=farbe;
     line(x,y,x2,y2,where);
END;

PROCEDURE Rechteck_zeichnen(x1, y1, x2, y2 : Integer; Farbe : Byte; where : word);
BEGIN
  { Rechteck mit Hilfe der Linienprozedur zeichnen }
  Linie_zeichnen(x1, y1, x2, y1, Farbe, where);
  Linie_zeichnen(x2, y1, x2, y2, Farbe, where);
  Linie_zeichnen(x2, y2, x1, y2, Farbe, where);
  Linie_zeichnen(x1, y2, x1, y1, Farbe, where);
END;

Function lineFill(x, y, d, prevXL, prevXR : Integer;farbe :byte; where : word) : Integer;
Var
  xl, xr, i : Integer;
Label
  _1, _2, _3;
begin
  xl := x;
  xr := x;

  Repeat
    dec(xl);
  Until (PunktFarbe(xl, y,where) <> fillVal) or (xl < 0);

  inc(xl);

  Repeat
    inc(xr);
  Until (PunktFarbe(xr, y,where) <> fillVal) or (xr > 319);

  dec(xr);
  Linie_zeichnen(xl,y, xr, y,farbe,where);
  inc(y, d);

  if Word(y) <= 199 then
  For x := xl to xr do
    if (PunktFarbe(x, y,where) = fillVal) then
    begin
      x := lineFill(x, y, d, xl, xr,farbe,where);
      if Word(x) > xr then
        Goto _1;
    end;

  _1 :

  dec(y, d + d);
  Asm
    neg d;
  end;
  if Word(y) <= 199 then
  begin
  For x := xl to prevXL do
    if (PunktFarbe(x, y,where) = fillVal) then
    begin
      i := lineFill(x, y, d, xl, xr,farbe,where);
      if Word(x) > prevXL then
        Goto _2;
    end;

    _2 :

    for x := prevXR to xr do
      if (PunktFarbe(x, y,where) = fillVal) then
      begin
        i := lineFill(x, y, d, xl, xr,farbe,where);
        if Word(x) > xr then
          Goto _3;
      end;

      _3 :

      end;

  lineFill := xr;
end;

PROCEDURE FILL(x, y : Integer; Farbe : Byte; where : word);
begin
  fillVal := PunktFarbe(x, y,where);
  if fillVal <> farbe then
    lineFill(x, y, 1, x, x,farbe,where);
end;

PROCEDURE Bereich_loeschen(x1, y1, x2, y2 : Integer; where : word);
BEGIN
  { Bereich durch F걄len mit der Farbnummer 0 l봲chen }
  Bereich_fuellen(x1, y1, x2, y2, 0,where);
END;

PROCEDURE Integer_vertauschen(VAR a, b : Integer);

VAR
  hilf : Integer;

BEGIN
  hilf := a;
  a := b;
  b := hilf;
END;

PROCEDURE In_Puffer_kopieren(x1, y1, x2, y2 : Integer; x_Laenge,
  y_Laenge : Word; VAR Puffer : Pointer; where : word);

VAR
  lauf : Integer;                                            { Z꼑lvariable }
  Adr  : Word;             { Offset-Adresse der Speicherstelle im Video-RAM }

BEGIN

  { pr갽en, ob die Koordinaten vertauscht }
  { werden m걌sen und ggf. vertauschen    }

  IF x1 > x2 THEN
    Integer_vertauschen(x1, x2);
  IF y1 > y2 THEN
    Integer_vertauschen(y1, y2);

  { Speicherplatz f걊 den Puffer reservieren }

  GetMem(Puffer, x_Laenge*y_Laenge);

  FOR lauf := y1 TO y2 DO
    BEGIN

      { Adresse der Speicherstelle rel. zum Anfang des Video-RAM ermitteln }

      Adr := x_Aufl*lauf+x1;

      { Bereich in den Puffer kopieren }

      FASTMove(Mem[where:Adr], Mem[Seg(Puffer^):Ofs(Puffer^)+
        x_Laenge*(lauf-y1)], x_Laenge);

    END;
END;

PROCEDURE In_VRAM_kopieren(x, y : Integer; x_Laenge, y_Laenge : Word;
  Puffer : Pointer; where : word);

VAR
  lauf : Integer;                                            { Z꼑lvariable }
  Adr  : Word;             { Offset-Adresse der Speicherstelle im Video-RAM }

BEGIN
  FOR lauf := y TO y+y_Laenge-1 DO
    BEGIN

      { Adresse der Speicherstelle rel. zum Anfang des Video-RAM ermitteln }

      Adr := x_Aufl*lauf+x;

      { Inhalt des Puffers in das Video-RAM kopieren }

      FASTMove(Mem[Seg(Puffer^):Ofs(Puffer^)+x_Laenge*(lauf-y)],
        Mem[where:Adr], x_Laenge);

    END;

  FreeMem(Puffer, x_Laenge*y_Laenge);             { Speicherplatz freigeben }
END;

PROCEDURE In_VRAM_kopieren_new(x, y : Integer; x_Laenge, y_Laenge : Word;
  Puffer : Pointer; where : word);

VAR
  lauf : Integer;                                            { Z꼑lvariable }
  Adr  : Word;             { Offset-Adresse der Speicherstelle im Video-RAM }

BEGIN
  FOR lauf := y TO y+y_Laenge-1 DO
    BEGIN

      { Adresse der Speicherstelle rel. zum Anfang des Video-RAM ermitteln }

      Adr := x_Aufl*lauf+x;

      { Inhalt des Puffers in das Video-RAM kopieren }

      FASTMove(Mem[Seg(Puffer^):Ofs(Puffer^)+x_Laenge*(lauf-y)],
        Mem[where:Adr], x_Laenge);

    END;

END;

PROCEDURE Standard_Zeichensatz_laden;

BEGIN

  { Standard-Zeichensatz aus dem ROM kopieren }

  Move(Ptr($FFA6, $E)^, Zeichensatz, 128*8);
  Move(Ptr(MemW[$0:$7E], MemW[$0:$7C])^, Ptr(Seg(Zeichensatz),
  Ofs(Zeichensatz)+128*8)^, 128*8);

END;

PROCEDURE Zeichensatz_laden(Dateiname : PathStr; VAR Fehlercode : Byte);

VAR
  Zeichensatz_Datei : FILE OF Zeichensatz_Typ;

BEGIN

  { Fehlercodes: 0 = kein Fehler          }
  {              1 = Datei nicht gefunden }
  {              2 = ung걄tige Dateigr붳e }
  {              3 = Lesefehler           }

  Fehlercode := 0;

  { Zeichensatzdatei zum Lesen 봣fnen }

  Assign(Zeichensatz_Datei, Dateiname);
  Reset(Zeichensatz_Datei);

  { Ist der Wert von IOResult ungleich 0, dann existiert }
  { die Zeichensatzdatei mit diesem Namen nicht.         }

  IF IOResult <> 0 THEN
    Fehlercode := 1
  ELSE

    { Test auf Dateigr붳e }

    IF FileSize(Zeichensatz_Datei) <> 1 THEN
      BEGIN
        Close(Zeichensatz_Datei);
        Fehlercode := 2;
      END

    ELSE
      BEGIN

        { Zeichensatz aus der Datei in Variable lesen }

        Read(Zeichensatz_Datei, Zeichensatz);

        { Ist der Wert von IOResult ungleich 0, }
        { dann Lesefehler registrieren.         }

        IF IOResult <> 0 THEN
          BEGIN
            Close(Zeichensatz_Datei);
            Fehlercode := 3;
          END;

      END;
END;

PROCEDURE Zeichen_ausgeben(x, y : Integer; Anzahl : Word; Zeichen : Char;
  Farbe : Byte; where : word);

VAR
  lauf          : Integer;                                   { Z꼑lvariable }
  Zeile, Spalte : Word;
  Bitwert       : Byte;

BEGIN

  { Zeichen auf dem Bildschirm ausgeben }

  Dec(x, 8);
  lauf := 1;

  WHILE lauf <= Anzahl DO
    BEGIN

      { pr갽en, ob der rechte Rand des Bildschirms schon erreicht }
      { wurde, ggf. in der n꼊hsten Zeile schreiben               }

      IF x < x_Aufl-14 THEN
        Inc(x, 8)
      ELSE
        BEGIN
          x := 0;
          Inc(y, 8);
        END;

      { Zeichen punktweise aus der Zeichentabelle }
      { lesen und dementsprechend Punkte setzen   }

      FOR Zeile := 0 TO 7 DO
        FOR Spalte := 0 TO 7 DO
          BEGIN
            Bitwert := 128 SHR Spalte;
            IF (Zeichensatz[Ord(Zeichen), Zeile] AND Bitwert) = Bitwert THEN
              Punkt_setzen(x+Spalte, y+Zeile, Farbe,where);
          END;

      Inc(lauf);                                           { Z꼑ler erh봦en }

    END;
END;

PROCEDURE Text_ausgeben(x, y : Integer; s : String_80; Farbe : Byte; where : word);

VAR
  lauf : Integer;                                            { Z꼑lvariable }

BEGIN
  Dec(x, 8);
  FOR lauf := 1 TO Length(s) DO
    BEGIN

      { pr갽en, ob der rechte Rand des Bildschirms schon erreicht }
      { wurde, ggf. in der n꼊hsten Zeile schreiben               }

      IF x < x_Aufl-14 THEN
        Inc(x, 8)
      ELSE
        BEGIN
          x := 0;
          Inc(y, 8);
        END;

      Zeichen_ausgeben(x, y, 1, s[lauf], Farbe,where);

    END;
END;

PROCEDURE DAC_Reg_setzen(Reg_Nr, Rotanteil, Gruenanteil, Blauanteil : Byte);
ASSEMBLER;
ASM
  MOV   DX,3C8h
  MOV   AL,Reg_Nr
  OUT   DX,AL

  CLI                                                  { Interrupts sperren }
  MOV   DX,3C9h                    { 3C9h (Portnummer) in DX-Register laden }

  MOV   AL,Rotanteil
  OUT   DX,AL
  MOV   AL,Gruenanteil
  OUT   DX,AL
  MOV   AL,Blauanteil
  OUT   DX,AL

  STI                                                { Interrupts freigeben }
END;

PROCEDURE DAC_Reg_lesen(Reg_Nr : Byte; VAR Rotanteil, Gruenanteil,
  Blauanteil : Byte);
VAR
  R, G, B : Byte;                                          { Hilfsvariablen }
BEGIN
  ASM
    MOV   DX,3C7h
    MOV   AL,Reg_Nr
    OUT   DX,AL

    CLI                                                { Interrupts sperren }
    MOV   DX,3C9h                  { 3C9h (Portnummer) in DX-Register laden }

    IN    AL,DX
    MOV   R,AL
    IN    AL,DX
    MOV   G,AL
    IN    AL,DX
    MOV   B,AL

    STI                                              { Interrupts freigeben }
  END;
  BEGIN
    Rotanteil := R;
    Gruenanteil := G;
    Blauanteil := B;
  END;
END;

PROCEDURE DAC_Block_setzen(Startindex : Byte; Anzahl_Reg : Word;
  Block : DAC_Block);
VAR
  Regs : Registers;                                    { Prozessor-Register }
BEGIN
  WITH Regs DO
    BEGIN
      AH := $10;                                   { BIOS-Funktion aufrufen }
      AL := $12;                              { BIOS-Unterfunktion aufrufen }
      BX := Startindex;                             { Startindex in BX-Reg. }
      CX := Anzahl_Reg;                { Anzahl der Farbregister in CX-Reg. }
      ES := Seg(Block);                         { Segment-Adresse ermitteln }
      DX := Ofs(Block)+Startindex*3;             { Offset-Adresse ermitteln }
    END;
  Intr($10, Regs);                                { BIOS-Interrupt aufrufen }
END;

PROCEDURE DAC_Block_lesen(Startindex : Byte; Anzahl_Reg : Word;
  VAR Block : DAC_Block);
VAR
  Regs : Registers;                                    { Prozessor-Register }
BEGIN
  WITH Regs DO
    BEGIN
      AH := $10;                                   { BIOS-Funktion aufrufen }
      AL := $17;                              { BIOS-Unterfunktion aufrufen }
      BX := Startindex;                             { Startindex in BX-Reg. }
      CX := Anzahl_Reg;                { Anzahl der Farbregister in CX-Reg. }
      ES := Seg(Block);                         { Segment-Adresse ermitteln }
      DX := Ofs(Block)+Startindex*3;             { Offset-Adresse ermitteln }
    END;
  Intr($10, Regs);                                { BIOS-Interrupt aufrufen }
END;

PROCEDURE Graustufen(Startindex : Byte; Anzahl_Reg : Word;
  VAR Block : DAC_Block);
VAR
  Regs : Registers;                                    { Prozessor-Register }
BEGIN
  WITH Regs DO
    BEGIN
      AH := $10;                                   { BIOS-Funktion aufrufen }
      AL := $1B;                              { BIOS-Unterfunktion aufrufen }
      BX := Startindex;                             { Startindex in BX-Reg. }
      CX := Anzahl_Reg;                { Anzahl der Farbregister in CX-Reg. }
    END;
  Intr($10, Regs);                                 { BIOS-Interupt aufrufen }
  DAC_Block_lesen(Startindex, Anzahl_Reg, Block);
END;

PROCEDURE Load_Palette(Fname : String; VAR Block : Dac_Block);
var f : File of Dac_Block;
begin
	assign(f,fname);
	reset(f);
	read(f,block);
	close(f);
end;


PROCEDURE Save_Palette(Fname : String; Block : Dac_Block);
var f : File of Dac_Block;

begin
  assign(f,Fname);
  rewrite(f);
  write(f,Block);
  close(f);
end;

PROCEDURE Farb_Rotation(Start, Ende : Byte; hin : Boolean; VAR Block :  Dac_Block; Zeit : Integer);
var sicher0,
    sicher1,
    sicher2,
    i            : Integer;

procedure setvgapal(var pal:Dac_Block); assembler;
asm
  push ds
  xor ax,ax
  mov cx,0300h/2
  lds si,pal
  mov dx,03c8h
  out dx,al
  inc dx
  mov bx,dx
  cld
  mov dx,03dah
  @vsync0:
    in al,dx
    test al,8
  jz @vsync0
  mov dx,bx
  rep outsb
  mov bx,dx
  mov dx,03dah
  @vsync1:
    in al,dx
    test al,8
  jz @vsync1
  mov dx,bx
  mov cx,0300h/2
  rep outsb
  pop ds
end;

begin
          if hin then
          begin
               sicher0:=Block[ende,1];
               sicher1:=Block[ende,2];
               sicher2:=Block[ende,3];
               for i:= ende downto start+1 do
               begin
                 Block[i,1]:=Block[i-1,1];
                 Block[i,2]:=Block[i-1,2];
                 Block[i,3]:=Block[i-1,3];
               end;
               Block[start,1]:=sicher0;
               Block[start,2]:=sicher1;
               Block[start,3]:=sicher2;
          end
          else
          begin
               sicher0:=Block[start,1];
               sicher1:=Block[start,2];
               sicher2:=Block[start,3];
               for i:= start to ende-1 do
               begin
                 Block[i,1]:=Block[i+1,1];
                 Block[i,2]:=Block[i+1,2];
                 Block[i,3]:=Block[i+1,3];
               end;
               Block[ende,1]:=sicher0;
               Block[ende,2]:=sicher1;
               Block[ende,3]:=sicher2;
          end;
          Setvgapal(Block);
          delay(zeit);
end;

Procedure Get_Image(ImgPtr : pointer; XOfs,YOfs,XSize,YSize : Word;
                    where : Word );
 Assembler;
  asm
     PUSH DS
     MOV AX,where
     MOV DS,AX
     LES DI,Imgptr

     MOV BX,YOfs
     XCHG BH,BL
     MOV DX,BX
     SHR BX,1
     SHR BX,1
     ADD DX,BX
     ADD DX,XOfs

     MOV AX,xsize
     STOSW
     MOV BX,AX
     MOV AX,ysize
     STOSW

    @JP1:
     MOV SI,DX
     MOV CX,BX
     shr cx,1
     jnc @Jp2
     movsb
    @Jp2:
     repz movsw
     ADD DX,0140h
     DEC AX
     JNZ @JP1
     POP DS
 end;

Procedure Put_Image( ImgPtr : pointer; XOfs,YOfs  : Word; where : Word);
 Assembler;
  asm
     PUSH DS
     MOV AX,where
     MOV ES,AX
     LDS SI,ImgPtr

     MOV BX,YOfs
     XCHG BH,BL
     MOV CX,BX
     SHR BX,1
     SHR BX,1
     ADD CX,BX
     ADD CX,XOfs

     lodsw
     or ax,ax
     jz @Exit
     mov dx,ax
     lodsw
     or ax,ax
     jz @Exit
     mov bx,ax

     mov ax,cx

    @JP1:
     MOV DI,AX
     MOV CX,DX
     SHR CX,1
     JNC @JP2
     MOVSB
    @JP2:
     REPZ MOVSW
     ADD AX,140h
     DEC BX
     JNZ @JP1

    @Exit:
     POP DS
  end;


Procedure PutTransparent(XOfs, YOfs : Word; ImgPtr : pointer;
                         tcolor : byte; where : Word);
 var
  Addr : word;
  cols : word;
  bcolor : byte;
  curbuf : Pointer;

 begin
  Addr := XOfs + (YOfs shl 8) + (YOfs shl 6);
  curbuf := ptr(where,$0);

  asm
    push ds
    les di, CurBuf
    add di, Addr
    lds si, ImgPtr
    lodsw
    mov dx, ax                         { image width }
    lodsw
    mov bx, ax                         { image height }

 @jump1:
    mov cols,dx

  @jump2:
      mov ax, 0
      lodsb
      mov bcolor, al
      xor al, tcolor
      not al
      inc ax
      shr ax,8
      inc ax
      mov cx,ax

    loop @jump3
        mov al,bcolor
        stosb
        mov cx,5
      loop @jump4
    @jump3:
      inc di
    @jump4:
      mov cx,cols
      dec cols
  loop @jump2

    add di, 320
    sub di, dx
    dec bx
 jnz @jump1
    pop ds
  end
 end;



{ Initialisierungsroutine der Unit }

PROCEDURE Init;
var loop1: integer;
BEGIN
  Standard_Zeichensatz_laden;
  For Loop1 := 0 to 199 do
      Scr_Ofs[Loop1] := Loop1 * 320;

END;

{ Dieser Teil der Unit wird beim Einbinden in ein Programm mit Hilfe }
{ der Anweisung USES ausgef갿rt.                                     }

BEGIN
  Init;                                               { Unit initialisieren }
END.