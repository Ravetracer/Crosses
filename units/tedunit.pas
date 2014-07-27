unit tedunit;

interface
USES DOS,CRT;

 PROCEDURE TED_DONE;
 PROCEDURE TED_SCROLL(S : STRING; Y : WORD; WHERE : WORD);
 PROCEDURE TED_INIT(FNTNAME : STRING; LOADPAL : BOOLEAN);
 PROCEDURE TED_WRITEXY(X, Y : INTEGER; S : STRING; WHERE : WORD);

implementation
VAR
 TED_BITMAP         : ARRAY [1..25,0..319] OF BYTE;
 TED_PALETTE        : ARRAY [0..255,1..3] OF BYTE;
 TED_CHARS          : ARRAY [' '..']'] OF POINTER;
 TED_CHARSDATA      : ARRAY [' '..']',1..3] OF BYTE;
 TED_F              : FILE;
 TED_B,
 TED_ROW,
 TED_NR       : BYTE;
 TED_X,
 TED_Y,
 TED_I          : INTEGER;
 TED_CH,
 TED_K           : CHAR;
 TED_TEKST       : STRING;

 PROCEDURE TED_INITVGA; ASSEMBLER;
 ASM
    MOV AX,0013H
    INT 10H
 END;

 PROCEDURE TED_CLOSEVGA; ASSEMBLER;
 ASM
    MOV AX,0003H
    INT 10H
 END;

 PROCEDURE TED_VSYNC; ASSEMBLER;
 ASM
    MOV DX,03DAH
    @L1: IN AL,DX; TEST AL,8; JNZ @L1;
    @L2: IN AL,DX; TEST AL,8; JZ  @L2;
 END;

 PROCEDURE TED_DRAWBITMAP(YPOS : WORD; WHERE : WORD); ASSEMBLER;
 ASM
    MOV DI,YPOS
    MOV ES,WHERE
    MOV SI,OFFSET TED_BITMAP
    MOV CX,4000
    CLD
    REP MOVSW
 END;

 PROCEDURE TED_SCROLLBITMAP(VAR MAP); ASSEMBLER;
 ASM
    LDS SI,MAP
    LES DI,MAP
    INC SI
    INC SI
    INC SI
    MOV CX,4000
    REP MOVSW
 END;

 PROCEDURE TED_SETCOLOR(NR,R,G,B: BYTE); ASSEMBLER;
 ASM
    MOV DX,3C8H
    MOV AL,NR
    OUT DX,AL
    INC DX
    MOV AL,R
    OUT DX,AL
    MOV AL,G
    OUT DX,AL
    MOV AL,B
    OUT DX,AL
 END;

 PROCEDURE TED_LOADPAL(NAME: STRING);
 BEGIN
      ASSIGN(TED_F,NAME+'.256');
      RESET(TED_F,1);
      BLOCKREAD(TED_F,TED_PALETTE,768);
      CLOSE(TED_F);
      FOR TED_B:=0 TO 255 DO TED_SETCOLOR(TED_B,TED_PALETTE[TED_B,1],TED_PALETTE[TED_B,2],TED_PALETTE[TED_B,3]);
 END;

 PROCEDURE TED_LOADFONT(NAME: STRING);
 VAR TX,TY: BYTE; CH: CHAR;
 BEGIN
      ASSIGN(TED_F,NAME+'.CHR');
      RESET(TED_F,1);
      SEEK(TED_F,20);
      WHILE NOT(EOF(TED_F)) DO
      BEGIN
           BLOCKREAD(TED_F,CH,1);
           BLOCKREAD(TED_F,TX,1);
           BLOCKREAD(TED_F,TY,1);
           GETMEM(TED_CHARS[CH],TX*TY);
           TED_CHARSDATA[CH,1]:=TX; TED_CHARSDATA[CH,2]:=TY; TED_CHARSDATA[CH,3]:=1;
           BLOCKREAD(TED_F,TED_CHARS[CH]^,TX*TY);
      END;
      IF TED_CHARSDATA[' ',3]<>1 THEN { IF NOT SPACE " " THEN CREATE IT }
      BEGIN
           TX:=TED_CHARSDATA['A',1];
           TY:=TED_CHARSDATA['A',2];
           GETMEM(TED_CHARS[' '],TX*TY);
           FILLCHAR(TED_CHARS[' ']^,TX*TY,0);
           TED_CHARSDATA[' ',3]:=1;
           TED_CHARSDATA[' ',1]:=TX;
           TED_CHARSDATA[' ',2]:=TY;
      END;
      CLOSE(TED_F);
 END;


 PROCEDURE TED_DONE;
 VAR CH: CHAR;
 BEGIN
      FOR CH:=' ' TO ']' DO
      BEGIN
           IF TED_CHARSDATA[CH,3]=1 THEN
           BEGIN
                FREEMEM(TED_CHARS[CH],TED_CHARSDATA[CH,1]*TED_CHARSDATA[CH,2]);
                TED_CHARSDATA[CH,3]:=0;
           END;
      END;
 END;

 PROCEDURE TED_NEWROW(CH: CHAR; RO: BYTE; POS: INTEGER);
 VAR TX,TY: INTEGER;
 BEGIN
      IF TED_CHARSDATA[CH,3]<>1 THEN EXIT;
      IF RO=TED_CHARSDATA[CH,1]+1 THEN
      FOR TY:=1 TO TED_CHARSDATA[CH,2] DO TED_BITMAP[TY,POS]:=0 { SKIP ONE ROW }
          ELSE
      FOR TY:=1 TO TED_CHARSDATA[CH,2] DO
          TED_BITMAP[TY,POS]:=MEM[SEG(TED_CHARS[CH]^):OFS(TED_CHARS[CH]^)+(TY-1)*TED_CHARSDATA[CH,1]+RO-1];
 END;

 PROCEDURE TED_UPDATE;
 BEGIN
      INC(TED_ROW);
      IF TED_ROW>TED_CHARSDATA[TED_TEKST[TED_NR],1]+1 THEN
      BEGIN
           TED_ROW:=1;
           INC(TED_NR);
           IF TED_NR>LENGTH(TED_TEKST) THEN TED_NR:=1;
      END;
 END;

 procedure ted_init(FNTNAME : STRING; LOADPAL : BOOLEAN);
 begin
      fillchar(ted_bitmap,8000,0);
      TED_ROW:=1;
      TED_NR:=1;
      TED_LOADFONT(fntname);
      if loadpal=true then ted_loadpal(fntname);
 end;

 PROCEDURE TED_SCROLL(S : STRING;Y : WORD; WHERE : WORD);
 begin
                TED_TEKST:=S;
                ted_scrollbitmap(ted_bitmap);
                ted_update;
                TED_NEWROW(TED_TEKST[TED_NR],TED_ROW,317);
                TED_UPDATE;
                TED_NEWROW(TED_TEKST[TED_NR],TED_ROW,318);
                TED_UPDATE;
                TED_NEWROW(TED_TEKST[TED_NR],TED_ROW,319);
                TED_DRAWBITMAP(Y*320,WHERE);
 end;

 PROCEDURE TED_PUTPIX(X,Y : INTEGER; C: BYTE; WHERE : WORD); ASSEMBLER; { PLOT PIXEL AT (X,Y) }
 ASM
  MOV   AX, WHERE
  MOV   ES, AX
  MOV   AX, 320
  MUL   Y
  ADD   AX, X
  MOV   DI, AX
  MOV   AL, C
  STOSB
 END;

 PROCEDURE TED_PUTIT(X,Y: INTEGER; CH: CHAR; where : Word); { DRAW ONE CHAR }
 VAR XR,YR,AX,AY: BYTE;
 BEGIN
  XR:=TED_CHARSDATA[CH,1];
  YR:=TED_CHARSDATA[CH,2];
  FOR AY:=1 TO YR DO
   FOR AX:=1 TO XR DO
   BEGIN
    TED_B:=MEM[SEG(TED_CHARS[CH]^):OFS(TED_CHARS[CH]^)+(AY-1)*XR+AX-1];
    IF TED_B>0 THEN TED_PUTPIX(X+AX-1,Y+AY-1,TED_B,where);
   END;
 END;

 PROCEDURE TED_WRITEXY(X,Y: INTEGER; S: STRING; WHERE : WORD); { WRITE A STRING ON SCREEN }
 VAR ABSX,ABSY: INTEGER; NR: BYTE;
 BEGIN
  ABSX:=X; ABSY:=Y;
  FOR NR:=1 TO LENGTH(S) DO
  IF S[NR] IN [' '..']'] THEN
  BEGIN
   IF S[NR]=' ' THEN INC(ABSX,TED_CHARSDATA[S[NR+1],1]+1)
   ELSE BEGIN
    TED_PUTIT(ABSX,ABSY,S[NR], WHERE);
    INC(ABSX,TED_CHARSDATA[S[NR],1]+1);
   END;
  END;
 END;


end.