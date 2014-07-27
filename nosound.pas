uses x320x200,          { Grafikroutinen }
     crt,               { CRT-Routinen }
     dos,               { DOS-Routinen }
     fpack,             { Bildhandling }
     asytimer,          { Zeitunit }
     fade,              { Fade-Unit }
     tedunit;

type levels = array[1..99] of Record
                               pos      : array[1..11,1..11] of Boolean;
                               til      : Byte;
                               time     : LongInt;
                               password : String[8];
                             end;

     highscore = array[1..3] of Record
                                 score : LongInt;
                                 name  : String[20];
                               end;

const    wait : LongInt = 1000000;               { FÅr die Zeitabfrage }
	 fontname = 'crosses';
         fadenr : array[1..3] of Byte = (92,93,94);

var x, y        : Integer;                        { Standardvariablen }
    cx, cy      : Integer;                        { Zeigerkoordinaten }
    ch          : Char;                           { Readkeyabfragevar }
    a           : Byte;                           { ZÑhlervariable    }
    fertig      : Boolean;                        { Ist das Spiel gerÑumt? }

    actual_tile : Byte;                           { Aktuelle Spielsteinfarbe}
    beenden     : Boolean;                        { Programm beenden ? }
    b           : Byte;                           { blinkender MenÅkasten }
    line        : Byte;                           { Scrolltextlinie }
    xlin, ylin  : Integer;                   { Koordinaten fÅr Scrolltext}

    levelfile   : file of levels;            { Leveldatei }
    levels2     : levels;                    { Variable des Typs Levels }
    lev         : Byte;                      { Levelnummer }

    score       : LongInt;                      { Punktzahl }
    hs          : highscore;                    { Variable des Typs Hiscore }
    hsf         : file of highscore;            { Hiscore-Datei }

    SpritePointers : array[0..10] of Pointer;   { Sprite-Zeiger }
    SpriteZaehler  : Byte;                      { Zaehlbyte fÅr die Sprites }
    ZeigerP        : Pointer;                   { Zeiger auf Cursor }
    SpriteFile     : File;                      { Sprite-Datei }

    zeit           : Word;                         { Spielzeit }
    scrolltext     : String;                       { selbsterklÑrend }
    Cheater        : Boolean;

Function File_Exists(name : String) : Boolean;  { Abfrage, ob Datei existiert}
Var
  DirInfo : SearchRec;
begin
  FindFirst(Name, AnyFile, DirInfo);
  if (DosError = 0) then
    File_Exists := True
  else
    File_Exists := False;
end;

procedure copy_area(xs, ys, xl, yl, xd, yd : Integer; fromwhere, towhere : Word);
{ Kopiert einen Bereich von einer Seite auf eine andere oder auf die gleiche
  Seite an einer beliebigen Stelle.

  XS, YS     : Quellkoordinaten (<S>ource)
  XL, YL     : Breite und LÑnge des Bereiches
  XD, YD     : Zielkoordinaten (<D>estination)
  FROMWHERE,
  TOWHERE    : Seitenadressen
}
var  y      : Word;
begin
     for y:=0 to yl do
         move(mem[fromwhere:(ys+y)*320+xs],mem[towhere:(yd+y)*320+xd],xl);
end;


{------------------------------------ ZAHLEN --------------------------------}
const nums : array[0..9,1..3] of Integer =
     ((298,148,13),(213,130,4),(218,130,12),(231,130,11),(243,130,14),
      (258,130,12),(272,130,12),(285,130,12),(297,130,13),
      (283,148,12));
      { Positionen und Breiten der Zahlen bei der Zeitanzeige }

var s          : String;
    nr         : String;
    num        : Byte;
    code       : Integer;
    zaehl      : Byte;
    spc        : Byte;

procedure write_number(xnr,ynr : Integer; nummer : LongInt);
{ Schreibt eine Nummer auf den Bildschirm }
begin
    str(nummer,s);
    spc:=0;
    bereich_loeschen(xnr,ynr,xnr+30,ynr+13,hiddenpage);
    for zaehl:=1 to length(s) do
    begin
        val(s[zaehl],num,code);
        copy_area(nums[num,1],nums[num,2],nums[num,3],13,xnr+spc,ynr,page_adr[2],hiddenpage);
        spc:=spc+nums[num,3]+2;
    end;
end;
{-------------------------------- ZAHLEN END --------------------------------}

procedure ablauf;                       { Spielablauf }
var actual_level : Byte;                { Aktuelles Level }
    zeitende     : Boolean;             { Ist die Zeit veronnen ?!?! }
    zspf         : Byte;                { Anzahl der aufgelîsten Tiles }
    gameover     : Boolean;             { Spielende: GAME OVER }
    gamesolved	 : Boolean;             { Spielende: GELôST }
    gamepal      : Dac_Block;           { Palette fÅr den Spielscreen }
    gameoverpal  : Dac_Block;           { Palette fÅr den Gameoverscreen }
    gamesolvedpal: Dac_Block;           { Palette fÅr den Endscreen }
    w            : Byte;                { Variable zum Tile-Setzen }
    savetime     : Word;                { Sichert die Zeit bei PAUSE }
    passwordlev  : Byte;                { ZÑhler fÅr Passwîrter (nach jedem
                                          fÅnften Level) }
 procedure load_sprites;                { Sprites laden }
 begin
      for SpriteZaehler:=0 to 8 do                    { Tiles, 8 St. }
          getmem(SpritePointers[SpriteZaehler],200);  { Speicher reservieren }
     getmem(zeigerp,70);                              { Cursor im Spiel }

     assign(spritefile,'sprites.crs');               { Spritefile zuweisen }
     reset(spritefile,1);                            { Spritefile îffnen }
     for SpriteZaehler:=0 to 8 do                    { wieder einmal zÑhlen }
         blockread(spritefile,spritepointers[Spritezaehler]^,200);
                                                     { Tiles lesen }
     blockread(spritefile,zeigerp^,70);              { Cursor lesen }
     close(spritefile);                              { Spritefile schlie·en }
 end;

 procedure put_sprite(nr : Byte; x, y : Integer; topage : Word);
 { Sprite setzen:
                 NR     = Spritenummer
                 X,Y    = Bildschirmkoordinaten
                 TOPAGE = Bildschirmadresse }
 begin
      put_image(spritepointers[nr],x,y,topage); { Sprites aus SpritePointers }
 end;

 procedure free_sprites;        { Reservierten Speicher wieder freigeben }
 begin
     for SpriteZaehler:=8 downto 0 do
        freemem(SpritePointers[SpriteZaehler],196);
     freemem(zeigerp,65);
 end;

 procedure set_pointer(x, y : Integer; Page : Word);
 { Cursor Setzen }
 begin
     puttransparent(x,y,zeigerp,0,page);
 end;

 {-- LEVEL ZEICHNEN --}
 procedure write_array(level : Byte);
 var crossx, crossy : Integer;          { Koordinaten des Spielfeldes }
     x1, y1         : Integer;          { Koordinaten fÅr Spriteausgabe }
     w              : Byte;             { Tile-Outfit }
     w2             : Byte;             { Wegen BOOLEAN-Abfrage }

 begin
      w:=levels2[level].til;            { Aktuelles Tile-Outfit }
      for crossx:=1 to 11 do
       for crossy:=1 to 11 do
       begin
        if levels2[level].pos[crossx,crossy]=true then w2:=w-1 else w2:=8;
        x1:=crossx*17-8;
        y1:=crossy*17-8;
        put_sprite(w2,x1,y1,hiddenpage);
       end;
     set_pointer(cx*17-5,cy*17-4,hiddenpage);
 end;


 {-- KREUZ SETZEN --}
 procedure put_cross(cx, cy : Integer); { Kreuze setzen }
 begin
     levels2[actual_level].pos[cx,cy]:=not levels2[actual_level].pos[cx,cy];

     if cx-1<1 then else
     levels2[actual_level].pos[cx-1,cy]:=not levels2[actual_level].pos[cx-1,cy];

     if cx+1>11 then else
     levels2[actual_level].pos[cx+1,cy]:=not levels2[actual_level].pos[cx+1,cy];

     if cy-1<1 then else
     levels2[actual_level].pos[cx,cy-1]:=not levels2[actual_level].pos[cx,cy-1];

     if cy+1>11 then else
     levels2[actual_level].pos[cx,cy+1]:=not levels2[actual_level].pos[cx,cy+1];
 end;

 procedure load_levels;                 { Levels laden }
 begin
      assign(levelfile,'levels.crs');
      reset(levelfile);
      read(levelfile,levels2);
      close(levelfile);
 end;

 function get_string(x, y : Integer) : String;
 { Zeichenstring lesen > GRAFIKMODUS }
 var ch   : Char;
    t, z : Byte;
    s2   : String;

 begin
     t:=0;
     s2:='';
     repeat
           ch:=readkey;
           if ch=#13 then else
           begin
                if ch=#8 then
                begin
                     if t=0 then else
                     begin
                          delete(s2,length(s2),1);
                          t:=t-8;
                          text_ausgeben(x+t,y,'€',0,hiddenpage);
                          flip;
                     end;
                end
                else
                begin
                     if ch in ['a'..'z','A'..'Z','0'..'9','#','*','+','-',#32] then
                     begin
                          s2:=s2+ch;
                          text_ausgeben(x+t,y,ch,1,hiddenpage);
                          t:=t+8;
                          flip;
                     end;
                end;
           end;
     until ch=#13;
     if length(s2)>20 then delete(s2,21,length(s2)-20);
     { String abschneiden, wenn die LÑnge 20 Åberschreitet (wegen HISCORE) }
     get_string:=s2;
 end;

 function get_string_ted(x,y, xw, yw : Integer; l : Byte) : String;
 var ch   : Char;
     z    : Byte;
     s2   : String;

 begin
       s2:='';
          repeat
           ch:=readkey;
           if ch=#13 then else
           begin
            if ch=#8 then
            begin
              delete(s2,length(s2),1);
              bereich_loeschen(x,y,xw,yw,hiddenpage);
              ted_writexy(x,y,s2,hiddenpage);
              flip;
            end
            else
            begin
             if ch in ['a'..'z','A'..'Z','0'..'9','#','*','+','-',#32] then
             begin
              s2:=s2+upcase(ch);
              bereich_loeschen(x,y,xw,yw,hiddenpage);
              ted_writexy(x,y,s2,hiddenpage);
              flip;
             end;
            end;
           end;
     until ch=#13;
     if length(s2)>l then delete(s2,l,length(s2)-l);
     get_string_ted:=s2;
 end;


 procedure get_score;  { Name vom User einlesen }
 const nrs : array[1..3] of String =
       ('YEAH. THE FIRST PLACE.',
        'GOOD. THE SECOND.',
        'SHIT. ONLY THE THIRD.');
 var nr : Byte;
     scrpal : DAC_BLOCK;

 begin
    ted_init('score',true);
    dac_block_lesen(1,255,scrpal);
    if score<hs[3].score then else
    begin
      if score>hs[1].score then
      begin
           nr:=1;
           hs[3]:=hs[2];
           hs[2]:=hs[1];
      end;
      if (score<hs[1].score) and (score>hs[3].score) then
      begin
           nr:=2;
           hs[3]:=hs[2];
      end;
      if (score>hs[3].score) and (score<hs[2].score) then nr:=3;
      hs[nr].score:=score;
      ted_writexy(0,0,nrs[nr],hiddenpage);
      ted_writexy(0,20,'ENTER YOUR NAME:',hiddenpage);
      blackenpalette;
      flip;
      fadein(scrpal);
      hs[nr].name:=get_string_ted(0,40,250,70,20);
      fadeout(scrpal);
      loesche_screen;
      loesche_page(hiddenpage);
      assign(hsf,'hiscore.crs');
      rewrite(hsf);
      write(hsf,hs);
      close(hsf);
      ted_done;
    end;
 end;

 procedure get_password;
 var passpal : Dac_Block;
     chpass  : CHAR;
     passtr  : String[8];
     zaehler : Byte;
     isit    : Boolean;
     this    : Byte;
     pstr    : String;
     pcount  : Byte;
     passchar: Char;

 begin
      isit:=false;
      ted_init('pass',true);
      dac_block_lesen(1,255,passpal);
      loesche_screen;
      loesche_page(hiddenpage);
      ted_writexy(0,0,'PASSWORD:',hiddenpage);
      blackenpalette;
      flip;
      fadein(passpal);
      passtr:=get_string_ted(0,50,319,140,8);
      actual_level:=1;
      zaehler:=1;
      this:=0;
      repeat
       pstr:='';
       for pcount:=1 to length(levels2[zaehler].password) do
       begin
            passchar:=chr(ord(levels2[zaehler].password[pcount])-pcount);
            pstr:=pstr+upcase(passchar);
       end;
       if pstr=passtr
       then isit:=true
       else inc(zaehler);
      until (isit=true) or (zaehler>95);
      if isit=true then actual_level:=zaehler;
      if passtr=#69+#77+#73+#84+#79+#78 then 
      begin
	cheater:=true;
        ted_writexy(0,100,'CHEATER',hiddenpage);
        flip; readkey;
      end;
      fadeout(passpal);
      loesche_screen;
      loesche_page(hiddenpage);
      ted_done;
 end;

 procedure showpassword;
 var passpal : Dac_Block;
     pw      : String;
     pcount  : Byte;
     passchar: Char;

 begin
      ted_init('pass',true);
      dac_block_lesen(1,255,passpal);
      ted_writexy(0,0,'CODE:',hiddenpage);

       pw:='';
       for pcount:=1 to length(levels2[actual_level].password) do
       begin
            passchar:=chr(ord(levels2[actual_level].password[pcount])-pcount);
            pw:=pw+upcase(passchar);
       end;

      ted_writexy(0,60,pw,hiddenpage);
      blackenpalette;
      flip;
      fadein(passpal);
      repeat ch:=readkey until ch=#13;
      fadeout(passpal);
      loesche_screen;
      loesche_page(hiddenpage);
      ted_done;
 end;

 procedure init_game;
 begin
      cx:=1; cy:=1;             { Startkoordinaten des Cursors }
      load_levels;
      get_password;
      zspf:=0;
      zeit:=0;
      score:=0;
      gameover:=false;
      gamesolved:=false;
 end;

{-- HAUPTPROZEDUR --}
begin
     total:=1;
     infodat[1]:='field.pcx';
     loadpcx(1,page_adr[2],true);
     load_sprites;
     dac_block_lesen(1,255,gamepal);
     init_game;

     copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);
     copy_area(198,5,122,120,195,5,page_adr[2],hiddenpage);

     write_number(276,105,zeit);
     write_array(actual_level);
     write_number(216,82,score);
     write_number(216,43,actual_level);

     blackenpalette;
     flip;
     fadein(gamepal);

     zeit:=levels2[actual_level].time+1;
     SetCycleTime(Wait);
     repeat

        while not timeover do
        begin
           if keypressed then
           begin

            ch:=readkey;
            if ord(ch)=0 then ch:=readkey;

            if ch in [#77,#75,#72,#80,#32,#27,#115,#83] then
            begin
             case ch of
                 #77 : begin
                            inc(cx);
                            if cx>11 then cx:=11;
                            if levels2[actual_level].pos[cx-1,cy]=true
                            then w:=levels2[actual_level].til-1
                            else w:=8;
                            put_sprite(w,(cx-1)*17-8,cy*17-8,hiddenpage)
                       end;
                 #75 : begin
                           dec(cx);
                           if cx<1 then cx:=1;
                            if levels2[actual_level].pos[cx+1,cy]=true
                            then w:=levels2[actual_level].til-1
                            else w:=8;
                            put_sprite(w,(cx+1)*17-8, cy*17-8,hiddenpage)
                       end;
                 #80 : begin
                           inc(cy);
                           if cy>11 then cy:=11;
                            if levels2[actual_level].pos[cx,cy-1]=true
                            then w:=levels2[actual_level].til-1
                            else w:=8;
                            put_sprite(w,cx*17-8,(cy-1)*17-8,hiddenpage)
                       end;
                 #72 : begin
                           dec(cy);
                           if cy<1 then cy:=1;
                            if levels2[actual_level].pos[cx,cy+1]=true
                            then w:=levels2[actual_level].til-1
                            else w:=8;
                            put_sprite(w,cx*17-8,(cy+1)*17-8,hiddenpage)
                       end;
                 #32 : begin
                           put_cross(cx,cy);
                           write_array(actual_level);
                           flip;
                       end;
                 #27 : gameover:=true;
            #83,#115 : begin
                            savetime:=zeit;
                            copy_area(204,145,62,10,230,130,page_adr[2],visiblepage);
                            readkey;
                            zeit:=savetime;
                            bereich_loeschen(210,155,271,165,visiblepage);
                       end;
             end;
             set_pointer(cx*17-5,cy*17-4,hiddenpage);
             flip;
            end;
           end;

            for x:=1 to 11 do
                for y:=1 to 11 do
                    if levels2[actual_level].pos[x,y]=false then inc(zspf);

             if actual_level>99 then gamesolved:=true;
             if gamesolved=false then
             begin
              if zspf=121 then
              begin
                fadeout(gamepal);
                if cheater=false then inc(score,zeit*5) else inc(score,zeit);
                inc(actual_level);
                if actual_level<=99 then
                begin
                 case actual_level of
                      5,10,15,20,25,30,35,40,45,
                      50,55,60,65,70,75,80,85,90,
                      95 : begin
                                 loesche_screen;
                                 loesche_page(hiddenpage);
                                 showpassword;
                                 copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);
                                 copy_area(198,5,122,120,195,5,page_adr[2],hiddenpage);
                            end;
                 end;
                 zeit:=levels2[actual_level].time+1;
                 bereich_loeschen(216,82,300,95,hiddenpage);
                 bereich_loeschen(216,43,300,56,hiddenpage);
                 write_array(actual_level);
                 write_number(216,82,score);
                 write_number(216,43,actual_level);
                 flip;
                 fadein(gamepal);
                end;
             end
             else zspf:=0;
            end;
           end;
           setcycletime(wait);
           if cheater=false then dec(zeit);
           write_number(276,105,zeit);
	   flip;
           trigger;

           if zeit<1 then gameover:=true;
     until (gameover=true) or (gamesolved=true);

     if gameover=true then
     begin
      fadeout(gamepal);
      loesche_page(hiddenpage);
      loesche_screen;
      total:=1;
      infodat[1]:='gameover.pcx';
      loadpcx(1,hiddenpage,true);
      dac_block_lesen(1,255,gameoverpal);
      blackenpalette;
      flip;
      fadein(gameoverpal);
      repeat ch:=readkey until ch=#27;
      fadeout(gameoverpal);
      loesche_screen;
      loesche_page(hiddenpage);
      get_score;
     end
     else
     begin
      loesche_page(hiddenpage);
      loesche_screen;
      total:=1;
      infodat[1]:='solved.pcx';
      loadpcx(1,hiddenpage,true);
      dac_block_lesen(1,255,gamesolvedpal);
      blackenpalette;
      flip;
      fadein(gamesolvedpal);
      repeat ch:=readkey until ch=#27;
      fadeout(gamesolvedpal);
      loesche_screen;
      loesche_page(hiddenpage);
      get_score;
     end;
     free_sprites;
end;

procedure hiscore;
var a1, a2, a3 : Byte;
    raus       : Boolean;
    scrstring  : String;
    hiscorepal : Dac_Block;
    fehlercode : Byte;

begin
     zeichensatz_laden('modern.zei',fehlercode);
     raus:=false;
     a1:=192; a2:=196; a3:=200;
     total:=1;
     infodat[1]:='hiscore.pcx';
     loadpcx(1,hiddenpage,true);
     dac_block_lesen(1,255,hiscorepal);
     blackenpalette;
     flip;
     fadein(hiscorepal);
     repeat
           while not keypressed do
           begin
                text_ausgeben(40,110,'1. '+hs[1].name,a1,hiddenpage);
                text_ausgeben(40,130,'2. '+hs[2].name,a2,hiddenpage);
                text_ausgeben(40,150,'3. '+hs[3].name,a3,hiddenpage);
                str(hs[1].score,scrstring);
                text_ausgeben(250,110,scrstring,a1,hiddenpage);
                str(hs[2].score,scrstring);
                text_ausgeben(250,130,scrstring,a2,hiddenpage);
                str(hs[3].score,scrstring);
                text_ausgeben(250,150,scrstring,a3,hiddenpage);
                flip;
                vret;
                inc(a1); if a1>223 then a1:=192;
                inc(a2); if a2>223 then a2:=192;
                inc(a3); if a3>223 then a3:=192;
           end;
           ch:=readkey;
           if ch<>#27 then raus:=false else raus:=true;
     until raus=true;
     fadeout(hiscorepal);
end;

procedure mainmenu;
type richtung = (links,rechts,oben,unten);
const posxy1 : array[1..3,1..2] of Integer =
               ((98,90),(95,116),(102,140));
      posxy2 : array[1..3,1..2] of Integer =
               ((222,114),(225,140),(212,165));

var a       : Byte;
    menupal : Dac_Block;
    gamepal : Dac_Block;

 procedure fadecolor(nr : Byte);
 const colors : array[0..31] of Byte =
                (0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,63,
                 60,56,52,48,44,40,36,32,28,24,20,16,12,8,4);
 begin
    dac_reg_setzen(fadenr[b],colors[a],colors[a],colors[a]);
    inc(a);
    if a>31 then a:=0;
 end;

begin
     a:=0;
     total:=1;
     infodat[1]:='menu.pcx';
     loadpcx(1,page_adr[4],true);
     page_to_page(page_adr[4],hiddenpage);
     dac_block_lesen(1,255,menupal);
     blackenpalette;

     ted_init(fontname,false);
     flip;
     fadein(menupal);
     repeat
           repeat
            while not keypressed do
            begin
                fadecolor(b);
                ted_scroll(scrolltext,175, hiddenpage);
                vret;
                flip;
            end;
            ch:=readkey;
           until ch in [#72,#80,#13];
           case ch of
                #72     : begin
                               dac_reg_setzen(fadenr[b],0,0,0);
                               dec(b);
                               if b<1 then b:=3;
                          end;
                #80     : begin
                               dac_reg_setzen(fadenr[b],0,0,0);
                               inc(b);
                               if b>3 then b:=1;
                          end;
                #13     : begin
                               case b of
                                    1 : begin
                                             fadeout(menupal);
                                             TED_DONE;
                                             loesche_screen;
					     loesche_page(hiddenpage);
                                             ablauf;
                                             xlin:=0; line:=1;
                                             loesche_page(hiddenpage);
                                             page_to_page(page_adr[4],hiddenpage);
                                             loesche_screen;
                                             blackenpalette;
                                             flip;
                                             fadein(menupal);
                                             TED_INIT(fontname,false);
                                        end;
                                    2 : begin
                                             fadeout(menupal);
                                             TED_DONE;
                                             loesche_page(hiddenpage);
                                             loesche_screen;
                                             hiscore;
                                             xlin:=0; line:=1;
                                             loesche_page(hiddenpage);
                                             page_to_page(page_adr[4],hiddenpage);
                                             loesche_screen;
                                             blackenpalette;
                                             flip;
                                             fadein(menupal);
                                             TED_INIT(fontname,false);
                                        end;
                                    3 : beenden:=true;
                               end;
                          end;
           end;
     until beenden=true;
     fadeout(menupal);
     TED_DONE;
end;

procedure init;
begin
     Cheater:=false;
     xlin:=0; line:=1; b:=1;
     beenden:=false;
     fertig:=false;
     cx:=1; cy:=1; score:=0;
     if file_exists('hiscore.crs') then
     begin
          assign(hsf,'hiscore.crs');
          reset(hsf);
          read(hsf,hs);
          close(hsf);
     end
     else
     begin
      for a:=1 to 3 do
      begin
           hs[a].score:=100;
           hs[a].name:='NONAME';
      end;
     end;
     actual_tile:=8;
     scrolltext:='     HELLO AND WELCOME TO ''CROSSES''.';
     scrolltext:=scrolltext+' WE HOPE THAT YOU LIKE THIS TINY THING.';
     scrolltext:=scrolltext+' ALL GRAPHICS AND MUSICS ARE BY RAVETRACER. CODE BY';
     scrolltext:=scrolltext+' RAVETRACER AND MYSTERIO OF CRYPTIC DEEP.';
     scrolltext:=scrolltext+' MORE INFORMATIONS ARE IN THE DOC-FILE.';
end;

procedure titel;
type inout = (ein, aus);
var x, y   : Integer;

    procedure fade(x,y : Integer; s : String;io : inout);
    var farbe : Byte;
    begin
         if io=ein then
         begin
                   for farbe:=122 to 159 do
                   begin
                        text_ausgeben(x,y,s,farbe,hiddenpage);
                        vret;
                        flip;
                   end;
         end
         else
         begin
                   for farbe:=160 downto 123 do
                   begin
                        text_ausgeben(x,y,s,farbe,hiddenpage);
                        vret;
                        flip;
                   end;
         end;
    end;

begin
     loesche_page(hiddenpage);
     total:=1;
     infodat[1]:='titel.pcx';
     loadpcx(1,page_adr[2],true);
     for y:=100 downto 0 do
     begin
         for x:=0 to 319 do
         begin
             linie_zeichnen(x,0,x,y,punktfarbe(x,y,page_adr[2]),hiddenpage);
             linie_zeichnen(x,199,x,199-y,punktfarbe(x,199-y,page_adr[2]),hiddenpage);
         end;
         vret;
         flip;
     end;
     while not keypressed do
     begin
          fade(70,180,'PRESS ANY KEY TO GO ON',ein);
          fade(70,180,'PRESS ANY KEY TO GO ON',aus);
     end;
     linie_zeichnen(0,100,319,100,0,page_adr[2]);
     linie_zeichnen(0,101,319,101,0,page_adr[2]);

     for y:=0 to 100 do
     begin
         for x:=0 to 319 do
         begin
               linie_zeichnen(x,0,x,y,punktfarbe(x,y,page_adr[2]),hiddenpage);
               linie_zeichnen(x,199,x,199-y,punktfarbe(x,199-y,page_adr[2]),hiddenpage);
         end;
         vret;
         flip;
     end;
     loesche_screen;
     loesche_page(hiddenpage);
     readkey;
end;


begin
     init;
     initvga(4);
     titel; 
     mainmenu;
     closevga;
end.