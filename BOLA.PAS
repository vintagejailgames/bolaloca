program bolaloca;
uses grafix,keyboard;

const dim_pala_ample=15;
      dim_pala_alt=19;
      dim_bola=8;
      dim_text=40;
      MARGE_INF_X=0;
      MARGE_SUP_X=323;
      MARGE_INF_Y=0;
      MARGE_SUP_Y=199;

type
      t_velocitat=record
             x,y:integer;
           end;
      t_bola=record
         x,y:word;
         velocitat:t_velocitat; {1,2,3,4}
       end;

      t_pala=record
         x,y:word;
       end;

      t_porteria=record
         x1,y1,x2,y2:word;
       end;

procedure black_out;
var i:word;
begin
   for i:=0 to 255 do set_color(i,0,0,0);
end;

function comprovar_fitxer_existeix(nom_fitxer:string):byte;
var f:file;
begin
    assign(f,nom_fitxer);
    {$I-}
	 reset(f);
	 {$I+}
	 if IOResult=0 then comprovar_fitxer_existeix:=1 {EXISTEIX}
	  else comprovar_fitxer_existeix:=0;  {NO EXISTEIX}
end;

procedure pinta_pilota(vaddr,mem_dest:word;ball:t_bola);
var i,j:byte;
    color:word;
begin
   put_sprite(vaddr,mem_dest,0,7,7,ball.x,ball.y);
end;

procedure pinta_pala(vaddr,mem_dest:word;x,y:word);
var i,j:byte;
    color:word;
begin
  put_sprite(vaddr,mem_dest,{320*35}11200,15,19,x,y);
end;

procedure pinta_seleccio(vaddr,mem_dest:word;num_sel:byte);
var i,j:byte;
    color:word;
    x,y:word;
begin
  case num_sel of
   1:begin x:=70;y:=111;end;
   2:begin x:=70;y:=133;end;
  end;
  put_sprite(vaddr,$a000,{320*86}27520,198,23,x,y);
end;

procedure retencio(vaddr,mem_dest:word);
var delay:byte;
begin
   put_sprite(vaddr,mem_dest,7,141,20,95,30);
   for delay:=1 to 60 do espera_vga;
end;

procedure mostra_marcador(vaddr,mem_dest:word;marca1,marca2:byte);
var i,j:byte;
    color:word;
begin
   put_sprite(vaddr,$a000,320*55+marca1*40,40,31,90,5);
   put_sprite(vaddr,$a000,320*55+marca2*40,40,31,200,130);
end;

procedure marcador(mem_num,mem_dest:word;marcador1,marcador2:byte);
var i:byte;
begin
   mostra_marcador(mem_num,mem_dest,marcador1,marcador2);
   for i:=0 to 60 do espera_vga;
end;

function test_colisio(ball:t_bola;pala:t_pala):byte;
{1=true;0=false}
var aux:byte;
    bola_xmin,bola_xmax,bola_ymin,bola_ymax:word;
    pala_xmin,pala_xmax,pala_ymin,pala_ymax:word;
begin
    aux:=1;
    bola_xmin:=ball.x;bola_xmax:=ball.x+dim_bola;
    bola_ymin:=ball.y;bola_ymax:=ball.y+dim_bola;
    pala_ymin:=pala.y;pala_ymax:=pala.y+dim_pala_alt;
    pala_xmin:=pala.x;pala_xmax:=pala.x+dim_pala_ample;

    if (pala_xmax<bola_xmin) then aux:=0;
    if (pala_xmin>bola_xmax) then aux:=0;
    if (pala_ymax<bola_ymin) then aux:=0;
    if (pala_ymin>bola_ymax) then aux:=0;
    test_colisio:=aux;
end;

function test_colisio2(ball:t_bola;goal:t_porteria):byte;
{1=true;0=false}
var aux:byte;
    bola_xmin,bola_xmax,bola_ymin,bola_ymax:word;
    goal_xmin,goal_xmax,goal_ymin,goal_ymax:word;
begin
    aux:=1;
    bola_xmin:=ball.x;bola_xmax:=ball.x+dim_bola;
    bola_ymin:=ball.y;bola_ymax:=ball.y+dim_bola;
    goal_ymin:=goal.y1;goal_ymax:=goal.y2;
    goal_xmin:=goal.x1;goal_xmax:=goal.x2;

    if (goal_xmax<bola_xmin) then aux:=0;
    if (goal_xmin>bola_xmax) then aux:=0;
    if (goal_ymax<bola_ymin) then aux:=0;
    if (goal_ymin>bola_ymax) then aux:=0;
    test_colisio2:=aux;
end;

procedure crea_bola(var ball:t_bola);
begin
   ball.x:=5;
   ball.y:=20;
   ball.velocitat.x:=6;
   ball.velocitat.y:=ball.velocitat.x-random(5);
end;

procedure mou_bola(var ball:t_bola);
var ajustar:byte;
begin
   ajustar:=0;
   if (ball.x+ball.velocitat.x>=MARGE_SUP_X-dim_bola) then
	begin
	  ball.velocitat.x:=-ball.velocitat.x;
     ball.x:=313;
     ajustar:=1
   end;
   if (ball.x+ball.velocitat.x<=MARGE_INF_X) then
   begin
	  ball.velocitat.x:=-ball.velocitat.x;
     ball.x:=0;
     ajustar:=1
   end;
   if (ball.y+ball.velocitat.y>=MARGE_SUP_Y-dim_bola) then ball.velocitat.y:=-ball.velocitat.y;
   if (ball.y+ball.velocitat.y<=MARGE_INF_Y) then ball.velocitat.y:=-ball.velocitat.y;

   if ajustar=0 then
   begin
     ball.x:=ball.x+ball.velocitat.x;
     ball.y:=ball.y+ball.velocitat.y;
   end;

end;

procedure seguir_bola(var pala:t_pala;ball:t_bola);
begin
   if (ball.y<pala.y) and (pala.y>3) then pala.y:=pala.y-2
    else if (ball.y>pala.y) and (pala.y<199-dim_pala_alt) then pala.y:=pala.y+2;

   if (ball.x>pala.x) and (pala.x<305-dim_pala_ample) then pala.x:=pala.x+2
    else if (pala.x>250) then pala.x:=pala.x-2;
end;

procedure equivocarse(var pala:t_pala;ball:t_bola);
begin
   if (ball.y>pala.y) and (pala.y>3) then pala.y:=pala.y-2
    else if (ball.y<pala.y) and (pala.y<199-dim_pala_alt) then pala.y:=pala.y+2;

   if (ball.x>pala.x) and (pala.x<305-dim_pala_ample) then pala.x:=pala.x+2
    else if (pala.x>250) then pala.x:=pala.x-2;
end;

procedure intel_pala(var pala_ord:t_pala;ball:t_bola);
var inteligencia:byte;
begin
   inteligencia:=random(10)+1;
   case inteligencia of
    1..6:seguir_bola(pala_ord,ball);
    7..10:equivocarse(pala_ord,ball);
   end;
end;

procedure mou_pala(var pala_A,pala_B:t_pala;mode:byte;ball:t_bola);
begin
   if mode =2 then
   begin
    if teclapuls(keya) then begin if pala_A.x>3 then pala_A.x:=pala_A.x-2;end;
    if teclapuls(keys) then begin if pala_A.y<197-dim_pala_alt then pala_A.y:=pala_A.y+2;end;
	 if teclapuls(keyd) then begin if pala_A.x<70-dim_pala_ample then pala_A.x:=pala_A.x+2;end;
	 if teclapuls(keyw) then begin if pala_A.y>3 then pala_A.y:=pala_A.y-2;end;

    if teclapuls(keyarrowleft) then begin if pala_B.x>250 then pala_B.x:=pala_B.x-2;end;
    if teclapuls(keyarrowdown) then begin if pala_B.y<197-dim_pala_alt then pala_B.y:=pala_B.y+2;end;
    if teclapuls(keyarrowright) then begin if pala_B.x<316-dim_pala_ample then pala_B.x:=pala_B.x+2;end;
    if teclapuls(keyarrowup) then begin if pala_B.y>3 then pala_B.y:=pala_B.y-2;end;
   end
   else
   begin
    if teclapuls(keya) then begin if pala_A.x>3 then pala_A.x:=pala_A.x-2;end;
    if teclapuls(keys) then begin if pala_A.y<197-dim_pala_alt then pala_A.y:=pala_A.y+2;end;
	 if teclapuls(keyd) then begin if pala_A.x<70-dim_pala_ample then pala_A.x:=pala_A.x+2;end;
	 if teclapuls(keyw) then begin if pala_A.y>3 then pala_A.y:=pala_A.y-2;end;

    intel_pala(pala_B,ball);
   end;
end;

procedure crea_pala(var pala:t_pala;x,y:word);
begin
   pala.x:=x;
   pala.y:=y;
end;

procedure crea_porteria(var goal:t_porteria;x1,y1,x2,y2:word);
begin
   goal.x1:=x1;
   goal.y1:=y1;
   goal.x2:=x2;
   goal.y2:=y2
end;

procedure rebot(var ball:t_bola);
var signe_x,signe_y:integer;
    nova_velocitat_x,nova_velocitat_y:integer;
begin
   signe_x:=ball.velocitat.x div abs(ball.velocitat.x);
   signe_y:=ball.velocitat.y div abs(ball.velocitat.y);
   nova_velocitat_x:=(random(6)+1);
   nova_velocitat_y:=(random(6)+1);
   ball.velocitat.x:=-(signe_x*nova_velocitat_x);
   ball.velocitat.y:=-(signe_y*nova_velocitat_y);
end;

procedure play_titol(var comptador:byte);
{4,12}
var red3_aux,blue3_aux,green3_aux:byte;
    red5_aux,blue5_aux,green5_aux:byte;
    esperes:byte;
begin
    if comptador=1 then
    begin
      get_color(3,red3_aux,green3_aux,blue3_aux);
      get_color(5,red5_aux,green5_aux,blue5_aux);
      espera_vga;
      set_color(3,red5_aux,green5_aux,blue5_aux);
      set_color(5,red3_aux,green3_aux,blue3_aux);
    end;
    if comptador=12 then comptador:=0;
end;


procedure titol1;
var vscr:ptr_pantalla;
    scr:word;
    copia_fila,nova_fila:array [0..319] of byte;
    i,y,amunt:word;

begin
    setup_virtual(vscr,scr);
    load_pcx('jail_g.pcx',scr);
    y:=40;
    amunt:=0;
    repeat
     for i:=0 to 319 do copia_fila[i]:=mem[$a000:320*y+i];
     for i:=0 to 319 do
      if copia_fila[i]=0 then nova_fila[i]:=0 else nova_fila[i]:=215;
     put_sprite(scr,$a000,0,68,23,118,71); {JAIL}
     put_sprite(scr,$a000,71,96,23,105,98); {GAMES}
     for i:=0 to 319 do mem[$a000:320*y+i]:=nova_fila[i];
     espera_vga;
    if (y<130) and (amunt=0) then inc(y);
    if y=130 then amunt:=1;
    if (amunt=1) and (y>30) then dec(y)
	 until (y=35);
end;

procedure intro(var sel:byte);
var tecla,count:byte;
    virt6,virt5,virt_vga:ptr_pantalla;
    vaddr6,vaddr5,vga2:word;
    paleta_titol:t_paleta;
begin
   setup_virtual(virt_vga,vga2);
   setup_virtual(virt5,vaddr5);
   setup_virtual(virt6,vaddr6);
   cls(0,vga2);
   load_pcx('sprites.pcx',vaddr5);
   load_pcx('titol.pcx',vaddr6);
   get_paleta(paleta_titol);
   black_out;
   volcar_pantalla(vaddr6,$a000);
   fadeup(paleta_titol,1);
   sel:=1;
   pinta_seleccio(vaddr5,vga2,sel);
   count:=0;
   repeat
    play_titol(count);
    espera_vga;
    volcar_pantalla(vga2,$a000);

    inc(count);
    volcar_pantalla(vaddr6,vga2);
    if qteclapuls then tecla:=agarrartecla;
    case tecla of
     keyw:begin sel:=1;pinta_seleccio(vaddr5,vga2,sel);end;
     keys:begin sel:=2;pinta_seleccio(vaddr5,vga2,sel);end;
     else pinta_seleccio(vaddr5,vga2,sel);
    end;
   until (tecla=keyenter);
   tancar_virtual(virt5);
   tancar_virtual(virt6);
   tancar_virtual(virt_vga);
end;

function seleccionar_pista:byte;
var scr_select,scr_vga:ptr_pantalla;
    p_select,vga2:word;
    key,sel:byte;

procedure generar_pantalla_seleccio;
begin
    put_sprite(p_select,vga2,0,130,9,5,5);
    put_sprite(p_select,vga2,130,73,33,200,50);
    put_sprite(p_select,vga2,203,73,33,200,90);
end;

procedure mostrar_pista(sel:byte);
begin
   case sel of
    0:put_sprite(p_select,vga2,320*9,128,79,15,60);
    1:put_sprite(p_select,vga2,320*33+128,128,79,15,60);
   end;
end;

procedure pintar_seleccio(tecla:byte;var sel:byte);
begin
   case tecla of
    keyw:sel:=0;
    keys:sel:=1;
   end;
   case sel of
    0:put_sprite(p_select,vga2,320*88,77,37,198,48);
    1:put_sprite(p_select,vga2,320*88,77,37,198,88);
   end;
end;


begin
    setup_virtual(scr_select,p_select);
    setup_virtual(scr_vga,vga2);
    cls(2,vga2);
    load_pcx('select.pcx',p_select);
    sel:=0;
    key:=keyw;
    repeat
     if qteclapuls then key:=agarrartecla;
     generar_pantalla_seleccio;
     pintar_seleccio(key,sel);
     mostrar_pista(sel);
     volcar_pantalla(vga2,$a000);
     cls(2,vga2);
	 until teclapuls(keyenter);
    seleccionar_pista:=sel;
    tancar_virtual(scr_select);
    tancar_virtual(scr_vga);
end;

var scr_sprites,scr_fondo,scr_vga2:ptr_pantalla;
    p_sprites,p_fondo,vga2:word;
    bola1:t_bola;
    sel,marcador1,marcador2,existeix_fitxer:byte;
    pala1,pala2:t_pala;
    porteria1,porteria2,paret_fondo1,paret_fondo2:t_porteria;
    colisio,temps,posesio_pala1,posesio_pala2,pista:byte;

procedure init_joc;
begin
   randomize;
   sel:=1;
   marcador1:=0;
   marcador2:=0;

   setup_virtual(scr_sprites,p_sprites);
   cls(0,p_sprites);

   setup_virtual(scr_fondo,p_fondo);
   cls(0,p_fondo);

   setup_virtual(scr_vga2,vga2);
   cls(0,vga2);

   load_pcx('sprites.pcx',p_sprites);

   crea_porteria(porteria1,0,81,3,119);
   crea_porteria(porteria2,317,81,320,119);

   crea_porteria(paret_fondo1,0,0,3,199);
   crea_porteria(paret_fondo2,317,0,320,199);

end;


procedure comensar;
begin
   crea_bola(bola1);
   crea_pala(pala1,15,80);
   crea_pala(pala2,290,100);
   volcar_pantalla(p_fondo,vga2);
   pinta_pilota(p_sprites,vga2,bola1);
   pinta_pala(p_sprites,vga2,pala1.x,pala1.y);
   pinta_pala(p_sprites,vga2,pala2.x,pala2.y);
   volcar_pantalla(vga2,$a000);
   marcador(p_sprites,vga2,marcador1,marcador2);
   posesio_pala1:=0;
   posesio_pala2:=0;
   temps:=0;{este temps val per a que la pilota no se quede enganxada en la
             pala del ordinador, una vegada que haja colisionat s'activa
             i es desactiva en el bucle seguent}
   colisio:=0
end;

procedure acabar;
begin
   desinstalarkb;
   setmode($3);
   tancar_virtual(scr_sprites);
   tancar_virtual(scr_fondo);
   tancar_virtual(scr_vga2);
end;

procedure comprobar_retencio;
begin
  if (posesio_pala1>10) or (posesio_pala2>10) then
    begin
	   retencio(p_sprites,$a000);
      comensar
    end;
end;

procedure select_pista;
begin
  pista:=seleccionar_pista;
   	case pista of
    		0:load_pcx('camp.pcx',p_fondo);
    		1:load_pcx('camp2.pcx',p_fondo);
   	end;
end;

begin
   instalarkb;
   setmode($13);
   titol1;
   init_joc;
   repeat
   	intro(sel);
      borrarkb;
      if paramstr(1) <> '' then existeix_fitxer:=comprovar_fitxer_existeix(paramstr(1));
      if existeix_fitxer=0 then select_pista
       else load_pcx(paramstr(1),p_fondo);
   	comensar;
   	repeat

    		mou_bola(bola1);

    		mou_pala(pala1,pala2,sel,bola1);
    		mou_pala(pala1,pala2,sel,bola1);
    		if (test_colisio(bola1,pala1)=1) and (colisio=0) then
    		begin
       		rebot(bola1);
       		inc(posesio_pala1);
       		posesio_pala2:=0;
       		colisio:=1
    		end;

    		if (test_colisio(bola1,pala2)=1) and (colisio=0) then
    		begin
       		rebot(bola1);
       		inc(posesio_pala2);
       		posesio_pala1:=0;
       		colisio:=1
    		end;

    		pinta_pilota(p_sprites,vga2,bola1);
    		pinta_pala(p_sprites,vga2,pala1.x,pala1.y);
    		pinta_pala(p_sprites,vga2,pala2.x,pala2.y);

    		espera_vga;
    		volcar_pantalla(vga2,$a000);

    		comprobar_retencio;

    		if test_colisio2(bola1,porteria1)=1 then
    		begin
       		marcador2:=marcador2+1;
       		espera_vga;
       		comensar;
    		end;

    		if test_colisio2(bola1,porteria2)=1 then
    		begin
       		marcador1:=marcador1+1;
       		espera_vga;
       		comensar;
    		end;

    		if test_colisio2(bola1,paret_fondo1)=1 then posesio_pala2:=0;

    		if test_colisio2(bola1,paret_fondo2)=1 then posesio_pala1:=0;

    		if colisio=1 then inc(temps);
    		if temps=3 then
    		begin
	  			colisio:=0;
     			temps:=0
    		end;

    		volcar_pantalla(p_fondo,vga2);
   	until (teclapuls($01)) or (marcador1=7) or (marcador2=7);
   marcador1:=0;
   marcador2:=0;
   until (teclapuls($01));
   acabar;
end.