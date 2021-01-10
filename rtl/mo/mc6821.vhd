--------------------------------------------------------------------------------
-- Thomson MO5 / MO6
--------------------------------------------------------------------------------
-- DO 12/2019
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY mc6821 IS
  PORT (
    ------------------------------------
    ad     : IN  uv2;
    dw     : IN  uv8;
    dr     : OUT uv8;
    req    : IN  std_logic;
    ack    : OUT std_logic;
    wr     : IN  std_logic;
    
    ------------------------------------
    pa_i   : IN  uv8;
    pa_o   : OUT uv8;
    pa_d   : OUT uv8;
    pb_i   : IN  uv8;
    pb_o   : OUT uv8;
    pb_d   : OUT uv8;
    ca1    : IN  std_logic;
    ca2_i  : IN  std_logic;
    ca2_o  : OUT std_logic;
    cb1    : IN  std_logic;
    cb2_i  : IN  std_logic;
    cb2_o  : OUT std_logic;

    irqa   : OUT std_logic;
    irqb   : OUT std_logic;
    ------------------------------------
    clk      : IN std_logic;
    reset_na : IN std_logic
   );
END mc6821;

ARCHITECTURE rtl OF mc6821 IS

  SIGNAL cra,crb : uv6;
  SIGNAL pra,prb,ddra,ddrb : uv8;
  SIGNAL ca1_delay,cb1_delay,ca1_trans,cb1_trans : std_logic;
  SIGNAL ca2_delay,cb2_delay,ca2_trans,cb2_trans : std_logic;
  SIGNAL irqa1,irqa2,irqb1,irqb2 : std_logic;
  SIGNAL rd_ora_delay,rd_orb_delay,wr_orb_delay : boolean;
BEGIN
  
  ack<=req;
  pa_o<=pra;
  pb_o<=prb;
  pa_d<=ddra; -- DDR : 0=IN 1=OUT
  pb_d<=ddrb;
  
  PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      --------------------------------------------
      ca1_delay<=ca1;
      ca2_delay<=ca2_i;
      cb1_delay<=cb1;
      cb2_delay<=cb2_i;
      
      --------------------------------------------
      ca1_trans<=(ca1 XNOR cra(1)) AND (ca1_delay XOR cra(1));
      ca2_trans<=(ca2_i XNOR cra(4)) AND (ca2_delay XOR cra(4));
      
      cb1_trans<=(cb1 XNOR crb(1)) AND (cb1_delay XOR crb(1));
      cb2_trans<=(cb2_i XNOR crb(4)) AND (cb2_delay XOR crb(4));
      
      --------------------------------------------
    END IF;
  END PROCESS;
  
  PROCESS(reset_na,clk) IS
    VARIABLE rd_ora_v,rd_orb_v,wr_ora_v,wr_orb_v : boolean;
  BEGIN
    IF reset_na='0' THEN
      cra<="000000";
      crb<="000000";
      ddra<=x"00";
      ddrb<=x"00";
      pra<=x"FF";
      prb<=x"FF";
      irqa1<='0';
      irqa2<='0';
      irqb1<='0';
      irqb2<='0';
      
    ELSIF rising_edge(clk) THEN
      rd_ora_v:=false;
      wr_ora_v:=false;
      rd_orb_v:=false;
      wr_orb_v:=false;
      --------------------------------------------
      CASE ad IS
        WHEN "00"   => -- Peripheral / Data direction register A
          IF wr='1' AND req='1' THEN
            IF cra(2)='1' THEN pra<=dw; ELSE  ddra<=dw; END IF;
          END IF;
          dr<=mux(cra(2),(pa_i AND NOT ddra) OR (pra AND ddra),ddra);
          rd_ora_v:=(wr='0' AND req='1' AND cra(2)='1');
          wr_ora_v:=(wr='1' AND req='1' AND cra(2)='1');
          
        WHEN "01"   => -- Control register A
          IF wr='1' AND req='1' THEN
            cra<=dw(5 DOWNTO 0);
          END IF;
          dr<=irqa1 & irqa2 & cra;
          
        WHEN "10"   => -- Peripheral / Data direction register B
          IF wr='1' AND req='1' THEN
            IF crb(2)='1' THEN prb<=dw; ELSE  ddrb<=dw; END IF;            
          END IF;
          dr<=mux(crb(2),(pb_i AND NOT ddrb) OR (prb AND ddrb),ddrb);
          rd_orb_v:=(wr='0' AND req='1' AND crb(2)='1');
          wr_orb_v:=(wr='1' AND req='1' AND crb(2)='1');
          
        WHEN OTHERS => -- Control register B
          IF wr='1' AND req='1' THEN
            crb<=dw(5 DOWNTO 0);
          END IF;
          dr<=irqb1 & irqb2 & crb;
          
      END CASE;
      
      --------------------------------------------
      rd_ora_delay<=rd_ora_v;
      rd_orb_delay<=rd_orb_v;
      wr_orb_delay<=wr_orb_v;
      
      --------------------------------------------
      irqa1<=(irqa1 AND NOT to_std_logic(rd_ora_v)) OR ca1_trans;
      irqa2<=(irqa2 AND NOT to_std_logic(rd_ora_v)) OR (ca2_trans AND NOT cra(5));

      irqb1<=(irqb1 AND NOT to_std_logic(rd_orb_v)) OR cb1_trans;
      irqb2<=(irqb2 AND NOT to_std_logic(rd_orb_v)) OR (cb2_trans AND NOT crb(5));
      
      --------------------------------------------
      IF cra(5)='1' THEN
        IF cra(4)='0' THEN
          IF rd_ora_delay THEN
            ca2_o<='0';
          END IF;
          IF cra(3)='0' THEN
            IF ca1_trans='1' THEN
              ca2_o<='1';
            END IF;
          ELSE
            IF rd_ora_delay THEN
              ca2_o<='1';
            END IF;
          END IF;
        ELSE
          ca2_o<=cra(3);
        END IF;
      ELSE
        ca2_o<=ca2_i;
      END IF;
      
      --------------------------------------------
      IF crb(5)='1' THEN
        IF crb(4)='0' THEN
          IF wr_orb_delay THEN
            cb2_o<='0';
          END IF;
          IF crb(3)='0' THEN
            IF cb1_trans='1' THEN
              cb2_o<='1';
            END IF;
          ELSE
            IF wr_orb_delay THEN
              cb2_o<='1';
            END IF;
          END IF;
        ELSE
          cb2_o<=crb(3);
        END IF;
      END IF;
      
      --------------------------------------------
      
    END IF;
  END PROCESS;
  
  irqa <= (irqa1 AND cra(0)) OR (irqa2 AND cra(3)); 
  irqb <= (irqb1 AND crb(0)) OR (irqb2 AND crb(3)); 
  
END rtl;
