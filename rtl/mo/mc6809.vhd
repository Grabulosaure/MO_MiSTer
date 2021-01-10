--------------------------------------------------------------------------------
-- MC6809 CPU
--------------------------------------------------------------------------------
-- DO 8/2017
--------------------------------------------------------------------------------

-- Data bus :
--    A,DW : Valid when REQ=ACK=1
--    DR   : Sampled one cycle _after_ REQ=ACK=1
--    WR   : 0=Read 1=Write
--    PHZ  : Transfer type : Opcode/Post-bytes/Data/Stack/Nothing
--    REQ  : Request transfer. Always set except in waiting stata (CWAI, SYNC)
--    ACK  : Acknowledge transfer.
--    Transfers occurs when REQ=ACK=1.

-- If the system clock is faster than the CPU clock,
--   the ACK signal can be gated with a clock-enable signal.

-- Interrupts are edge sensitive, positive.

-- To wake the CPU in SYNC state, either trigger an interrupt or a pulse on the
-- wake signal  (no short 3-cycles interrupts pulses1)

-- FAST : When FAST=0, this CPU tries to execute instructions with the same
--        number of cycles as a real MC6809. Cycles are counted when REQ=ACK=1
--        When FAST=1, many instructions are faster. FAST level can be changed
--        at any time.

-- <Invalid opcodes may behave differently from a real MC6809>
-- <No support for HM6309 instructions>

--------------------------------------------------------------------------------
-- This design can be used for any purpose.

-- Please send any bug report, suggestion or remark to : dev@temlib.org
--------------------------------------------------------------------------------
-- Version 201708
--   Initial
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;


USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;
USE work.mc6809_pack.ALL;

ENTITY mc6809 IS
  PORT (
    ad   : OUT uv16;      -- Address bus
    dw   : OUT uv8;       -- Data write
    dr   : IN  uv8;       -- Data read

    req  : OUT std_logic;
    ack  : IN  std_logic;
    wr   : OUT std_logic;
    
    ph   : OUT uv3; -- 000=CODE 001=POST 010=DATA 011=STACK 1xx=Dummy
    
    irq   : IN std_logic;
    firq  : IN std_logic;
    nmi   : IN std_logic;
    reset : IN std_logic;
    
    wake  : IN std_logic;
    
    fast : IN  std_logic;
    xpc : OUT uv16;
    
    clk      : IN std_logic;
    reset_na : IN std_logic
    );
END ENTITY mc6809;

ARCHITECTURE rtl OF mc6809 IS
  ------------------------------------------------
  SIGNAL req_i : std_logic;
  SIGNAL dw_i : uv8;
  SIGNAL ad_i : uv16;
  SIGNAL ph_i : uv3;
  SIGNAL wr_i : std_logic;
  SIGNAL dr_mem : uv8;
  SIGNAL ack_delay : std_logic;

  ------------------------------------------------
  SIGNAL ra,rb : uv8;     -- A, B registers (accumulators)
  SIGNAL rx,ry,ru : uv16; -- X, Y, U registers (indexes)
  SIGNAL ccr : type_ccr;  -- Flags register
  SIGNAL rsp : uv16;      -- Stack Pointer registers
  SIGNAL rdp : uv8;       -- Direct Page register
  SIGNAL pc_next  : uv16;     -- Program Counter
  SIGNAL pc : uv16;
  SIGNAL rt  : uv16;      -- Temporary register

  ------------------------------------------------
  SIGNAL op  : uv8;       -- Opcode
  SIGNAL post : uv8;      -- Post Byte
  SIGNAL dec : type_op;
  SIGNAL pre : natural RANGE 0 TO 2; -- Frefixes : 0=No 1=x10 2=x11
  SIGNAL alt : std_logic; -- Alternance for 16bits mem. accesses
  SIGNAL sel_stack : std_logic; -- Select stack for push/pop : 0=SP 1=U
  SIGNAL cycle : natural RANGE 0 TO 7;
  SIGNAL ea : uv16;
  SIGNAL rdw : uv8;

  ------------------------------------------------
  SIGNAL doint : std_logic;
  SIGNAL ivec : uv3;
  SIGNAL reset_pend,nmi_pend : std_logic;
  SIGNAL reset_ack, nmi_ack  : std_logic;
  SIGNAL reset_pre, nmi_pre  : std_logic;
  
  ------------------------------------------------
  SIGNAL xwait : natural RANGE 0 TO 15; -- Wait states for MC6809 cycle accuracy
  SIGNAL iwait : natural RANGE 0 TO 15; -- Wait states for indexed addressing
                                        -- 
  TYPE   enum_state IS (sFETCH,
                        sRELATIVE,sRELATIVE_BSR,
                        sIMMEDIATE,sDIRECT,sEXTENDED,
                        sINDEXED,sINDEXED2,
                        sPUSH,sPUSH_PRE,sPULL,sPULL_PRE,
                        sBSR,
                        sEA,sEA_FETCH,sEA_WRITE,
                        sINDIRECT,sINDIRECT2,
                        sEXG,
                        sDELAY,
                        sRTS,sSWI,sCWAI,
                        sSYNC,
                        sINT,
                        sINTERRUPT2,sINTERRUPT3);

  SIGNAL state : enum_state;

  CONSTANT PH_CODE  : uv3 :="000"; -- Opcode
  CONSTANT PH_POST  : uv3 :="001"; -- Post-bytes, immediate values
  CONSTANT PH_DATA  : uv3 :="010"; -- Data memory access
  CONSTANT PH_STACK : uv3 :="011"; -- Stack push/pull
  CONSTANT PH_VOID  : uv3 :="100"; -- No memory access.

  ------------------------------------------------
  FILE fil : text OPEN write_mode IS "trace.log";
  SIGNAL ad_delay : uv16; -- Trace
  SIGNAL trace_state : enum_state;
  SIGNAL trace_pre   : natural RANGE 0 TO 2;
  SIGNAL trace_dr : uv8;
  SIGNAL trace_ni : std_logic;
  SIGNAL trace_cyc : natural :=0;
  SIGNAL dias : string(1 TO 8);
  
BEGIN
  
  Machine:PROCESS(clk,reset_na) IS
    VARIABLE dr_v : uv8;
    VARIABLE dec_v : type_op;
    VARIABLE or_v : enum_reg; -- Output register select
    VARIABLE oml_v,omh_v : std_logic; -- Output register update HI/LO
    VARIABLE il1_v,ih1_v,il2_v,ih2_v : uv8; -- ALU inputs HI/LO
    VARIABLE ol_v,oh_v : uv8; -- ALU output HI/LO
    VARIABLE occ_v : type_ccr;
    VARIABLE aluop_v : std_logic;
    VARIABLE rtl_v,rth_v : uv8;
    VARIABLE op_v : uv8;
    VARIABLE ea_v : uv16;
    VARIABLE idx_v : uv16;
    VARIABLE idx_maj_v : std_logic;
    VARIABLE pc_v : uv16;
    VARIABLE state_v : enum_state;
    VARIABLE xwait_v : natural RANGE 0 TO 15;
    VARIABLE post_v : uv8;
    VARIABLE pcnext_rt_v : uv16;
    
  BEGIN
    IF reset_na='0' THEN
      state<=sFETCH;
      pre<=0;
      pc_next<=x"FFFE"; -- RESET vector
      reset_pend<='1';
      nmi_pend<='0';
      reset_pre<='1';
      reset_ack<='0';
      doint<='0';
      
    ELSIF rising_edge(clk) THEN
      occ_v:=ccr;
      oml_v:='0';
      omh_v:='0';
      aluop_v:='0';
      state_v:=state;
      xwait_v:=xwait;
      rth_v:=rt(15 DOWNTO 8);
      rtl_v:=rt(7 DOWNTO 0);
      reset_ack<='0';
      nmi_ack<='0';
      pc_v:=pc;
      
      ----------------------------------------------------
      ack_delay<=ack;

      IF ack='1' AND ack_delay='0' THEN
        dr_v:=dr_mem;
      ELSE
        dr_v:=dr;
      END IF;
      
      IF ack='1' THEN
        dr_mem<=dr;
      END IF;
      trace_dr<=dr;
      trace_state<=state;
      trace_pre  <=pre;
      trace_ni<=ack_delay;

      pcnext_rt_v:=pc_next+(rt(15 DOWNTO 8) & dr_v);
      
      ----------------------------------------------------
      IF state=sFETCH THEN
        op_v:=dr_v;
        op<=dr_v;
        dec_v:=opcodes(to_integer(dr_v));
        dec<=dec_v;
      ELSE
        dec_v:=dec;
        op_v:=op;
      END IF;

      IF req_i='1' AND ack='1' AND state/=sDELAY AND
        state/=sCWAI AND state/=sSYNC THEN
        cycle<=cycle+1;
      END IF;
      
      ----------------------------------------------------
      CASE dec_v.r IS -- Input register select
        WHEN M => il1_v:=dr_v; ih1_v:=dr_v; or_v:=M;
        WHEN A => il1_v:=ra; ih1_v:=ra; or_v:=A;
        WHEN B => il1_v:=rb; ih1_v:=rb; or_v:=B;
        WHEN X_Y_S =>
          IF pre=0 THEN
            il1_v:=rx(7 DOWNTO 0); ih1_v:=rx(15 DOWNTO 8);
            or_v:=X;
          ELSIF pre=1 THEN
            il1_v:=ry(7 DOWNTO 0); ih1_v:=ry(15 DOWNTO 8);
            or_v:=Y;
          ELSE
            il1_v:=rsp(7 DOWNTO 0); ih1_v:=rsp(15 DOWNTO 8);
            or_v:=SP;
          END IF;
        WHEN D_D_U =>
          IF pre/=2 THEN
            il1_v:=rb; ih1_v:=ra;
            or_v:=D;
          ELSE
            il1_v:=ru(7 DOWNTO 0); ih1_v:=ru(15 DOWNTO 8);
            or_v:=U;
          END IF;
        WHEN U_S =>
          IF pre=0 THEN
            il1_v:=ru(7 DOWNTO 0); ih1_v:=ru(15 DOWNTO 8);
            or_v:=U;
          ELSE
            il1_v:=rsp(7 DOWNTO 0); ih1_v:=rsp(15 DOWNTO 8);
            or_v:=SP;
          END IF;
        WHEN OTHERS =>
          il1_v:=dr_v; ih1_v:=dr_v;
          or_v:=M;
          
      END CASE;
      
      il2_v:=dr_v;
      ih2_v:=dr_v;
      
      ----------------------------------------------------
      IF ack='1' OR req_i='0' THEN
        
        CASE state IS
          --------------------------------------------------
          WHEN sFETCH =>
            -- An instruction was fetched, let's decode it.
            alt<='0';
            ea<=x"0000";
            pc_v:=pc_next;
            cycle<=0;
            sel_stack<='0';
            xwait_v:=0;
            iwait<=0;
            
            IF nmi_pend='1' OR (firq='1' AND ccr.f='0') OR reset_pend='1' OR
              (irq='1' AND ccr.i='0') THEN -- Pending interrupt
              state_v:=sINT;
              pc_v:=pc;
              
            ELSIF dr_v=x"10" THEN -- Prefix
              pre<=1;
              
            ELSIF dr_v=x"11" THEN
              pre<=2;
              
            ELSE
              IF pre=0 THEN
                dias<=opcodes(to_integer(dr_v)).dis;
              ELSIF pre=1 THEN
                dias<=opcodes(to_integer(dr_v)).dis10;
              ELSE
                dias<=opcodes(to_integer(dr_v)).dis11;
              END IF;
              
              CASE dec_v.m IS
                --------------------------------
                WHEN RELATIVE => -- Bcc, LBcc, ...
                  state_v:=sRELATIVE;
                  
                WHEN RELATIVE_BSR => -- BSR, LBSR ...
                  state_v:=sRELATIVE_BSR;
                  
                --------------------------------
                WHEN INHERENT =>
                  IF op_v=x"3A" THEN -- ABX
                    or_v:=X;
                    xwait_v:=2;
                    op_abx(rx,rb,ol_v,oh_v,oml_v,omh_v);
                    
                  ELSIF op_v=x"19" THEN -- DAA
                    or_v:=A;
                    xwait_v:=1;
                    op_daa(ra,ccr,ol_v,oml_v,occ_v);
                    
                  ELSIF op_v=x"1D" THEN -- SEX
                    or_v:=A;
                    xwait_v:=1;
                    op_sex(rb,ccr,ol_v,oml_v,occ_v);
                    
                  ELSIF op_v=x"3D" THEN -- MUL
                    xwait_v:=10;
                    or_v:=D;
                    op_mul(ra,rb,ccr,ol_v,oh_v,oml_v,omh_v,occ_v);

                  ELSIF op_v=x"39" THEN -- RTS
                    state_v:=sRTS;
                    sel_stack<='0';
                    
                  ELSIF op_v=x"3B" THEN -- RTI 
                    state_v:=sPULL_PRE; -- Same code as PULS
                    sel_stack<='0';
                    IF ccr.e='1' THEN
                      post<=x"FF"; -- CCR + A + B + DP + X + Y + U + PC
                    ELSE
                      post<=x"81"; -- CCR + PC
                    END IF;
                    
                  ELSIF op_v=x"3F" THEN -- SWI
                    state_v:=sPUSH_PRE; -- SWI starts as a PSHS
                    sel_stack<='0';
                    post<=x"FF";
                    alt<='0';
                    occ_v.e:='1';
                    
                  ELSIF op_v=x"12" THEN -- NOP
                    xwait_v:=1;
                    
                  ELSIF op_v=x"13" THEN -- SYNC
                    state_v:=sSYNC;
                    
                  ELSE -- Remaining Inherent unary ALU operations
                    il2_v:=il1_v;
                    ih2_v:=ih1_v;
                    state_v:=sFETCH;
                    xwait_v:=1;
                    aluop_v:='1';
                  END IF;
                  
                --------------------------------
                WHEN IMMEDIATE =>
                  state_v:=sIMMEDIATE;
                  
                --------------------------------
                WHEN DIRECT =>
                  state_v:=sDIRECT;

                WHEN EXTENDED =>
                  state_v:=sEXTENDED;
                  
                WHEN INDEXED =>
                  state_v:=sINDEXED;

              --------------------------------
              END CASE;
            END IF;
            
          --------------------------------------------------
          WHEN sRELATIVE => -- BRA,LBRA,BRcond, LBRcond
            xwait_v:=1;
            alt<='1';
            pc_v:=pc_next;
            rth_v:=dr_v;
            
            IF pre=0 AND op/=x"16" THEN -- BRA, BRcond
              IF bcond(op_v,ccr) THEN
                pc_v:=pc_next+sext(dr_v,16);
                state_v:=sDELAY;
              ELSE
                state_v:=sFETCH;
              END IF;

            ELSIF op=x"16" THEN -- LBRA
              IF alt='1' THEN
                pc_v:=pcnext_rt_v; --pc_next+(rt(15 DOWNTO 8) & dr_v);
                state_v:=sDELAY;
              END IF;
              xwait_v:=2;
              
            ELSE -- LBRcond
              IF alt='1' THEN
                IF bcond(op_v,ccr) THEN
                  pc_v:=pcnext_rt_v; --pc_next+(rt(15 DOWNTO 8) & dr_v);
                  state_v:=sDELAY;
                  xwait_v:=2;
                ELSE
                  state_v:=sFETCH;
                END IF;
              END IF;
            END IF;
            
          --------------------------------------------------
          WHEN sRELATIVE_BSR =>
            alt<='1';
            rth_v:=dr_v;
            rtl_v:=pc_next(15 DOWNTO 8);
            
            IF op(7)='1' THEN -- BSR
              rdw<=pc_next(7 DOWNTO 0);
              xwait_v:=3;
              rsp<=rsp-1;
              state_v:=sBSR;
              cycle<=0;
              pc_v:=pc_next+sext(dr_v,16);
            ELSE -- LBSR
              xwait_v:=4;
              IF alt='1' THEN
                rdw<=pc_next(7 DOWNTO 0);
                rsp<=rsp-1;
                state_v:=sBSR;
                cycle<=0;
                pc_v:=pcnext_rt_v; --pc_next+(rt(15 DOWNTO 8) & dr_v);
              ELSE
                pc_v:=pc_next;
              END IF;
            END IF;
            
          WHEN sBSR =>
            rdw<=rt(7 DOWNTO 0);
            IF cycle=0 THEN
              rsp<=rsp-1;
            ELSE
              state_v:=sDELAY;
            END IF;
            
          --------------------------------------------------
          WHEN sIMMEDIATE =>
            post<=dr_v;
            pc_v:=pc_next;
            
            IF op_v=x"1F" OR op_v=x"1E" THEN -- TFR, EXG
              CASE dr_v(7 DOWNTO 4) IS
                WHEN "0000" => ol_v:=rb;  oh_v:=ra;
                WHEN "0001" => ol_v:=rx(7 DOWNTO 0);  oh_v:=rx(15 DOWNTO 8);
                WHEN "0010" => ol_v:=ry(7 DOWNTO 0);  oh_v:=ry(15 DOWNTO 8);
                WHEN "0011" => ol_v:=ru(7 DOWNTO 0);  oh_v:=ru(15 DOWNTO 8);
                WHEN "0100" => ol_v:=rsp(7 DOWNTO 0); oh_v:=rsp(15 DOWNTO 8);
                WHEN "0101" => ol_v:=pc_next(7 DOWNTO 0);
                               oh_v:=pc_next(15 DOWNTO 8);
                WHEN "1000" => ol_v:=ra;  oh_v:=x"FF";
                WHEN "1001" => ol_v:=rb;  oh_v:=x"FF";
                WHEN "1010" => ol_v:=cc_conv(ccr); oh_v:=x"FF";
                WHEN OTHERS => ol_v:=ra;  oh_v:=x"FF";
              END CASE;
              
              CASE dr_v(3 DOWNTO 0) IS
                WHEN "0000" => rtl_v:=rb; rth_v:=ra;
                WHEN "0001" => rtl_v:=rx(7 DOWNTO 0);  rth_v:=rx(15 DOWNTO 8);
                WHEN "0010" => rtl_v:=ry(7 DOWNTO 0);  rth_v:=ry(15 DOWNTO 8);
                WHEN "0011" => rtl_v:=ru(7 DOWNTO 0);  rth_v:=ru(15 DOWNTO 8);
                WHEN "0100" => rtl_v:=rsp(7 DOWNTO 0); rth_v:=rsp(15 DOWNTO 8);
                WHEN "0101" => rtl_v:=pc_next(7 DOWNTO 0);
                               rth_v:=pc_next(15 DOWNTO 8);
                WHEN "1000" => rtl_v:=ra;  rth_v:=x"FF";
                WHEN "1001" => rtl_v:=rb;  rth_v:=x"FF";
                WHEN "1010" => rtl_v:=cc_conv(ccr); rth_v:=x"FF";
                WHEN OTHERS => rtl_v:=ra;  rth_v:=x"FF";
              END CASE;
              
              CASE dr_v(3 DOWNTO 0) IS
                WHEN "0000" => or_v:=D;   oml_v:='1'; omh_v:='1';
                WHEN "0001" => or_v:=X;   oml_v:='1'; omh_v:='1';
                WHEN "0010" => or_v:=Y;   oml_v:='1'; omh_v:='1';
                WHEN "0011" => or_v:=U;   oml_v:='1'; omh_v:='1';
                WHEN "0100" => or_v:=SP;  oml_v:='1'; omh_v:='1';
                WHEN "0101" => or_v:=CP;  oml_v:='1'; omh_v:='1';
                WHEN "1000" => or_v:=A;   oml_v:='1'; omh_v:='0';
                WHEN "1001" => or_v:=B;   oml_v:='1'; omh_v:='0';
                WHEN "1010" => or_v:=CC;  oml_v:='1'; omh_v:='0';
                WHEN "1011" => or_v:=DP;  oml_v:='1'; omh_v:='0';
                WHEN OTHERS => or_v:=D;   oml_v:='0'; omh_v:='0';
              END CASE;
              IF op=x"1F" THEN -- TFR
                xwait_v:=4;
                state_v:=sDELAY;
              ELSE
                xwait_v:=5;
                state_v:=sEXG;
              END IF;
              
            ELSIF op_v=x"1C" THEN -- ANDcc
              or_v:=CC;
              oml_v:='1';
              ol_v:=cc_conv(ccr) AND dr_v;
              xwait_v:=1;
              state_v:=sFETCH;
              
            ELSIF op_v=x"1A" THEN -- ORcc
              or_v:=CC;
              oml_v:='1';
              ol_v:=cc_conv(ccr) OR dr_v;
              xwait_v:=1;
              state_v:=sFETCH;
              
            ELSIF op_v=x"35" THEN -- PULS
              IF dr_v=x"00" THEN
                state_v:=sFETCH;
              ELSE
                state_v:=sPULL_PRE;
              END IF;
              sel_stack<='0';
              
            ELSIF op_v=x"37" THEN -- PULU
              IF dr_v=x"00" THEN
                state_v:=sFETCH;
              ELSE
                state_v:=sPULL_PRE;
              END IF;
              sel_stack<='1';
              
            ELSIF op_v=x"34" THEN -- PSHS
              IF dr_v=x"00" THEN
                state_v:=sFETCH;
              ELSE
                state_v:=sPUSH_PRE;
              END IF;
              sel_stack<='0';
              alt<='0';
              
            ELSIF op_v=x"36" THEN -- PSHU
              IF dr_v=x"00" THEN
                state_v:=sFETCH;
              ELSE
                state_v:=sPUSH_PRE;
              END IF;
              sel_stack<='1';
              alt<='0';
              
            ELSIF op_v=x"3C" THEN -- CWAI
              ccr<=cc_conv((cc_conv(ccr) AND dr_v) OR x"80");
              state_v:=sPUSH_PRE;  -- CWAI starts as a PSHS
              sel_stack<='0';
              alt<='0';
              post<=x"FF"; -- Save everything
              occ_v.e:='1'; -- Everything was saved
              
            ELSE -- Immediate ALUOP
              alt<='1';
              ih2_v:=rt(15 DOWNTO 8);
              il2_v:=dr_v;
              rth_v:=dr_v;
              IF dec.ea=R8 OR dec.ea=RT8 OR (dec.ea=R16 AND alt='1') THEN
                state_v:=sFETCH;
                aluop_v:='1';
              ELSIF alt='1' THEN -- ADDD : 4 cycles
                state_v:=sFETCH;
                aluop_v:='1';
                xwait_v:=1;
              END IF;
              
            END IF;
            
          --------------------------------------------------
          WHEN sDIRECT =>
            ea_v:=rdp & dr_v;
            ea<=ea_v;
            cycle<=0;
            rtl_v:=pc_next(15 DOWNTO 8);
            rdw<=ih1_v;
            pc_v:=pc_next;
            
            IF dec.ea=JMP THEN
              rdw<=pc_next(7 DOWNTO 0);
              pc_v:=ea_v;
              state_v:=sDELAY;
            ELSIF dec.ea=JSR THEN
              rdw<=pc_next(7 DOWNTO 0);
              pc_v:=ea_v;
              state_v:=sBSR;
              rsp<=rsp-1;
              xwait_v:=3;
            ELSIF dec.ea=W8 OR dec.ea=W16 THEN
              state_v:=sEA_WRITE;
            ELSE
              state_v:=sEA;
            END IF;
            
          --------------------------------------------------
          WHEN sEXTENDED =>
            ea_v:=ea(7 DOWNTO 0) & dr_v;
            ea<=ea_v;
            rtl_v:=pc_next(15 DOWNTO 8);
            rdw<=pc_next(7 DOWNTO 0);
            pc_v:=pc_next;
            
            IF cycle=1 THEN
              cycle<=0;
              IF dec.ea=JMP THEN
                pc_v:=ea(7 DOWNTO 0) & dr_v;
                state_v:=sDELAY;
              ELSIF dec.ea=JSR THEN
                pc_v:=ea(7 DOWNTO 0) & dr_v;
                rsp<=rsp-1;
                state_v:=sBSR;
                xwait_v:=3;
              ELSIF dec.ea=W8 OR dec.ea=W16 THEN
                state_v:=sEA_WRITE;
                rdw<=ih1_v;
              ELSE
                state_v:=sEA;
                rdw<=ih1_v;
              END IF;
            END IF;
            
          --------------------------------------------------
          WHEN sINDEXED =>
            post<=dr_v;
            idx_maj_v:='0';
            cycle<=0;
            pc_v:=pc_next;
            rdw<=ih1_v;
            rtl_v:=pc_next(15 DOWNTO 8);
            
            CASE dr_v(6 DOWNTO 5) IS
              WHEN "00"   => idx_v:=rx;  or_v:=X;
              WHEN "01"   => idx_v:=ry;  or_v:=Y;
              WHEN "10"   => idx_v:=ru;  or_v:=U;
              WHEN OTHERS => idx_v:=rsp; or_v:=SP;
            END CASE;
            ea_v:=idx_v;

            IF dr_v(7)='0' THEN -- 5 bits offset
              iwait<=1;
              ea_v:=idx_v + sext(dr_v(4 DOWNTO 0),16);
              state_v:=sEA;
              
            ELSIF dr_v(3 DOWNTO 0)="0100" THEN -- No offset
              ea_v:=idx_v;
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="0110" THEN -- A reg. offset
              iwait<=1;
              ea_v:=idx_v + sext(ra,16);
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="0101" THEN -- B reg. offset
              iwait<=1;
              ea_v:=idx_v + sext(rb,16);
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="1011" THEN -- D reg. offset
              iwait<=4;
              ea_v:=idx_v + (ra & rb);
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="0000" THEN -- Post Inc 1
              iwait<=2;
              ea_v:=idx_v;
              idx_v:=ea_v+1;
              idx_maj_v:='1';
              state_v:=sEA;
            ELSIF dr_v(3 DOWNTO 0)="0001" THEN -- Post Inc 2
              iwait<=3;
              ea_v:=idx_v;
              idx_v:=ea_v+2;
              idx_maj_v:='1';
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="0010" THEN -- Pre Dec 1
              iwait<=2;
              ea_v:=idx_v-1;
              idx_v:=ea_v;
              idx_maj_v:='1';
              state_v:=sEA;
            ELSIF dr_v(3 DOWNTO 0)="0011" THEN -- Pre Dec 2
              iwait<=3;
              ea_v:=idx_v-2;
              idx_v:=ea_v;
              idx_maj_v:='1';
              IF dr_v(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSIF dr_v(3 DOWNTO 0)="1000" THEN -- 8 bits offset
              ea_v:=idx_v;
              state_v:=sINDEXED2;
            ELSIF dr_v(3 DOWNTO 0)="1001" THEN -- 16 bits offset
              iwait<=2;
              ea_v:=idx_v;
              state_v:=sINDEXED2;
            ELSIF dr_v(3 DOWNTO 0)="1100" THEN -- PC offset 8
              ea_v:=pc_v+1;
              state_v:=sINDEXED2;
            ELSIF dr_v(3 DOWNTO 0)="1101" THEN -- PC offset 16
              iwait<=3;
              ea_v:=pc_v+2;
              state_v:=sINDEXED2;
            ELSIF dr_v(3 DOWNTO 0)="1111" THEN -- Extended Ind.
              ea_v:=x"0000";
              state_v:=sINDEXED2;
            ELSE -- Default : Invalid op.
              ea_v:=x"0000";
              state_v:=sEA;
            END IF;
            
            ea<=ea_v;
            
            IF state_v=sEA AND (dec.ea=W8 OR dec.ea=W16) THEN
              state_v:=sEA_WRITE;
            ELSIF state_v=sEA AND dec.ea=JMP THEN
              pc_v:=ea_v;
              state_v:=sDELAY;
            ELSIF state_v=sEA AND dec.ea=JSR THEN
              pc_v:=ea_v;
              state_v:=sBSR;
              rdw<=pc_next(7 DOWNTO 0);
              rsp<=rsp-1;
              cycle<=0;
            END IF;
            
            -- Update index register if auto inc/dec
            ol_v:=idx_v(7 DOWNTO 0);
            oh_v:=idx_v(15 DOWNTO 8);
            omh_v:=idx_maj_v;
            oml_v:=idx_maj_v;
            
          WHEN sINDEXED2 =>
            -- Immediate values of Indexed addressing
            pc_v:=pc_next;
            rdw<=ih1_v;
            IF post(3 DOWNTO 0)="1000" OR post(3 DOWNTO 0)="1100" THEN
              -- 8bits offset
              cycle<=0;
              ea_v:=ea+sext(dr_v,16);
              IF post(4)='1' THEN
                state_v:=sINDIRECT;
              ELSE
                state_v:=sEA;
              END IF;
            ELSE -- 16bits offset
              IF cycle=1 THEN
                cycle<=0;
                IF post(4)='1' THEN
                  state_v:=sINDIRECT;
                ELSE
                  state_v:=sEA;
                 END IF;
                ea_v:=ea+(x"00" & dr_v);
              ELSE
                ea_v:=ea+(dr_v & x"00");
              END IF;
            END IF;
            
            ea<=ea_v;
            
            IF state_v=sEA AND (dec.ea=W8 OR dec.ea=W16) THEN
              state_v:=sEA_WRITE;
            ELSIF state_v=sEA AND dec.ea=JMP THEN
              pc_v:=ea_v;
              state_v:=sDELAY;
            ELSIF state_v=sEA AND dec.ea=JSR THEN
              pc_v:=ea_v;
              state_v:=sBSR;
              rdw<=pc_next(7 DOWNTO 0);
              rsp<=rsp-1;
              cycle<=0;
              xwait_v:=iwait+3;
            END IF;
            
          --------------------------------------------------
          WHEN sINDIRECT =>
            rtl_v:=pc(15 DOWNTO 8);
            rth_v:=dr_v;
            IF cycle=0 THEN
              ea<=ea+1;
            ELSIF cycle=1 THEN
              state_v:=sINDIRECT2;
            END IF;
            
          WHEN sINDIRECT2 =>
            ea_v:=rt(15 DOWNTO 8) & dr_v;
            cycle<=0;
            state_v:=sEA;

            ea<=ea_v;
            
            IF state_v=sEA AND (dec.ea=W8 OR dec.ea=W16) THEN
              state_v:=sEA_WRITE;
            ELSIF state_v=sEA AND dec.ea=JMP THEN
              pc_v:=ea_v;
              state_v:=sDELAY;
            ELSIF state_v=sEA AND dec.ea=JSR THEN
              pc_v:=ea_v;
              state_v:=sBSR;
              rdw<=pc(7 DOWNTO 0);
              rsp<=rsp-1;
              cycle<=0;
              xwait_v:=iwait+3;
            END IF;
            
          --------------------------------------------------
          WHEN sEA | sEA_FETCH =>
            xwait_v:=iwait;
            
            CASE dec.ea IS
              WHEN LEA => -- LEA
                CASE op(1 DOWNTO 0) IS
                  WHEN "00"   => or_v:=X; occ_v.z:=to_std_logic(ea=x"0000");
                  WHEN "01"   => or_v:=Y; occ_v.z:=to_std_logic(ea=x"0000");
                  WHEN "10"   => or_v:=SP;
                  WHEN OTHERS => or_v:=U;
                END CASE;
                
                ol_v:=ea(7 DOWNTO 0);
                oh_v:=ea(15 DOWNTO 8);
                oml_v:='1';
                omh_v:='1';
                state_v:=sFETCH;
                xwait_v:=iwait+1;
                
              WHEN R8 => -- SUB/CMP/SBC/AND/BIT/...
                IF cycle=0 THEN
                  state_v:=sEA_FETCH;
                ELSE
                  state_v:=sFETCH;
                  aluop_v:='1';
                END IF;
                
              WHEN RMW8 => -- NEG/COM/LSR/...
                IF cycle=0 THEN
                  NULL;
                ELSIF cycle=1 THEN
                  state_v:=sEA_WRITE;
                  aluop_v:='1';
                END IF;
                
              WHEN RT8 => -- TST
                IF cycle=0 THEN
                  NULL;
                ELSIF cycle=1 THEN
                  state_v:=sDELAY;
                  aluop_v:='1';
                END IF;
                xwait_v:=2+iwait;
                
              WHEN R16  => -- LDS/LDU/LDX/LDY,
                IF cycle=0 THEN
                  ea<=ea+1;
                ELSIF cycle=1 THEN
                  state_v:=sEA_FETCH;
                  rth_v:=dr_v;
                ELSE
                  ih2_v:=rth_v;
                  state_v:=sFETCH;
                  aluop_v:='1';
                END IF;
                
              WHEN RC16 => -- ADDD, CMPD,CMPS,CMPX,CMPU,CMPY,SUBD
                IF cycle=0 THEN
                  ea<=ea+1;
                ELSIF cycle=1 THEN
                  state_v:=sEA_FETCH;
                  rth_v:=dr_v;
                ELSE
                  ih2_v:=rth_v;
                  state_v:=sFETCH;
                  aluop_v:='1';
                END IF;
                xwait_v:=1+iwait;
                
              WHEN OTHERS =>
                NULL;
                
            END CASE;
            
          --------------------------------------------------
          WHEN sEA_WRITE =>
            xwait_v:=iwait+1;
            
            CASE dec.ea IS
              WHEN W8 => -- STA, STB
                state_v:=sDELAY;
                occ_v.n:=ih1_v(7);
                occ_v.z:=to_std_logic(ih1_v=x"00");
                occ_v.v:='0';
                
              WHEN W16 => -- STS,STU,STX,STY
                IF cycle=0 THEN
                  ea<=ea+1;
                  rdw<=il1_v;
                ELSE
                  state_v:=sDELAY;
                  occ_v.n:=ih1_v(7);
                  occ_v.z:=to_std_logic(ih1_v & il1_v=x"0000");
                  occ_v.v:='0';
                END IF;
                
              WHEN RMW8 => -- NEG/COM/LSR/...
                state_v:=sDELAY;
                xwait_v:=1+iwait;
                
              WHEN OTHERS =>
                NULL;
            END CASE;
            
          --------------------------------------------------
          WHEN sEXG =>
            state_v:=sFETCH;
            oh_v:=rt(15 DOWNTO 8);
            ol_v:=rt(7 DOWNTO 0);
            
            CASE post(7 DOWNTO 4) IS
              WHEN "0000" => or_v:=D;  oml_v:='1'; omh_v:='1';
              WHEN "0001" => or_v:=X;  oml_v:='1'; omh_v:='1';
              WHEN "0010" => or_v:=Y;  oml_v:='1'; omh_v:='1';
              WHEN "0011" => or_v:=U;  oml_v:='1'; omh_v:='1';
              WHEN "0100" => or_v:=SP; oml_v:='1'; omh_v:='1';
              WHEN "0101" => or_v:=CP; oml_v:='1'; omh_v:='1';
              WHEN "1000" => or_v:=A;  oml_v:='1'; omh_v:='0';
              WHEN "1001" => or_v:=B;  oml_v:='1'; omh_v:='0';
              WHEN "1010" => or_v:=CC; oml_v:='1'; omh_v:='0';
              WHEN "1011" => or_v:=DP; oml_v:='1'; omh_v:='0';
              WHEN OTHERS => or_v:=D;  oml_v:='0'; omh_v:='0';
            END CASE;
            
          --------------------------------------------------
            
          WHEN sPUSH | sPUSH_PRE => -- PSHU / PSHS / Interrupts / SWI / CWAI
            cycle<=0;
            state_v:=sPUSH;
            
            IF post(7)='1' THEN -- PC
              rdw<=mux(alt,pc(15 DOWNTO 8),pc(7 DOWNTO 0));
              alt<=NOT alt;
              IF alt='1' THEN
                post(7)<='0';
              END IF;
            ELSIF post(6)='1' THEN -- U/S
              alt<=NOT alt;
              IF sel_stack='1' THEN
                rdw<=mux(alt,rsp(15 DOWNTO 8),rsp(7 DOWNTO 0));
              ELSE
                rdw<=mux(alt,ru (15 DOWNTO 8),ru (7 DOWNTO 0));
              END IF;
              IF alt='1' THEN
                post(6)<='0';
              END IF;
            ELSIF post(5)='1' THEN -- Y
              rdw<=mux(alt,ry(15 DOWNTO 8),ry(7 DOWNTO 0));
              alt<=NOT alt;
              IF alt='1' THEN
                post(5)<='0';
              END IF;
            ELSIF post(4)='1' THEN -- X
              rdw<=mux(alt,rx(15 DOWNTO 8),rx(7 DOWNTO 0));
              alt<=NOT alt;
              IF alt='1' THEN
                post(4)<='0';
              END IF;
            ELSIF post(3)='1' THEN -- DP
              rdw<=rdp;
              post(3)<='0';
            ELSIF post(2)='1' THEN -- B
              rdw<=rb;
              post(2)<='0';
            ELSIF post(1)='1' THEN -- A
              rdw<=ra;
              post(1)<='0';
            ELSIF post(0)='1' THEN -- CCR
              rdw<=cc_conv(ccr);
              post(0)<='0';
            END IF;
            IF post=x"00" THEN
              IF doint='1' THEN -- Interrupts
                state_v:=sINTERRUPT2;
              ELSIF op=x"3F" THEN -- SWI
                state_v:=sSWI;
              ELSIF op=x"3C" THEN -- CWAI
                state_v:=sCWAI;
              ELSIF op=x"34" OR op=x"36" THEN -- PSHS/PSHU
                state_v:=sDELAY;
              ELSE
                state_v:=sINTERRUPT2; -- Unused ?
              END IF;
              xwait_v:=2;
              
            ELSE
              IF sel_stack='0' THEN
                rsp<=rsp-1;
              ELSE
                ru<=ru-1;
              END IF;
            END IF;
            
          --------------------------------------------------
          WHEN sPULL_PRE =>            -- PULS,PULU, RTI
            state_v:=sPULL;
            IF sel_stack='0' THEN
              rsp<=rsp+1;
             ELSE
              ru<=ru+1;
            END IF;
            
          WHEN sPULL => -- PULU / PULS
            cycle<=0;
            oh_v:=dr_v;
            ol_v:=dr_v;
            post_v:=post;
            IF post(0)='1' THEN -- CCR
              post_v(0):='0';
              or_v:=CC;
              oml_v:='1';
            ELSIF post(1)='1' THEN -- A
              post_v(1):='0';
              or_v:=A;
              oml_v:='1';
            ELSIF post(2)='1' THEN -- B
              post_v(2):='0';
              or_v:=B;
              oml_v:='1';
            ELSIF post(3)='1' THEN -- DP
              post_v(3):='0';
              or_v:=DP;
              oml_v:='1';
            ELSIF post(4)='1' THEN -- X
              or_v:=X;
              alt<=NOT alt;
              IF alt='0' THEN
                omh_v:='1';
              ELSE
                oml_v:='1';
                post_v(4):='0';
              END IF;
            ELSIF post(5)='1' THEN -- Y
              or_v:=Y;
              IF alt='0' THEN
                omh_v:='1';
                alt<='1';
              ELSE
                oml_v:='1';
                post_v(5):='0';
                alt<='0';
              END IF;
            ELSIF post(6)='1' THEN -- U / SP
              IF sel_stack='0' THEN
                or_v:=U;
              ELSE
                or_v:=SP;
              END IF;
              
              IF alt='0' THEN
                omh_v:='1';
                alt<='1';
              ELSE
                oml_v:='1';
                post_v(6):='0';
                alt<='0';
              END IF;
            ELSIF post(7)='1' THEN -- PC
              or_v:=CP;
              IF alt='0' THEN
                omh_v:='1';
                alt<='1';
              ELSE
                oml_v:='1';
                post_v(7):='0';
                alt<='0';
              END IF;
            END IF;
            post<=post_v;
            IF post_v=x"00" THEN
              state_v:=sDELAY;
              xwait_v:=2;
            ELSE
              IF sel_stack='0' THEN
                rsp<=rsp+1;
              ELSE
                ru<=ru+1;
              END IF;
            END IF;
            
          --------------------------------------------------
          WHEN sRTS =>
            rtl_v:=dr_v;
            pc_v:=rt(7 DOWNTO 0) & dr_v;
            
            IF cycle=2 THEN
              state_v:=sDELAY;
              xwait_v:=1;
            ELSE
              rsp<=rsp+1;
            END IF;
            
          --------------------------------------------------
          WHEN sSWI =>
            IF pre=0 THEN
              ivec<=VEC_SWI;
            ELSIF pre=1 THEN
              ivec<=VEC_SWI2;
            ELSE
              ivec<=VEC_SWI3;
            END IF;
            state_v:=sINTERRUPT2;
            
          --------------------------------------------------
          WHEN sCWAI =>
            IF reset_pend='1' THEN
              ivec<=VEC_RESET;
              state_v:=sINTERRUPT2;
              
            ELSIF nmi_pend='1' THEN
              ivec<=VEC_NMI;
              state_v:=sINTERRUPT2;
              
            ELSIF firq='1' AND ccr.f='0' THEN
              ivec<=VEC_FIRQ;
              state_v:=sINTERRUPT2;
              
            ELSIF irq='1' AND ccr.i='0' THEN
              ivec<=VEC_IRQ;
              state_v:=sINTERRUPT2;
              
            END IF;
            
          --------------------------------------------------
          WHEN sDELAY =>
            IF xwait<=1 THEN
              state_v:=sFETCH;
            ELSE
              xwait_v:=xwait-1;
            END IF;
            
          --------------------------------------------------
          WHEN sSYNC =>
            IF nmi_pend='1' OR (firq='1' AND ccr.f='0') OR
              (irq='1' AND ccr.i='0') OR wake='1' THEN
              state_v:=sFETCH;
            END IF;
            
          --------------------------------------------------
          WHEN sINT =>
            -- save state
            state_v:=sPUSH_PRE;
            doint<='1';
            sel_stack<='0';
            IF reset_pend='1' THEN
              ivec<=VEC_RESET;
              post<=x"00";
            ELSIF nmi_pend='1' THEN
              ivec<=VEC_NMI;
              post<=x"FF";
              occ_v.e:='1'; -- Save entire state
            ELSIF firq='1' AND ccr.f='0' THEN
              ivec<=VEC_FIRQ;
              post<=x"81";
              occ_v.e:='0'; -- Save partial state
            ELSIF irq='1' AND ccr.i='0' THEN
              ivec<=VEC_IRQ;
              post<=x"FF";
              occ_v.e:='1'; -- Save entire state
            ELSE
              state_v:=sFETCH;
              doint<='0';
            END IF;
            
          WHEN sINTERRUPT2 =>
            -- Read vector
            -- mask further interrupts
            IF ivec=VEC_RESET THEN
              occ_v.f:='1';
              occ_v.i:='1';
              reset_ack<='1';
            ELSIF ivec=VEC_NMI THEN
              occ_v.f:='1';
              occ_v.i:='1';
              nmi_ack<='1';
            ELSIF ivec=VEC_FIRQ THEN
              occ_v.f:='1';
              occ_v.i:='1';
            ELSIF ivec=VEC_IRQ THEN
              occ_v.i:='1';
            END IF;
            state_v:=sINTERRUPT3;
            ea<=x"FFF" & ivec & '0';
            cycle<=0;
            doint<='0';
            
          WHEN sINTERRUPT3 =>
            IF cycle=0 THEN
              ea<=ea+1;
            ELSIF cycle=1 THEN
              rth_v:=dr_v;
            ELSE
              pc_v:=rt(15 DOWNTO 8) & dr_v;
              state_v:=sDELAY;
            END IF;
            
        END CASE;

        ----------------------------------------------------
        state<=state_v;
        IF fast='1' THEN
          xwait_v:=0;
        END IF;
        xwait<=xwait_v;
        
        IF state_v=sFETCH AND state/=sFETCH THEN
          -- Clear prefixes
          pre<=0;
        END IF;
        IF state_v=sFETCH AND xwait_v>0 AND state/=sDELAY THEN
          state<=sDELAY;
        END IF;
        
      END IF; -- ack='1' or req_i='0'
      
      ----------------------------------------------------
      IF aluop_v='1' THEN
        op_alu(op_v,pre,il1_v,ih1_v,il2_v,ih2_v,ccr,
               ol_v,oh_v,oml_v,omh_v,occ_v);
      END IF;

      ----------------------------------------------------
      
      ccr<=occ_v;
      
      IF oml_v='1' THEN
        CASE or_v IS
          WHEN M =>   rdw<=ol_v;
          WHEN A =>   ra<=ol_v;
          WHEN B =>   rb<=ol_v;
          WHEN CC =>  ccr<=cc_conv(ol_v);
          WHEN DP =>  rdp<=ol_v;
          WHEN D =>   rb<=ol_v;
          WHEN X =>   rx(7 DOWNTO 0)<=ol_v;
          WHEN Y =>   ry(7 DOWNTO 0)<=ol_v;
          WHEN U =>   ru(7 DOWNTO 0)<=ol_v;
          WHEN SP =>  rsp(7 DOWNTO 0)<=ol_v;
          WHEN CP =>  pc_v(7 DOWNTO 0):=ol_v;
          WHEN OTHERS => NULL;
        END CASE;
      END IF;

      IF omh_v='1' THEN
        CASE or_v IS
          WHEN M =>   rdw<=oh_v;
          WHEN A =>   ra<=oh_v;
          WHEN B =>   rb<=oh_v;
          WHEN CC =>  NULL;
          WHEN DP =>  NULL;           
          WHEN D =>   ra<=oh_v;
          WHEN X =>   rx(15 DOWNTO 8)<=oh_v;
          WHEN Y =>   ry(15 DOWNTO 8)<=oh_v;
          WHEN U =>   ru(15 DOWNTO 8)<=oh_v;
          WHEN SP =>  rsp(15 DOWNTO 8)<=oh_v;
          WHEN CP =>  pc_v(15 DOWNTO 8):=oh_v;
          WHEN OTHERS => NULL;
        END CASE;
      END IF;
      
      pc<=pc_v;
      pc_next<=pc_v+1;
      rt<=rth_v & rtl_v;
      --------------------------------------
      reset_pre<=reset;
      nmi_pre  <=nmi;
      
      reset_pend<=(reset_pend OR (reset AND NOT reset_pre)) AND NOT reset_ack;
      nmi_pend  <=(nmi_pend   OR (nmi   AND NOT nmi_pre))   AND NOT nmi_ack;
      
      --------------------------------------
      
    END IF;
  END PROCESS Machine;
  
  --############################################################################
  
  AutoBus:PROCESS(state,pc_next,ea,rsp,ru,rdw,sel_stack,dec,pc) IS
  BEGIN
    dw_i<=rdw;

    IF state/=sSYNC AND state/=sCWAI THEN
      req_i<='1';
    ELSE
      req_i<='0';
    END IF;
    
    CASE state IS
      WHEN sFETCH =>
        ad_i<=pc_next;
        wr_i<='0';
        ph_i<=PH_CODE;

      WHEN sEXG | sDELAY | sEA_FETCH =>
        ad_i<=pc;
        wr_i<='0';
        ph_i<=PH_CODE;

      WHEN sRELATIVE | sRELATIVE_BSR | sIMMEDIATE |
           sDIRECT | sEXTENDED | sINDEXED | sINDEXED2 =>
        ad_i<=pc_next;
        wr_i<='0';
        ph_i<=PH_POST;
        
      WHEN sPUSH | sBSR =>
        ad_i<=mux(sel_stack,ru,rsp);
        wr_i<='1';
        ph_i<=PH_STACK;

      WHEN sPULL | sPULL_PRE | sRTS =>
        ad_i<=mux(sel_stack,ru,rsp);
        wr_i<='0';
        ph_i<=PH_STACK;
        
      WHEN sEA =>
        IF dec.ea=LEA THEN
          ad_i<=pc;
          ph_i<=PH_CODE;
        ELSE
          ad_i<=ea;
          ph_i<=PH_DATA;
        END IF;
        wr_i<='0';

      WHEN sEA_WRITE =>
        ad_i<=ea;
        wr_i<='1';
        ph_i<=PH_DATA;
        
      WHEN sINDIRECT | sINTERRUPT2 | sINTERRUPT3 =>
        ad_i<=ea;
        wr_i<='0';
        ph_i<=PH_DATA;

      WHEN sSWI | sCWAI | sSYNC | sINT | sPUSH_PRE | sINDIRECT2 =>
        ad_i<=x"FFFF";
        wr_i<='0';
        ph_i<=PH_VOID;
        
    END CASE;
    
  END PROCESS Autobus;

  req<=req_i;
  ad <=ad_i;
  ph <=ph_i;
  dw <=dw_i;
  wr <=wr_i;
  
--pragma synthesis_off
--##############################################################################
  -- Instruction trace
  Trace:PROCESS IS
    VARIABLE op_v  : uv8;
    VARIABLE dec_v : type_op;
    VARIABLE pre_v : natural RANGE 0 TO 2;
    VARIABLE ra_v,rb_v,rdp_v,rcc : uv8;
    VARIABLE rx_v,ry_v,rsp_v,ru_v,pc_next_v : uv16;
    VARIABLE ccr_v : uv8;
    VARIABLE tt_v : uv16;
    VARIABLE rd_v : std_logic :='0';
    VARIABLE phmem : uv3;
    VARIABLE admem : uv16;
    VARIABLE c_v,cil_v,cir_v : character;
    VARIABLE lout : line;
    VARIABLE csa ,csb ,csc ,csd  : string(1 TO 1000) :=(OTHERS =>NUL);
    VARIABLE csa2,csb2,csc2,cst  : string(1 TO 1000) :=(OTHERS =>NUL);
    VARIABLE dr_v : uv8;
    VARIABLE cyi_v,cyp_v : natural RANGE 0 TO 31;
    VARIABLE cyc_v : natural;
    -----------------------------------------------
    PROCEDURE write(cs: INOUT string;
                    s : IN string) IS
      VARIABLE j,k : integer;
    BEGIN
      j:=-1;
      FOR i IN 1 TO cs'length LOOP
        IF cs(i)=nul THEN j:=i; EXIT; END IF;
      END LOOP;
      
      k:=s'length;
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN k:=i; EXIT; END IF;
      END LOOP;
      
      IF j>0 THEN
        cs(j TO j+k-1):=s(1 TO k);
      END IF;
    END PROCEDURE write;

    FUNCTION strip(s : IN string) RETURN string IS
    BEGIN
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN RETURN s(1 TO i-1); END IF;
      END LOOP;
      RETURN s;
    END FUNCTION;
    
    PROCEDURE pad(s : INOUT string; l : natural) IS
      VARIABLE j : integer;
    BEGIN
      j:=-1;
      FOR i IN 1 TO s'length LOOP
        IF s(i)=nul THEN j:=i; EXIT; END IF;
      END LOOP;
      IF j>0 THEN
        s(j TO l):=(OTHERS =>' ');
      END IF;
    END PROCEDURE;
    -----------------------------------------------
    PROCEDURE waitdata IS
    BEGIN
      
      LOOP
        wure(clk);
        EXIT WHEN trace_ni='1';
      END LOOP;
      write (csb,to_hstring(trace_dr) & " ");
    END PROCEDURE;

    TYPE arr_string4 IS ARRAY(natural RANGE <>) OF string(1 TO 4);
    CONSTANT ph_txt : arr_string4(0 TO 7):=("CODE","POST","DATA","STAK",
                                            "VOID","????","????","????");
  BEGIN
    WAIT UNTIL reset_na='1';
    LOOP
      wure(clk);
      pre_v:=0;
      IF trace_ni='1' THEN
        IF rd_v='1' THEN
          write(lout," RD("&to_hstring(admem) &")=" & to_hstring(trace_dr));
          write(lout," <" & ph_txt(to_integer(phmem)) & ">");
          write(lout,"  <" & time'image(now));
          writeline(fil,lout);
          rd_v:='0';
        END IF;
        IF ph_i=PH_DATA OR ph_i=PH_STACK THEN
          IF wr_i='1' THEN
            write(lout," WR("&to_hstring(ad_i) &")=" & to_hstring(dw_i));
            write(lout," <" & ph_txt(to_integer(ph_i)) & ">");
            write(lout,"  <" & time'image(now));
            writeline(fil,lout);
          ELSE
            rd_v:='1';
            admem:=ad_i;
            phmem:=ph_i;
          END IF;
        ELSIF trace_state=sFETCH THEN
          cyc_v:=trace_cyc;
          
          write (csb,to_hstring(trace_dr) & " ");
          csa:=(OTHERS =>nul);
          write (csa,to_hstring(pc_next-1) & " : ");
          WHILE trace_dr=x"10" OR trace_dr=x"11" LOOP
            IF trace_dr=x"10" THEN pre_v:=1; END IF;
            IF trace_dr=x"11" THEN pre_v:=2; END IF;
            waitdata;
          END LOOP;
          
          op_v:=trace_dr;
          dec_v:=opcodes(to_integer(trace_dr));
          cyi_v:=0;
          cyp_v:=0;
          ra_v:=ra;
          rb_v:=rb;
          rdp_v:=rdp;
          ccr_v:=cc_conv(ccr);
          rx_v:=rx;
          ry_v:=ry;
          pc_next_v:=pc_next;
          rsp_v:=rsp;
          ru_v:=ru;
          
          -- New instruction;
          --     PC : BIN : OPCODE : Adressing : D= X= Y= U= S= PC= CCR=
          IF pre_v=0 THEN
            write (csc,dec_v.dis);
          ELSIF pre_v=1 THEN
            write (csc,dec_v.dis10);
            cyp_v:=1;
          ELSE
            write (csc,dec_v.dis11);
            cyp_v:=1;
          END IF;
          
          CASE dec_v.m IS
            WHEN INHERENT =>  -- No more blues.
              write(csc,string'(" "));
              
            WHEN IMMEDIATE => -- 2 bytes / 3 bytes
              waitdata;
              write(csc, " #" & to_hstring(trace_dr));
              IF dec_v.s='1' THEN
                waitdata;
                write(csc,to_hstring(trace_dr) & ' ');
              END IF;
              
            WHEN DIRECT => -- 2 bytes
              waitdata;
              write(csc," d<" & to_hstring(trace_dr) & ' ');
              
            WHEN EXTENDED => -- 3 bytes
              waitdata;
              write(csc," >" & to_hstring(trace_dr));
              waitdata;
              write(csc,to_hstring(trace_dr) & ' ');
              
            WHEN RELATIVE | RELATIVE_BSR =>
              waitdata;
              tt_v:=x"00" & trace_dr;
              IF pre_v/=0 OR op_v=x"16" OR op_v=x"17" THEN
                waitdata;
                tt_v:=tt_v(7 DOWNTO 0) & trace_dr;
                write (csc,to_hstring(pc_next+tt_v) & ' ');
                IF op_v/=x"16" AND op_v/=x"17" THEN -- Not LBRA/LBSR
                  cyi_v:=1; -- Prefix : Bcc->LBcc
                END IF;
              ELSE
                write (csc,to_hstring(pc_next+sext (tt_v(7 DOWNTO 0),16)) &' ');
              END IF;
              
            WHEN INDEXED =>

              waitdata;
              
              tt_v(7 DOWNTO 0):=trace_dr;
              CASE trace_dr(6 DOWNTO 5) IS
                WHEN "00" => c_v:='X';
                WHEN "01" => c_v:='Y';
                WHEN "10" => c_v:='U';
                WHEN OTHERS => c_v:='S';
              END CASE;
              IF trace_dr(4)='0' OR trace_dr(7)='0' THEN
                cil_v:=' '; cir_v:=' ';
              ELSE
                cil_v:='['; cir_v:=']';
                cyp_v:=cyp_v+3;
              END IF; -- Indirect
              IF trace_dr(7)='0' THEN -- 5 bits offset : X,
                cyi_v:=1;
                write(csc,' ' &
                      integer'image(to_integer(signed(trace_dr(4 DOWNTO 0)))) &
                      ',' & c_v & ' ');
              ELSIF trace_dr(3 DOWNTO 0)="0100" THEN -- No offset
                cyi_v:=0;
                write(csc,cil_v & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1000" THEN -- 8bits offset
                cyi_v:=1;
                waitdata;
                write(csc,cil_v & integer'image(to_integer(signed(trace_dr))) &
                       ',' & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1001" THEN -- 16bits offset
                cyi_v:=4;
                waitdata;
                tt_v:=trace_dr & trace_dr;
                waitdata;
                tt_v:=tt_v(15 DOWNTO 8) & trace_dr;
                write(csc,cil_v & "x" & to_hstring(tt_v) & ',' & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0110" THEN  -- A reg. offset
                cyi_v:=1;
                write(csc,cil_v & "A," & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0101" THEN  -- B reg. offset
                cyi_v:=1;
                write(csc,cil_v & "B," & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1011" THEN  -- D reg. offset
                cyi_v:=4;
                write(csc,cil_v & "D," & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0000" THEN  -- Post-inc 1
                cyi_v:=2;
                write(csc,cil_v & c_v & "+" & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0001" THEN  -- Post-inc 2
                cyi_v:=3;
                write(csc,cil_v & c_v & "++" & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0010" THEN  -- Pre-dec 1
                cyi_v:=2;
                write(csc,cil_v & "-" & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="0011" THEN  -- Pre-dec 2
                cyi_v:=3;
                write(csc,cil_v & "--" & c_v & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1100" THEN  -- PC, 8bits offset
                cyi_v:=1;
                waitdata;
                write(csc,cil_v & "PC," & 
                       integer'image(to_integer(signed(trace_dr))) &
                       cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1101" THEN  -- PC, 16bits offset
                cyi_v:=5;
                waitdata;
                tt_v:=trace_dr & trace_dr;
                waitdata;
                tt_v:=tt_v(15 DOWNTO 8) & trace_dr;
                write(csc,cil_v & "PC,x" & to_hstring(tt_v) & cir_v);
              ELSIF trace_dr(3 DOWNTO 0)="1111" THEN -- Extended Indirect
                cyi_v:=2;
                waitdata;
                tt_v:=trace_dr & trace_dr;
                waitdata;
                tt_v:=tt_v(15 DOWNTO 8) & trace_dr;
                write(csc,cil_v & "x" & to_hstring(tt_v) & cir_v);
              END IF;
          END CASE;
          pad(csc,20);
          write(csc,"| D=" & to_hstring(ra_v & rb_v) &
                " X=" & to_hstring(rx_v) &
                " Y=" & to_hstring(ry_v) & " U=" & to_hstring(ru_v) &
                " SP=" & to_hstring(rsp_v) & " DP=" & to_hstring(rdp_v) &
                " CC=" & to_hstring(ccr_v));
          csd:=(OTHERS =>nul);
          write(csd,"[" & integer'image(cyc_v+1) & "] ");
          write(csc,"<" & time'image(now));
          pad(csb,16);
          write(csb,"{" & integer'image(cyi_v+cyp_v+dec_v.cy) & "}");

          cst:=(OTHERS =>nul);
          write(cst,csa2); -- PC :
          write(cst,csb2); -- opcodes
          write(cst,csd);  -- Ilen
          write(cst,csc2); -- Disas
          write(lout,strip(cst));
          csa2:=csa;
          csb2:=csb;
          csc2:=csc;
          writeline(fil,lout);
          csa:=(OTHERS =>nul);
          csb:=(OTHERS =>nul);
          csc:=(OTHERS =>nul);
        END IF;
      END IF;
    END LOOP;
  END PROCESS Trace;

  ad_delay<=ad_i WHEN rising_edge(clk) AND req_i='1' AND ack='1';

  Trace_Cyclo:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      IF trace_ni='1' THEN
        IF trace_state=sFETCH AND trace_pre=0 THEN
          trace_cyc<=0;
        ELSE
          trace_cyc<=trace_cyc+1;
        END IF;
      END IF;
    END IF;
  END PROCESS Trace_Cyclo;
  
--pragma synthesis_on

  xpc<=pc;
  
END ARCHITECTURE rtl;

