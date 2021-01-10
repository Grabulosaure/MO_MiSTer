--------------------------------------------------------------------------------
-- 
-- MC6809 CPU
--------------------------------------------------------------------------------

-- Package :
-- - ALU operations
-- - Instruction decode

--------------------------------------------------------------------------------
-- DO 8/2017
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- This design can be used for any purpose.
-- Please send any bug report or remark to : dev@temlib.org
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY work;
USE work.base_pack.ALL;

PACKAGE mc6809_pack IS
  
  CONSTANT VEC_RESET : unsigned(2 DOWNTO 0) := "111";
  CONSTANT VEC_NMI   : unsigned(2 DOWNTO 0) := "110";
  CONSTANT VEC_SWI   : unsigned(2 DOWNTO 0) := "101";
  CONSTANT VEC_IRQ   : unsigned(2 DOWNTO 0) := "100";
  CONSTANT VEC_FIRQ  : unsigned(2 DOWNTO 0) := "011";
  CONSTANT VEC_SWI2  : unsigned(2 DOWNTO 0) := "010";
  CONSTANT VEC_SWI3  : unsigned(2 DOWNTO 0) := "001";
  
  ------------------------------------------------

  TYPE enum_mode IS (DIRECT,
                     EXTENDED,
                     INHERENT,
                     RELATIVE,RELATIVE_BSR,
                     IMMEDIATE,
                     INDEXED);

  TYPE enum_reg IS (M,A,B,CC,DP,
                    D,X,Y,U,SP,CP,
                    X_Y_S,D_D_U,U_S);
  
  TYPE enum_ea IS (R8,W8,R16,RC16,W16,RMW8,RT8,LEA,JMP,JSR);
                   
  TYPE type_op IS RECORD
    m : enum_mode;
    r : enum_reg;
    ea : enum_ea;
    s : std_logic;              -- TRACE : 0=8bits 1=16bits
    cy : natural RANGE 0 TO 31; -- TRACE : Cycles
    dis : string(1 TO 8);       -- TRACE : Instruction
    dis10 : string(1 TO 8);     -- TRACE : Instruction x10
    dis11 : string(1 TO 8);     -- TRACE : Instruction x11
  END RECORD;
  TYPE arr_op IS ARRAY (natural RANGE <>) OF type_op;
  
  CONSTANT opcodes:arr_op(0 TO 255):=(
    (DIRECT,M,RMW8,'0',6,"NEG     ","Invalid ","Invalid "), -- 00
    (DIRECT,M,RMW8,'0',6,"(NEG)   ","Invalid ","Invalid "), -- 01 = 00
    (DIRECT,M,RMW8,'0',6,"(NEGCOM)","Invalid ","Invalid "), -- 02 Inval NEG/COM
    (DIRECT,M,RMW8,'0',6,"COM     ","Invalid ","Invalid "), -- 03
    (DIRECT,M,RMW8,'0',6,"LSR     ","Invalid ","Invalid "), -- 04
    (DIRECT,M,RMW8,'0',0,"(LSR)   ","Invalid ","Invalid "), -- 05 = 04
    (DIRECT,M,RMW8,'0',6,"ROR     ","Invalid ","Invalid "), -- 06
    (DIRECT,M,RMW8,'0',6,"ASR     ","Invalid ","Invalid "), -- 07
    (DIRECT,M,RMW8,'0',6,"ASL     ","Invalid ","Invalid "), -- 08
    (DIRECT,M,RMW8,'0',6,"ROL     ","Invalid ","Invalid "), -- 09
    (DIRECT,M,RMW8,'0',6,"DEC     ","Invalid ","Invalid "), -- 0A
    (DIRECT,M,RMW8,'0',6,"(DEC)   ","Invalid ","Invalid "), -- 0B = 0A
    (DIRECT,M,RMW8,'0',6,"INC     ","Invalid ","Invalid "), -- 0C
    (DIRECT,M,RT8,'0',6,"TST     ","Invalid ","Invalid "), -- 0D
    (DIRECT,M,JMP,'0',3,"JMP     ","Invalid ","Invalid "), -- 0E
    (DIRECT,M,RMW8,'0',6,"CLR     ","Invalid ","Invalid "), -- 0F
    (INHERENT,M,RMW8,'0',0,"PREFIX  ","PREFIX  ","PREFIX  "), -- 10
    (INHERENT,M,RMW8,'0',0,"PREFIX  ","PREFIX  ","PREFIX  "), -- 11
    (INHERENT,M,RMW8,'0',2,"NOP     ","Invalid ","Invalid "), -- 12
    (INHERENT,M,RMW8,'0',4,"SYNC    ","Invalid ","Invalid "), -- 13
    (RELATIVE,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 14
    (RELATIVE,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 15
    (RELATIVE,M,RMW8,'0',5,"LBRA    ","Invalid ","Invalid "), -- 16
    (RELATIVE_BSR,M,RMW8,'0',9,"LBSR    ","Invalid ","Invalid "), -- 17
    (INHERENT,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 18
    (INHERENT,M,RMW8,'0',2,"DAA     ","Invalid ","Invalid "), -- 19
    (IMMEDIATE,M,RMW8,'0',3,"ORCC    ","Invalid ","Invalid "), -- 1A
    (IMMEDIATE,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 1B
    (IMMEDIATE,M,RMW8,'0',3,"ANDCC   ","Invalid ","Invalid "), -- 1C
    (INHERENT,M,RT8,'0',2,"SEX     ","Invalid ","Invalid "), -- 1D
    (IMMEDIATE,M,JMP,'0',8,"EXG     ","Invalid ","Invalid "), -- 1E
    (IMMEDIATE,M,RMW8,'0',6,"TFR     ","Invalid ","Invalid "), -- 1F
    (RELATIVE,M,LEA,'0',3,"BRA     "," LBRA in","Invalid "), -- 20
    (RELATIVE,M,LEA,'0',3,"BRN     ","LBRN    ","Invalid "), -- 21
    (RELATIVE,M,LEA,'0',3,"BHI     ","LBHI    ","Invalid "), -- 22
    (RELATIVE,M,LEA,'0',3,"BLS     ","LBLS    ","Invalid "), -- 23
    (RELATIVE,M,LEA,'0',3,"BHS     ","LBHS    ","Invalid "), -- 24
    (RELATIVE,M,LEA,'0',3,"BLO     ","LBLO    ","Invalid "), -- 25
    (RELATIVE,M,LEA,'0',3,"BNE     ","LBNE    ","Invalid "), -- 26
    (RELATIVE,M,LEA,'0',3,"BEQ     ","LBEQ    ","Invalid "), -- 27
    (RELATIVE,M,LEA,'0',3,"BVC     ","LBVC    ","Invalid "), -- 28
    (RELATIVE,M,LEA,'0',3,"BVS     ","LBVS    ","Invalid "), -- 29
    (RELATIVE,M,LEA,'0',3,"BPL     ","LBPL    ","Invalid "), -- 2A
    (RELATIVE,M,LEA,'0',3,"BMI     ","LBMI    ","Invalid "), -- 2B
    (RELATIVE,M,LEA,'0',3,"BGE     ","LBGE    ","Invalid "), -- 2C
    (RELATIVE,M,LEA,'0',3,"BLT     ","LBLT    ","Invalid "), -- 2D
    (RELATIVE,M,LEA,'0',3,"BGT     ","LBGT    ","Invalid "), -- 2E
    (RELATIVE,M,LEA,'0',3,"BLE     ","LBLE    ","Invalid "), -- 2F
    (INDEXED,M,LEA,'1',4,"LEAX    ","Invalid ","Invalid "), -- 30
    (INDEXED,M,LEA,'1',4,"LEAY    ","Invalid ","Invalid "), -- 31
    (INDEXED,M,LEA,'1',4,"LEAS    ","Invalid ","Invalid "), -- 32
    (INDEXED,M,LEA,'1',4,"LEAU    ","Invalid ","Invalid "), -- 33
    (IMMEDIATE,M,LEA,'0',5,"PSHS    ","Invalid ","Invalid "), -- 34
    (IMMEDIATE,M,LEA,'0',5,"PULS    ","Invalid ","Invalid "), -- 35
    (IMMEDIATE,M,LEA,'0',5,"PSHU    ","Invalid ","Invalid "), -- 36
    (IMMEDIATE,M,LEA,'0',5,"PULU    ","Invalid ","Invalid "), -- 37
    (INHERENT,M,LEA,'0',0,"Invalid ","Invalid ","Invalid "), -- 38
    (INHERENT,M,LEA,'0',5,"RTS     ","Invalid ","Invalid "), -- 39
    (INHERENT,M,LEA,'0',3,"ABX     ","Invalid ","Invalid "), -- 3A
    (INHERENT,M,LEA,'0',6,"RTI     ","Invalid ","Invalid "), -- 3B
    (IMMEDIATE,M,LEA,'0',20,"CWAI    ","Invalid ","Invalid "), -- 3C
    (INHERENT,M,LEA,'0',11,"MUL     ","Invalid ","Invalid "), -- 3D
    (INHERENT,M,LEA,'0',0,"Invalid ","Invalid ","Invalid "), -- 3E
    (INHERENT,M,LEA,'0',19,"SWI     ","SWI2    ","SWI3    "), -- 3F
    (INHERENT,A,R8,'0',2,"NEGA    ","Invalid ","Invalid "), -- 40
    (INHERENT,A,R8,'0',2,"(NEGA)  ","Invalid ","Invalid "), -- 41 = 40
    (INHERENT,A,R8,'0',2,"(COMA)  ","Invalid ","Invalid "), -- 42 = 43
    (INHERENT,A,R8,'0',2,"COMA    ","Invalid ","Invalid "), -- 43
    (INHERENT,A,R8,'0',2,"LSRA    ","Invalid ","Invalid "), -- 44
    (INHERENT,A,R8,'0',2,"(LSRA)  ","Invalid ","Invalid "), -- 45 = 44
    (INHERENT,A,R8,'0',2,"RORA    ","Invalid ","Invalid "), -- 46
    (INHERENT,A,R8,'0',2,"ASRA    ","Invalid ","Invalid "), -- 47
    (INHERENT,A,R8,'0',2,"ASLA    ","Invalid ","Invalid "), -- 48
    (INHERENT,A,R8,'0',2,"ROLA    ","Invalid ","Invalid "), -- 49
    (INHERENT,A,R8,'0',2,"DECA    ","Invalid ","Invalid "), -- 4A
    (INHERENT,A,R8,'0',2,"(DECA)  ","Invalid ","Invalid "), -- 4B = 4A
    (INHERENT,A,R8,'0',2,"INCA    ","Invalid ","Invalid "), -- 4C
    (INHERENT,A,R8,'0',2,"TSTA    ","Invalid ","Invalid "), -- 4D
    (INHERENT,A,R8,'0',2,"(CLRA)  ","Invalid ","Invalid "), -- 4E = 4F
    (INHERENT,A,R8,'0',2,"CLRA    ","Invalid ","Invalid "), -- 4F
    (INHERENT,B,R8,'0',2,"NEGB    ","Invalid ","Invalid "), -- 50
    (INHERENT,B,R8,'0',2,"(NEGB)  ","Invalid ","Invalid "), -- 51 = 50
    (INHERENT,B,R8,'0',2,"(COMB)  ","Invalid ","Invalid "), -- 52 = 53
    (INHERENT,B,R8,'0',2,"COMB    ","Invalid ","Invalid "), -- 53
    (INHERENT,B,R8,'0',2,"LSRB    ","Invalid ","Invalid "), -- 54
    (INHERENT,B,R8,'0',0,"Invalid ","Invalid ","Invalid "), -- 55 = 54
    (INHERENT,B,R8,'0',2,"RORB    ","Invalid ","Invalid "), -- 56
    (INHERENT,B,R8,'0',2,"ASRB    ","Invalid ","Invalid "), -- 57
    (INHERENT,B,R8,'0',2,"ASLB/LSL","Invalid ","Invalid "), -- 58
    (INHERENT,B,R8,'0',2,"ROLB    ","Invalid ","Invalid "), -- 59
    (INHERENT,B,R8,'0',2,"DECB    ","Invalid ","Invalid "), -- 5A
    (INHERENT,B,R8,'0',0,"Invalid ","Invalid ","Invalid "), -- 5B = 5A
    (INHERENT,B,R8,'0',2,"INCB    ","Invalid ","Invalid "), -- 5C
    (INHERENT,B,R8,'0',2,"TSTB    ","Invalid ","Invalid "), -- 5D
    (INHERENT,B,R8,'0',0,"Invalid ","Invalid ","Invalid "), -- 5E = 5F
    (INHERENT,B,R8,'0',2,"CLRB    ","Invalid ","Invalid "), -- 5F
    (INDEXED,M,RMW8,'0',6,"NEG     ","Invalid ","Invalid "), -- 60
    (INDEXED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 61 = 60?
    (INDEXED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 62 = 63?
    (INDEXED,M,RMW8,'0',6,"COM     ","Invalid ","Invalid "), -- 63
    (INDEXED,M,RMW8,'0',6,"LSR     ","Invalid ","Invalid "), -- 64
    (INDEXED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 65 = 64?
    (INDEXED,M,RMW8,'0',6,"ROR     ","Invalid ","Invalid "), -- 66
    (INDEXED,M,RMW8,'0',6,"ASR     ","Invalid ","Invalid "), -- 67
    (INDEXED,M,RMW8,'0',6,"ASL     ","Invalid ","Invalid "), -- 68
    (INDEXED,M,RMW8,'0',6,"ROL     ","Invalid ","Invalid "), -- 69
    (INDEXED,M,RMW8,'0',6,"DEC     ","Invalid ","Invalid "), -- 6A
    (INDEXED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 6B = 6A?
    (INDEXED,M,RMW8,'0',6,"INC     ","Invalid ","Invalid "), -- 6C
    (INDEXED,M,RT8,'0',6,"TST     ","Invalid ","Invalid "), -- 6D
    (INDEXED,M,JMP,'0',3,"JMP     ","Invalid ","Invalid "), -- 6E
    (INDEXED,M,RMW8,'0',6,"CLR     ","Invalid ","Invalid "), -- 6F
    (EXTENDED,M,RMW8,'0',7,"NEG     ","Invalid ","Invalid "), -- 70
    (EXTENDED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 71 = 70
    (EXTENDED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 72 = 73
    (EXTENDED,M,RMW8,'0',7,"COM     ","Invalid ","Invalid "), -- 73
    (EXTENDED,M,RMW8,'0',7,"LSR     ","Invalid ","Invalid "), -- 74
    (EXTENDED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 75 = 74
    (EXTENDED,M,RMW8,'0',7,"ROR     ","Invalid ","Invalid "), -- 76
    (EXTENDED,M,RMW8,'0',7,"ASR     ","Invalid ","Invalid "), -- 77
    (EXTENDED,M,RMW8,'0',7,"ASL     ","Invalid ","Invalid "), -- 78
    (EXTENDED,M,RMW8,'0',7,"ROL     ","Invalid ","Invalid "), -- 79
    (EXTENDED,M,RMW8,'0',7,"DEC     ","Invalid ","Invalid "), -- 7A
    (EXTENDED,M,RMW8,'0',0,"Invalid ","Invalid ","Invalid "), -- 7B = 7A
    (EXTENDED,M,RMW8,'0',7,"INC     ","Invalid ","Invalid "), -- 7C
    (EXTENDED,M,RT8,'0',7,"TST     ","Invalid ","Invalid "), -- 7D
    (EXTENDED,M,JMP,'0',4,"JMP     ","Invalid ","Invalid "), -- 7E
    (EXTENDED,M,RMW8,'0',7,"CLR     ","Invalid ","Invalid "), -- 7F
    (IMMEDIATE,A,R8,'0',2,"SUBA    ","Invalid ","Invalid "), -- 80
    (IMMEDIATE,A,R8,'0',2,"CMPA    ","Invalid ","Invalid "), -- 81
    (IMMEDIATE,A,R8,'0',2,"SBCA    ","Invalid ","Invalid "), -- 82
    (IMMEDIATE,D_D_U,RC16,'1',4,"SUBD    ","CMPD    ","CMPU    "), -- 83
    (IMMEDIATE,A,R8,'0',2,"ANDA    ","Invalid ","Invalid "), -- 84
    (IMMEDIATE,A,R8,'0',2,"BITA    ","Invalid ","Invalid "), -- 85
    (IMMEDIATE,A,R8,'0',2,"LDA     ","Invalid ","Invalid "), -- 86
    (IMMEDIATE,A,W8,'0',0,"Invalid ","Invalid ","Invalid "), -- 87
    (IMMEDIATE,A,R8,'0',2,"EORA    ","Invalid ","Invalid "), -- 88
    (IMMEDIATE,A,R8,'0',2,"ADCA    ","Invalid ","Invalid "), -- 89
    (IMMEDIATE,A,R8,'0',2,"ORA     ","Invalid ","Invalid "), -- 8A
    (IMMEDIATE,A,R8,'0',2,"ADDA    ","Invalid ","Invalid "), -- 8B
    (IMMEDIATE,X_Y_S,RC16,'1',4,"CMPX    ","CMPY    ","CMPS    "), -- 8C
    (RELATIVE_BSR,X_Y_S,JSR,'0',7,"BSR     ","Invalid ","Invalid "), -- 8D
    (IMMEDIATE,X_Y_S,R16,'1',3,"LDX     ","LDY     ","Invalid "), -- 8E
    (IMMEDIATE,X_Y_S,W16,'0',0,"Invalid ","Invalid ","Invalid "), -- 8F
    (DIRECT,A,R8,'0',4,"SUBA    ","Invalid ","Invalid "), -- 90
    (DIRECT,A,R8,'0',4,"CMPA    ","Invalid ","Invalid "), -- 91
    (DIRECT,A,R8,'0',4,"SBCA    ","Invalid ","Invalid "), -- 92
    (DIRECT,D_D_U,RC16,'1',6,"SUBD    ","CMPD    ","CMPU    "), -- 93
    (DIRECT,A,R8,'0',4,"ANDA    ","Invalid ","Invalid "), -- 94
    (DIRECT,A,R8,'0',4,"BITA    ","Invalid ","Invalid "), -- 95
    (DIRECT,A,R8,'0',4,"LDA     ","Invalid ","Invalid "), -- 96
    (DIRECT,A,W8,'0',4,"STA     ","Invalid ","Invalid "), -- 97
    (DIRECT,A,R8,'0',4,"EORA    ","Invalid ","Invalid "), -- 98
    (DIRECT,A,R8,'0',4,"ADCA    ","Invalid ","Invalid "), -- 99
    (DIRECT,A,R8,'0',4,"ORA     ","Invalid ","Invalid "), -- 9A
    (DIRECT,A,R8,'0',4,"ADDA    ","Invalid ","Invalid "), -- 9B
    (DIRECT,X_Y_S,RC16,'1',6,"CMPX    ","CMPY    ","CMPS    "), -- 9C
    (DIRECT,X_Y_S,JSR,'0',7,"JSR     ","Invalid ","Invalid "), -- 9D
    (DIRECT,X_Y_S,R16,'1',5,"LDX     ","LDY     ","Invalid "), -- 9E
    (DIRECT,X_Y_S,W16,'1',5,"STX     ","STY     ","Invalid "), -- 9F
    (INDEXED,A,R8,'0',4,"SUBA    ","Invalid ","Invalid "), -- A0
    (INDEXED,A,R8,'0',4,"CMPA    ","Invalid ","Invalid "), -- A1
    (INDEXED,A,R8,'0',4,"SBCA    ","Invalid ","Invalid "), -- A2
    (INDEXED,D_D_U,RC16,'1',6,"SUBD    ","CMPD    ","CMPU    "), -- A3
    (INDEXED,A,R8,'0',4,"ANDA    ","Invalid ","Invalid "), -- A4
    (INDEXED,A,R8,'0',4,"BITA    ","Invalid ","Invalid "), -- A5
    (INDEXED,A,R8,'0',4,"LDA     ","Invalid ","Invalid "), -- A6
    (INDEXED,A,W8,'0',4,"STA     ","Invalid ","Invalid "), -- A7
    (INDEXED,A,R8,'0',4,"EORA    ","Invalid ","Invalid "), -- A8
    (INDEXED,A,R8,'0',4,"ADCA    ","Invalid ","Invalid "), -- A9
    (INDEXED,A,R8,'0',4,"ORA     ","Invalid ","Invalid "), -- AA
    (INDEXED,A,R8,'0',4,"ADDA    ","Invalid ","Invalid "), -- AB
    (INDEXED,X_Y_S,RC16,'1',6,"CMPX    ","CMPY    ","CMPS    "), -- AC
    (INDEXED,X_Y_S,JSR,'0',7,"JSR     ","Invalid ","Invalid "), -- AD
    (INDEXED,X_Y_S,R16,'1',5,"LDX     ","LDY     ","Invalid "), -- AE
    (INDEXED,X_Y_S,W16,'1',5,"STX     ","STY     ","Invalid "), -- AF
    (EXTENDED,A,R8,'0',5,"SUBA    ","Invalid ","Invalid "), -- B0
    (EXTENDED,A,R8,'0',5,"CMPA    ","Invalid ","Invalid "), -- B1
    (EXTENDED,A,R8,'0',5,"SBCA    ","Invalid ","Invalid "), -- B2
    (EXTENDED,D_D_U,RC16,'1',7,"SUBD    ","CMPD    ","CMPU    "), -- B3
    (EXTENDED,A,R8,'0',5,"ANDA    ","Invalid ","Invalid "), -- B4
    (EXTENDED,A,R8,'0',5,"BITA    ","Invalid ","Invalid "), -- B5
    (EXTENDED,A,R8,'0',5,"LDA     ","Invalid ","Invalid "), -- B6
    (EXTENDED,A,W8,'0',5,"STA     ","Invalid ","Invalid "), -- B7
    (EXTENDED,A,R8,'0',5,"EORA    ","Invalid ","Invalid "), -- B8
    (EXTENDED,A,R8,'0',5,"ADCA    ","Invalid ","Invalid "), -- B9
    (EXTENDED,A,R8,'0',5,"ORA     ","Invalid ","Invalid "), -- BA
    (EXTENDED,A,R8,'0',5,"ADDA    ","Invalid ","Invalid "), -- BB
    (EXTENDED,X_Y_S,RC16,'1',7,"CMPX    ","CMPY    ","CMPS    "), -- BC
    (EXTENDED,X_Y_S,JSR,'0',8,"JSR     ","Invalid ","Invalid "), -- BD
    (EXTENDED,X_Y_S,R16,'1',6,"LDX     ","LDY     ","Invalid "), -- BE
    (EXTENDED,X_Y_S,W16,'1',6,"STX     ","STY     ","Invalid "), -- BF
    (IMMEDIATE,B,R8,'0',2,"SUBB    ","Invalid ","Invalid "), -- C0
    (IMMEDIATE,B,R8,'0',2,"CMPB    ","Invalid ","Invalid "), -- C1
    (IMMEDIATE,B,R8,'0',2,"SBCB    ","Invalid ","Invalid "), -- C2
    (IMMEDIATE,D_D_U,RC16,'1',4,"ADDD    ","Invalid ","Invalid "), -- C3
    (IMMEDIATE,B,R8,'0',2,"ANDB    ","Invalid ","Invalid "), -- C4
    (IMMEDIATE,B,R8,'0',2,"BITB    ","Invalid ","Invalid "), -- C5
    (IMMEDIATE,B,R8,'0',2,"LDB     ","Invalid ","Invalid "), -- C6
    (IMMEDIATE,B,W8,'0',0,"Invalid ","Invalid ","Invalid "), -- C7
    (IMMEDIATE,B,R8,'0',2,"EORB    ","Invalid ","Invalid "), -- C8
    (IMMEDIATE,B,R8,'0',2,"ADCB    ","Invalid ","Invalid "), -- C9
    (IMMEDIATE,B,R8,'0',2,"ORB     ","Invalid ","Invalid "), -- CA
    (IMMEDIATE,B,R8,'0',2,"ADDB    ","Invalid ","Invalid "), -- CB
    (IMMEDIATE,D_D_U,R16,'1',3,"LDD     ","Invalid ","Invalid "), -- CC
    (IMMEDIATE,D_D_U,W16,'0',0,"Invalid ","Invalid ","Invalid "), -- CD
    (IMMEDIATE,U_S,R16,'1',3,"LDU     ","LDS     ","Invalid "), -- CE
    (IMMEDIATE,U_S,W16,'0',0,"Invalid ","Invalid ","Invalid "), -- CF
    (DIRECT,B,R8,'0',4,"SUBB    ","Invalid ","Invalid "), -- D0
    (DIRECT,B,R8,'0',4,"CMPB    ","Invalid ","Invalid "), -- D1
    (DIRECT,B,R8,'0',4,"SBCB    ","Invalid ","Invalid "), -- D2
    (DIRECT,D_D_U,RC16,'1',6,"ADDD    ","Invalid ","Invalid "), -- D3
    (DIRECT,B,R8,'0',4,"ANDB    ","Invalid ","Invalid "), -- D4
    (DIRECT,B,R8,'0',4,"BITB    ","Invalid ","Invalid "), -- D5
    (DIRECT,B,R8,'0',4,"LDB     ","Invalid ","Invalid "), -- D6
    (DIRECT,B,W8,'0',4,"STB     ","Invalid ","Invalid "), -- D7
    (DIRECT,B,R8,'0',4,"EORB    ","Invalid ","Invalid "), -- D8
    (DIRECT,B,R8,'0',4,"ADCB    ","Invalid ","Invalid "), -- D9
    (DIRECT,B,R8,'0',4,"ORB     ","Invalid ","Invalid "), -- DA
    (DIRECT,B,R8,'0',4,"ADDB    ","Invalid ","Invalid "), -- DB
    (DIRECT,D_D_U,R16,'1',5,"LDD     ","Invalid ","Invalid "), -- DC
    (DIRECT,D_D_U,W16,'1',5,"STD     ","Invalid ","Invalid "), -- DD
    (DIRECT,U_S,R16,'1',5,"LDU     ","LDS     ","Invalid "), -- DE
    (DIRECT,U_S,W16,'1',5,"STU     ","STS     ","Invalid "), -- DF
    (INDEXED,B,R8,'0',4,"SUBB    ","Invalid ","Invalid "), -- E0
    (INDEXED,B,R8,'0',4,"CMPB    ","Invalid ","Invalid "), -- E1
    (INDEXED,B,R8,'0',4,"SBCB    ","Invalid ","Invalid "), -- E2
    (INDEXED,D_D_U,RC16,'1',6,"ADDD    ","Invalid ","Invalid "), -- E3
    (INDEXED,B,R8,'0',4,"ANDB    ","Invalid ","Invalid "), -- E4
    (INDEXED,B,R8,'0',4,"BITB    ","Invalid ","Invalid "), -- E5
    (INDEXED,B,R8,'0',4,"LDB     ","Invalid ","Invalid "), -- E6
    (INDEXED,B,W8,'0',4,"STB     ","Invalid ","Invalid "), -- E7
    (INDEXED,B,R8,'0',4,"EORB    ","Invalid ","Invalid "), -- E8
    (INDEXED,B,R8,'0',4,"ADCB    ","Invalid ","Invalid "), -- E9
    (INDEXED,B,R8,'0',4,"ORB     ","Invalid ","Invalid "), -- EA
    (INDEXED,B,R8,'0',4,"ADDB    ","Invalid ","Invalid "), -- EB
    (INDEXED,D_D_U,R16,'1',5,"LDD     ","Invalid ","Invalid "), -- EC
    (INDEXED,D_D_U,W16,'1',5,"STD     ","Invalid ","Invalid "), -- ED
    (INDEXED,U_S,R16,'1',5,"LDU     ","LDS     ","Invalid "), -- EE
    (INDEXED,U_S,W16,'1',5,"STU     ","STS     ","Invalid "), -- EF
    (EXTENDED,B,R8,'0',5,"SUBB    ","Invalid ","Invalid "), -- F0
    (EXTENDED,B,R8,'0',5,"CMPB    ","Invalid ","Invalid "), -- F1
    (EXTENDED,B,R8,'0',5,"SBCB    ","Invalid ","Invalid "), -- F2
    (EXTENDED,D_D_U,RC16,'1',7,"ADDD    ","Invalid ","Invalid "), -- F3
    (EXTENDED,B,R8,'0',5,"ANDB    ","Invalid ","Invalid "), -- F4
    (EXTENDED,B,R8,'0',5,"BITB    ","Invalid ","Invalid "), -- F5
    (EXTENDED,B,R8,'0',5,"LDB     ","Invalid ","Invalid "), -- F6
    (EXTENDED,B,W8,'0',5,"STB     ","Invalid ","Invalid "), -- F7
    (EXTENDED,B,R8,'0',5,"EORB    ","Invalid ","Invalid "), -- F8
    (EXTENDED,B,R8,'0',5,"ADCB    ","Invalid ","Invalid "), -- F9
    (EXTENDED,B,R8,'0',5,"ORB     ","Invalid ","Invalid "), -- FA
    (EXTENDED,B,R8,'0',5,"ADDB    ","Invalid ","Invalid "), -- FB
    (EXTENDED,D_D_U,R16,'1',6,"LDD     ","Invalid ","Invalid "), -- FC
    (EXTENDED,D_D_U,W16,'1',6,"STD     ","Invalid ","Invalid "), -- FD
    (EXTENDED,U_S,R16,'1',6,"LDU     ","LDS     ","Invalid "), -- FE
    (EXTENDED,U_S,W16,'1',6,"STU     ","STS     ","Invalid ") -- FF

    
    );

  
  
  ------------------------------------------------
  TYPE type_ccr IS RECORD
    c : std_logic; -- 0 : Carry
    v : std_logic; -- 1 : 'Verflow
    z : std_logic; -- 2 : Zero
    n : std_logic; -- 3 : Negative
    i : std_logic; -- 4 : IRQ mask
    h : std_logic; -- 5 : Half Carry
    f : std_logic; -- 6 : FIRQ mask
    e : std_logic; -- 7 : Entire Save
  END RECORD;

  FUNCTION cc_conv(x:uv8) RETURN type_ccr;
  
  FUNCTION cc_conv(x:type_ccr) RETURN uv8;
  
  ------------------------------------------------
  PROCEDURE op_alu(
    op  : IN  uv8;       -- opcode
    pre : IN  natural RANGE 0 TO 2; -- No prefix / x10 /x11 prefix
    il1 : IN  uv8; -- Input1  LO
    ih1 : IN  uv8; -- Input1  HI
    il2 : IN  uv8; -- Input2  LO
    ih2 : IN  uv8; -- Input2  HI
    icc : IN  type_ccr;
    ol  : OUT uv8; -- Output LO
    oh  : OUT uv8; -- Output HI
    oml : OUT std_logic;  -- Modified Output regs LO.
    omh : OUT std_logic;  -- Modified Output regs HI.
    occ : OUT type_ccr);
  
  PROCEDURE op_abx(
    rx  : IN uv16;
    rb  : IN uv8;
    ol  : OUT uv8;
    oh  : OUT uv8;
    oml : OUT std_logic;
    omh : OUT std_logic);
  
  PROCEDURE op_mul(
    il1 : IN uv8;
    il2 : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oh  : OUT uv8;
    oml : OUT std_logic;
    omh : OUT std_logic;
    occ : OUT type_ccr);
  
  PROCEDURE op_daa(
    il  : IN  uv8;
    icc : IN  type_ccr;
    ol  : OUT uv8;
    oml : OUT std_logic;
    occ : OUT type_ccr);
  
  PROCEDURE op_sex(
    il  : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oml : OUT std_logic;
    occ : OUT type_ccr);
  
  ------------------------------------------------
  FUNCTION bcond(op : uv8;
                 cc : type_ccr) RETURN boolean;

END PACKAGE;

--##############################################################################
PACKAGE BODY mc6809_pack IS

  FUNCTION cc_conv(x:type_ccr) RETURN uv8 IS
    VARIABLE t:uv8;
  BEGIN
    t:=x.e & x.f & x.h & x.i & x.n & x.z & x.v & x.c;
    RETURN t;
  END FUNCTION cc_conv;

  FUNCTION cc_conv(x:uv8) RETURN type_ccr IS
  BEGIN
    RETURN (c=>x(0),v=>x(1),z=>x(2),n=>x(3),
            i=>x(4),h=>x(5),f=>x(6),e=>x(7));
  END FUNCTION cc_conv;
  
  --------------------------------------
  PROCEDURE op_abx(
    rx  : IN uv16;
    rb  : IN uv8;
    ol  : OUT uv8;
    oh  : OUT uv8;
    oml : OUT std_logic;
    omh : OUT std_logic) IS
    VARIABLE tmp : uv16;
  BEGIN
    tmp:=rx+rb;
    ol:=tmp(7 DOWNTO 0);
    oh:=tmp(15 DOWNTO 8);
    oml:='1';
    omh:='1';
    
  END PROCEDURE op_abx;
  
  PROCEDURE op_alu(
    op  : IN uv8;       -- opcode
    pre : IN natural RANGE 0 TO 2; -- No prefix / x10 / x11 prefix
    il1 : IN uv8;
    ih1 : IN uv8;
    il2 : IN uv8;
    ih2 : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oh  : OUT uv8;
    oml : OUT std_logic;  -- Modified Output regs.
    omh : OUT std_logic;  -- Modified Output regs.
    occ : OUT type_ccr) IS
    VARIABLE opx : unsigned(4 DOWNTO 0);
    VARIABLE ol_v,oh_v : uv8;
    VARIABLE v16_v : uv16;
    VARIABLE cin_v : std_logic;
    VARIABLE add5_v : unsigned(4 DOWNTO 0);
  BEGIN
    occ :=icc;
    ol_v:=il1;
    oh_v:=ih1;
    oml:='0';
    omh:='0';
    
    opx:=op(7) & op(3 DOWNTO 0);
    cin_v:=icc.c AND (op(1) XOR op(3));
    
    CASE opx IS
      WHEN "00000" | "00001" => -- NEG + (invalid NEG)
        ol_v:=x"00" - il2(7 DOWNTO 0);
        oml:='1';
        occ.c:=to_std_logic(ol_v/=x"00");
        occ.v:=to_std_logic(il2=x"80");
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);

      WHEN "00010" => -- Invalid NEG+COM ???
        IF op=x"02" THEN
          IF icc.c='0' THEN
            ol_v:=x"00" - il2(7 DOWNTO 0);
            occ.c:=to_std_logic(ol_v/=x"00");
            occ.v:=to_std_logic(il2=x"80");
          ELSE
            ol_v:=NOT il2;
            occ.c:='1';
            occ.v:='0';
          END IF;
          oml:='1';
          occ.z:=to_std_logic(ol_v=x"00");
          occ.n:=ol_v(7);
        ELSE
          ol_v:=il1;
          oh_v:=ih1;
        END IF;
        
      WHEN "00011" => -- COM = NOT
        ol_v:=NOT il2;
        oml:='1';
        occ.c:='1';
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "00110" |  -- ROR : b7=carry
           "00100" |  -- LSR : b7=0
           "00111" => -- ASR : b7=b7
        ol_v:=((icc.c AND op(1) AND NOT op(0)) OR
               (il2(7) AND op(1) AND op(0))) & il2(7 DOWNTO 1);
        oml:='1';
        occ.c:=il2(0);
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "01001" |  -- ROL : b0=carry
           "01000" => -- ASL / LSL : b0=0
        ol_v:=il2(6 DOWNTO 0) & (icc.c AND op(0));
        oml:='1';
        occ.c:=il2(7);
        occ.v:=il2(6) XOR il2(7);
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "01100" => -- INC
        ol_v:=il2 + 1;
        oml:='1';
        occ.v:=(il2(7) AND NOT ol_v(7)) OR (NOT il2(7) AND ol_v(7));
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "01010" | "01011" => -- DEC  + inval. DEC
        ol_v:=il2 - 1;
        oml:='1';
        occ.v:=(il2(7) AND NOT ol_v(7)) OR (NOT il2(7) AND ol_v(7));
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "01101" => -- TST
        ol_v:=il2;
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "01111" => -- CLR
        ol_v:=x"00";
        oml:='1';
        occ.c:='0';
        occ.v:='0';
        occ.n:=ol_v(7);
        occ.z:=to_std_logic(ol_v=x"00");
        
      WHEN "10000" |  -- SUB
           "10001" |  -- CMP
           "10010" => -- SBC
        --cin_v:=icc.c AND op(1);
        ol_v:=il1 - (il2 + ("0000000" & cin_v));
        oml:=NOT op(0);
        add5_v:='0' & il1(3 DOWNTO 0) - ('0' & il2(3 DOWNTO 0))
               - ("0000" & cin_v);
        occ.c:=(NOT il1(7) AND il2(7)) OR
                (ol_v(7) AND (NOT il1(7) OR il2(7)));
        occ.v:=(il1(7) AND NOT il2(7) AND NOT ol_v(7)) OR
               (NOT il1(7) AND il2(7) AND ol_v(7));
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        occ.h:=add5_v(4);
        
      WHEN "11001" |  -- ADC
           "11011" => -- ADD
        --cin_v:=icc.c AND NOT op(1);
        ol_v:=il1 + il2 + ("0000000" & cin_v);
        oml:='1';
        add5_v:='0' & il1(3 DOWNTO 0) + ('0' & il2(3 DOWNTO 0))
                 + ("0000" & cin_v);
        occ.c:=(il1(7) AND il2(7)) OR (NOT ol_v(7) AND (il1(7) OR il2(7)));
        occ.v:=(il1(7) AND il2(7) AND NOT ol_v(7)) OR
               (NOT il1(7) AND NOT il2(7) AND ol_v(7));
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        occ.h:=add5_v(4);
        
      WHEN "10011" => -- SUB16/ADD16/CMP16
        IF op(6)='1' THEN -- ADD16: ADDD
          v16_v:=(ih1 & il1) + (ih2 & il2);
          oh_v:=v16_v(15 DOWNTO 8);
          ol_v:=v16_v( 7 DOWNTO 0);
          oml:='1';
          omh:='1';
          occ.c:=(ih1(7) AND ih2(7)) OR (NOT oh_v(7) AND (ih1(7) OR ih2(7)));
          occ.v:=(ih1(7) AND ih2(7) AND NOT oh_v(7)) OR
                  (NOT ih1(7) AND NOT ih2(7) AND oh_v(7));
          
        ELSE -- SUB16 / CMP16
          v16_v:=(ih1 & il1) - (ih2 & il2);
          oh_v:=v16_v(15 DOWNTO 8);
          ol_v:=v16_v( 7 DOWNTO 0);
          occ.c:=(NOT ih1(7) AND ih2(7)) OR
                  (oh_v(7) AND (NOT ih1(7) OR ih2(7)));
          occ.v:=(ih1(7) AND NOT ih2(7) AND NOT oh_v(7)) OR
                  (NOT ih1(7) AND ih2(7) AND oh_v(7));
          oml:=to_std_logic(pre=0);
          omh:=to_std_logic(pre=0); -- CMP16 : CMPD / CMPU
        END IF;
        occ.z:=to_std_logic((oh_v & ol_v)=x"0000");
        occ.n:=oh_v(7);
        
      WHEN "10100" |  -- AND
           "10101" => -- BIT
        ol_v:=il1 AND il2;
        oml:=NOT op(0);
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "10110" =>  -- LD
        ol_v:=il2;
        oml:='1';
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "10111" => -- ST
        ol_v:=il1;
        oml:='1';
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "11000" => -- EOR
        ol_v:=il1 XOR il2;
        oml:='1';
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "11010" => -- OR
        ol_v:=il1 OR il2;
        oml:='1';
        occ.v:='0';
        occ.z:=to_std_logic(ol_v=x"00");
        occ.n:=ol_v(7);
        
      WHEN "11100" => -- CMP16/LD16
        IF op(6)='0' THEN -- CMP16
          v16_v:=(ih1 & il1) - (ih2 & il2);
          oh_v:=v16_v(15 DOWNTO 8);
          ol_v:=v16_v( 7 DOWNTO 0);
          occ.c:=(NOT ih1(7) AND ih2(7)) OR
                  (oh_v(7) AND (NOT ih1(7) OR ih2(7)));
          occ.v:=(ih1(7) AND NOT ih2(7) AND NOT oh_v(7)) OR
                  (NOT ih1(7) AND ih2(7) AND oh_v(7));
          oml:='0';
          omh:='0';
        ELSE -- LD16
          ol_v:=il2;
          oh_v:=ih2;
          oml:='1';
          omh:='1';
          occ.v:='0';
        END IF;
        occ.z:=to_std_logic((oh_v & ol_v)=x"0000");
        occ.n:=oh_v(7);
        
      WHEN "11101" |   -- ST16
           "11111" =>
        ol_v:=il1;
        oh_v:=ih1;
        oml:='1';
        omh:='1';
        occ.v:='0';
        occ.z:=to_std_logic((oh_v & ol_v)=x"0000");
        occ.n:=oh_v(7);
        
      WHEN "11110" => -- LD16
        ol_v:=il2;
        oh_v:=ih2;
        oml:='1';
        omh:='1';
        occ.v:='0';
        occ.z:=to_std_logic((oh_v & ol_v)=x"0000");
        occ.n:=oh_v(7);
        
      WHEN --"00001" |  -- No arith. op. => invalid
           --"00010" |  -- No arith. op. (NOP)
           --"01011" |  -- No arith. op.
           --"00101" |  -- No arith. op.
           --"01110" |  -- No arith. op. (JMP)
           OTHERS =>
        ol_v:=il1;
        oh_v:=ih1;
        
    END CASE;
    
    ol:=ol_v;
    oh:=oh_v;
    
  END PROCEDURE op_alu;

  ------------------------------------------------
  PROCEDURE op_mul(
    il1 : IN uv8;
    il2 : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oh  : OUT uv8;
    oml : OUT std_logic;
    omh : OUT std_logic;
    occ : OUT type_ccr) IS
    VARIABLE v16_v : uv16;
    VARIABLE oh_v,ol_v : uv8;
  BEGIN
    v16_v := il1 * il2;
    ol_v:=v16_v( 7 DOWNTO 0);
    oh_v:=v16_v(15 DOWNTO 8);
    occ:=icc;
    occ.z:=to_std_logic((oh_v & ol_v)=x"0000");
    occ.c:=ol_v(7);
    ol:=ol_v;
    oh:=oh_v;
    oml:='1';
    omh:='1';
  END PROCEDURE op_mul;
    
  ------------------------------------------------
  PROCEDURE op_daa(
    il  : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oml : OUT std_logic;
    occ : OUT type_ccr) IS
    VARIABLE ol_v : unsigned(8 DOWNTO 0);
  BEGIN
    IF icc.c='1' OR il(3 DOWNTO 0)>x"9" THEN
      ol_v(3 DOWNTO 0):=il(3 DOWNTO 0)+"0110";
    ELSE
      ol_v(3 DOWNTO 0):=il(3 DOWNTO 0);
    END IF;
    IF icc.c='1' OR il(7 DOWNTO 4)>x"9" OR
      (il(7 DOWNTO 4)>x"8" AND il(3 DOWNTO 0)>x"9") THEN
      ol_v(8 DOWNTO 4):=('0' & il(7 DOWNTO 4))+"00110";
    ELSE
      ol_v(8 DOWNTO 4):='0' & il(7 DOWNTO 4);
    END IF;
    ol:=ol_v(7 DOWNTO 0);
    occ:=icc;
    occ.c:=icc.c OR ol_v(8);
    occ.z:=to_std_logic(ol_v(7 DOWNTO 0)=x"00");
    occ.n:=ol_v(7);
    oml:='1';
            
  END PROCEDURE op_daa;

  ------------------------------------------------
  PROCEDURE op_sex(
    il  : IN uv8;
    icc : IN type_ccr;
    ol  : OUT uv8;
    oml : OUT std_logic;
    occ : OUT type_ccr) IS
  BEGIN

    occ:=icc;
    IF il(7)='1' THEN
      ol:=x"FF";
      occ.n:='1';
    ELSE
      ol:=x"00";
      occ.n:='0';
    END IF;
    occ.z:=to_std_logic(il=x"00");
    oml:='1';
    
  END PROCEDURE op_sex;
  
  ------------------------------------------------
  FUNCTION bcond(op : uv8;
                 cc : type_ccr) RETURN boolean IS
    VARIABLE br : boolean;
  BEGIN
    CASE op(3 DOWNTO 0) IS
      WHEN x"0" => -- BRA
        br:=true;
      WHEN x"1" => -- BRN
        br:=false;
      WHEN x"2" => -- BHI 
        br:=(cc.c='0' AND cc.z='0');
      WHEN x"3" => -- BLS
        br:=(cc.c='1' OR cc.z='1');
      WHEN x"4" => -- BHS, BCC
        br:=(cc.c='0');
      WHEN x"5" => -- BLO, BCS
        br:=(cc.c='1');
      WHEN x"6" => -- BNE
        br:=(cc.z='0');
      WHEN x"7" => -- BEQ
        br:=(cc.z='1');
      WHEN x"8" => -- BVC
        br:=(cc.v='0');
      WHEN x"9" => -- BVS
        br:=(cc.v='1');
      WHEN x"A" => -- BPL
        br:=(cc.n='0');
      WHEN x"B" => -- BMI
        br:=(cc.n='1');
      WHEN x"C" => -- BGE
        br:=(cc.n=cc.v);
      WHEN x"D" => -- BLT
        br:=(cc.n/=cc.v);
      WHEN x"E" => -- BGT
        br:=(cc.n=cc.v AND cc.z='0');
      WHEN x"F" => -- BLE
        br:=(cc.n/=cc.v OR cc.z='1');
      WHEN OTHERS =>
        br:=false;
    END CASE;
    IF op=x"16" THEN -- LBRA
      br:=true;
    END IF;
    RETURN br;
  END FUNCTION bcond;
  
END PACKAGE BODY mc6809_pack;


-- _0000 : Auto-inc
-- _0001 : Auto-inc 2
-- _0010 : Auto-dec
-- _0011 : Auto-dec 2
-- _0100 : No offset
-- _0101 : B reg. offset
-- _0110 : A reg. offset
-- _0111 : ?
-- _1000 : 8bits offset
-- _1001 : 16bits offset
-- _1010 : ?
-- _1011 : D reg. offset
-- _1100 : 8bits offset PC
-- _1101 : 16bits offset PC
-- _1110 : ?
-- _1111 : Extended Indirect






