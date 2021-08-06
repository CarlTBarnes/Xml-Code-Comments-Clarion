!Clarion !!!! <XML> Code Comment Generator
!by Carl Barnes (c) 2021 released under the MIT License on Github. Use at yourown risk.
!
!1. Paste in a complete Prototype:  Label PROCEDURE(Type Name,...),ReturnType
!2. Click XML Generate
!
!The !!! <Xml> needs to be in the INC file for the intellisense to appear in
!the editor when you Include that .INC file. \
!If you put the <Xml> in the .CLW file the intellisense only appears in THAT CLW.
! 
!Putting several !!! <Xml> lines before each Procedure in the INC file can make
!it long and hard to scan quickly. One option is to put single line with just
!the <Summary> and <Returns> so you leave out the <param>'s.  
!
!Example of this in https://github.com/CarlTBarnes/FindCleanCwIDE/blob/main/CbFindCleanCls.inc
  !!!<summary>Join Patterns in Queue with delimeter C3BFh</summary><Returns>*STRING to replace FindPatterns Value</Returns>
  !PatternsQ2String    PROCEDURE(PatternQType PatQ, *LONG OutLength),*STRING

!-------------------------------------------------------------------------------
  PROGRAM
    INCLUDE 'KeyCodes.CLW'
!   INCLUDE('CbWndPreview.inc'),ONCE   !Available at GitHub CarlTBarnes

  MAP
XmlCommentCW        PROCEDURE() 
ClarionDataType     PROCEDURE(STRING InType,<*STRING OutEntityBug>),BYTE  !1=Type 2=Entity *implied
ClarionNamedType    PROCEDURE(*STRING InOutType, BYTE pUpper)  !Upper 1st Letter of Named Type
ParseString2Queue   PROCEDURE(CONST *STRING pString, STRING cDelim1, QUEUE OutQueue, *STRING QueString, BYTE SkipBlanks=1),LONG,PROC
TestProtoCode       PROCEDURE(*STRING OutProto),BOOL,PROC !Popup with Test Prototypes
  END

  CODE
  XmlCommentCW()
  RETURN 
!========================================================================================
XmlCommentCW PROCEDURE()
!Region Data Declarations
ProtoCode   STRING(1000)
CallTxt     STRING(4000)  

ConfigGrp   GROUP,PRE(CFG) !TODO: Button to Save these to Registry or INI
xSummaryChk      BYTE(1)            !Do we want <Summary>
xSummaryXtra     BYTE(0)            !  Xtra Lines
xReturnsChk      BYTE(1)            !Do we want <Returns>
xReturnsXtra     BYTE(0)            !  Xtra Lines
xRemarksChk      BYTE(1)            !Do we want <Remarks>
xRemarksXtra     BYTE(0)            !  Xtra Lines 
OmitBang3        BYTE(0)  !No !!! just <Xml> so can validate ? Allow \\\ for C#
AlignParmGT      BYTE(1)  !For <param align ">"
UpperTypes       BYTE(0)
            END 

XMLcc     ANY
xCRLF     EQUATE('<13,10>') 
Bang3     PSTRING(5)        ! EQUATE('!!! ') or None        !TODO Add Indent to This

ProtoGrp  GROUP,PRE()
Prot:Name   STRING(64)
Prot:LongLabel  BYTE
Prot:RV:Source  STRING(256)  !All Return Source after (Parms)
Prot:RV:Type    STRING(128)  !Type Returned
Prot:RV:PROC    PSTRING(5)   !Blank or PROC
Prot:RV:Virtual PSTRING(9)   !Blank or VIRTUAL DERIVED REPLACE 
Prot:RV:Other   STRING(256)  !Not Type PROC VIRTUAL
Prot:Parms      STRING(1000)
          END             
                
ParmsQ  QUEUE,PRE(PrmQ)
Omittable   BYTE          !PrmQ:Omittable
Const1      BYTE          !PrmQ:Const1
Type        STRING(32)    !PrmQ:Type
Label       STRING(32)    !PrmQ:Label
Default     STRING(32)    !PrmQ:Default   Type=
Source      STRING(128)   !PrmQ:Source 
LenLabel    BYTE          !PrmQ:LenLabel
        END
        
ReturnQ QUEUE,PRE(RetQ)
NameU       STRING(12)      !RetQ:NameU     Before Paren  
Source      STRING(64)      !RetQ:Source
        END 
IncFileTxt  STRING(32000)
!EndRegion Data Declarations
        
Window WINDOW('<<Xml> Code Comment Generate from Prototype for Clarion'),AT(,,430,250),GRAY,SYSTEM,ICON('XmlComGn.ICO'), |
            FONT('Segoe UI',9),RESIZE
        SHEET,AT(3,4),FULL,USE(?SHEET1)
            TAB(' Input '),USE(?TAB1)
                PROMPT('Prototype:'),AT(9,21),USE(?Prototype1)
                PROMPT('ProcedureName  PROCEDURE( ... parameters ...),Return'),AT(46,21),USE(?Prototype2),FONT('Consolas')
                BUTTON('Test...'),AT(397,19,26,10),USE(?TestBtn),SKIP,FONT(,8),TIP('Pick Test prototype from popup'),FLAT
                CHECK('<<Summary>'),AT(188,66),USE(Cfg:xSummaryChk),TRN
                TEXT,AT(9,31,414,30),USE(ProtoCode),VSCROLL,FONT('Consolas'),DROPID('~TEXT'),ALRT(CtrlQ)
                BUTTON('Paste'),AT(9,67,34,21),USE(?PasteBtn),SKIP,TIP('Paste clipboard into above Procedure Prototype e' & |
                        'ntry and Generate')
                BUTTON('!!! &XML Generate'),AT(49,67,41,21),USE(?XmlBtn),SKIP,TIP('Parse Procedure above and generate !!' & |
                        '! XML comments')
                BUTTON('&Copy<13,10>XML'),AT(96,67,41,21),USE(?CopyBtn),SKIP,TIP('Copy Generated XML at bottom')
                BUTTON('Parse<13,10>Test'),AT(150,67,30,21),USE(?ParseBtn),SKIP,FONT(,8),TIP('Carl wants to just test Parse')
                CHECK('<<Summary>'),AT(188,66),USE(Cfg:xSummaryChk,, ?Cfg:xSummaryChk:2),TRN
                SPIN(@n1),AT(241,66,25,10),USE(Cfg:xSummaryXtra),HVSCROLL,TIP('Extra Lines')
                CHECK('<<Returns>'),AT(279,66),USE(Cfg:xReturnsChk),TRN
                SPIN(@n1),AT(327,66,25,10),USE(Cfg:xReturnsXtra),HVSCROLL,TIP('Extra Lines')
                CHECK('<<Remarks>'),AT(188,78),USE(Cfg:xRemarksChk),TRN
                SPIN(@n1),AT(241,78,25,10),USE(Cfg:xRemarksXtra),HVSCROLL,TIP('Extra Lines')
                CHECK('Align ">"'),AT(279,78),USE(Cfg:AlignParmGT),TRN,TIP('Align closing ">" on Parms')
                CHECK('No !!!'),AT(326,78),USE(Cfg:OmitBang3),TRN,TIP('Omit !!! prefix so just XML is output')
                CHECK('UPR Types'),AT(365,78),USE(Cfg:UpperTypes),TRN,TIP('Standard Clarion Types are UPPER<13,10,13,10>' & |
                        'Check box to Upper ALL Types e.g. STRINGTHEORY<13,10,13,10>Easier to read in Intellisense.<13>' & |
                        '<10>Requires Parsing again ')
                ENTRY(@s64),AT(9,94,183,10),USE(Prot:Name),SKIP,TIP('Prot:Name'),READONLY
                ENTRY(@s255),AT(207,94,205,10),USE(Prot:RV:Source),SKIP,TIP('Prot:RV:Source'),READONLY
                TEXT,AT(9,110,413,29),USE(Prot:Parms),SKIP,VSCROLL,FONT('Consolas'),TIP('Prot:Parms')
                TEXT,AT(9,145),FULL,USE(CallTxt),SKIP,HVSCROLL,FONT('Consolas',10)
            END
            TAB(' Parms List '),USE(?TAB2)
                STRING('Parameters parsed into a List for debug'),AT(8,20),USE(?LIST:ParmsQ:FYI)
                LIST,AT(8,34),FULL,USE(?LIST:ParmsQ),VSCROLL,FROM(ParmsQ),FORMAT('21C|M~Omit~L(2)@n1b@23C|M~Const~L(2)@n' & |
                        '1b@80L(2)|M~Type~@s32@80L(2)|M~Label~@s32@80L(2)|M~Default~@s32@20L(2)|M~Source~@s128@')
            END
            TAB(' Return List '),USE(?TAB3)
                PROMPT('RV Source:'),AT(10,23),USE(?PROMPT:Rv1)
                ENTRY(@s255),AT(47,22,205,10),USE(Prot:RV:Source,, ?Prot:RV:Source:2),SKIP,READONLY
                PROMPT('RV Type:'),AT(10,36),USE(?PROMPT:RV2)
                ENTRY(@s64),AT(47,35,205,10),USE(Prot:RV:Type),SKIP
                PROMPT('RV Other:'),AT(10,49),USE(?PROMPT:RV3)
                ENTRY(@s255),AT(47,48,205,10),USE(Prot:RV:Other),SKIP
                PROMPT('Return type and attributes <13,10>parsed into a List for debug'),AT(281,29,111,24),USE(?LIST:ReturnQ:FYI) |
                        
                LIST,AT(8,68),FULL,USE(?LIST:ReturnQ),VSCROLL,FROM(ReturnQ),FORMAT('90L(2)|M~Type~@s32@20L(2)~Source~@s128@')
            END
            TAB(' INC File '),USE(?TAB:IncFile)
                PROMPT('Paste .INC File  Source here to Copy / Paste into Input tab'),AT(10,21),USE(?PROMPT:IncFileTxt)
                TEXT,AT(9,33),FULL,USE(IncFileTxt),HVSCROLL,FONT('Consolas',10)
            END
        END
    END

DOO     CLASS
ProtoParse          PROCEDURE()
OneParmQParse       PROCEDURE()
ReturnValueParse    PROCEDURE() 
XmlGenerate         PROCEDURE()
XmlGenElement       PROCEDURE(STRING ElementOpen, STRING ElementClose, BYTE pXtraLines, STRING pContent)
        END   
!WndPrvCls   CBWndPreviewClass
    CODE
    OPEN(Window)
    ?SHEET1{PROP:TabSheetStyle}=2  
    SYSTEM{PROP:PropVScroll}=1  
    SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
!    WndPrvCls.Init(1)
    ACCEPT
        CASE EVENT()
        OF EVENT:Drop ; ProtoCode=DropID() ; DISPLAY 
        OF EVENT:AlertKey
           CASE KEYCODE()
           OF CtrlQ ; UPDATE ; SETCLIPBOARD(QUOTE(CLIP(ProtoCode)))  !Quote for Test
           END 
        END
        
        CASE ACCEPTED() 
        OF ?PasteBtn ; ProtoCode=CLIPBOARD() ; POST(EVENT:Accepted,?XmlBtn)
        OF ?TestBtn  ; IF TestProtoCode(ProtoCode) THEN POST(EVENT:Accepted,?XmlBtn).
        OF ?ParseBtn ; DOO.ProtoParse() ; DISPLAY 
        OF ?XmlBtn   ; DOO.ProtoParse() ; DOO.XmlGenerate() ; DISPLAY 
        OF ?CopyBtn  ; SetCLIPBOARD(CallTxt)
        END
    END
 
    RETURN
!--------------------------------------------------------
DOO.XmlGenerate     PROCEDURE()
xSummary1   EQUATE('<<summary>')        !!! <summary>xxx</summary>
xSummary2   EQUATE('<</summary>')
xRemarks1   EQUATE('<<remarks>')
xRemarks2   EQUATE('<</remarks>')
xParamName1 EQUATE('<param name="')     !!!! <param name="label">desc </param>
xParamEnd   EQUATE('</param>')
xReturns1   EQUATE('<<returns>')
xReturns2   EQUATE('<</returns>')

bRemarksCDATA   BYTE(0) ! Stick a CDATA in Remarks <![CDATA[ <test>Data</test> ]]>
QX          USHORT
Parm1       CSTRING(256)
PadLabel    PSTRING(128)
    CODE
    XMLcc='' 
    Bang3=CHOOSE(~Cfg:OmitBang3,'!!! ','')
   
    !---- <summary>  </summary> --------------------------------------- <summary>
    IF Cfg:xSummaryChk THEN
       DOO.XmlGenElement(xSummary1,xSummary2,Cfg:xSummaryXtra, CLIP(Prot:Name) & ALL(' ',10))
    END

    !---- Parameters:  <param name="Variable"> Description </param> --------------     
    LOOP QX=1 TO RECORDS(ParmsQ)
        GET(ParmsQ,QX)
        Parm1 = Bang3 & |
                xParamName1 & |
                  CLIP(PrmQ:Label) & '"' & |
                      CHOOSE(~Cfg:AlignParmGT,'',ALL(' ',Prot:LongLabel-PrmQ:LenLabel)) & |  !Align >
                  '>  ' & |    !Leave 2 spaces so reads better
                CHOOSE(~PrmQ:Const1,'','CONST ') & |
                CLIP(PrmQ:Type) &' '& |
                CHOOSE(~PrmQ:Omittable,'','Optional ') & |
                'Parameter '  & CLIP(PrmQ:Label) &'   '& |
                CHOOSE(~PrmQ:Default,'','  Default=' & CLIP(PrmQ:Default) ) & |
                ALL(' ',10) & xParamEnd & xCRLF                
        XMLcc=XMLcc & CLIP(Parm1)
    END

    !---- <returns>  </returns> --------------------------------------- <returns>
    IF Prot:RV:Type THEN
       Parm1 = CLIP(Prot:RV:Type) &' '& |       !Note CString
               CLIP(Prot:RV:PROC) & ALL(' ',10)
       IF Cfg:xReturnsChk THEN
          DOO.XmlGenElement(xReturns1,xReturns2,Cfg:xReturnsXtra, Parm1 )
       END        
    END
    
        !---- <remarks>  </remarks> ----------------------------------- <remarks>
    IF Cfg:xRemarksChk THEN 
       DOO.XmlGenElement(xRemarks1,xRemarks2,Cfg:xRemarksXtra, CLIP(Prot:RV:Source) & ALL(' ',10))           
    END 
    CallTxt=XMLcc
    RETURN
   
!!! <param name="SearchValue">Sub-string to search for</param>
!!! <param name="Step">The number of characters to jump. Default is 1</param>
!!! <param name="Start">Optional parameter to indicate what position to start search. Default is beginning.</param>
!!! <param name="End">Optional parameter to indicate what position to end search. Default is end of string.</param>

!!! <summary>Return the Base64 encoded version of the current string</summary>
!!! <remarks>The caller must pass in a string of adequate length.
!!!  data will be returned in pText
!!!</remarks>
!----------------------------------------------------
DOO.XmlGenElement PROCEDURE(STRING ElementOpen, STRING ElementClose, BYTE pXtraLines, STRING pContent)
LNo USHORT
    CODE 
    XMLcc=XMLcc & Bang3 & ElementOpen & CHOOSE(pXtraLines>1,'',pContent)
    LOOP LNo=1 TO pXtraLines
        XMLcc=XMLcc & xCRLF & Bang3 & |  
                CHOOSE(LNo<>2,'',pContent) & |
                CHOOSE(LNo < 2 OR LNo >= pXtraLines,'','<para>  </para>')  
    END
    XMLcc=XMLcc & ElementClose & xCRLF 
    RETURN 
!---------------------------- 
DOO.ProtoParse PROCEDURE()
X       USHORT
P1      USHORT
P2      USHORT
Source  STRING(1000)
Parms   STRING(1000)
LenP    USHORT 
InBrackets  BOOL    ![] 
B1      USHORT 
    CODE
    CLEAR(ProtoGrp)
    FREE(ParmsQ)
    FREE(ReturnQ)  
    LOOP X=1 TO SIZE(ProtoCode)  !Remove 9,13,10 |
        CASE VAL(ProtoCode[X])
        OF 0 TO 31 
        OROF VAL('|') ; ProtoCode[X]=''
        END
    END
    Prot:Parms = 'Failed'
    Source = ProtoCode
    P1=INSTRING('(',Source)
    P2=INSTRING(')',Source,1,P1)
    IF ~P1 OR ~P2 OR P2<=P1+1 THEN
        Prot:Parms = 'Failed: (Parens) ' & |
                      CHOOSE(~P1,', No (','') & CHOOSE(P1 AND ~P2,', No )','') & |
                      CHOOSE(P1 AND P2 AND P2<=P1+1,', ) <= (','')
                                
        RETURN
    END 
    X=INSTRING(' ',SUB(Source,1,P1-1))  
    IF ~X THEN 
        Prot:Parms = 'Failed: No Prototype Name Space' ; RETURN  
    END
!    Message('P1=' & P1 &'  P2=' & P2 & |
!            '||RV="' & clip(SUB(Source,P2+1,99)) &'"'& |
!            '||Src:|' & CLIP(Source) ,'ProtoParseRtn')
!   Name  Procedure (parms),Return
    Prot:Name  = SUB(Source,1,X)
    Prot:Parms = Source[P1+1 : P2-1]
    Prot:RV:Source    = LEFT(SUB(Source,P2+1,999))
    IF Prot:RV:Source[1]=',' THEN Prot:RV:Source = LEFT(SUB(Prot:RV:Source,2,999)) . 
    
    !Prototype:   [CONST] Type [ LABEL ]
    !Prototype: < [CONST] Type [ Label ] >      Omittable / Optional
    !Prototype: Type [ Label ] = Default        Default Optional
    !Prototype: Type[] [ Label ]                Array[] has Parens

    Prot:LongLabel = 0
    B1=1       
    Parms = Prot:Parms
    LenP=LEN(CLIP(Parms))
    LOOP X=1 TO LenP + 1
         IF X > LenP OR (~InBrackets AND Parms[X]=',') THEN
            CLEAR(ParmsQ)
            PrmQ:Source=LEFT(SUB(Parms,B1,X-B1))
            DOO.OneParmQParse()
            ADD(ParmsQ) 
            B1=X+1
            IF X > LenP THEN BREAK.
         END 
         IF Parms[X]='[' THEN    !Array Type[]
            InBrackets=1
            CYCLE
         ELSIF InBrackets THEN
            IF Parms[X]=']' THEN InBrackets=0.
            CYCLE 
         END        
    END 
    IF Prot:RV:Source THEN DOO.ReturnValueParse().
    RETURN 
!----------------------------
DOO.OneParmQParse PROCEDURE()  
X       USHORT
P1      USHORT
P2      USHORT
Src  STRING(128)
Parms   STRING(128)
LenP    USHORT 
InBrackets  BOOL    ![] 
B1      USHORT 
    CODE
    !Prototype: < [CONST] Type [ Label ] = Default >      Omittable / Optional
    !Prototype: Type[] [ Label ]                Array[] has Parens

    Src = PrmQ:Source
    IF Src[1]='<<' THEN                     ! < [CONST] Type [ Label ] = Default >
       PrmQ:Omittable = 1              !-->   <
       Src=SUB(Src,2,99)
       LenP=LEN(CLIP(Src))
       IF LenP AND Src[LenP]='>' THEN  !---------------------------------------> >
          Src[LenP]=' '
       END
    END                                     !   [CONST] TYPE  Label = Default 
    IF UPPER(SUB(Src,1,6))='CONST ' THEN  !--->  CONST 
       PrmQ:Const1 = 1    ! 123456
       Src=LEFT(SUB(Src,6,99))
    END                                     !           TYPE Label = Default 
    X=INSTRING('=',Src)                   !----------------------> = Default
    IF X THEN 
       PrmQ:Default=LEFT(SUB(Src,X+1,99)) 
       Src=SUB(Src,1,X-1)
    END                                     !           TYPE Label
                                            !     Array TYPE[] Label 
    X=INSTRING(']',Src)     !Array[]  ? --------------------> ]
    IF ~X THEN X=INSTRING(' ',Src).    !------------------->       Space after TYPE
    PrmQ:Type = SUB(Src,1,X)           !                TYPE
    PrmQ:Label= LEFT(SUB(Src,X+1,99))  !                     Label 
    PrmQ:LenLabel = LEN(CLIP(PrmQ:Label))
    IF Prot:LongLabel < PrmQ:LenLabel THEN
       Prot:LongLabel = PrmQ:LenLabel 
    END
    
    IF ClarionDataType(PrmQ:Type,PrmQ:Type) THEN 
       PrmQ:Type=UPPER(PrmQ:Type) 
    ELSE   !Not a Cla Type 
       ClarionNamedType(PrmQ:Type, CFG:UpperTypes)  !Upper
    END
    RETURN
!------------------------------------------------
DOO.ReturnValueParse PROCEDURE() 
QX  LONG
X   USHORT
    CODE           !Could there be NAME(' , commas , ') ???
    CLEAR(ReturnQ)
    ParseString2Queue(Prot:RV:Source,',',ReturnQ,RetQ:Source) 
    LOOP QX=1 TO RECORDS(ReturnQ)
        GET(ReturnQ,QX)
        X=INSTRING('(',RetQ:Source)         !Is it DLL(1) or NAME() ?
        IF ~X THEN X=SIZE(RetQ:Source)+1 .  !No ( so take it all
        RetQ:NameU=UPPER(SUB(RetQ:Source,1,X-1)) 
        PUT(ReturnQ)
    END
    LOOP QX=1 TO RECORDS(ReturnQ)
        GET(ReturnQ,QX) 
        IF ClarionDataType(RetQ:NameU,RetQ:NameU) THEN  !Is this a data Type 
           Prot:RV:Type=CHOOSE(~Prot:RV:Type,'',CLIP(Prot:RV:Type)&',') & |
                        RetQ:NameU
        ELSE 
            CASE RetQ:NameU
            OROF 'C'
            OROF 'DLL'
            OROF 'NAME'
            OROF 'PASCAL'
            OROF 'PRIVATE'
            OROF 'PROC'      ; Prot:RV:PROC='PROC'
            OROF 'PROTECTED'
            OROF 'RAW'
            OROF 'TYPE'
            
            OF   'VIRTUAL'   
            OROF 'DERIVED'   
            OROF 'REPLACE'   ; Prot:RV:Virtual=RetQ:NameU

            ELSE    !Not a known Attribute must be Type
                ClarionNamedType(RetQ:Source, CFG:UpperTypes)  !Upper
                Prot:RV:Type=CHOOSE(~Prot:RV:Type,'',CLIP(Prot:RV:Type)&',') & |
                             RetQ:Source   
            END 
        END 
        PUT(ReturnQ)
    END 
    RETURN
!=================================================================
ClarionDataType PROCEDURE(STRING InType,<*STRING OutEntityBug>)!,LONG  1=Type 2=Entity *implied
RetType BYTE
    CODE
    IF SUB(InType,1,1)='*' THEN InType=SUB(InType,2,99). 
    InType=UPPER(InType) 
    CASE InType
    OF '?' OrOf 'ANY' OrOf 'ASTRING'  OrOf 'BFLOAT4'  OrOf 'BFLOAT8' OrOf 'BOOL'   OrOf 'BSTRING' OrOf 'BYTE'
           OrOf 'CSTRING' OrOf 'DATE' OrOf 'DECIMAL'  OrOf 'GROUP'   OrOf 'INT64'  OrOf 'LONG' OrOf 'PDECIMAL' OrOf 'PSTRING'
           OrOf 'REAL'   OrOf 'SHORT' OrOf 'SIGNED'   OrOf 'SREAL'   OrOf 'STRING' OrOf 'TIME' 
           OrOf 'UINT64' OrOf 'ULONG' OrOf 'UNSIGNED' OrOf 'USHORT' OrOf 'USTRING' OrOf 'VARIANT' 
        RetType = 1    
    OF 'FILE' OROF 'VIEW' OROF 'KEY' OROF 'INDEX' OROF 'QUEUE' OROF 'WINDOW' OROF 'REPORT' OROF 'BLOB'
        IF ~OMITTED(OutEntityBug) THEN 
            OutEntityBug='*' & InType
        END
        RetType = 2
    END
  !  stop('RetType=' & RetType & '  "' & InType &'"')
    RETURN RetType 
!================================================================= 
ClarionNamedType PROCEDURE(*STRING InOutType, BYTE pUpper)  !Upper 1st Letter of Named Type
Let1 BYTE
    CODE    
    IF pUpper THEN 
       InOutType = UPPER(InOutType) 
    ELSE 
       Let1=CHOOSE(InOutType[1]>='A',1,2)   !Is it Name or *Name
       InOutType[Let1] = UPPER(InOutType[Let1])  !Make First letter
    END 
    RETURN 
!================================================================= 
ParseString2Queue PROCEDURE(CONST *STRING pString, STRING cDelim1, QUEUE OutQueue, *STRING QueString, BYTE SkipBlanks=1)!,LONG,PROC
X       LONG,AUTO
LenStr  LONG,AUTO
B1      LONG,AUTO
AddCnt  LONG
    CODE
    B1=1
    LenStr=LEN(CLIP(pString))
    LOOP X=1 TO LenStr + 1
         IF X > LenStr OR pString[X]=cDelim1 THEN
            QueString=LEFT(SUB(pString,B1,X-B1))
            IF ~SkipBlanks OR QueString THEN 
               ADD(OutQueue)
               AddCnt += 1
            END 
            IF X > LenStr THEN BREAK.
            B1=X+1 
         END 
    END
    RETURN AddCnt 
!=================================================================
TestProtoCode PROCEDURE(*STRING OutProto)!,BOOL !Popup with Test Prototypes
    CODE
    EXECUTE POPUP('ReplaceBetween ( 9 parms and <<Omit> )' & |
                  '|FindBetween( *Parms )' & |
                  '|LinesViewSplit( Class,  *Parms, <<Omit> )' & |
                  '|SerializeQueue( *Q ) no return' & |
                  '|AddCount( Arrays[,] )' & |
                  '|PatternsQ2String( NamedType),*String' & |
                  '')
        OutProto='ReplaceBetween PROCEDURE(string pLeft, <<string pRight>, string pOldValue, string pNewValue, long pCount=0, long pStart=1, long pEnd=0, long pNoCase=0, long pReplaceAll=false),Long,Proc,virtual'
        OutProto='FindBetween PROCEDURE(const *string pFindIn, string pLeft, string pRight, *long outStart, *long outEnd, bool pNoCase=false, long pExclusive=true),string'
        OutProto='LinesViewSplit  PROCEDURE(StringTheory STwithLinesSplitDone, STRING SplitDelim, <<STRING QuoteStart>,<<STRING QuoteEnd>, BYTE RemoveQuotes=TRUE, bool pClip=false, bool pLeft=false, <<STRING pSeparator>, long pNested=false)'
        OutProto='SerializeQueue PROCEDURE(*Queue pQueue,<<string pRecordBoundary>,*StringTheory pFieldBoundary,<<string pQuotestart>,<<string pQuoteEnd>,Long pLevel=1,Long pFree=true),virtual'
        OutProto='AddCount PROCEDURE(*LONG[,] Total,*LONG[,] Current)'
        OutProto='PatternsQ2String  PROCEDURE(PatternQType PatQ, *LONG OutLength),*STRING'
    ELSE
        RETURN FALSE
    END 
    RETURN TRUE
 