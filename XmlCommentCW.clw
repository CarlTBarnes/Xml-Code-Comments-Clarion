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
!the INC long and hard to scan quickly. One option is to put single line with just
!the <Summary> and <Returns> so you leave out the <param>'s.  
!
!Example of this in https://github.com/CarlTBarnes/FindCleanCwIDE/blob/main/CbFindCleanCls.inc
  !!!<summary>Join Patterns in Queue with delimeter C3BFh</summary><Returns>*STRING to replace FindPatterns Value</Returns>
  !PatternsQ2String    PROCEDURE(PatternQType PatQ, *LONG OutLength),*STRING

!-------------------------------------------------------------------------------
! History
! 20-Oct-2024 Region now has 6 options e.g. "Region Proc ====" ==> !Region ProcName() Help =======
! 20-Oct-2024 Move Dash Line !---- before REGION (checks DashLineBefore & RegionEndRegion). Dash State3 is line of Equals
! 17-Oct-2024 xReturnsChk add STATE3 to CHECK which ALWAYS Generates <Returns>. Done for use in CLW where may not have RV.
! 16-Oct-2024 New "Region" option put !Region !EndRegion around !!! <XML> Comments so can be collapsed
! 29-May-2022 Fix bugs with no parameters "PROCEDURE()", "PROCEDURE,ReturnType" and "PROCEDURE"
!             Adjust Window to work with Manifest   
! 02-Jun-2022 Option to Number <Param > #. to help some with commas on omitted e.g. ST.Replace has 9 (ParamNumbered) 
!             Window Cosmetic: More logical layout and grouping
! 03-Jun-2022 Remove extra spaces in Prototype. 
!             <Param > improve: Remove 'Parameter', move 'Optional' after Label. Alsign better
!             Window Cosmetic: Prototype Text is now FULL and MinWidth=Width at Open
! 03-Jun-2022 Window reformat to move debug fields off first tab
! 03-Jun-2022 Spot By *Address and adjust alignment. Move CONST after Label
!-------------------------------------------------------------------------------

  PROGRAM
    INCLUDE('KeyCodes.CLW')

_CbWndPreview_  EQUATE(0)               !To build in change to (1) and UnComment below Include. Puts Secret Button on window top
!    INCLUDE('CbWndPreview.inc'),ONCE   !https://github.com/CarlTBarnes/WindowPreview
    COMPILE('!***',_CbWndPreview_)
WndPrvCls CBWndPreviewClass
             !***
    
  MAP
XmlCommentCW        PROCEDURE() 
ClarionDataType     PROCEDURE(STRING InType,<*STRING OutEntity_PlusBug>,<*PSTRING OutEntity_BugOnly>),BYTE  !returns 1=ClaType 2=Entity *implied 0=Not CLA
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
XmlComText  STRING(4000)  

ConfigGrp   GROUP,PRE(CFG) !TODO: Button to Save these to Registry or INI
xSummaryChk      BYTE(1)            !Do we want <Summary>
xSummaryXtra     BYTE(0)            !  Xtra Lines
xReturnsChk      BYTE(1)            !Do we want <Returns>   10/17/24 3=State3 => Always even if no Return Prot:RV:Type
xReturnsXtra     BYTE(0)            !  Xtra Lines
xRemarksChk      BYTE(1)            !Do we want <Remarks>
xRemarksXtra     BYTE(0)            !  Xtra Lines 
OmitBang3        BYTE(0)  !No !!! just <Xml> so can validate ? Allow \\\ for C#
AlignParmGT      BYTE(1)  !For <param align ">"
UpperTypes       BYTE(0)
DashLineBefore   BYTE(0)  !05/29/22 way to add !---- to generated   !10/20/24 State3(3) is !=======
RegionEndRegion  BYTE(0)  !10/20/24 has values 10 to 33 -- 10/16/24 add !Region / EndRegion around XML Comments
ParamNumbered    BYTE(1)  !06/02/22 <param ...> 1. 2. 
            END 

XMLcc     ANY
xCRLF     EQUATE('<13,10>') 
Bang3     PSTRING(5)        ! EQUATE('!!! ') or None        !TODO Add Indent to This

ProtoGrp  GROUP,PRE()
Prot:Name   STRING(64)
Prot:BigLabel   BYTE       !Big=MaxLen of Labels to align  
Prot:BigTypeCLA BYTE       !Big=MaxLen of only Clarion Types BYTE SHORT LONG
Prot:BigTypeAll BYTE       !Big=MaxLen Type Name of All Types
Prot:ByAddressZ BYTE       !There are *TYPE By Addressez for All Types
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
Asterisk    PSTRING(2)    !PrmQ:Asterisk    *TYPE passed by address, adjust alignment
Type        STRING(32)    !PrmQ:Type        !Does NOT have *
LenType     BYTE          !PrmQ:LenType
TypeIsCLA   BYTE          !PrmQ:TypeIsCLA   !Is Clarion Type BYTE SHORT LONG
Label       STRING(32)    !PrmQ:Label
LenLabel    BYTE          !PrmQ:LenLabel
Default     STRING(32)    !PrmQ:Default     Type=Default Value
Source      STRING(128)   !PrmQ:Source
        END
        
ReturnQ QUEUE,PRE(RetQ)
NameU       STRING(12)      !RetQ:NameU     Before Paren  
Source      STRING(64)      !RetQ:Source
        END 
IncFileTxt  STRING(32000)
!EndRegion Data Declarations
        
Window WINDOW('<<Xml> Code Comment Generate from Prototype for Clarion'),AT(,,430,230),GRAY,SYSTEM, |
            ICON('XmlComGn.ICO'),FONT('Segoe UI',9),RESIZE
        SHEET,AT(3,4),FULL,USE(?SHEET1)
            TAB(' &Input  '),USE(?Tab:Input)
                PROMPT('&Prototype:'),AT(9,20),USE(?Prototype1)
                PROMPT('ProcedureName  PROCEDURE(...parameters...),Return'),AT(46,21),USE(?Prototype2), |
                        FONT('Consolas')
                BUTTON('Copy'),AT(293,19,27,10),USE(?CopyProtBtn),SKIP,FONT(,8),TIP('Copy Prototype ' & |
                        'to Clipboard'),FLAT
                BUTTON('Parse Test'),AT(334,19,41,10),USE(?ParseBtn),SKIP,FONT(,8),TIP('Carl wants t' & |
                        'o just test Parse of the Prototype'),FLAT
                BUTTON('Test Data...'),AT(385,19,41,10),USE(?TestBtn),SKIP,FONT(,8),TIP('Pick Test P' & |
                        'rototype from Popup'),FLAT
                TEXT,AT(9,31,,30),FULL,USE(ProtoCode),VSCROLL,FONT('Consolas'),TIP('Prototype of Pro' & |
                        'cedure with (Parameters),RetunValue<13,10>Single line without continuation'), |
                        DROPID('~TEXT'),ALRT(CtrlQ)
                BUTTON('Past&e'),AT(8,67,28,21),USE(?PasteBtn),SKIP,TIP('Paste clipboard into above ' & |
                        'Procedure Prototype entry and Generate')
                BUTTON('!!! &XML Generate'),AT(40,67,34,21),USE(?XmlBtn),SKIP,TIP('Parse Procedure a' & |
                        'bove and generate !!! XML comments')
                BUTTON('&Copy<0Dh,0Ah>XML'),AT(78,67,30,21),USE(?CopyBtn),SKIP,TIP('Copy Generated X' & |
                        'ML at bottom')
                CHECK('<<Summary>'),AT(152,66),USE(Cfg:xSummaryChk),TRN
                SPIN(@n1),AT(207,66,25,10),USE(Cfg:xSummaryXtra),HVSCROLL,TIP('Summary Extra Lines'), |
                        RANGE(0,9)
                CHECK('<<Remarks>'),AT(152,79),USE(Cfg:xRemarksChk),TRN
                SPIN(@n1),AT(207,79,25,10),USE(Cfg:xRemarksXtra),HVSCROLL,TIP('Remarks Extra Lines'), |
                        RANGE(0,9)
                CHECK('<<Returns>'),AT(246,66),USE(Cfg:xReturnsChk),TRN,STATE3('3')
                SPIN(@n1),AT(296,66,25,10),USE(Cfg:xReturnsXtra),HVSCROLL,TIP('Returns Extra Lines'), |
                        RANGE(0,9)
                CHECK('<<Parm> #.'),AT(246,79),USE(Cfg:ParamNumbered),TRN,TIP('<<Param> lines are Nu' & |
                        'mbered 1,2,3')
                CHECK('Align ">"'),AT(296,79),USE(Cfg:AlignParmGT),TRN,TIP('Align closing ">" on Params')
                CHECK('UPR Types'),AT(340,66),USE(Cfg:UpperTypes),TRN,TIP('Standard Clarion Types ar' & |
                        'e UPPER<13,10,13,10>Check box to Upper ALL Types e.g. STRINGTHEORY<13,10>' & |
                        '<13,10>Easier to read in Intellisense.<13><10>Requires Generate XML again ')
                CHECK('No !!!'),AT(113,74),USE(Cfg:OmitBang3),TRN,FONT(,8),TIP('Omit !!! prefix so j' & |
                        'ust XML is output<13,10>Allows working with XML e.g. in an XML Validator')
                CHECK('! -------'),AT(389,66),USE(Cfg:DashLineBefore),TRN,TIP('Dashed line before Su' & |
                        'mmary<13,10>State 3 is !===== equals'),STATE3('3')
                TEXT,AT(9,94),FULL,USE(XmlComText),SKIP,HVSCROLL,FONT('Consolas',10)
                LIST,AT(341,79,85,10),USE(Cfg:RegionEndRegion),VSCROLL,TIP('!Region and !EndRegion l' & |
                        'ines to allow collapsing XML Comments'),DROP(9),FROM('No Region|#0|Region|#' & |
                        '10|Region Procedure()|#21|Region Proc ------|#22|Region Proc ====|#23|Regio' & |
                        'n ------------|#32|Region ========|#33')
            END
            TAB(' Par&ms List  '),USE(?Tab:Parms)
                STRING('Parameters parsed from Prototype into a List for debug'),AT(8,21), |
                        USE(?LIST:ParmsQ:FYI)
                LIST,AT(8,34),FULL,USE(?LIST:ParmsQ),VSCROLL,FROM(ParmsQ),FORMAT('21C|M~Omit~L(2)@n1' & |
                        'b@23C|M~Const~L(2)@n1b@10C|M~*~@s1@Q''By Address''50L(2)|M~Type~@s32@16R(3)' & |
                        '|M~Len~C(0)@n3@Q''Length of Type''14C|M~Cla~@n1b@Q''Clarion Native Type''80' & |
                        'L(2)|M~Label~@s32@16R(3)|M~Len~C(0)@n3@Q''Length of Label''80L(2)|M~Default' & |
                        '~@s32@20L(2)|M~Source~@s128@')
            END
            TAB(' &Return List  '),USE(?Tab:Return)
                PROMPT('Name:'),AT(10,22),USE(?PROMPT:Rv0)
                ENTRY(@s64),AT(47,22,205,10),USE(Prot:Name),SKIP,TIP('Procedure Name - Prot:Name'), |
                        READONLY
                PROMPT('RV Source:'),AT(10,37),USE(?PROMPT:Rv1)
                ENTRY(@s255),AT(47,37,205,10),USE(Prot:RV:Source),SKIP,READONLY
                PROMPT('RV Type:'),AT(10,50),USE(?PROMPT:RV2)
                ENTRY(@s64),AT(47,50,205,10),USE(Prot:RV:Type),SKIP
                PROMPT('RV Other:'),AT(10,63),USE(?PROMPT:RV3)
                ENTRY(@s255),AT(47,63,205,10),USE(Prot:RV:Other),SKIP
                PROMPT('Return Type and Attributes <0Ah,0Dh>parsed into a List for debug'), |
                        AT(281,42,111,24),USE(?LIST:ReturnQ:FYI)
                LIST,AT(8,80),FULL,USE(?LIST:ReturnQ),VSCROLL,FROM(ReturnQ),FORMAT('90L(2)|M~Type / ' & |
                        'Attribute~@s32@20L(2)~Source~@s128@')
            END
            TAB(' INC &File  '),USE(?TAB:IncFile)
                PROMPT('Paste .INC File  Source here to Copy / Paste into Input tab'),AT(10,21), |
                        USE(?PROMPT:IncFileTxt)
                TEXT,AT(9,33),FULL,USE(IncFileTxt),HVSCROLL,FONT('Consolas',10)
            END
        END
        BUTTON('Run Another'),AT(376,3,50,10),USE(?RunAgainBtn),SKIP,FONT(,8),TIP('Run another instance')
    END

DOO     CLASS
ProtoParse          PROCEDURE()
OneParmQParse       PROCEDURE()
ReturnValueParse    PROCEDURE() 
XmlGenerate         PROCEDURE()
XmlGenElement       PROCEDURE(STRING ElementOpen, STRING ElementClose, BYTE pXtraLines, STRING pContent)
XmlGenAddLine       PROCEDURE(STRING pLineContent, BOOL NoCrLf=False)
        END   
    CODE
    OPEN(Window)
    ?SHEET1{PROP:TabSheetStyle}=2  
    ?SHEET1{PROP:NoTheme}=1         !Incase Manifest adds Visual Stylesmakes sheet look better
    ?LIST:ParmsQ{PROP:NoTheme}=1    !Incase Manifest
    ?LIST:ReturnQ{PROP:NoTheme}=1   !Incase Manifest
    SYSTEM{PROP:PropVScroll}=1  
    SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
    0{PROP:MinWidth} = 0{PROP:Width} * 1 ; 0{PROP:MinHeight} = 0{PROP:Height} * .70
    COMPILE('!***',_CbWndPreview_)
        WndPrvCls.Init(2)           !Add Carl's Window Preview Class to allow runtime window design
             !***
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow ; SELECT(?ProtoCode)
        OF EVENT:Drop ; ProtoCode=DropID() ; DISPLAY 
        OF EVENT:AlertKey
           CASE KEYCODE()
           OF CtrlQ ; UPDATE ; SETCLIPBOARD(QUOTE(CLIP(ProtoCode)))  !Quote for Test
           END 
        END
        
        CASE ACCEPTED() 
        OF ?PasteBtn ; ProtoCode=CLIPBOARD() ; POST(EVENT:Accepted,?XmlBtn)
        OF ?TestBtn  ; IF TestProtoCode(ProtoCode) THEN 
                          CLEAR(ProtoGrp) ; CLEAR(XmlComText) ; DISPLAY 
                          POST(EVENT:Accepted,?XmlBtn) !10/16/24 was SELECT(?XmlBtn) but that's SKIP
                       END
        OF ?ParseBtn ; DOO.ProtoParse() ; DISPLAY 
        OF ?XmlBtn   ; DOO.ProtoParse() ; DOO.XmlGenerate() ; DISPLAY 
        OF ?CopyBtn  ; SetCLIPBOARD(XmlComText)
        OF ?RunAgainBtn ; RUN(COMMAND('0'))
        OF ?CopyProtBtn ; SetCLIPBOARD(ProtoCode)
        OF ?Cfg:OmitBang3       ; POST(EVENT:Accepted,?XmlBtn) 
        OF ?Cfg:DashLineBefore  ; ?{PROP:Text}=CHOOSE(Cfg:DashLineBefore<3,'! -------','! =====')
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
TypeLen     USHORT
Parm1Len    USHORT
RegionProc  PSTRING(48)
RegionLine  PSTRING(96)
    CODE
    XMLcc='' 
    IF Prot:Parms[1:6] = 'Failed' THEN
       XmlComText = Prot:Parms
       RETURN 
    END    
    Bang3=CHOOSE(~Cfg:OmitBang3,'!!! ','')

    CASE Cfg:DashLineBefore         ! --- Dashed Line -------------------------------------------------
    OF 1 ; XMLcc='! -{78}<13,10>'   !Must be !--- not !!!--- or it will generate into Intellisense without CRLF so messedup
    OF 3 ; XMLcc='! ={78}<13,10>'   !10/20/24 STATE3(3) is Equals !========
    END                             !Could do !!! <!-- ----- --> still in Intellisense but looks better 

    IF Cfg:RegionEndRegion THEN  ! --- Wrap in !Region !EndRegion Lines --------------------------
       RegionProc = CLIP(Prot:Name) &'() Help '
       CASE Cfg:RegionEndRegion
       OF 21 ; RegionLine='!Region '& RegionProc                ! Region Proc Name      |#21
       OF 22 ; RegionLine='!Region '& RegionProc & ALL('-')     ! Region Proc ------    |#22
       OF 23 ; RegionLine='!Region '& RegionProc & ALL('=')     ! Region Proc ====      |#23
       OF 32 ; RegionLine='!Region '&              ALL('-')     ! Region ------------   |#32
       OF 33 ; RegionLine='!Region '&              ALL('=')     ! Region ========       |#33
       ELSE  ; RegionLine='!Region '                            ! Region                |#10
       END
       RegionLine = CLIP(SUB(RegionLine,1,80)) &' '
       DOO.XmlGenAddLine(RegionLine)
    END 
    
    !---- <summary>  </summary> --------------------------------------- <summary>
    IF Cfg:xSummaryChk THEN
       DOO.XmlGenElement(xSummary1,xSummary2,Cfg:xSummaryXtra, CLIP(Prot:Name) &'() {10}')
    END

    !Want Clarion Types aligned as same width. If Named Type's are +4 use that length.
    IF INRANGE(Prot:BigTypeAll,Prot:BigTypeCLA+1,Prot:BigTypeCLA+4) THEN   !Named types just 4 longer
       Prot:BigTypeCLA = Prot:BigTypeAll
    END

    !---- Parameters:  <param name="Variable"> Description </param> --------------
    LOOP QX=1 TO RECORDS(ParmsQ)
        GET(ParmsQ,QX)
        IF Prot:ByAddressZ AND ~PrmQ:Asterisk THEN      !Some are *Address but not this, so leave 1 Space
           PrmQ:Asterisk=' '                            !note PSTRING(2)
        END
        TypeLen=PrmQ:LenType
        IF TypeLen < Prot:BigTypeCLA AND ~PrmQ:Const1 THEN TypeLen=Prot:BigTypeCLA.   !06/03/22 UNSIGNED is 8
        Parm1 = Bang3 & |
                xParamName1 & |
                  CLIP(PrmQ:Label) & '"' & |
                      CHOOSE(~Cfg:AlignParmGT,'',ALL(' ',Prot:BigLabel-PrmQ:LenLabel)) & |  !Align >
                  '> ' & |    !Leave 2 spaces so reads better
             CHOOSE(~Cfg:ParamNumbered,' ',QX &'. ') & |        !06/02/22 <Param ... > #.
                     PrmQ:Asterisk                   & |        !* or ' ', or '' IF NONE HAVE *
                 SUB(PrmQ:Type,1,TypeLen)        &' '& |        !Type BYTE SHORT LONG STRING or Named e.g. StringTheory
                CLIP(PrmQ:Label)                 &' '& |        !06/03/22 remove 'Parameter '  &
             CHOOSE(~PrmQ:Const1,'',' CONST ')       & |        !06/03/22 moved after TYPE so Type Aligns
             CHOOSE(~PrmQ:Omittable,'',' Optional ') & |        !06/03/22 moved Optional after Label
             CHOOSE(~PrmQ:Default,'',' Default=' & CLIP(PrmQ:Default) )

        Parm1Len=LEN(CLIP(Parm1))
        IF Parm1Len < 80 AND Cfg:AlignParmGT THEN       !Align </param> somewhat
           Parm1=Parm1 & ALL(' ')                       !note CString
           Parm1Len=80
        END
        XMLcc=XMLcc & SUB(Parm1,1,Parm1Len) & |
                      ALL(' ',10) & xParamEnd & xCRLF
    END

    !---- <returns>  </returns> --------------------------------------- <returns>
    IF Cfg:xReturnsChk AND (Cfg:xReturnsChk=3 OR Prot:RV:Type) THEN     !10/17/24 State3 means always, else only if RV Type
       Parm1 = CLIP(Prot:RV:Type) & |       !Note CString
               CLIP(' '&Prot:RV:PROC) & ALL(' ',10)
       DOO.XmlGenElement(xReturns1,xReturns2,Cfg:xReturnsXtra, Parm1 )
    END    
    
        !---- <remarks>  </remarks> ----------------------------------- <remarks>
    IF Cfg:xRemarksChk THEN 
       DOO.XmlGenElement(xRemarks1,xRemarks2,Cfg:xRemarksXtra, CLIP(Prot:RV:Source) & ALL(' ',10))           
    END

    IF Cfg:RegionEndRegion THEN  ! --- Wrap in !Region !EndRegion Lines --------------------------
       DOO.XmlGenAddLine('!EndRegion ')
    END 

    XmlComText=XMLcc
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
!----------------------------------------------------
DOO.XmlGenAddLine PROCEDURE(STRING pLineContent, BOOL NoCrLf=False)
    CODE 
    IF NoCrLf THEN 
       XMLcc=XMLcc & pLineContent
    ELSE
       XMLcc=XMLcc & pLineContent & xCRLF
    END
    RETURN
!---------------------------- 
DOO.ProtoParse PROCEDURE()
X       USHORT
O       USHORT      !06/03/22 Compress extra spaces
P1      USHORT      !1st (
P2      USHORT      !1st )
Cma1    USHORT      !Comma 1 after PROCEDURE
Source  STRING(1000)
Parms   STRING(1000)
LenP    USHORT 
InBrackets  BOOL    ![] 
B1      USHORT 
    CODE
    CLEAR(ProtoGrp)
    FREE(ParmsQ)
    FREE(ReturnQ)
    O=0
    LOOP X=1 TO SIZE(ProtoCode)  !Remove 9,13,10 | line continuation
        CASE VAL(ProtoCode[X])
        OF 0  TO 31 
        OROF VAL('|') 
             ProtoCode[X]=''   !Change 9,13,10 | to spaces
!FYI OROF below ALSO executes for 0-31 | changed to spaces above so those if those are extra spaces they are skipped 
        OROF 32
             IF O AND ProtoCode[O]='' THEN CYCLE.    !06/03/22 Last char is a Space and so is this so skip double spaces
        END                                         !06/03/22 Comment the above "IF O.." to test with extra spaces
        O += 1
        IF O = X THEN CYCLE.
        ProtoCode[O] = ProtoCode[X] ; ProtoCode[X]=''
    END
    ProtoCode = LEFT(ProtoCode)                 !No Leading Spaces, likely pasted from MAP using old syntax e.g. BuiltIns.clw 
    Prot:Parms = 'Failed: ProtoParse ?'
    Source = ProtoCode
    P1=INSTRING('(',Source)                     !Find "(" in  "Name PROCEDURE()"
    P2=INSTRING(')',Source,1,P1)                !Find ")" in  "Name PROCEDURE()"
    Cma1=INSTRING(',',Source)                   !Find "," in  "Name PROCEDURE,ReturnValue"
    IF P1=0 AND P2=0 AND Cma1<>0 THEN           !No (Parens), but a Comma assume "Name PROCEDURE,ReturnValue"
       P1=Cma1                                  !Set "(" pos = Comma pos ... also ")" pos
       P2=Cma1                                  
    ELSIF P1=0 AND P2=0 THEN                    !No (Parens) and No Comma assume "Name PROCEDURE" w/o () or ,Return
       P1=LEN(CLIP(Source))+1                   !Set "(" pos = Length + 1 ... also ")" pos
       P2=P1
    ELSIF ~P1 OR ~P2 OR P2<P1+1 THEN            !Bad Paren Positions like )(   05/29/22 allow ()
        Prot:Parms = 'Failed: to find (Parens) ' & |
                      CHOOSE(~P1,', No (','') & CHOOSE(P1 AND ~P2,', No )','') & |
                      CHOOSE(P1 AND P2 AND P2<=P1+1,', ) <= (','')
                                
        RETURN
    END 
    X=INSTRING(' ',SUB(Source,1,P1-1))      !Find space between "LABEL Procedure("  P1=First (
    IF ~X AND P1 > 1 THEN                   !Assume old syntax  "    LABEL(parms)"  That does not start in column 1
        X = P1-1                            !X=End of LABEL .... It's 1 byte before P1=First ( 
    ELSIF ~X THEN 
        Prot:Parms = 'Failed: No Space between NAME and (Prototype)' ; RETURN  
    END
!    Message('P1=' & P1 &'  P2=' & P2 & '  Cma1=' & Cma1 & |
!            '||RV="' & clip(SUB(Source,P2+1,99)) &'"'& |
!            '||Src:|' & CLIP(Source) ,'ProtoParseRtn')
!   Name  Procedure (parms),Return
    Prot:Name  = SUB(Source,1,X)                                    !take LABEL in "LABEL Procedure
    Prot:Parms = CHOOSE(P1+1>=P2 OR P2=0,'', Source[P1+1 : P2-1])   !take between "()" the (parms)
    Prot:RV:Source    = LEFT(SUB(Source,P2+1,999))                  !take after ")" the ",Return", may be "Return" if no ()
    IF Prot:RV:Source[1]=',' THEN Prot:RV:Source = LEFT(SUB(Prot:RV:Source,2,999)) . 
    
    !Prototype:   [CONST] Type [ LABEL ]
    !Prototype: < [CONST] Type [ Label ] >      Omittable / Optional
    !Prototype: Type [ Label ] = Default        Default Optional
    !Prototype: Type[] [ Label ]                Array[] has Parens

    IF Prot:Parms THEN
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
       Src=LEFT(SUB(Src,2,199))
       LenP=LEN(CLIP(Src))
       IF LenP AND Src[LenP]='>' THEN  !---------------------------------------> >
          Src[LenP]=' '
       END
    END                                     !   [CONST] TYPE  Label = Default 
    IF UPPER(SUB(Src,1,6))='CONST ' THEN  !--->  CONST 
       PrmQ:Const1 = 1    ! 123456
       Src=LEFT(SUB(Src,6,199))
    END                                     !           TYPE Label = Default 
    X=INSTRING('=',Src)                   !----------------------> = Default
    IF X THEN 
       PrmQ:Default=LEFT(SUB(Src,X+1,199)) 
       Src=SUB(Src,1,X-1)
    END
    IF Src[1:2]='* ' THEN               !By Address "* TYPE" with Space after Asterisk
       Src='*' & LEFT(SUB(Src,2,199))   !Take out the Space(s)
    END 
                                            !           TYPE Label
                                            !     Array TYPE[] Label 
    X=INSTRING(']',Src)     !Array[]  ? -------------------> ]
    IF ~X THEN X=INSTRING(' ',Src).    !------------------->       Space after TYPE
    PrmQ:Type = SUB(Src,1,X)           !                TYPE
    PrmQ:Label= LEFT(SUB(Src,X+1,199))  !                     Label
    PrmQ:LenLabel = LEN(CLIP(PrmQ:Label))
    IF Prot:BigLabel < PrmQ:LenLabel THEN
       Prot:BigLabel = PrmQ:LenLabel
    END

    IF PrmQ:Type[1]='*' THEN
       PrmQ:Asterisk='*'
       PrmQ:Type=LEFT(SUB(PrmQ:Type,2,199))             !Cutoff * so not measured in Len
    END
    
    PrmQ:LenType = LEN(CLIP(PrmQ:Type))
    IF ClarionDataType(PrmQ:Type,,PrmQ:Asterisk) THEN   !Note Asterisk set ='*' for Entity like QUEUE FILE 
       PrmQ:Type=UPPER(PrmQ:Type)
       PrmQ:TypeIsCLA=True
       IF Prot:BigTypeCLA < PrmQ:LenType THEN
          Prot:BigTypeCLA = PrmQ:LenType
       END
    ELSE   !Not a Cla Type
       ClarionNamedType(PrmQ:Type, CFG:UpperTypes)  !Upper
       PrmQ:TypeIsCLA=False
    END
    IF Prot:BigTypeAll < PrmQ:LenType THEN
       Prot:BigTypeAll = PrmQ:LenType
    END
    IF PrmQ:Asterisk='*' THEN Prot:ByAddressZ=1.
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
            OF   'C'
            OROF 'DLL'
            OROF 'NAME'
            OROF 'PASCAL'
            OROF 'PRIVATE'
            OROF 'PROTECTED'
            OROF 'RAW'
            OROF 'TYPE'      ! Not a Named Type for ELSE below

            OF 'PROC'        ; Prot:RV:PROC='PROC'
            
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
ClarionDataType PROCEDURE(STRING InType,<*STRING OutEntity_PlusBug>,<*PSTRING OutEntity_BugOnly>)!,BYTE returns 1=ClaType 2=Entity *implied 0=NotCla
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
        IF ~OMITTED(OutEntity_PlusBug) THEN 
            OutEntity_PlusBug='*' & InType
        END
        IF ~OMITTED(OutEntity_BugOnly) THEN 
            OutEntity_BugOnly='*'
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
                  '|LinesViewSplit( Class,  Defaults, <<Omit> ) has 13,10 and Continuaton' & |
                  '|SerializeQueue( *Q ) no return' & |
                  '|AddCount( Arrays[,] )' & |
                  '|PatternsQ2String( NamedType),*String' & |
                  '')
        OutProto='ReplaceBetween PROCEDURE(string pLeft, <<string pRight>, string pOldValue, string pNewValue, long pCount=0, long pStart=1, long pEnd=0, long pNoCase=0, long pReplaceAll=false),Long,Proc,virtual'
        OutProto='FindBetween PROCEDURE(const *string pFindIn, string pLeft, string pRight, *long outStart, *long outEnd, bool pNoCase=false, long pExclusive=true),string'
        OutProto='LinesViewSplit PROCEDURE(StringTheory STwithLinesSplitDone, STRING SplitDelim, <<STRING QuoteStart>, <<STRING QuoteEnd>, BYTE RemoveQuotes=TRUE, |<13,10> {25} bool pClip=false, bool pLeft=false, <<STRING pSeparator>, long pNested=false)'
        OutProto='SerializeQueue PROCEDURE(*Queue pQueue,<<string pRecordBoundary>,*StringTheory pFieldBoundary,<<string pQuotestart>,<<string pQuoteEnd>,Long pLevel=1,Long pFree=true),virtual'
        OutProto='AddCount PROCEDURE(*LONG[,] Total,*LONG[,] Current)'
        OutProto='PatternsQ2String  PROCEDURE(PatternQType PatQ, *LONG OutLength),*STRING'
    ELSE
        RETURN FALSE
    END 
    RETURN TRUE 