unit SepiDelphiParser;

interface

{$D-,L-}

uses
  SysUtils, Contnrs, SepiCompilerErrors, SepiParseTrees, SepiParserUtils,
  SepiLL1ParserUtils, SepiDelphiLexer;

const
  ChoiceCount = 412;
  FirstNonTerminal = 112;
  LastNonTerminal = 359;

  ntSource = 112; // Source
  ntInPreProcessorExpression = 113; // InPreProcessorExpression
  ntInterface = 114; // Interface
  ntImplementation = 115; // Implementation
  ntIntfSection = 116; // IntfSection
  ntImplSection = 117; // ImplSection
  ntInitFinit = 118; // InitFinit
  ntIdentifier = 119; // Identifier
  ntUsesSection = 120; // UsesSection
  ntCommaIdentList = 121; // CommaIdentList
  ntCommaIdentDeclList = 122; // CommaIdentDeclList
  ntQualifiedIdent = 123; // QualifiedIdent
  ntIdentifierDecl = 124; // IdentifierDecl
  ntInitializationExpression = 125; // InitializationExpression
  ntArrayInitializationExpression = 126; // ArrayInitializationExpression
  ntArrayInitialization = 127; // ArrayInitialization
  ntRecordInitializationExpression = 128; // RecordInitializationExpression
  ntRecordInitialization = 129; // RecordInitialization
  ntGUIDInitializationExpression = 130; // GUIDInitializationExpression
  ntGUIDInitialization = 131; // GUIDInitialization
  ntOtherInitializationExpression = 132; // OtherInitializationExpression
  ntOtherInitialization = 133; // OtherInitialization
  ntExpression = 134; // Expression
  ntExpressionNoEquals = 135; // ExpressionNoEquals
  ntConstExpression = 136; // ConstExpression
  ntConstExpressionNoEquals = 137; // ConstExpressionNoEquals
  ntConstOrType = 138; // ConstOrType
  ntConstOrTypeNoEquals = 139; // ConstOrTypeNoEquals
  ntSingleExpr = 140; // SingleExpr
  ntUnaryOpExpr = 141; // UnaryOpExpr
  ntParenthesizedExpr = 142; // ParenthesizedExpr
  ntNextExpr = 143; // NextExpr
  ntUnaryOpModifier = 144; // UnaryOpModifier
  ntDereferenceOp = 145; // DereferenceOp
  ntParameters = 146; // Parameters
  ntIdentTestParam = 147; // IdentTestParam
  ntArrayIndices = 148; // ArrayIndices
  ntExprList = 149; // ExprList
  ntFieldSelection = 150; // FieldSelection
  ntSingleValue = 151; // SingleValue
  ntIntegerConst = 152; // IntegerConst
  ntFloatConst = 153; // FloatConst
  ntStringConst = 154; // StringConst
  ntIdentifierSingleValue = 155; // IdentifierSingleValue
  ntInheritedSingleValue = 156; // InheritedSingleValue
  ntNilValue = 157; // NilValue
  ntSetValue = 158; // SetValue
  ntSetRange = 159; // SetRange
  ntBinaryOp = 160; // BinaryOp
  ntBinaryOpNoEquals = 161; // BinaryOpNoEquals
  ntUnaryOp = 162; // UnaryOp
  ntAddressOfOp = 163; // AddressOfOp
  ntConstSection = 164; // ConstSection
  ntConstKeyWord = 165; // ConstKeyWord
  ntConstDecl = 166; // ConstDecl
  ntInnerConstDecl = 167; // InnerConstDecl
  ntVarSection = 168; // VarSection
  ntGlobalVar = 169; // GlobalVar
  ntInnerGlobalVar = 170; // InnerGlobalVar
  ntTypeSection = 171; // TypeSection
  ntTypeDecl = 172; // TypeDecl
  ntTypeDesc = 173; // TypeDesc
  ntTypeName = 174; // TypeName
  ntPackedDesc = 175; // PackedDesc
  ntArrayDesc = 176; // ArrayDesc
  ntPackedArrayDesc = 177; // PackedArrayDesc
  ntArrayDims = 178; // ArrayDims
  ntArrayRange = 179; // ArrayRange
  ntTypeModifiers = 180; // TypeModifiers
  ntCloneDesc = 181; // CloneDesc
  ntRangeOrEnumDesc = 182; // RangeOrEnumDesc
  ntRangeDesc = 183; // RangeDesc
  ntEnumDesc = 184; // EnumDesc
  ntSetDesc = 185; // SetDesc
  ntStringDesc = 186; // StringDesc
  ntPointerDesc = 187; // PointerDesc
  ntRecordDesc = 188; // RecordDesc
  ntPackedRecordDesc = 189; // PackedRecordDesc
  ntRecordContents = 190; // RecordContents
  ntRecordCaseBlock = 191; // RecordCaseBlock
  ntRecordCase = 192; // RecordCase
  ntCaseLabels = 193; // CaseLabels
  ntRecordCaseContents = 194; // RecordCaseContents
  ntNextRecordCaseContents = 195; // NextRecordCaseContents
  ntNextRecordCaseContentsEx = 196; // NextRecordCaseContentsEx
  ntRecordField = 197; // RecordField
  ntRecordCaseField = 198; // RecordCaseField
  ntField = 199; // Field
  ntClassDesc = 200; // ClassDesc
  ntClassExDesc = 201; // ClassExDesc
  ntClassContents = 202; // ClassContents
  ntClassHeritage = 203; // ClassHeritage
  ntClassMemberLists = 204; // ClassMemberLists
  ntVisibility = 205; // Visibility
  ntClassMemberList = 206; // ClassMemberList
  ntInterfaceDesc = 207; // InterfaceDesc
  ntInterfaceHeritage = 208; // InterfaceHeritage
  ntDispInterfaceDesc = 209; // DispInterfaceDesc
  ntDispInterfaceHeritage = 210; // DispInterfaceHeritage
  ntInterfaceGUID = 211; // InterfaceGUID
  ntInterfaceMemberList = 212; // InterfaceMemberList
  ntClassMethodProp = 213; // ClassMethodProp
  ntClassMethodDecl = 214; // ClassMethodDecl
  ntIntfMethodProp = 215; // IntfMethodProp
  ntIntfMethodRedirector = 216; // IntfMethodRedirector
  ntIntfMethodRedirKind = 217; // IntfMethodRedirKind
  ntMethodDecl = 218; // MethodDecl
  ntPropertyDecl = 219; // PropertyDecl
  ntPropertyKind = 220; // PropertyKind
  ntPropertyNextDecl = 221; // PropertyNextDecl
  ntRedefineMarker = 222; // RedefineMarker
  ntPropInfo = 223; // PropInfo
  ntPropReadAccess = 224; // PropReadAccess
  ntPropWriteAccess = 225; // PropWriteAccess
  ntPropIndex = 226; // PropIndex
  ntPropDefaultValue = 227; // PropDefaultValue
  ntPropStorage = 228; // PropStorage
  ntIgnoredPropInfo = 229; // IgnoredPropInfo
  ntPropertyModifier = 230; // PropertyModifier
  ntDefaultMarker = 231; // DefaultMarker
  ntIgnoredPropertyModifier = 232; // IgnoredPropertyModifier
  ntEventDesc = 233; // EventDesc
  ntEventModifiers = 234; // EventModifiers
  ntEventIsOfObject = 235; // EventIsOfObject
  ntRoutineDecl = 236; // RoutineDecl
  ntRoutineKind = 237; // RoutineKind
  ntMethodKind = 238; // MethodKind
  ntClassMethodKind = 239; // ClassMethodKind
  ntMethodNameDeclaration = 240; // MethodNameDeclaration
  ntRoutineModifier = 241; // RoutineModifier
  ntMethodModifier = 242; // MethodModifier
  ntCallingConvention = 243; // CallingConvention
  ntMethodLinkKind = 244; // MethodLinkKind
  ntAbstractMarker = 245; // AbstractMarker
  ntOverloadMarker = 246; // OverloadMarker
  ntIgnoredRoutineModifier = 247; // IgnoredRoutineModifier
  ntIgnoredMethodModifier = 248; // IgnoredMethodModifier
  ntMethodSignature = 249; // MethodSignature
  ntPropertySignature = 250; // PropertySignature
  ntMethodParamList = 251; // MethodParamList
  ntParamList = 252; // ParamList
  ntReturnType = 253; // ReturnType
  ntPropType = 254; // PropType
  ntParam = 255; // Param
  ntParamKind = 256; // ParamKind
  ntParamNameList = 257; // ParamNameList
  ntParamName = 258; // ParamName
  ntParamTypeAndDefault = 259; // ParamTypeAndDefault
  ntComplexParamType = 260; // ComplexParamType
  ntParamIsArray = 261; // ParamIsArray
  ntParamArrayType = 262; // ParamArrayType
  ntParamType = 263; // ParamType
  ntParamDefault = 264; // ParamDefault
  ntMethodImpl = 265; // MethodImpl
  ntMethodImplHeader = 266; // MethodImplHeader
  ntForwardOrMethodBody = 267; // ForwardOrMethodBody
  ntMethodBody = 268; // MethodBody
  ntInMethodSection = 269; // InMethodSection
  ntForwardMarker = 270; // ForwardMarker
  ntUnitInitialization = 271; // UnitInitialization
  ntUnitFinalization = 272; // UnitFinalization
  ntLocalVarSection = 273; // LocalVarSection
  ntLocalVar = 274; // LocalVar
  ntInstructionList = 275; // InstructionList
  ntInstruction = 276; // Instruction
  ntNoInstruction = 277; // NoInstruction
  ntBeginEndBlock = 278; // BeginEndBlock
  ntIfThenElseInstruction = 279; // IfThenElseInstruction
  ntElseBranch = 280; // ElseBranch
  ntWhileInstruction = 281; // WhileInstruction
  ntRepeatInstruction = 282; // RepeatInstruction
  ntForInstruction = 283; // ForInstruction
  ntForControlVar = 284; // ForControlVar
  ntForToDownTo = 285; // ForToDownTo
  ntForTo = 286; // ForTo
  ntForDownTo = 287; // ForDownTo
  ntTryInstruction = 288; // TryInstruction
  ntNextTryInstruction = 289; // NextTryInstruction
  ntExceptClause = 290; // ExceptClause
  ntNextExceptClause = 291; // NextExceptClause
  ntMultiOn = 292; // MultiOn
  ntOnClause = 293; // OnClause
  ntExceptionVarAndType = 294; // ExceptionVarAndType
  ntMultiOnElseClause = 295; // MultiOnElseClause
  ntFinallyClause = 296; // FinallyClause
  ntRaiseInstruction = 297; // RaiseInstruction
  ntExpressionInstruction = 298; // ExpressionInstruction
  ntNextExpressionInstruction = 299; // NextExpressionInstruction
  ntCallInstruction = 300; // CallInstruction
  ntAssignmentInstruction = 301; // AssignmentInstruction
  ntPriv0 = 302; // Priv0
  ntPriv1 = 303; // Priv1
  ntPriv2 = 304; // Priv2
  ntPriv3 = 305; // Priv3
  ntPriv4 = 306; // Priv4
  ntPriv5 = 307; // Priv5
  ntPriv6 = 308; // Priv6
  ntPriv7 = 309; // Priv7
  ntPriv8 = 310; // Priv8
  ntPriv9 = 311; // Priv9
  ntPriv10 = 312; // Priv10
  ntPriv11 = 313; // Priv11
  ntPriv12 = 314; // Priv12
  ntPriv13 = 315; // Priv13
  ntPriv14 = 316; // Priv14
  ntPriv15 = 317; // Priv15
  ntPriv16 = 318; // Priv16
  ntPriv17 = 319; // Priv17
  ntPriv18 = 320; // Priv18
  ntPriv19 = 321; // Priv19
  ntPriv20 = 322; // Priv20
  ntPriv21 = 323; // Priv21
  ntPriv22 = 324; // Priv22
  ntPriv23 = 325; // Priv23
  ntPriv24 = 326; // Priv24
  ntPriv25 = 327; // Priv25
  ntPriv26 = 328; // Priv26
  ntPriv27 = 329; // Priv27
  ntPriv28 = 330; // Priv28
  ntPriv29 = 331; // Priv29
  ntPriv30 = 332; // Priv30
  ntPriv31 = 333; // Priv31
  ntPriv32 = 334; // Priv32
  ntPriv33 = 335; // Priv33
  ntPriv34 = 336; // Priv34
  ntPriv35 = 337; // Priv35
  ntPriv36 = 338; // Priv36
  ntPriv37 = 339; // Priv37
  ntPriv38 = 340; // Priv38
  ntPriv39 = 341; // Priv39
  ntPriv40 = 342; // Priv40
  ntPriv41 = 343; // Priv41
  ntPriv42 = 344; // Priv42
  ntPriv43 = 345; // Priv43
  ntPriv44 = 346; // Priv44
  ntPriv45 = 347; // Priv45
  ntPriv46 = 348; // Priv46
  ntPriv47 = 349; // Priv47
  ntPriv48 = 350; // Priv48
  ntPriv49 = 351; // Priv49
  ntPriv50 = 352; // Priv50
  ntPriv51 = 353; // Priv51
  ntPriv52 = 354; // Priv52
  ntPriv53 = 355; // Priv53
  ntPriv54 = 356; // Priv54
  ntPriv55 = 357; // Priv55
  ntPriv56 = 358; // Priv56
  ntPriv57 = 359; // Priv57

type
  {*
    Analyseur syntaxique
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiParser = class(TSepiCustomLL1Parser)
  private
    procedure PushChoice1;
    procedure PushChoice2;
    procedure PushChoice3;
    procedure PushChoice4;
    procedure PushChoice5;
    procedure PushChoice6;
    procedure PushChoice7;
    procedure PushChoice8;
    procedure PushChoice9;
    procedure PushChoice10;
    procedure PushChoice11;
    procedure PushChoice12;
    procedure PushChoice13;
    procedure PushChoice14;
    procedure PushChoice15;
    procedure PushChoice16;
    procedure PushChoice17;
    procedure PushChoice18;
    procedure PushChoice19;
    procedure PushChoice20;
    procedure PushChoice21;
    procedure PushChoice22;
    procedure PushChoice23;
    procedure PushChoice24;
    procedure PushChoice25;
    procedure PushChoice26;
    procedure PushChoice27;
    procedure PushChoice28;
    procedure PushChoice29;
    procedure PushChoice30;
    procedure PushChoice31;
    procedure PushChoice32;
    procedure PushChoice33;
    procedure PushChoice34;
    procedure PushChoice35;
    procedure PushChoice36;
    procedure PushChoice37;
    procedure PushChoice38;
    procedure PushChoice39;
    procedure PushChoice40;
    procedure PushChoice41;
    procedure PushChoice42;
    procedure PushChoice43;
    procedure PushChoice44;
    procedure PushChoice45;
    procedure PushChoice46;
    procedure PushChoice47;
    procedure PushChoice48;
    procedure PushChoice49;
    procedure PushChoice50;
    procedure PushChoice51;
    procedure PushChoice52;
    procedure PushChoice53;
    procedure PushChoice54;
    procedure PushChoice55;
    procedure PushChoice56;
    procedure PushChoice57;
    procedure PushChoice58;
    procedure PushChoice59;
    procedure PushChoice60;
    procedure PushChoice61;
    procedure PushChoice62;
    procedure PushChoice63;
    procedure PushChoice64;
    procedure PushChoice65;
    procedure PushChoice66;
    procedure PushChoice67;
    procedure PushChoice68;
    procedure PushChoice69;
    procedure PushChoice70;
    procedure PushChoice71;
    procedure PushChoice72;
    procedure PushChoice73;
    procedure PushChoice74;
    procedure PushChoice75;
    procedure PushChoice76;
    procedure PushChoice77;
    procedure PushChoice78;
    procedure PushChoice79;
    procedure PushChoice80;
    procedure PushChoice81;
    procedure PushChoice82;
    procedure PushChoice83;
    procedure PushChoice84;
    procedure PushChoice85;
    procedure PushChoice86;
    procedure PushChoice87;
    procedure PushChoice88;
    procedure PushChoice89;
    procedure PushChoice90;
    procedure PushChoice91;
    procedure PushChoice92;
    procedure PushChoice93;
    procedure PushChoice94;
    procedure PushChoice95;
    procedure PushChoice96;
    procedure PushChoice97;
    procedure PushChoice98;
    procedure PushChoice99;
    procedure PushChoice100;
    procedure PushChoice101;
    procedure PushChoice102;
    procedure PushChoice103;
    procedure PushChoice104;
    procedure PushChoice105;
    procedure PushChoice106;
    procedure PushChoice107;
    procedure PushChoice108;
    procedure PushChoice109;
    procedure PushChoice110;
    procedure PushChoice111;
    procedure PushChoice112;
    procedure PushChoice113;
    procedure PushChoice114;
    procedure PushChoice115;
    procedure PushChoice116;
    procedure PushChoice117;
    procedure PushChoice118;
    procedure PushChoice119;
    procedure PushChoice120;
    procedure PushChoice121;
    procedure PushChoice122;
    procedure PushChoice123;
    procedure PushChoice124;
    procedure PushChoice125;
    procedure PushChoice126;
    procedure PushChoice127;
    procedure PushChoice128;
    procedure PushChoice129;
    procedure PushChoice130;
    procedure PushChoice131;
    procedure PushChoice132;
    procedure PushChoice133;
    procedure PushChoice134;
    procedure PushChoice135;
    procedure PushChoice136;
    procedure PushChoice137;
    procedure PushChoice138;
    procedure PushChoice139;
    procedure PushChoice140;
    procedure PushChoice141;
    procedure PushChoice142;
    procedure PushChoice143;
    procedure PushChoice144;
    procedure PushChoice145;
    procedure PushChoice146;
    procedure PushChoice147;
    procedure PushChoice148;
    procedure PushChoice149;
    procedure PushChoice150;
    procedure PushChoice151;
    procedure PushChoice152;
    procedure PushChoice153;
    procedure PushChoice154;
    procedure PushChoice155;
    procedure PushChoice156;
    procedure PushChoice157;
    procedure PushChoice158;
    procedure PushChoice159;
    procedure PushChoice160;
    procedure PushChoice161;
    procedure PushChoice162;
    procedure PushChoice163;
    procedure PushChoice164;
    procedure PushChoice165;
    procedure PushChoice166;
    procedure PushChoice167;
    procedure PushChoice168;
    procedure PushChoice169;
    procedure PushChoice170;
    procedure PushChoice171;
    procedure PushChoice172;
    procedure PushChoice173;
    procedure PushChoice174;
    procedure PushChoice175;
    procedure PushChoice176;
    procedure PushChoice177;
    procedure PushChoice178;
    procedure PushChoice179;
    procedure PushChoice180;
    procedure PushChoice181;
    procedure PushChoice182;
    procedure PushChoice183;
    procedure PushChoice184;
    procedure PushChoice185;
    procedure PushChoice186;
    procedure PushChoice187;
    procedure PushChoice188;
    procedure PushChoice189;
    procedure PushChoice190;
    procedure PushChoice191;
    procedure PushChoice192;
    procedure PushChoice193;
    procedure PushChoice194;
    procedure PushChoice195;
    procedure PushChoice196;
    procedure PushChoice197;
    procedure PushChoice198;
    procedure PushChoice199;
    procedure PushChoice200;
    procedure PushChoice201;
    procedure PushChoice202;
    procedure PushChoice203;
    procedure PushChoice204;
    procedure PushChoice205;
    procedure PushChoice206;
    procedure PushChoice207;
    procedure PushChoice208;
    procedure PushChoice209;
    procedure PushChoice210;
    procedure PushChoice211;
    procedure PushChoice212;
    procedure PushChoice213;
    procedure PushChoice214;
    procedure PushChoice215;
    procedure PushChoice216;
    procedure PushChoice217;
    procedure PushChoice218;
    procedure PushChoice219;
    procedure PushChoice220;
    procedure PushChoice221;
    procedure PushChoice222;
    procedure PushChoice223;
    procedure PushChoice224;
    procedure PushChoice225;
    procedure PushChoice226;
    procedure PushChoice227;
    procedure PushChoice228;
    procedure PushChoice229;
    procedure PushChoice230;
    procedure PushChoice231;
    procedure PushChoice232;
    procedure PushChoice233;
    procedure PushChoice234;
    procedure PushChoice235;
    procedure PushChoice236;
    procedure PushChoice237;
    procedure PushChoice238;
    procedure PushChoice239;
    procedure PushChoice240;
    procedure PushChoice241;
    procedure PushChoice242;
    procedure PushChoice243;
    procedure PushChoice244;
    procedure PushChoice245;
    procedure PushChoice246;
    procedure PushChoice247;
    procedure PushChoice248;
    procedure PushChoice249;
    procedure PushChoice250;
    procedure PushChoice251;
    procedure PushChoice252;
    procedure PushChoice253;
    procedure PushChoice254;
    procedure PushChoice255;
    procedure PushChoice256;
    procedure PushChoice257;
    procedure PushChoice258;
    procedure PushChoice259;
    procedure PushChoice260;
    procedure PushChoice261;
    procedure PushChoice262;
    procedure PushChoice263;
    procedure PushChoice264;
    procedure PushChoice265;
    procedure PushChoice266;
    procedure PushChoice267;
    procedure PushChoice268;
    procedure PushChoice269;
    procedure PushChoice270;
    procedure PushChoice271;
    procedure PushChoice272;
    procedure PushChoice273;
    procedure PushChoice274;
    procedure PushChoice275;
    procedure PushChoice276;
    procedure PushChoice277;
    procedure PushChoice278;
    procedure PushChoice279;
    procedure PushChoice280;
    procedure PushChoice281;
    procedure PushChoice282;
    procedure PushChoice283;
    procedure PushChoice284;
    procedure PushChoice285;
    procedure PushChoice286;
    procedure PushChoice287;
    procedure PushChoice288;
    procedure PushChoice289;
    procedure PushChoice290;
    procedure PushChoice291;
    procedure PushChoice292;
    procedure PushChoice293;
    procedure PushChoice294;
    procedure PushChoice295;
    procedure PushChoice296;
    procedure PushChoice297;
    procedure PushChoice298;
    procedure PushChoice299;
    procedure PushChoice300;
    procedure PushChoice301;
    procedure PushChoice302;
    procedure PushChoice303;
    procedure PushChoice304;
    procedure PushChoice305;
    procedure PushChoice306;
    procedure PushChoice307;
    procedure PushChoice308;
    procedure PushChoice309;
    procedure PushChoice310;
    procedure PushChoice311;
    procedure PushChoice312;
    procedure PushChoice313;
    procedure PushChoice314;
    procedure PushChoice315;
    procedure PushChoice316;
    procedure PushChoice317;
    procedure PushChoice318;
    procedure PushChoice319;
    procedure PushChoice320;
    procedure PushChoice321;
    procedure PushChoice322;
    procedure PushChoice323;
    procedure PushChoice324;
    procedure PushChoice325;
    procedure PushChoice326;
    procedure PushChoice327;
    procedure PushChoice328;
    procedure PushChoice329;
    procedure PushChoice330;
    procedure PushChoice331;
    procedure PushChoice332;
    procedure PushChoice333;
    procedure PushChoice334;
    procedure PushChoice335;
    procedure PushChoice336;
    procedure PushChoice337;
    procedure PushChoice338;
    procedure PushChoice339;
    procedure PushChoice340;
    procedure PushChoice341;
    procedure PushChoice342;
    procedure PushChoice343;
    procedure PushChoice344;
    procedure PushChoice345;
    procedure PushChoice346;
    procedure PushChoice347;
    procedure PushChoice348;
    procedure PushChoice349;
    procedure PushChoice350;
    procedure PushChoice351;
    procedure PushChoice352;
    procedure PushChoice353;
    procedure PushChoice354;
    procedure PushChoice355;
    procedure PushChoice356;
    procedure PushChoice357;
    procedure PushChoice358;
    procedure PushChoice359;
    procedure PushChoice360;
    procedure PushChoice361;
    procedure PushChoice362;
    procedure PushChoice363;
    procedure PushChoice364;
    procedure PushChoice365;
    procedure PushChoice366;
    procedure PushChoice367;
    procedure PushChoice368;
    procedure PushChoice369;
    procedure PushChoice370;
    procedure PushChoice371;
    procedure PushChoice372;
    procedure PushChoice373;
    procedure PushChoice374;
    procedure PushChoice375;
    procedure PushChoice376;
    procedure PushChoice377;
    procedure PushChoice378;
    procedure PushChoice379;
    procedure PushChoice380;
    procedure PushChoice381;
    procedure PushChoice382;
    procedure PushChoice383;
    procedure PushChoice384;
    procedure PushChoice385;
    procedure PushChoice386;
    procedure PushChoice387;
    procedure PushChoice388;
    procedure PushChoice389;
    procedure PushChoice390;
    procedure PushChoice391;
    procedure PushChoice392;
    procedure PushChoice393;
    procedure PushChoice394;
    procedure PushChoice395;
    procedure PushChoice396;
    procedure PushChoice397;
    procedure PushChoice398;
    procedure PushChoice399;
    procedure PushChoice400;
    procedure PushChoice401;
    procedure PushChoice402;
    procedure PushChoice403;
    procedure PushChoice404;
    procedure PushChoice405;
    procedure PushChoice406;
    procedure PushChoice407;
    procedure PushChoice408;
    procedure PushChoice409;
    procedure PushChoice410;
    procedure PushChoice411;
  protected
    function IsTerminal(Symbol: TSepiSymbolClass): Boolean; override;
    function IsNonTerminal(
      Symbol: TSepiSymbolClass): Boolean; override;

    procedure InitPushChoiceProcs; override;

    function GetExpectedString(
      ExpectedSymbol: TSepiSymbolClass): string; override;

    function GetParsingTable(NonTerminalClass: TSepiSymbolClass;
      TerminalClass: TSepiSymbolClass): TRuleID; override;

    function GetNonTerminalClass(
      Symbol: TSepiSymbolClass): TSepiNonTerminalClass; override;
  end;

var
  NonTerminalClasses:
    array[FirstNonTerminal..LastNonTerminal] of TSepiNonTerminalClass;

implementation

type
  TParsingTable = array[FirstNonTerminal..LastNonTerminal,
    FirstTerminal..LastTerminal] of TRuleID;

const
  ParsingTable: TParsingTable = (
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   2,   2,   2,   2,   2,  -1,   2,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   2,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   2,  -1,  -1,  -1,  -1,  -1,   2,   2,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   2,  -1,  -1,  -1,  -1,  -1,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   2),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   3,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   4,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   5,   6,   6,   7,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   8,   8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   9,  10,  10,  11,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  12,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  12,  12,  -1,  12,  12,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  13,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  14,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  43,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  44,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  44,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  46,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  47,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  51,  51,  51,  51,  51,  -1,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  -1,  -1,  -1,  -1,  51,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  -1,  -1,  -1,  -1,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51),
    ( -1,  -1,  -1,  -1,  -1,  52,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  53,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  54,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  55,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  56,  56,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  58,  57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  59,  59,  59,  59,  59,  -1,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  59,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,  -1,  -1,  -1,  -1,  -1,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  59,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59),
    ( -1,  60,  60,  60,  60,  60,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  60,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  -1,  -1,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  60),
    ( -1,  61,  61,  61,  61,  61,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  61,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61,  -1,  -1,  -1,  -1,  -1,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  61),
    ( -1,  62,  62,  62,  62,  62,  -1,  62,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  62,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  62,  -1,  -1,  -1,  -1,  -1,  62,  62,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  62,  -1,  -1,  -1,  -1,  -1,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  62,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  62),
    ( -1,  63,  63,  63,  63,  63,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  63,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,  -1,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  63,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  63),
    ( -1,  64,  64,  64,  64,  64,  -1,  64,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  64,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  64,  -1,  -1,  -1,  -1,  -1,  64,  64,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  64,  -1,  -1,  -1,  -1,  -1,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  64),
    ( -1,  65,  65,  65,  65,  65,  -1,  65,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  65,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  65,  -1,  -1,  -1,  -1,  -1,  65,  65,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  65,  -1,  -1,  -1,  -1,  -1,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  65,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  65),
    ( -1,  66,  66,  66,  66,  66,  -1,  66,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  66,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  66,  -1,  -1,  -1,  -1,  -1,  66,  66,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  66,  -1,  -1,  -1,  -1,  -1,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  66,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  66),
    ( -1,  68,  68,  68,  68,  67,  -1,  68,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  69,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  68,  -1,  -1,  -1,  -1,  -1,  69,  69,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  69,  -1,  -1,  -1,  -1,  -1,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  68),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  71,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  70,  70,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  70,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  72,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  74,  -1,  75,  -1,  -1,  -1,  -1,  -1,  76,  -1,  73,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  77,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  78,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  79,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  80,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  81,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  82,  82,  82,  82,  82,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  82,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82,  -1,  -1,  -1,  -1,  -1,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  82,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  82),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  83,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  87,  84,  85,  86,  -1,  -1,  90,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  89,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  88),
    ( -1,  -1,  91,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  92,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  93,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  94,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  94,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  95),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  96,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  97,  97,  97,  97,  97,  -1,  97,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  97,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  97,  -1,  -1,  -1,  -1,  -1,  97,  97,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  97,  -1,  -1,  -1,  -1,  -1,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  97),
    ( -1,  98,  98,  98,  98,  98,  -1,  98,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  98,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  98,  -1,  -1,  -1,  -1,  -1,  98,  98,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  98,  -1,  -1,  -1,  -1,  -1,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  98,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  98),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 110,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,  -1, 111, 112, 113, 114, 115,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,  -1, 127, 128, 129, 130, 131,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 132, 133,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 134,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 135,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 136, 136,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 137, 138,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 139,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 141,  -1, 140,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 142,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 143,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 145, 144,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 146,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 147,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 149, 149, 149, 149, 149,  -1, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 152, 149,  -1,  -1,  -1, 148,  -1,  -1,  -1,  -1, 153, 150,  -1,  -1, 155, 154,  -1, 157, 158, 156,  -1,  -1,  -1,  -1,  -1,  -1, 149, 159, 159,  -1,  -1,  -1, 149, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 149,  -1,  -1,  -1,  -1,  -1, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 151, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 149),
    ( -1, 160,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 161,  -1,  -1,  -1,  -1, 162,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 163,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 164,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 165,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 166, 166, 166, 166, 166,  -1, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1,  -1,  -1,  -1,  -1, 166, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166,  -1,  -1,  -1,  -1,  -1, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 166),
    ( -1, 167,  -1,  -1,  -1,  -1, 167,  -1,  -1,  -1,  -1,  -1, 167,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 167,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 168,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 170, 170, 170, 170, 169,  -1, 170,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 170,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 170,  -1,  -1,  -1,  -1,  -1, 170, 170,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 170,  -1,  -1,  -1,  -1,  -1, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 170),
    ( -1, 171, 171, 171, 171, 171,  -1, 171,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 171,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 171,  -1,  -1,  -1,  -1,  -1, 171, 171,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 171,  -1,  -1,  -1,  -1,  -1, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 171),
    ( -1,  -1,  -1,  -1,  -1, 172,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 173,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 174,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 175,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 176,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 177,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 178,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 178,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 178,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 179,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 180, 180, 180, 180, 180,  -1, 180,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 180,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 180,  -1,  -1,  -1,  -1,  -1, 180, 180,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 180,  -1,  -1,  -1,  -1,  -1, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 180),
    ( -1, 181, 181, 181, 181, 181,  -1, 181,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 181,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 181,  -1,  -1,  -1,  -1,  -1, 181, 181,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 181,  -1,  -1,  -1,  -1,  -1, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 181),
    ( -1, 182,  -1,  -1,  -1,  -1, 182,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 182,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182, 182,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1, 184,  -1,  -1,  -1,  -1,  -1, 183,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 184,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 185,  -1,  -1,  -1,  -1, 186,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 186,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 187,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187, 187,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 188,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188, 188,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 189,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 190,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 191,  -1,  -1,  -1, 191, 191,  -1,  -1, 191,  -1,  -1, 191,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 192,  -1,  -1,  -1, 191,  -1,  -1, 191, 191, 191, 191, 191,  -1, 191,  -1, 191, 191, 191, 191, 191,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 193,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1, 193, 193, 193, 193, 193,  -1, 193,  -1, 193, 193, 193, 193, 193,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1, 194,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 195,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 195, 195, 195, 195, 195,  -1, 195,  -1, 195, 195, 195, 195, 195,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195, 195,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 196, 197, 198, 199,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 200,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 200, 200, 200, 200, 200,  -1, 200,  -1, 200, 200, 200, 200, 200,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 201,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1, 202,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 203,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 204,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 205,  -1,  -1,  -1,  -1,  -1, 205,  -1, 205, 205, 205, 205, 205,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 206,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 206, 206, 207, 206, 206,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 209,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 208, 208,  -1, 209, 209,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 210,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 210, 210, 211, 210, 210,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 212, 212,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 213, 214,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 215,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 215, 215,  -1, 215, 215,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 216,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 217,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 218,  -1,  -1,  -1, 218, 219,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 219, 219, 219, 219, 219, 219, 219, 219, 219,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 222, 220, 221, 223, 223, 224, 225, 225, 225,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 226,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 228,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 229, 230,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 231,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 234, 232, 233,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 235,  -1,  -1,  -1,  -1,  -1,  -1, 236,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 237,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 238,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 239, 239,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1, 242,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 241,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 240, 240, 240, 240, 240, 240,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 243,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 244, 244,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 245, 246,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 247,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 248, 249,  -1, 250, 251,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 252, 253,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 254,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 255, 255, 255, 255, 255, 255,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 257, 257, 256,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 257,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 258, 258, 258, 258, 258, 258,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 262,  -1,  -1,  -1, 262, 262, 259, 260, 260, 260, 260, 261,  -1, 262, 262,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 263, 264, 265, 266, 267, 268,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 269, 270, 271, 272,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 273,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 274,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 275, 276,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 277,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 282,  -1,  -1,  -1, 278, 279,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 281, 280,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 283,  -1,  -1,  -1, 283, 283,  -1,  -1, 283,  -1, 283, 283,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 283,  -1,  -1,  -1, 283,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283, 283,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 284,  -1,  -1,  -1, 284,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 285,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 285,  -1, 285, 285,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285, 285,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 286,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 286,  -1, 286, 286,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286, 286,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1, 287,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 288,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 289,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 289,  -1, 289, 289,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289, 289,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 290,  -1, 291, 292,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 293,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293, 293,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 294,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294, 294,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1, 295,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 297,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 296,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297, 297,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 298,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 299,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 300,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299, 299,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 301,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 302,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 303,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 303, 303,  -1, 303, 303,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 304,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 304, 304,  -1, 304, 304,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 306, 306, 306, 306,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 306,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 305,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 307, 307, 307, 307,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 307,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 308, 309, 309, 310,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 311,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 312,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 313,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 314,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 315,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315, 315,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 316, 316, 316, 316, 316,  -1, 316,  -1,  -1,  -1,  -1, 316,  -1,  -1,  -1, 316,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 316, 316, 316,  -1,  -1,  -1,  -1,  -1, 316, 316,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 316,  -1,  -1,  -1,  -1,  -1, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316,  -1,  -1,  -1, 316, 316,  -1, 316, 316,  -1, 316, 316, 316,  -1,  -1, 316, 316, 316, 316, 316, 316),
    ( -1, 325, 325, 325, 325, 325,  -1, 325,  -1,  -1,  -1,  -1, 317,  -1,  -1,  -1, 325,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 318,  -1, 325,  -1,  -1,  -1,  -1,  -1, 325, 325,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 325,  -1,  -1,  -1,  -1,  -1, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325, 325,  -1,  -1,  -1,  -1, 319,  -1, 317, 320,  -1, 321,  -1, 322,  -1,  -1, 323,  -1,  -1,  -1, 324, 325),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 326,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 327,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 329,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 328,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 330,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 331,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 332,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 333,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333, 333,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 334, 335,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 336,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 337,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 338,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 339,  -1, 340,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 341,  -1,  -1,  -1,  -1),
    ( -1, 342, 342, 342, 342, 342,  -1, 342,  -1,  -1,  -1,  -1, 342,  -1,  -1,  -1, 342,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 342, 342, 342,  -1,  -1,  -1,  -1,  -1, 342, 342,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 342,  -1,  -1,  -1,  -1,  -1, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342, 342,  -1,  -1,  -1,  -1, 342,  -1,  -1, 342,  -1, 342,  -1, 342,  -1,  -1, 342,  -1, 343,  -1, 342, 342),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 344,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 345,  -1,  -1,  -1),
    ( -1, 346,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346, 346,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 347,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 348,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 349,  -1),
    ( -1, 350, 350, 350, 350, 350,  -1, 350,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 350,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 350,  -1,  -1,  -1,  -1,  -1, 350, 350,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 350,  -1,  -1,  -1,  -1,  -1, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 350),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 351,  -1,  -1,  -1,  -1, 352,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 351,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 353,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 354,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 355, 355, 355, 355,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 355, 355,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 356, 356, 356, 356,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 356,  -1,  -1,  -1,  -1,  -1,   0,  -1, 356, 356,  -1, 356, 356,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 357,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 358,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 359,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,   0,   0,  -1,   0,   0,   0,   0,   0, 360,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 361,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1, 362,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,   0,  -1,  -1,  -1,  -1,   0,  -1,   0, 363,   0,   0,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 363, 363, 363, 363, 363, 363, 363, 363, 363, 363, 363,  -1, 363, 363, 363, 363, 363,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 364, 364, 364, 364, 364, 364, 364, 364, 364, 364, 364,  -1, 364, 364, 364, 364, 364,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,   0,  -1,  -1,  -1, 365,   0, 365,   0,   0,   0,   0,   0, 365,   0, 365,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,   0,  -1,  -1,  -1, 366,   0, 366,   0,   0,   0,   0,   0, 366,   0, 366,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1, 367,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 368,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,  -1, 369,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0, 370,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,   0,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370, 370,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0, 371,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,   0,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371, 371,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 372,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 373,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0, 374,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,   0,   0,  -1,   0,   0,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374, 374,   0,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1, 375,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,  -1, 376,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 377,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377, 377,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1, 378,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0, 379,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 380,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380, 380,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 381,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 382, 382, 382, 382, 382,   0, 382,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 382,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, 382,  -1,  -1,  -1,  -1,  -1, 382, 382,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 382,  -1,  -1,  -1,  -1,  -1, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382, 382,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 382),
    ( -1,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1, 383,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 384,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 385,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 386, 386, 386, 386,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 387,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387, 387,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 388,   0,   0,   0,   0,  -1,   0,  -1, 388, 388, 388, 388, 388,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1, 389,   0, 389,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1, 389,  -1,  -1,  -1,  -1,  -1, 389,  -1, 389, 389, 389, 389, 389,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1,  -1,   0, 390,  -1,   0,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1, 390,  -1,  -1,  -1,  -1,  -1, 390,  -1, 390, 390, 390, 390, 390,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 391,  -1,  -1,  -1,  -1,  -1,   0,  -1, 391, 391, 391, 391, 391,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 392, 392, 392, 392, 392, 392,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 392,  -1,  -1,  -1, 392, 392, 392, 392, 392, 392, 392, 392,  -1, 392, 392,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 393, 393, 393, 393, 393, 393, 393, 393, 393,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,  -1,   0,  -1,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 394,  -1,  -1,  -1,  -1,  -1,  -1, 394,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 395, 395, 395, 395, 395, 395,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 395, 395, 395,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 395,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    (  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 396, 396, 396, 396, 396, 396,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 396, 396, 396,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 396,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1, 397,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1, 398,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,   0,  -1,  -1,  -1, 399,   0,  -1,  -1,   0,  -1,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1, 400,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,  -1, 401,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1, 402,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0, 403,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 404, 404, 404, 404, 404, 404,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 404, 404, 404,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 404,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 405, 405, 405, 405, 405, 405,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 405, 405, 405,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 405,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 406, 406, 406, 406,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 407,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 408, 408, 408, 408, 408,  -1, 408,  -1,  -1,  -1,  -1, 408,  -1,  -1,  -1, 408,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 408,   0, 408,  -1,  -1,  -1,  -1,  -1, 408, 408,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 408,  -1,  -1,  -1,  -1,  -1, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408, 408,  -1,  -1,  -1,   0, 408,  -1,   0, 408,  -1, 408,   0, 408,  -1,  -1, 408,   0,   0,   0, 408, 408),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 409,  -1,  -1,  -1),
    ( -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 410,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
    ( -1, 411, 411, 411, 411, 411,  -1, 411,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1, 411,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 411,  -1,  -1,  -1,  -1,  -1, 411, 411,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 411,  -1,  -1,  -1,  -1,  -1, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411, 411,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 411)
  );

{ TSepiDelphiParser class }

procedure TSepiDelphiParser.PushChoice1;
begin
  PushBackToParent;
  PushFakeSymbol(tkEof);
  PushSymbol(ntPriv0);
  PushSymbol(ntInterface);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkUnit);
end;

procedure TSepiDelphiParser.PushChoice2;
begin
  PushBackToParent;
  PushFakeSymbol(tkEof);
  PushSymbol(ntConstExpression);
end;

procedure TSepiDelphiParser.PushChoice3;
begin
  PushBackToParent;
  PushSymbol(ntPriv1);
  PushSymbol(ntUsesSection);
  PushFakeSymbol(tkInterface);
end;

procedure TSepiDelphiParser.PushChoice4;
begin
  PushBackToParent;
  PushSymbol(ntInitFinit);
  PushSymbol(ntPriv2);
  PushFakeSymbol(tkImplementation);
end;

procedure TSepiDelphiParser.PushChoice5;
begin
  PushBackToParent;
  PushSymbol(ntTypeSection);
end;

procedure TSepiDelphiParser.PushChoice6;
begin
  PushBackToParent;
  PushSymbol(ntConstSection);
end;

procedure TSepiDelphiParser.PushChoice7;
begin
  PushBackToParent;
  PushSymbol(ntVarSection);
end;

procedure TSepiDelphiParser.PushChoice8;
begin
  PushBackToParent;
  PushSymbol(ntRoutineDecl);
end;

procedure TSepiDelphiParser.PushChoice9;
begin
  PushBackToParent;
  PushSymbol(ntTypeSection);
end;

procedure TSepiDelphiParser.PushChoice10;
begin
  PushBackToParent;
  PushSymbol(ntConstSection);
end;

procedure TSepiDelphiParser.PushChoice11;
begin
  PushBackToParent;
  PushSymbol(ntVarSection);
end;

procedure TSepiDelphiParser.PushChoice12;
begin
  PushBackToParent;
  PushSymbol(ntMethodImpl);
end;

procedure TSepiDelphiParser.PushChoice13;
begin
  PushBackToParent;
  PushSymbol(ntPriv3);
  PushSymbol(ntUnitInitialization);
end;

procedure TSepiDelphiParser.PushChoice14;
begin
  PushBackToParent;
  PushSymbol(tkIdentifier);
end;

procedure TSepiDelphiParser.PushChoice15;
begin
  PushBackToParent;
  PushSymbol(tkRegister);
end;

procedure TSepiDelphiParser.PushChoice16;
begin
  PushBackToParent;
  PushSymbol(tkCDecl);
end;

procedure TSepiDelphiParser.PushChoice17;
begin
  PushBackToParent;
  PushSymbol(tkPascal);
end;

procedure TSepiDelphiParser.PushChoice18;
begin
  PushBackToParent;
  PushSymbol(tkStdCall);
end;

procedure TSepiDelphiParser.PushChoice19;
begin
  PushBackToParent;
  PushSymbol(tkSafeCall);
end;

procedure TSepiDelphiParser.PushChoice20;
begin
  PushBackToParent;
  PushSymbol(tkAssembler);
end;

procedure TSepiDelphiParser.PushChoice21;
begin
  PushBackToParent;
  PushSymbol(tkName);
end;

procedure TSepiDelphiParser.PushChoice22;
begin
  PushBackToParent;
  PushSymbol(tkIndex);
end;

procedure TSepiDelphiParser.PushChoice23;
begin
  PushBackToParent;
  PushSymbol(tkRead);
end;

procedure TSepiDelphiParser.PushChoice24;
begin
  PushBackToParent;
  PushSymbol(tkWrite);
end;

procedure TSepiDelphiParser.PushChoice25;
begin
  PushBackToParent;
  PushSymbol(tkDefault);
end;

procedure TSepiDelphiParser.PushChoice26;
begin
  PushBackToParent;
  PushSymbol(tkNoDefault);
end;

procedure TSepiDelphiParser.PushChoice27;
begin
  PushBackToParent;
  PushSymbol(tkStored);
end;

procedure TSepiDelphiParser.PushChoice28;
begin
  PushBackToParent;
  PushSymbol(tkDispID);
end;

procedure TSepiDelphiParser.PushChoice29;
begin
  PushBackToParent;
  PushSymbol(tkReadOnly);
end;

procedure TSepiDelphiParser.PushChoice30;
begin
  PushBackToParent;
  PushSymbol(tkWriteOnly);
end;

procedure TSepiDelphiParser.PushChoice31;
begin
  PushBackToParent;
  PushSymbol(tkString);
end;

procedure TSepiDelphiParser.PushChoice32;
begin
  PushBackToParent;
  PushSymbol(tkDeprecated);
end;

procedure TSepiDelphiParser.PushChoice33;
begin
  PushBackToParent;
  PushSymbol(tkPlatform);
end;

procedure TSepiDelphiParser.PushChoice34;
begin
  PushBackToParent;
  PushSymbol(tkOverload);
end;

procedure TSepiDelphiParser.PushChoice35;
begin
  PushBackToParent;
  PushSymbol(tkVirtual);
end;

procedure TSepiDelphiParser.PushChoice36;
begin
  PushBackToParent;
  PushSymbol(tkDynamic);
end;

procedure TSepiDelphiParser.PushChoice37;
begin
  PushBackToParent;
  PushSymbol(tkMessage);
end;

procedure TSepiDelphiParser.PushChoice38;
begin
  PushBackToParent;
  PushSymbol(tkOverride);
end;

procedure TSepiDelphiParser.PushChoice39;
begin
  PushBackToParent;
  PushSymbol(tkAbstract);
end;

procedure TSepiDelphiParser.PushChoice40;
begin
  PushBackToParent;
  PushSymbol(tkStatic);
end;

procedure TSepiDelphiParser.PushChoice41;
begin
  PushBackToParent;
  PushSymbol(tkReintroduce);
end;

procedure TSepiDelphiParser.PushChoice42;
begin
  PushBackToParent;
  PushSymbol(tkInline);
end;

procedure TSepiDelphiParser.PushChoice43;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntCommaIdentList);
  PushFakeSymbol(tkUses);
end;

procedure TSepiDelphiParser.PushChoice44;
begin
  PushBackToParent;
  PushSymbol(ntPriv4);
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice45;
begin
  PushBackToParent;
  PushSymbol(ntPriv5);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiDelphiParser.PushChoice46;
begin
  PushBackToParent;
  PushSymbol(ntPriv6);
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice47;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice48;
begin
  PushBackToParent;
  PushSymbol(ntArrayInitializationExpression);
end;

procedure TSepiDelphiParser.PushChoice49;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitializationExpression);
end;

procedure TSepiDelphiParser.PushChoice50;
begin
  PushBackToParent;
  PushSymbol(ntGUIDInitializationExpression);
end;

procedure TSepiDelphiParser.PushChoice51;
begin
  PushBackToParent;
  PushSymbol(ntOtherInitializationExpression);
end;

procedure TSepiDelphiParser.PushChoice52;
begin
  PushBackToParent;
  PushSymbol(ntArrayInitialization);
end;

procedure TSepiDelphiParser.PushChoice53;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntPriv7);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice54;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitialization);
end;

procedure TSepiDelphiParser.PushChoice55;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntPriv8);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice56;
begin
  PushBackToParent;
  PushSymbol(ntGUIDInitialization);
end;

procedure TSepiDelphiParser.PushChoice57;
begin
  PushBackToParent;
  PushSymbol(ntRecordInitialization);
end;

procedure TSepiDelphiParser.PushChoice58;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiDelphiParser.PushChoice59;
begin
  PushBackToParent;
  PushSymbol(ntOtherInitialization);
end;

procedure TSepiDelphiParser.PushChoice60;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
end;

procedure TSepiDelphiParser.PushChoice61;
begin
  PushBackToParent;
  PushSymbol(ntPriv9);
  PushSymbol(ntSingleExpr);
end;

procedure TSepiDelphiParser.PushChoice62;
begin
  PushBackToParent;
  PushSymbol(ntPriv10);
  PushSymbol(ntSingleExpr);
end;

procedure TSepiDelphiParser.PushChoice63;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

procedure TSepiDelphiParser.PushChoice64;
begin
  PushBackToParent;
  PushSymbol(ntExpressionNoEquals);
end;

procedure TSepiDelphiParser.PushChoice65;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

procedure TSepiDelphiParser.PushChoice66;
begin
  PushBackToParent;
  PushSymbol(ntExpressionNoEquals);
end;

procedure TSepiDelphiParser.PushChoice67;
begin
  PushBackToParent;
  PushSymbol(ntPriv11);
  PushSymbol(ntParenthesizedExpr);
end;

procedure TSepiDelphiParser.PushChoice68;
begin
  PushBackToParent;
  PushSymbol(ntPriv12);
  PushSymbol(ntSingleValue);
end;

procedure TSepiDelphiParser.PushChoice69;
begin
  PushBackToParent;
  PushSymbol(ntUnaryOpExpr);
end;

procedure TSepiDelphiParser.PushChoice70;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushSymbol(ntUnaryOp);
end;

procedure TSepiDelphiParser.PushChoice71;
begin
  PushBackToParent;
  PushSymbol(ntSingleExpr);
  PushSymbol(ntAddressOfOp);
end;

procedure TSepiDelphiParser.PushChoice72;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice73;
begin
  PushBackToParent;
  PushSymbol(ntUnaryOpModifier);
end;

procedure TSepiDelphiParser.PushChoice74;
begin
  PushBackToParent;
  PushSymbol(ntParameters);
end;

procedure TSepiDelphiParser.PushChoice75;
begin
  PushBackToParent;
  PushSymbol(ntArrayIndices);
end;

procedure TSepiDelphiParser.PushChoice76;
begin
  PushBackToParent;
  PushSymbol(ntFieldSelection);
end;

procedure TSepiDelphiParser.PushChoice77;
begin
  PushBackToParent;
  PushSymbol(ntDereferenceOp);
end;

procedure TSepiDelphiParser.PushChoice78;
begin
  PushBackToParent;
  PushSymbol(tkHat);
end;

procedure TSepiDelphiParser.PushChoice79;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntExprList);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice80;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice81;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntExprList);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice82;
begin
  PushBackToParent;
  PushSymbol(ntPriv13);
  PushSymbol(ntExpression);
end;

procedure TSepiDelphiParser.PushChoice83;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkDot);
end;

procedure TSepiDelphiParser.PushChoice84;
begin
  PushBackToParent;
  PushSymbol(ntIntegerConst);
end;

procedure TSepiDelphiParser.PushChoice85;
begin
  PushBackToParent;
  PushSymbol(ntFloatConst);
end;

procedure TSepiDelphiParser.PushChoice86;
begin
  PushBackToParent;
  PushSymbol(ntStringConst);
end;

procedure TSepiDelphiParser.PushChoice87;
begin
  PushBackToParent;
  PushSymbol(ntIdentifierSingleValue);
end;

procedure TSepiDelphiParser.PushChoice88;
begin
  PushBackToParent;
  PushSymbol(ntInheritedSingleValue);
end;

procedure TSepiDelphiParser.PushChoice89;
begin
  PushBackToParent;
  PushSymbol(ntNilValue);
end;

procedure TSepiDelphiParser.PushChoice90;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntSetValue);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice91;
begin
  PushBackToParent;
  PushSymbol(tkInteger);
end;

procedure TSepiDelphiParser.PushChoice92;
begin
  PushBackToParent;
  PushSymbol(tkFloat);
end;

procedure TSepiDelphiParser.PushChoice93;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiDelphiParser.PushChoice94;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice95;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkInherited);
end;

procedure TSepiDelphiParser.PushChoice96;
begin
  PushBackToParent;
  PushSymbol(tkNil);
end;

procedure TSepiDelphiParser.PushChoice97;
begin
  PushBackToParent;
  PushSymbol(ntPriv14);
  PushSymbol(ntSetRange);
end;

procedure TSepiDelphiParser.PushChoice98;
begin
  PushBackToParent;
  PushSymbol(ntPriv15);
  PushSymbol(ntExpression);
end;

procedure TSepiDelphiParser.PushChoice99;
begin
  PushBackToParent;
  PushSymbol(tkPlus);
end;

procedure TSepiDelphiParser.PushChoice100;
begin
  PushBackToParent;
  PushSymbol(tkMinus);
end;

procedure TSepiDelphiParser.PushChoice101;
begin
  PushBackToParent;
  PushSymbol(tkTimes);
end;

procedure TSepiDelphiParser.PushChoice102;
begin
  PushBackToParent;
  PushSymbol(tkDivide);
end;

procedure TSepiDelphiParser.PushChoice103;
begin
  PushBackToParent;
  PushSymbol(tkDiv);
end;

procedure TSepiDelphiParser.PushChoice104;
begin
  PushBackToParent;
  PushSymbol(tkMod);
end;

procedure TSepiDelphiParser.PushChoice105;
begin
  PushBackToParent;
  PushSymbol(tkShl);
end;

procedure TSepiDelphiParser.PushChoice106;
begin
  PushBackToParent;
  PushSymbol(tkShr);
end;

procedure TSepiDelphiParser.PushChoice107;
begin
  PushBackToParent;
  PushSymbol(tkOr);
end;

procedure TSepiDelphiParser.PushChoice108;
begin
  PushBackToParent;
  PushSymbol(tkAnd);
end;

procedure TSepiDelphiParser.PushChoice109;
begin
  PushBackToParent;
  PushSymbol(tkXor);
end;

procedure TSepiDelphiParser.PushChoice110;
begin
  PushBackToParent;
  PushSymbol(tkEquals);
end;

procedure TSepiDelphiParser.PushChoice111;
begin
  PushBackToParent;
  PushSymbol(tkLowerThan);
end;

procedure TSepiDelphiParser.PushChoice112;
begin
  PushBackToParent;
  PushSymbol(tkLowerEq);
end;

procedure TSepiDelphiParser.PushChoice113;
begin
  PushBackToParent;
  PushSymbol(tkGreaterThan);
end;

procedure TSepiDelphiParser.PushChoice114;
begin
  PushBackToParent;
  PushSymbol(tkGreaterEq);
end;

procedure TSepiDelphiParser.PushChoice115;
begin
  PushBackToParent;
  PushSymbol(tkNotEqual);
end;

procedure TSepiDelphiParser.PushChoice116;
begin
  PushBackToParent;
  PushSymbol(tkPlus);
end;

procedure TSepiDelphiParser.PushChoice117;
begin
  PushBackToParent;
  PushSymbol(tkMinus);
end;

procedure TSepiDelphiParser.PushChoice118;
begin
  PushBackToParent;
  PushSymbol(tkTimes);
end;

procedure TSepiDelphiParser.PushChoice119;
begin
  PushBackToParent;
  PushSymbol(tkDivide);
end;

procedure TSepiDelphiParser.PushChoice120;
begin
  PushBackToParent;
  PushSymbol(tkDiv);
end;

procedure TSepiDelphiParser.PushChoice121;
begin
  PushBackToParent;
  PushSymbol(tkMod);
end;

procedure TSepiDelphiParser.PushChoice122;
begin
  PushBackToParent;
  PushSymbol(tkShl);
end;

procedure TSepiDelphiParser.PushChoice123;
begin
  PushBackToParent;
  PushSymbol(tkShr);
end;

procedure TSepiDelphiParser.PushChoice124;
begin
  PushBackToParent;
  PushSymbol(tkOr);
end;

procedure TSepiDelphiParser.PushChoice125;
begin
  PushBackToParent;
  PushSymbol(tkAnd);
end;

procedure TSepiDelphiParser.PushChoice126;
begin
  PushBackToParent;
  PushSymbol(tkXor);
end;

procedure TSepiDelphiParser.PushChoice127;
begin
  PushBackToParent;
  PushSymbol(tkLowerThan);
end;

procedure TSepiDelphiParser.PushChoice128;
begin
  PushBackToParent;
  PushSymbol(tkLowerEq);
end;

procedure TSepiDelphiParser.PushChoice129;
begin
  PushBackToParent;
  PushSymbol(tkGreaterThan);
end;

procedure TSepiDelphiParser.PushChoice130;
begin
  PushBackToParent;
  PushSymbol(tkGreaterEq);
end;

procedure TSepiDelphiParser.PushChoice131;
begin
  PushBackToParent;
  PushSymbol(tkNotEqual);
end;

procedure TSepiDelphiParser.PushChoice132;
begin
  PushBackToParent;
  PushSymbol(tkPlus);
end;

procedure TSepiDelphiParser.PushChoice133;
begin
  PushBackToParent;
  PushSymbol(tkMinus);
end;

procedure TSepiDelphiParser.PushChoice134;
begin
  PushBackToParent;
  PushSymbol(tkNot);
end;

procedure TSepiDelphiParser.PushChoice135;
begin
  PushBackToParent;
  PushSymbol(tkAt);
end;

procedure TSepiDelphiParser.PushChoice136;
begin
  PushBackToParent;
  PushSymbol(ntPriv16);
  PushSymbol(ntConstDecl);
  PushFakeSymbol(ntConstKeyWord);
end;

procedure TSepiDelphiParser.PushChoice137;
begin
  PushBackToParent;
  PushSymbol(tkConst);
end;

procedure TSepiDelphiParser.PushChoice138;
begin
  PushBackToParent;
  PushSymbol(tkResourceString);
end;

procedure TSepiDelphiParser.PushChoice139;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntInnerConstDecl);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiDelphiParser.PushChoice140;
begin
  PushBackToParent;
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkEquals);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice141;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkEquals);
end;

procedure TSepiDelphiParser.PushChoice142;
begin
  PushBackToParent;
  PushSymbol(ntPriv17);
  PushSymbol(ntGlobalVar);
  PushFakeSymbol(tkVar);
end;

procedure TSepiDelphiParser.PushChoice143;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntInnerGlobalVar);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiDelphiParser.PushChoice144;
begin
  PushBackToParent;
  PushSymbol(ntPriv18);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice145;
begin
  PushBackToParent;
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
  PushSymbol(ntPriv19);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice146;
begin
  PushBackToParent;
  PushSymbol(ntPriv20);
  PushSymbol(ntTypeDecl);
  PushFakeSymbol(tkType);
end;

procedure TSepiDelphiParser.PushChoice147;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkEquals);
  PushSymbol(ntIdentifierDecl);
end;

procedure TSepiDelphiParser.PushChoice148;
begin
  PushBackToParent;
  PushSymbol(ntCloneDesc);
end;

procedure TSepiDelphiParser.PushChoice149;
begin
  PushBackToParent;
  PushSymbol(ntRangeOrEnumDesc);
end;

procedure TSepiDelphiParser.PushChoice150;
begin
  PushBackToParent;
  PushSymbol(ntSetDesc);
end;

procedure TSepiDelphiParser.PushChoice151;
begin
  PushBackToParent;
  PushSymbol(ntStringDesc);
end;

procedure TSepiDelphiParser.PushChoice152;
begin
  PushBackToParent;
  PushSymbol(ntPointerDesc);
end;

procedure TSepiDelphiParser.PushChoice153;
begin
  PushBackToParent;
  PushSymbol(ntArrayDesc);
end;

procedure TSepiDelphiParser.PushChoice154;
begin
  PushBackToParent;
  PushSymbol(ntRecordDesc);
end;

procedure TSepiDelphiParser.PushChoice155;
begin
  PushBackToParent;
  PushSymbol(ntPackedDesc);
  PushFakeSymbol(tkPacked);
end;

procedure TSepiDelphiParser.PushChoice156;
begin
  PushBackToParent;
  PushSymbol(ntClassDesc);
end;

procedure TSepiDelphiParser.PushChoice157;
begin
  PushBackToParent;
  PushSymbol(ntInterfaceDesc);
end;

procedure TSepiDelphiParser.PushChoice158;
begin
  PushBackToParent;
  PushSymbol(ntDispInterfaceDesc);
end;

procedure TSepiDelphiParser.PushChoice159;
begin
  PushBackToParent;
  PushSymbol(ntEventDesc);
end;

procedure TSepiDelphiParser.PushChoice160;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
end;

procedure TSepiDelphiParser.PushChoice161;
begin
  PushBackToParent;
  PushSymbol(ntPackedArrayDesc);
end;

procedure TSepiDelphiParser.PushChoice162;
begin
  PushBackToParent;
  PushSymbol(ntPackedRecordDesc);
end;

procedure TSepiDelphiParser.PushChoice163;
begin
  PushBackToParent;
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkOf);
  PushSymbol(ntArrayDims);
  PushFakeSymbol(tkArray);
end;

procedure TSepiDelphiParser.PushChoice164;
begin
  PushBackToParent;
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkOf);
  PushSymbol(ntArrayDims);
  PushFakeSymbol(tkArray);
end;

procedure TSepiDelphiParser.PushChoice165;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntPriv21);
  PushSymbol(ntArrayRange);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice166;
begin
  PushBackToParent;
  PushSymbol(ntPriv22);
  PushSymbol(ntConstOrType);
end;

procedure TSepiDelphiParser.PushChoice167;
begin
  PushBackToParent;
  PushSymbol(ntPriv23);
end;

procedure TSepiDelphiParser.PushChoice168;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
  PushFakeSymbol(tkType);
end;

procedure TSepiDelphiParser.PushChoice169;
begin
  PushTry(170);
  PushBackToParent;
  PushSymbol(ntEnumDesc);
end;

procedure TSepiDelphiParser.PushChoice170;
begin
  PushBackToParent;
  PushSymbol(ntRangeDesc);
end;

procedure TSepiDelphiParser.PushChoice171;
begin
  PushBackToParent;
  PushSymbol(ntPriv24);
  PushSymbol(ntConstOrTypeNoEquals);
end;

procedure TSepiDelphiParser.PushChoice172;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntCommaIdentList);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice173;
begin
  PushBackToParent;
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkOf);
  PushFakeSymbol(tkSet);
end;

procedure TSepiDelphiParser.PushChoice174;
begin
  PushBackToParent;
  PushSymbol(ntPriv25);
  PushSymbol(tkString);
end;

procedure TSepiDelphiParser.PushChoice175;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkHat);
end;

procedure TSepiDelphiParser.PushChoice176;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntRecordContents);
  PushFakeSymbol(tkRecord);
end;

procedure TSepiDelphiParser.PushChoice177;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntRecordContents);
  PushFakeSymbol(tkRecord);
end;

procedure TSepiDelphiParser.PushChoice178;
begin
  PushBackToParent;
  PushSymbol(ntRecordCaseBlock);
  PushSymbol(ntPriv26);
end;

procedure TSepiDelphiParser.PushChoice179;
begin
  PushBackToParent;
  PushSymbol(ntPriv28);
  PushFakeSymbol(tkOf);
  PushSymbol(ntPriv27);
  PushFakeSymbol(ntIdentifier);
  PushFakeSymbol(tkCase);
end;

procedure TSepiDelphiParser.PushChoice180;
begin
  PushBackToParent;
  PushSymbol(ntPriv29);
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntRecordCaseContents);
  PushFakeSymbol(tkOpenBracket);
  PushFakeSymbol(tkColon);
  PushFakeSymbol(ntCaseLabels);
end;

procedure TSepiDelphiParser.PushChoice181;
begin
  PushBackToParent;
  PushSymbol(ntPriv30);
  PushSymbol(ntConstExpression);
end;

procedure TSepiDelphiParser.PushChoice182;
begin
  PushBackToParent;
  PushSymbol(ntNextRecordCaseContentsEx);
end;

procedure TSepiDelphiParser.PushChoice183;
begin
  PushBackToParent;
  PushSymbol(ntNextRecordCaseContentsEx);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiDelphiParser.PushChoice184;
begin
  PushBackToParent;
  PushSymbol(ntRecordCaseBlock);
end;

procedure TSepiDelphiParser.PushChoice185;
begin
  PushBackToParent;
  PushSymbol(ntNextRecordCaseContents);
  PushSymbol(ntRecordCaseField);
end;

procedure TSepiDelphiParser.PushChoice186;
begin
  PushBackToParent;
  PushSymbol(ntRecordCaseBlock);
end;

procedure TSepiDelphiParser.PushChoice187;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiDelphiParser.PushChoice188;
begin
  PushBackToParent;
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiDelphiParser.PushChoice189;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeModifiers);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiDelphiParser.PushChoice190;
begin
  PushBackToParent;
  PushSymbol(ntClassExDesc);
  PushFakeSymbol(tkClass);
end;

procedure TSepiDelphiParser.PushChoice191;
begin
  PushBackToParent;
  PushSymbol(ntClassContents);
  PushSymbol(ntClassHeritage);
end;

procedure TSepiDelphiParser.PushChoice192;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushSymbol(tkOf);
end;

procedure TSepiDelphiParser.PushChoice193;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntClassMemberLists);
end;

procedure TSepiDelphiParser.PushChoice194;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntPriv31);
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice195;
begin
  PushBackToParent;
  PushSymbol(ntPriv32);
  PushSymbol(ntClassMemberList);
end;

procedure TSepiDelphiParser.PushChoice196;
begin
  PushBackToParent;
  PushSymbol(tkPrivate);
end;

procedure TSepiDelphiParser.PushChoice197;
begin
  PushBackToParent;
  PushSymbol(tkProtected);
end;

procedure TSepiDelphiParser.PushChoice198;
begin
  PushBackToParent;
  PushSymbol(tkPublic);
end;

procedure TSepiDelphiParser.PushChoice199;
begin
  PushBackToParent;
  PushSymbol(tkPublished);
end;

procedure TSepiDelphiParser.PushChoice200;
begin
  PushBackToParent;
  PushSymbol(ntPriv34);
  PushSymbol(ntPriv33);
end;

procedure TSepiDelphiParser.PushChoice201;
begin
  PushBackToParent;
  PushSymbol(ntPriv35);
  PushFakeSymbol(tkInterface);
end;

procedure TSepiDelphiParser.PushChoice202;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice203;
begin
  PushBackToParent;
  PushSymbol(ntPriv36);
  PushFakeSymbol(tkDispInterface);
end;

procedure TSepiDelphiParser.PushChoice204;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice205;
begin
  PushBackToParent;
  PushSymbol(ntPriv37);
end;

procedure TSepiDelphiParser.PushChoice206;
begin
  PushBackToParent;
  PushSymbol(ntClassMethodDecl);
end;

procedure TSepiDelphiParser.PushChoice207;
begin
  PushBackToParent;
  PushSymbol(ntPropertyDecl);
end;

procedure TSepiDelphiParser.PushChoice208;
begin
  PushTry(209);
  PushBackToParent;
  PushSymbol(ntIntfMethodRedirector);
end;

procedure TSepiDelphiParser.PushChoice209;
begin
  PushBackToParent;
  PushSymbol(ntMethodDecl);
end;

procedure TSepiDelphiParser.PushChoice210;
begin
  PushBackToParent;
  PushSymbol(ntMethodDecl);
end;

procedure TSepiDelphiParser.PushChoice211;
begin
  PushBackToParent;
  PushSymbol(ntPropertyDecl);
end;

procedure TSepiDelphiParser.PushChoice212;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkEquals);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkDot);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(ntIntfMethodRedirKind);
end;

procedure TSepiDelphiParser.PushChoice213;
begin
  PushBackToParent;
  PushSymbol(tkProcedure);
end;

procedure TSepiDelphiParser.PushChoice214;
begin
  PushBackToParent;
  PushSymbol(tkFunction);
end;

procedure TSepiDelphiParser.PushChoice215;
begin
  PushBackToParent;
  PushSymbol(ntPriv38);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntMethodSignature);
  PushSymbol(ntMethodNameDeclaration);
  PushSymbol(ntMethodKind);
end;

procedure TSepiDelphiParser.PushChoice216;
begin
  PushBackToParent;
  PushSymbol(ntPriv40);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntPriv39);
  PushSymbol(ntPropertyNextDecl);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(ntPropertyKind);
end;

procedure TSepiDelphiParser.PushChoice217;
begin
  PushBackToParent;
  PushSymbol(tkProperty);
end;

procedure TSepiDelphiParser.PushChoice218;
begin
  PushBackToParent;
  PushSymbol(ntPropertySignature);
end;

procedure TSepiDelphiParser.PushChoice219;
begin
  PushBackToParent;
  PushSymbol(ntRedefineMarker);
end;

procedure TSepiDelphiParser.PushChoice220;
begin
  PushBackToParent;
  PushSymbol(ntPropReadAccess);
end;

procedure TSepiDelphiParser.PushChoice221;
begin
  PushBackToParent;
  PushSymbol(ntPropWriteAccess);
end;

procedure TSepiDelphiParser.PushChoice222;
begin
  PushBackToParent;
  PushSymbol(ntPropIndex);
end;

procedure TSepiDelphiParser.PushChoice223;
begin
  PushBackToParent;
  PushSymbol(ntPropDefaultValue);
end;

procedure TSepiDelphiParser.PushChoice224;
begin
  PushBackToParent;
  PushSymbol(ntPropStorage);
end;

procedure TSepiDelphiParser.PushChoice225;
begin
  PushBackToParent;
  PushSymbol(ntIgnoredPropInfo);
end;

procedure TSepiDelphiParser.PushChoice226;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkRead);
end;

procedure TSepiDelphiParser.PushChoice227;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkWrite);
end;

procedure TSepiDelphiParser.PushChoice228;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkIndex);
end;

procedure TSepiDelphiParser.PushChoice229;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkDefault);
end;

procedure TSepiDelphiParser.PushChoice230;
begin
  PushBackToParent;
  PushFakeSymbol(tkNoDefault);
end;

procedure TSepiDelphiParser.PushChoice231;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkStored);
end;

procedure TSepiDelphiParser.PushChoice232;
begin
  PushBackToParent;
  PushSymbol(tkReadOnly);
end;

procedure TSepiDelphiParser.PushChoice233;
begin
  PushBackToParent;
  PushSymbol(tkWriteOnly);
end;

procedure TSepiDelphiParser.PushChoice234;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushSymbol(tkDispID);
end;

procedure TSepiDelphiParser.PushChoice235;
begin
  PushBackToParent;
  PushSymbol(ntDefaultMarker);
end;

procedure TSepiDelphiParser.PushChoice236;
begin
  PushBackToParent;
  PushSymbol(ntIgnoredPropertyModifier);
end;

procedure TSepiDelphiParser.PushChoice237;
begin
  PushBackToParent;
  PushSymbol(tkDefault);
end;

procedure TSepiDelphiParser.PushChoice238;
begin
  PushBackToParent;
  PushSymbol(tkDeprecated);
end;

procedure TSepiDelphiParser.PushChoice239;
begin
  PushBackToParent;
  PushSymbol(ntEventModifiers);
  PushSymbol(ntMethodSignature);
  PushSymbol(ntRoutineKind);
end;

procedure TSepiDelphiParser.PushChoice240;
begin
  PushTry(241);
  PushBackToParent;
  PushSymbol(ntEventModifiers);
  PushSymbol(ntCallingConvention);
end;

procedure TSepiDelphiParser.PushChoice241;
begin
  PushTry(242);
  PushBackToParent;
  PushSymbol(ntEventModifiers);
  PushSymbol(ntEventIsOfObject);
end;

procedure TSepiDelphiParser.PushChoice242;
begin
  PushTry(0);
  PushBackToParent;
  PushSymbol(ntEventModifiers);
  PushSymbol(ntCallingConvention);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiDelphiParser.PushChoice243;
begin
  PushBackToParent;
  PushSymbol(tkObject);
  PushSymbol(tkOf);
end;

procedure TSepiDelphiParser.PushChoice244;
begin
  PushBackToParent;
  PushSymbol(ntPriv42);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntPriv41);
  PushSymbol(ntMethodSignature);
  PushSymbol(ntMethodNameDeclaration);
  PushSymbol(ntRoutineKind);
end;

procedure TSepiDelphiParser.PushChoice245;
begin
  PushBackToParent;
  PushSymbol(tkProcedure);
end;

procedure TSepiDelphiParser.PushChoice246;
begin
  PushBackToParent;
  PushSymbol(tkFunction);
end;

procedure TSepiDelphiParser.PushChoice247;
begin
  PushBackToParent;
  PushSymbol(ntClassMethodKind);
  PushSymbol(tkClass);
end;

procedure TSepiDelphiParser.PushChoice248;
begin
  PushBackToParent;
  PushSymbol(tkProcedure);
end;

procedure TSepiDelphiParser.PushChoice249;
begin
  PushBackToParent;
  PushSymbol(tkFunction);
end;

procedure TSepiDelphiParser.PushChoice250;
begin
  PushBackToParent;
  PushSymbol(tkConstructor);
end;

procedure TSepiDelphiParser.PushChoice251;
begin
  PushBackToParent;
  PushSymbol(tkDestructor);
end;

procedure TSepiDelphiParser.PushChoice252;
begin
  PushBackToParent;
  PushSymbol(tkProcedure);
end;

procedure TSepiDelphiParser.PushChoice253;
begin
  PushBackToParent;
  PushSymbol(tkFunction);
end;

procedure TSepiDelphiParser.PushChoice254;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice255;
begin
  PushBackToParent;
  PushSymbol(ntCallingConvention);
end;

procedure TSepiDelphiParser.PushChoice256;
begin
  PushBackToParent;
  PushSymbol(ntOverloadMarker);
end;

procedure TSepiDelphiParser.PushChoice257;
begin
  PushBackToParent;
  PushSymbol(ntIgnoredRoutineModifier);
end;

procedure TSepiDelphiParser.PushChoice258;
begin
  PushBackToParent;
  PushSymbol(ntCallingConvention);
end;

procedure TSepiDelphiParser.PushChoice259;
begin
  PushBackToParent;
  PushSymbol(ntOverloadMarker);
end;

procedure TSepiDelphiParser.PushChoice260;
begin
  PushBackToParent;
  PushSymbol(ntMethodLinkKind);
end;

procedure TSepiDelphiParser.PushChoice261;
begin
  PushBackToParent;
  PushSymbol(ntAbstractMarker);
end;

procedure TSepiDelphiParser.PushChoice262;
begin
  PushBackToParent;
  PushSymbol(ntIgnoredMethodModifier);
end;

procedure TSepiDelphiParser.PushChoice263;
begin
  PushBackToParent;
  PushSymbol(tkRegister);
end;

procedure TSepiDelphiParser.PushChoice264;
begin
  PushBackToParent;
  PushSymbol(tkCDecl);
end;

procedure TSepiDelphiParser.PushChoice265;
begin
  PushBackToParent;
  PushSymbol(tkPascal);
end;

procedure TSepiDelphiParser.PushChoice266;
begin
  PushBackToParent;
  PushSymbol(tkStdCall);
end;

procedure TSepiDelphiParser.PushChoice267;
begin
  PushBackToParent;
  PushSymbol(tkSafeCall);
end;

procedure TSepiDelphiParser.PushChoice268;
begin
  PushBackToParent;
  PushSymbol(tkAssembler);
end;

procedure TSepiDelphiParser.PushChoice269;
begin
  PushBackToParent;
  PushSymbol(tkVirtual);
end;

procedure TSepiDelphiParser.PushChoice270;
begin
  PushBackToParent;
  PushSymbol(tkDynamic);
end;

procedure TSepiDelphiParser.PushChoice271;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushSymbol(tkMessage);
end;

procedure TSepiDelphiParser.PushChoice272;
begin
  PushBackToParent;
  PushSymbol(tkOverride);
end;

procedure TSepiDelphiParser.PushChoice273;
begin
  PushBackToParent;
  PushSymbol(tkAbstract);
end;

procedure TSepiDelphiParser.PushChoice274;
begin
  PushBackToParent;
  PushSymbol(tkOverload);
end;

procedure TSepiDelphiParser.PushChoice275;
begin
  PushBackToParent;
  PushSymbol(ntPriv43);
  PushSymbol(tkDeprecated);
end;

procedure TSepiDelphiParser.PushChoice276;
begin
  PushBackToParent;
  PushSymbol(tkPlatform);
end;

procedure TSepiDelphiParser.PushChoice277;
begin
  PushBackToParent;
  PushSymbol(tkInline);
end;

procedure TSepiDelphiParser.PushChoice278;
begin
  PushBackToParent;
  PushSymbol(ntPriv44);
  PushSymbol(tkDeprecated);
end;

procedure TSepiDelphiParser.PushChoice279;
begin
  PushBackToParent;
  PushSymbol(tkPlatform);
end;

procedure TSepiDelphiParser.PushChoice280;
begin
  PushBackToParent;
  PushSymbol(tkInline);
end;

procedure TSepiDelphiParser.PushChoice281;
begin
  PushBackToParent;
  PushSymbol(tkReintroduce);
end;

procedure TSepiDelphiParser.PushChoice282;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushSymbol(tkDispID);
end;

procedure TSepiDelphiParser.PushChoice283;
begin
  PushBackToParent;
  PushSymbol(ntReturnType);
  PushSymbol(ntPriv45);
end;

procedure TSepiDelphiParser.PushChoice284;
begin
  PushBackToParent;
  PushSymbol(ntPropType);
  PushSymbol(ntPriv46);
end;

procedure TSepiDelphiParser.PushChoice285;
begin
  PushBackToParent;
  PushSymbol(ntParamList);
end;

procedure TSepiDelphiParser.PushChoice286;
begin
  PushBackToParent;
  PushSymbol(ntPriv47);
  PushSymbol(ntParam);
end;

procedure TSepiDelphiParser.PushChoice287;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice288;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice289;
begin
  PushBackToParent;
  PushSymbol(ntParamTypeAndDefault);
  PushSymbol(ntParamNameList);
  PushSymbol(ntParamKind);
end;

procedure TSepiDelphiParser.PushChoice290;
begin
  PushBackToParent;
  PushSymbol(tkConst);
end;

procedure TSepiDelphiParser.PushChoice291;
begin
  PushBackToParent;
  PushSymbol(tkVar);
end;

procedure TSepiDelphiParser.PushChoice292;
begin
  PushBackToParent;
  PushSymbol(tkOut);
end;

procedure TSepiDelphiParser.PushChoice293;
begin
  PushBackToParent;
  PushSymbol(ntPriv48);
  PushSymbol(ntParamName);
end;

procedure TSepiDelphiParser.PushChoice294;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice295;
begin
  PushBackToParent;
  PushSymbol(ntPriv49);
  PushSymbol(ntComplexParamType);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice296;
begin
  PushBackToParent;
  PushSymbol(ntParamArrayType);
  PushSymbol(ntParamIsArray);
end;

procedure TSepiDelphiParser.PushChoice297;
begin
  PushBackToParent;
  PushSymbol(ntParamType);
end;

procedure TSepiDelphiParser.PushChoice298;
begin
  PushBackToParent;
  PushSymbol(tkOf);
  PushSymbol(tkArray);
end;

procedure TSepiDelphiParser.PushChoice299;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
end;

procedure TSepiDelphiParser.PushChoice300;
begin
  PushBackToParent;
  PushFakeSymbol(tkConst);
end;

procedure TSepiDelphiParser.PushChoice301;
begin
  PushBackToParent;
  PushSymbol(ntTypeName);
end;

procedure TSepiDelphiParser.PushChoice302;
begin
  PushBackToParent;
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkEquals);
end;

procedure TSepiDelphiParser.PushChoice303;
begin
  PushBackToParent;
  PushSymbol(ntForwardOrMethodBody);
  PushSymbol(ntMethodImplHeader);
end;

procedure TSepiDelphiParser.PushChoice304;
begin
  PushBackToParent;
  PushSymbol(ntPriv51);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntPriv50);
  PushSymbol(ntMethodSignature);
  PushSymbol(ntQualifiedIdent);
  PushSymbol(ntMethodKind);
end;

procedure TSepiDelphiParser.PushChoice305;
begin
  PushBackToParent;
  PushSymbol(ntForwardMarker);
end;

procedure TSepiDelphiParser.PushChoice306;
begin
  PushBackToParent;
  PushSymbol(ntMethodBody);
end;

procedure TSepiDelphiParser.PushChoice307;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntBeginEndBlock);
  PushSymbol(ntPriv52);
end;

procedure TSepiDelphiParser.PushChoice308;
begin
  PushBackToParent;
  PushSymbol(ntTypeSection);
end;

procedure TSepiDelphiParser.PushChoice309;
begin
  PushBackToParent;
  PushSymbol(ntConstSection);
end;

procedure TSepiDelphiParser.PushChoice310;
begin
  PushBackToParent;
  PushSymbol(ntLocalVarSection);
end;

procedure TSepiDelphiParser.PushChoice311;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(tkForward);
end;

procedure TSepiDelphiParser.PushChoice312;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkInitialization);
end;

procedure TSepiDelphiParser.PushChoice313;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkFinalization);
end;

procedure TSepiDelphiParser.PushChoice314;
begin
  PushBackToParent;
  PushSymbol(ntPriv53);
  PushSymbol(ntLocalVar);
  PushFakeSymbol(tkVar);
end;

procedure TSepiDelphiParser.PushChoice315;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntTypeDesc);
  PushFakeSymbol(tkColon);
  PushSymbol(ntCommaIdentDeclList);
end;

procedure TSepiDelphiParser.PushChoice316;
begin
  PushBackToParent;
  PushSymbol(ntPriv54);
end;

procedure TSepiDelphiParser.PushChoice317;
begin
  PushBackToParent;
  PushSymbol(ntNoInstruction);
end;

procedure TSepiDelphiParser.PushChoice318;
begin
  PushBackToParent;
  PushSymbol(ntBeginEndBlock);
end;

procedure TSepiDelphiParser.PushChoice319;
begin
  PushBackToParent;
  PushSymbol(ntIfThenElseInstruction);
end;

procedure TSepiDelphiParser.PushChoice320;
begin
  PushBackToParent;
  PushSymbol(ntWhileInstruction);
end;

procedure TSepiDelphiParser.PushChoice321;
begin
  PushBackToParent;
  PushSymbol(ntRepeatInstruction);
end;

procedure TSepiDelphiParser.PushChoice322;
begin
  PushBackToParent;
  PushSymbol(ntForInstruction);
end;

procedure TSepiDelphiParser.PushChoice323;
begin
  PushBackToParent;
  PushSymbol(ntTryInstruction);
end;

procedure TSepiDelphiParser.PushChoice324;
begin
  PushBackToParent;
  PushSymbol(ntRaiseInstruction);
end;

procedure TSepiDelphiParser.PushChoice325;
begin
  PushBackToParent;
  PushSymbol(ntExpressionInstruction);
end;

procedure TSepiDelphiParser.PushChoice326;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkBegin);
end;

procedure TSepiDelphiParser.PushChoice327;
begin
  PushBackToParent;
  PushSymbol(ntElseBranch);
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkThen);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkIf);
end;

procedure TSepiDelphiParser.PushChoice328;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkElse);
end;

procedure TSepiDelphiParser.PushChoice329;
begin
  PushBackToParent;
  PushSymbol(ntNoInstruction);
end;

procedure TSepiDelphiParser.PushChoice330;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkWhile);
end;

procedure TSepiDelphiParser.PushChoice331;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkUntil);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkRepeat);
end;

procedure TSepiDelphiParser.PushChoice332;
begin
  PushBackToParent;
  PushSymbol(ntInstruction);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExpression);
  PushSymbol(ntForToDownTo);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkAssign);
  PushSymbol(ntForControlVar);
  PushFakeSymbol(tkFor);
end;

procedure TSepiDelphiParser.PushChoice333;
begin
  PushBackToParent;
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice334;
begin
  PushBackToParent;
  PushSymbol(ntForTo);
end;

procedure TSepiDelphiParser.PushChoice335;
begin
  PushBackToParent;
  PushSymbol(ntForDownTo);
end;

procedure TSepiDelphiParser.PushChoice336;
begin
  PushBackToParent;
  PushSymbol(tkTo);
end;

procedure TSepiDelphiParser.PushChoice337;
begin
  PushBackToParent;
  PushSymbol(tkDownTo);
end;

procedure TSepiDelphiParser.PushChoice338;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntNextTryInstruction);
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkTry);
end;

procedure TSepiDelphiParser.PushChoice339;
begin
  PushBackToParent;
  PushSymbol(ntExceptClause);
end;

procedure TSepiDelphiParser.PushChoice340;
begin
  PushBackToParent;
  PushSymbol(ntFinallyClause);
end;

procedure TSepiDelphiParser.PushChoice341;
begin
  PushBackToParent;
  PushSymbol(ntNextExceptClause);
  PushFakeSymbol(tkExcept);
end;

procedure TSepiDelphiParser.PushChoice342;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
end;

procedure TSepiDelphiParser.PushChoice343;
begin
  PushBackToParent;
  PushSymbol(ntMultiOn);
end;

procedure TSepiDelphiParser.PushChoice344;
begin
  PushBackToParent;
  PushSymbol(ntMultiOnElseClause);
  PushSymbol(ntPriv55);
  PushSymbol(ntOnClause);
end;

procedure TSepiDelphiParser.PushChoice345;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkDo);
  PushSymbol(ntExceptionVarAndType);
  PushFakeSymbol(tkOn);
end;

procedure TSepiDelphiParser.PushChoice346;
begin
  PushBackToParent;
  PushSymbol(ntPriv56);
  PushSymbol(ntQualifiedIdent);
end;

procedure TSepiDelphiParser.PushChoice347;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkElse);
end;

procedure TSepiDelphiParser.PushChoice348;
begin
  PushBackToParent;
  PushSymbol(ntInstructionList);
  PushFakeSymbol(tkFinally);
end;

procedure TSepiDelphiParser.PushChoice349;
begin
  PushBackToParent;
  PushSymbol(ntPriv57);
  PushFakeSymbol(tkRaise);
end;

procedure TSepiDelphiParser.PushChoice350;
begin
  PushBackToParent;
  PushSymbol(ntNextExpressionInstruction);
  PushSymbol(ntExpression);
end;

procedure TSepiDelphiParser.PushChoice351;
begin
  PushBackToParent;
  PushSymbol(ntCallInstruction);
end;

procedure TSepiDelphiParser.PushChoice352;
begin
  PushBackToParent;
  PushSymbol(ntAssignmentInstruction);
end;

procedure TSepiDelphiParser.PushChoice353;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkAssign);
end;

procedure TSepiDelphiParser.PushChoice354;
begin
  PushBackToParent;
  PushFakeSymbol(tkDot);
  PushFakeSymbol(tkEnd);
  PushSymbol(ntImplementation);
end;

procedure TSepiDelphiParser.PushChoice355;
begin
  PushBackToParent;
  PushSymbol(ntPriv1);
  PushSymbol(ntIntfSection);
end;

procedure TSepiDelphiParser.PushChoice356;
begin
  PushBackToParent;
  PushSymbol(ntPriv2);
  PushSymbol(ntImplSection);
end;

procedure TSepiDelphiParser.PushChoice357;
begin
  PushBackToParent;
  PushSymbol(ntUnitFinalization);
end;

procedure TSepiDelphiParser.PushChoice358;
begin
  PushBackToParent;
  PushSymbol(ntPriv4);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice359;
begin
  PushBackToParent;
  PushSymbol(ntPriv5);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice360;
begin
  PushBackToParent;
  PushSymbol(ntPriv6);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkDot);
end;

procedure TSepiDelphiParser.PushChoice361;
begin
  PushBackToParent;
  PushSymbol(ntPriv7);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice362;
begin
  PushBackToParent;
  PushSymbol(ntPriv8);
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkColon);
  PushSymbol(ntIdentifier);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiDelphiParser.PushChoice363;
begin
  PushBackToParent;
  PushSymbol(ntPriv9);
  PushSymbol(ntSingleExpr);
  PushSymbol(ntBinaryOp);
end;

procedure TSepiDelphiParser.PushChoice364;
begin
  PushBackToParent;
  PushSymbol(ntPriv10);
  PushSymbol(ntSingleExpr);
  PushSymbol(ntBinaryOpNoEquals);
end;

procedure TSepiDelphiParser.PushChoice365;
begin
  PushBackToParent;
  PushSymbol(ntPriv11);
  PushSymbol(ntNextExpr);
end;

procedure TSepiDelphiParser.PushChoice366;
begin
  PushBackToParent;
  PushSymbol(ntPriv12);
  PushSymbol(ntNextExpr);
end;

procedure TSepiDelphiParser.PushChoice367;
begin
  PushBackToParent;
  PushSymbol(ntPriv13);
  PushSymbol(ntExpression);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice368;
begin
  PushBackToParent;
  PushSymbol(ntPriv14);
  PushSymbol(ntSetRange);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice369;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
  PushFakeSymbol(tkRange);
end;

procedure TSepiDelphiParser.PushChoice370;
begin
  PushBackToParent;
  PushSymbol(ntPriv16);
  PushSymbol(ntConstDecl);
end;

procedure TSepiDelphiParser.PushChoice371;
begin
  PushBackToParent;
  PushSymbol(ntPriv17);
  PushSymbol(ntGlobalVar);
end;

procedure TSepiDelphiParser.PushChoice372;
begin
  PushBackToParent;
  PushSymbol(ntInitializationExpression);
  PushFakeSymbol(tkEquals);
end;

procedure TSepiDelphiParser.PushChoice373;
begin
  PushBackToParent;
  PushSymbol(ntPriv19);
  PushSymbol(ntIdentifierDecl);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice374;
begin
  PushBackToParent;
  PushSymbol(ntPriv20);
  PushSymbol(ntTypeDecl);
end;

procedure TSepiDelphiParser.PushChoice375;
begin
  PushBackToParent;
  PushSymbol(ntPriv21);
  PushSymbol(ntArrayRange);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice376;
begin
  PushBackToParent;
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkRange);
end;

procedure TSepiDelphiParser.PushChoice377;
begin
  PushBackToParent;
  PushSymbol(ntPriv23);
  PushSymbol(ntIdentifier);
end;

procedure TSepiDelphiParser.PushChoice378;
begin
  PushBackToParent;
  PushSymbol(ntConstExpressionNoEquals);
  PushFakeSymbol(tkRange);
end;

procedure TSepiDelphiParser.PushChoice379;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntConstExpression);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice380;
begin
  PushBackToParent;
  PushSymbol(ntPriv26);
  PushSymbol(ntRecordField);
end;

procedure TSepiDelphiParser.PushChoice381;
begin
  PushBackToParent;
  PushFakeSymbol(ntIdentifier);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice382;
begin
  PushBackToParent;
  PushSymbol(ntPriv28);
  PushSymbol(ntRecordCase);
end;

procedure TSepiDelphiParser.PushChoice383;
begin
  PushBackToParent;
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiDelphiParser.PushChoice384;
begin
  PushBackToParent;
  PushSymbol(ntPriv30);
  PushSymbol(ntConstExpression);
  PushSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice385;
begin
  PushBackToParent;
  PushSymbol(ntPriv31);
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice386;
begin
  PushBackToParent;
  PushSymbol(ntPriv32);
  PushSymbol(ntClassMemberList);
  PushSymbol(ntVisibility);
end;

procedure TSepiDelphiParser.PushChoice387;
begin
  PushBackToParent;
  PushSymbol(ntPriv33);
  PushSymbol(ntField);
end;

procedure TSepiDelphiParser.PushChoice388;
begin
  PushBackToParent;
  PushSymbol(ntPriv34);
  PushSymbol(ntClassMethodProp);
end;

procedure TSepiDelphiParser.PushChoice389;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntInterfaceMemberList);
  PushSymbol(ntInterfaceGUID);
  PushSymbol(ntInterfaceHeritage);
end;

procedure TSepiDelphiParser.PushChoice390;
begin
  PushBackToParent;
  PushFakeSymbol(tkEnd);
  PushSymbol(ntInterfaceMemberList);
  PushSymbol(ntInterfaceGUID);
  PushSymbol(ntDispInterfaceHeritage);
end;

procedure TSepiDelphiParser.PushChoice391;
begin
  PushBackToParent;
  PushSymbol(ntPriv37);
  PushSymbol(ntIntfMethodProp);
end;

procedure TSepiDelphiParser.PushChoice392;
begin
  PushBackToParent;
  PushSymbol(ntPriv38);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntMethodModifier);
end;

procedure TSepiDelphiParser.PushChoice393;
begin
  PushBackToParent;
  PushSymbol(ntPriv39);
  PushSymbol(ntPropInfo);
end;

procedure TSepiDelphiParser.PushChoice394;
begin
  PushBackToParent;
  PushSymbol(ntPriv40);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntPropertyModifier);
end;

procedure TSepiDelphiParser.PushChoice395;
begin
  PushBackToParent;
  PushSymbol(ntPriv41);
  PushSymbol(ntRoutineModifier);
end;

procedure TSepiDelphiParser.PushChoice396;
begin
  PushBackToParent;
  PushSymbol(ntPriv42);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntRoutineModifier);
end;

procedure TSepiDelphiParser.PushChoice397;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiDelphiParser.PushChoice398;
begin
  PushBackToParent;
  PushSymbol(tkStringCst);
end;

procedure TSepiDelphiParser.PushChoice399;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseBracket);
  PushSymbol(ntMethodParamList);
  PushFakeSymbol(tkOpenBracket);
end;

procedure TSepiDelphiParser.PushChoice400;
begin
  PushBackToParent;
  PushFakeSymbol(tkCloseSqBracket);
  PushSymbol(ntParamList);
  PushFakeSymbol(tkOpenSqBracket);
end;

procedure TSepiDelphiParser.PushChoice401;
begin
  PushBackToParent;
  PushSymbol(ntPriv47);
  PushSymbol(ntParam);
  PushFakeSymbol(tkSemiColon);
end;

procedure TSepiDelphiParser.PushChoice402;
begin
  PushBackToParent;
  PushSymbol(ntPriv48);
  PushSymbol(ntParamName);
  PushFakeSymbol(tkComma);
end;

procedure TSepiDelphiParser.PushChoice403;
begin
  PushBackToParent;
  PushSymbol(ntParamDefault);
end;

procedure TSepiDelphiParser.PushChoice404;
begin
  PushBackToParent;
  PushSymbol(ntPriv50);
  PushSymbol(ntRoutineModifier);
end;

procedure TSepiDelphiParser.PushChoice405;
begin
  PushBackToParent;
  PushSymbol(ntPriv51);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntRoutineModifier);
end;

procedure TSepiDelphiParser.PushChoice406;
begin
  PushBackToParent;
  PushSymbol(ntPriv52);
  PushSymbol(ntInMethodSection);
end;

procedure TSepiDelphiParser.PushChoice407;
begin
  PushBackToParent;
  PushSymbol(ntPriv53);
  PushSymbol(ntLocalVar);
end;

procedure TSepiDelphiParser.PushChoice408;
begin
  PushBackToParent;
  PushSymbol(ntPriv54);
  PushFakeSymbol(tkSemiColon);
  PushSymbol(ntInstruction);
end;

procedure TSepiDelphiParser.PushChoice409;
begin
  PushBackToParent;
  PushSymbol(ntPriv55);
  PushSymbol(ntOnClause);
end;

procedure TSepiDelphiParser.PushChoice410;
begin
  PushBackToParent;
  PushSymbol(ntQualifiedIdent);
  PushFakeSymbol(tkColon);
end;

procedure TSepiDelphiParser.PushChoice411;
begin
  PushBackToParent;
  PushSymbol(ntExpression);
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiParser.IsTerminal(Symbol: TSepiSymbolClass): Boolean;
begin
  Result := (Symbol >= FirstTerminal) and (Symbol <= LastTerminal);
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiParser.IsNonTerminal(Symbol: TSepiSymbolClass): Boolean;
begin
  Result := (Symbol >= FirstNonTerminal) and (Symbol <= LastNonTerminal);
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiParser.InitPushChoiceProcs;
begin
  SetLength(PushChoiceProcs, ChoiceCount);

  inherited;

  PushChoiceProcs[1] := PushChoice1;
  PushChoiceProcs[2] := PushChoice2;
  PushChoiceProcs[3] := PushChoice3;
  PushChoiceProcs[4] := PushChoice4;
  PushChoiceProcs[5] := PushChoice5;
  PushChoiceProcs[6] := PushChoice6;
  PushChoiceProcs[7] := PushChoice7;
  PushChoiceProcs[8] := PushChoice8;
  PushChoiceProcs[9] := PushChoice9;
  PushChoiceProcs[10] := PushChoice10;
  PushChoiceProcs[11] := PushChoice11;
  PushChoiceProcs[12] := PushChoice12;
  PushChoiceProcs[13] := PushChoice13;
  PushChoiceProcs[14] := PushChoice14;
  PushChoiceProcs[15] := PushChoice15;
  PushChoiceProcs[16] := PushChoice16;
  PushChoiceProcs[17] := PushChoice17;
  PushChoiceProcs[18] := PushChoice18;
  PushChoiceProcs[19] := PushChoice19;
  PushChoiceProcs[20] := PushChoice20;
  PushChoiceProcs[21] := PushChoice21;
  PushChoiceProcs[22] := PushChoice22;
  PushChoiceProcs[23] := PushChoice23;
  PushChoiceProcs[24] := PushChoice24;
  PushChoiceProcs[25] := PushChoice25;
  PushChoiceProcs[26] := PushChoice26;
  PushChoiceProcs[27] := PushChoice27;
  PushChoiceProcs[28] := PushChoice28;
  PushChoiceProcs[29] := PushChoice29;
  PushChoiceProcs[30] := PushChoice30;
  PushChoiceProcs[31] := PushChoice31;
  PushChoiceProcs[32] := PushChoice32;
  PushChoiceProcs[33] := PushChoice33;
  PushChoiceProcs[34] := PushChoice34;
  PushChoiceProcs[35] := PushChoice35;
  PushChoiceProcs[36] := PushChoice36;
  PushChoiceProcs[37] := PushChoice37;
  PushChoiceProcs[38] := PushChoice38;
  PushChoiceProcs[39] := PushChoice39;
  PushChoiceProcs[40] := PushChoice40;
  PushChoiceProcs[41] := PushChoice41;
  PushChoiceProcs[42] := PushChoice42;
  PushChoiceProcs[43] := PushChoice43;
  PushChoiceProcs[44] := PushChoice44;
  PushChoiceProcs[45] := PushChoice45;
  PushChoiceProcs[46] := PushChoice46;
  PushChoiceProcs[47] := PushChoice47;
  PushChoiceProcs[48] := PushChoice48;
  PushChoiceProcs[49] := PushChoice49;
  PushChoiceProcs[50] := PushChoice50;
  PushChoiceProcs[51] := PushChoice51;
  PushChoiceProcs[52] := PushChoice52;
  PushChoiceProcs[53] := PushChoice53;
  PushChoiceProcs[54] := PushChoice54;
  PushChoiceProcs[55] := PushChoice55;
  PushChoiceProcs[56] := PushChoice56;
  PushChoiceProcs[57] := PushChoice57;
  PushChoiceProcs[58] := PushChoice58;
  PushChoiceProcs[59] := PushChoice59;
  PushChoiceProcs[60] := PushChoice60;
  PushChoiceProcs[61] := PushChoice61;
  PushChoiceProcs[62] := PushChoice62;
  PushChoiceProcs[63] := PushChoice63;
  PushChoiceProcs[64] := PushChoice64;
  PushChoiceProcs[65] := PushChoice65;
  PushChoiceProcs[66] := PushChoice66;
  PushChoiceProcs[67] := PushChoice67;
  PushChoiceProcs[68] := PushChoice68;
  PushChoiceProcs[69] := PushChoice69;
  PushChoiceProcs[70] := PushChoice70;
  PushChoiceProcs[71] := PushChoice71;
  PushChoiceProcs[72] := PushChoice72;
  PushChoiceProcs[73] := PushChoice73;
  PushChoiceProcs[74] := PushChoice74;
  PushChoiceProcs[75] := PushChoice75;
  PushChoiceProcs[76] := PushChoice76;
  PushChoiceProcs[77] := PushChoice77;
  PushChoiceProcs[78] := PushChoice78;
  PushChoiceProcs[79] := PushChoice79;
  PushChoiceProcs[80] := PushChoice80;
  PushChoiceProcs[81] := PushChoice81;
  PushChoiceProcs[82] := PushChoice82;
  PushChoiceProcs[83] := PushChoice83;
  PushChoiceProcs[84] := PushChoice84;
  PushChoiceProcs[85] := PushChoice85;
  PushChoiceProcs[86] := PushChoice86;
  PushChoiceProcs[87] := PushChoice87;
  PushChoiceProcs[88] := PushChoice88;
  PushChoiceProcs[89] := PushChoice89;
  PushChoiceProcs[90] := PushChoice90;
  PushChoiceProcs[91] := PushChoice91;
  PushChoiceProcs[92] := PushChoice92;
  PushChoiceProcs[93] := PushChoice93;
  PushChoiceProcs[94] := PushChoice94;
  PushChoiceProcs[95] := PushChoice95;
  PushChoiceProcs[96] := PushChoice96;
  PushChoiceProcs[97] := PushChoice97;
  PushChoiceProcs[98] := PushChoice98;
  PushChoiceProcs[99] := PushChoice99;
  PushChoiceProcs[100] := PushChoice100;
  PushChoiceProcs[101] := PushChoice101;
  PushChoiceProcs[102] := PushChoice102;
  PushChoiceProcs[103] := PushChoice103;
  PushChoiceProcs[104] := PushChoice104;
  PushChoiceProcs[105] := PushChoice105;
  PushChoiceProcs[106] := PushChoice106;
  PushChoiceProcs[107] := PushChoice107;
  PushChoiceProcs[108] := PushChoice108;
  PushChoiceProcs[109] := PushChoice109;
  PushChoiceProcs[110] := PushChoice110;
  PushChoiceProcs[111] := PushChoice111;
  PushChoiceProcs[112] := PushChoice112;
  PushChoiceProcs[113] := PushChoice113;
  PushChoiceProcs[114] := PushChoice114;
  PushChoiceProcs[115] := PushChoice115;
  PushChoiceProcs[116] := PushChoice116;
  PushChoiceProcs[117] := PushChoice117;
  PushChoiceProcs[118] := PushChoice118;
  PushChoiceProcs[119] := PushChoice119;
  PushChoiceProcs[120] := PushChoice120;
  PushChoiceProcs[121] := PushChoice121;
  PushChoiceProcs[122] := PushChoice122;
  PushChoiceProcs[123] := PushChoice123;
  PushChoiceProcs[124] := PushChoice124;
  PushChoiceProcs[125] := PushChoice125;
  PushChoiceProcs[126] := PushChoice126;
  PushChoiceProcs[127] := PushChoice127;
  PushChoiceProcs[128] := PushChoice128;
  PushChoiceProcs[129] := PushChoice129;
  PushChoiceProcs[130] := PushChoice130;
  PushChoiceProcs[131] := PushChoice131;
  PushChoiceProcs[132] := PushChoice132;
  PushChoiceProcs[133] := PushChoice133;
  PushChoiceProcs[134] := PushChoice134;
  PushChoiceProcs[135] := PushChoice135;
  PushChoiceProcs[136] := PushChoice136;
  PushChoiceProcs[137] := PushChoice137;
  PushChoiceProcs[138] := PushChoice138;
  PushChoiceProcs[139] := PushChoice139;
  PushChoiceProcs[140] := PushChoice140;
  PushChoiceProcs[141] := PushChoice141;
  PushChoiceProcs[142] := PushChoice142;
  PushChoiceProcs[143] := PushChoice143;
  PushChoiceProcs[144] := PushChoice144;
  PushChoiceProcs[145] := PushChoice145;
  PushChoiceProcs[146] := PushChoice146;
  PushChoiceProcs[147] := PushChoice147;
  PushChoiceProcs[148] := PushChoice148;
  PushChoiceProcs[149] := PushChoice149;
  PushChoiceProcs[150] := PushChoice150;
  PushChoiceProcs[151] := PushChoice151;
  PushChoiceProcs[152] := PushChoice152;
  PushChoiceProcs[153] := PushChoice153;
  PushChoiceProcs[154] := PushChoice154;
  PushChoiceProcs[155] := PushChoice155;
  PushChoiceProcs[156] := PushChoice156;
  PushChoiceProcs[157] := PushChoice157;
  PushChoiceProcs[158] := PushChoice158;
  PushChoiceProcs[159] := PushChoice159;
  PushChoiceProcs[160] := PushChoice160;
  PushChoiceProcs[161] := PushChoice161;
  PushChoiceProcs[162] := PushChoice162;
  PushChoiceProcs[163] := PushChoice163;
  PushChoiceProcs[164] := PushChoice164;
  PushChoiceProcs[165] := PushChoice165;
  PushChoiceProcs[166] := PushChoice166;
  PushChoiceProcs[167] := PushChoice167;
  PushChoiceProcs[168] := PushChoice168;
  PushChoiceProcs[169] := PushChoice169;
  PushChoiceProcs[170] := PushChoice170;
  PushChoiceProcs[171] := PushChoice171;
  PushChoiceProcs[172] := PushChoice172;
  PushChoiceProcs[173] := PushChoice173;
  PushChoiceProcs[174] := PushChoice174;
  PushChoiceProcs[175] := PushChoice175;
  PushChoiceProcs[176] := PushChoice176;
  PushChoiceProcs[177] := PushChoice177;
  PushChoiceProcs[178] := PushChoice178;
  PushChoiceProcs[179] := PushChoice179;
  PushChoiceProcs[180] := PushChoice180;
  PushChoiceProcs[181] := PushChoice181;
  PushChoiceProcs[182] := PushChoice182;
  PushChoiceProcs[183] := PushChoice183;
  PushChoiceProcs[184] := PushChoice184;
  PushChoiceProcs[185] := PushChoice185;
  PushChoiceProcs[186] := PushChoice186;
  PushChoiceProcs[187] := PushChoice187;
  PushChoiceProcs[188] := PushChoice188;
  PushChoiceProcs[189] := PushChoice189;
  PushChoiceProcs[190] := PushChoice190;
  PushChoiceProcs[191] := PushChoice191;
  PushChoiceProcs[192] := PushChoice192;
  PushChoiceProcs[193] := PushChoice193;
  PushChoiceProcs[194] := PushChoice194;
  PushChoiceProcs[195] := PushChoice195;
  PushChoiceProcs[196] := PushChoice196;
  PushChoiceProcs[197] := PushChoice197;
  PushChoiceProcs[198] := PushChoice198;
  PushChoiceProcs[199] := PushChoice199;
  PushChoiceProcs[200] := PushChoice200;
  PushChoiceProcs[201] := PushChoice201;
  PushChoiceProcs[202] := PushChoice202;
  PushChoiceProcs[203] := PushChoice203;
  PushChoiceProcs[204] := PushChoice204;
  PushChoiceProcs[205] := PushChoice205;
  PushChoiceProcs[206] := PushChoice206;
  PushChoiceProcs[207] := PushChoice207;
  PushChoiceProcs[208] := PushChoice208;
  PushChoiceProcs[209] := PushChoice209;
  PushChoiceProcs[210] := PushChoice210;
  PushChoiceProcs[211] := PushChoice211;
  PushChoiceProcs[212] := PushChoice212;
  PushChoiceProcs[213] := PushChoice213;
  PushChoiceProcs[214] := PushChoice214;
  PushChoiceProcs[215] := PushChoice215;
  PushChoiceProcs[216] := PushChoice216;
  PushChoiceProcs[217] := PushChoice217;
  PushChoiceProcs[218] := PushChoice218;
  PushChoiceProcs[219] := PushChoice219;
  PushChoiceProcs[220] := PushChoice220;
  PushChoiceProcs[221] := PushChoice221;
  PushChoiceProcs[222] := PushChoice222;
  PushChoiceProcs[223] := PushChoice223;
  PushChoiceProcs[224] := PushChoice224;
  PushChoiceProcs[225] := PushChoice225;
  PushChoiceProcs[226] := PushChoice226;
  PushChoiceProcs[227] := PushChoice227;
  PushChoiceProcs[228] := PushChoice228;
  PushChoiceProcs[229] := PushChoice229;
  PushChoiceProcs[230] := PushChoice230;
  PushChoiceProcs[231] := PushChoice231;
  PushChoiceProcs[232] := PushChoice232;
  PushChoiceProcs[233] := PushChoice233;
  PushChoiceProcs[234] := PushChoice234;
  PushChoiceProcs[235] := PushChoice235;
  PushChoiceProcs[236] := PushChoice236;
  PushChoiceProcs[237] := PushChoice237;
  PushChoiceProcs[238] := PushChoice238;
  PushChoiceProcs[239] := PushChoice239;
  PushChoiceProcs[240] := PushChoice240;
  PushChoiceProcs[241] := PushChoice241;
  PushChoiceProcs[242] := PushChoice242;
  PushChoiceProcs[243] := PushChoice243;
  PushChoiceProcs[244] := PushChoice244;
  PushChoiceProcs[245] := PushChoice245;
  PushChoiceProcs[246] := PushChoice246;
  PushChoiceProcs[247] := PushChoice247;
  PushChoiceProcs[248] := PushChoice248;
  PushChoiceProcs[249] := PushChoice249;
  PushChoiceProcs[250] := PushChoice250;
  PushChoiceProcs[251] := PushChoice251;
  PushChoiceProcs[252] := PushChoice252;
  PushChoiceProcs[253] := PushChoice253;
  PushChoiceProcs[254] := PushChoice254;
  PushChoiceProcs[255] := PushChoice255;
  PushChoiceProcs[256] := PushChoice256;
  PushChoiceProcs[257] := PushChoice257;
  PushChoiceProcs[258] := PushChoice258;
  PushChoiceProcs[259] := PushChoice259;
  PushChoiceProcs[260] := PushChoice260;
  PushChoiceProcs[261] := PushChoice261;
  PushChoiceProcs[262] := PushChoice262;
  PushChoiceProcs[263] := PushChoice263;
  PushChoiceProcs[264] := PushChoice264;
  PushChoiceProcs[265] := PushChoice265;
  PushChoiceProcs[266] := PushChoice266;
  PushChoiceProcs[267] := PushChoice267;
  PushChoiceProcs[268] := PushChoice268;
  PushChoiceProcs[269] := PushChoice269;
  PushChoiceProcs[270] := PushChoice270;
  PushChoiceProcs[271] := PushChoice271;
  PushChoiceProcs[272] := PushChoice272;
  PushChoiceProcs[273] := PushChoice273;
  PushChoiceProcs[274] := PushChoice274;
  PushChoiceProcs[275] := PushChoice275;
  PushChoiceProcs[276] := PushChoice276;
  PushChoiceProcs[277] := PushChoice277;
  PushChoiceProcs[278] := PushChoice278;
  PushChoiceProcs[279] := PushChoice279;
  PushChoiceProcs[280] := PushChoice280;
  PushChoiceProcs[281] := PushChoice281;
  PushChoiceProcs[282] := PushChoice282;
  PushChoiceProcs[283] := PushChoice283;
  PushChoiceProcs[284] := PushChoice284;
  PushChoiceProcs[285] := PushChoice285;
  PushChoiceProcs[286] := PushChoice286;
  PushChoiceProcs[287] := PushChoice287;
  PushChoiceProcs[288] := PushChoice288;
  PushChoiceProcs[289] := PushChoice289;
  PushChoiceProcs[290] := PushChoice290;
  PushChoiceProcs[291] := PushChoice291;
  PushChoiceProcs[292] := PushChoice292;
  PushChoiceProcs[293] := PushChoice293;
  PushChoiceProcs[294] := PushChoice294;
  PushChoiceProcs[295] := PushChoice295;
  PushChoiceProcs[296] := PushChoice296;
  PushChoiceProcs[297] := PushChoice297;
  PushChoiceProcs[298] := PushChoice298;
  PushChoiceProcs[299] := PushChoice299;
  PushChoiceProcs[300] := PushChoice300;
  PushChoiceProcs[301] := PushChoice301;
  PushChoiceProcs[302] := PushChoice302;
  PushChoiceProcs[303] := PushChoice303;
  PushChoiceProcs[304] := PushChoice304;
  PushChoiceProcs[305] := PushChoice305;
  PushChoiceProcs[306] := PushChoice306;
  PushChoiceProcs[307] := PushChoice307;
  PushChoiceProcs[308] := PushChoice308;
  PushChoiceProcs[309] := PushChoice309;
  PushChoiceProcs[310] := PushChoice310;
  PushChoiceProcs[311] := PushChoice311;
  PushChoiceProcs[312] := PushChoice312;
  PushChoiceProcs[313] := PushChoice313;
  PushChoiceProcs[314] := PushChoice314;
  PushChoiceProcs[315] := PushChoice315;
  PushChoiceProcs[316] := PushChoice316;
  PushChoiceProcs[317] := PushChoice317;
  PushChoiceProcs[318] := PushChoice318;
  PushChoiceProcs[319] := PushChoice319;
  PushChoiceProcs[320] := PushChoice320;
  PushChoiceProcs[321] := PushChoice321;
  PushChoiceProcs[322] := PushChoice322;
  PushChoiceProcs[323] := PushChoice323;
  PushChoiceProcs[324] := PushChoice324;
  PushChoiceProcs[325] := PushChoice325;
  PushChoiceProcs[326] := PushChoice326;
  PushChoiceProcs[327] := PushChoice327;
  PushChoiceProcs[328] := PushChoice328;
  PushChoiceProcs[329] := PushChoice329;
  PushChoiceProcs[330] := PushChoice330;
  PushChoiceProcs[331] := PushChoice331;
  PushChoiceProcs[332] := PushChoice332;
  PushChoiceProcs[333] := PushChoice333;
  PushChoiceProcs[334] := PushChoice334;
  PushChoiceProcs[335] := PushChoice335;
  PushChoiceProcs[336] := PushChoice336;
  PushChoiceProcs[337] := PushChoice337;
  PushChoiceProcs[338] := PushChoice338;
  PushChoiceProcs[339] := PushChoice339;
  PushChoiceProcs[340] := PushChoice340;
  PushChoiceProcs[341] := PushChoice341;
  PushChoiceProcs[342] := PushChoice342;
  PushChoiceProcs[343] := PushChoice343;
  PushChoiceProcs[344] := PushChoice344;
  PushChoiceProcs[345] := PushChoice345;
  PushChoiceProcs[346] := PushChoice346;
  PushChoiceProcs[347] := PushChoice347;
  PushChoiceProcs[348] := PushChoice348;
  PushChoiceProcs[349] := PushChoice349;
  PushChoiceProcs[350] := PushChoice350;
  PushChoiceProcs[351] := PushChoice351;
  PushChoiceProcs[352] := PushChoice352;
  PushChoiceProcs[353] := PushChoice353;
  PushChoiceProcs[354] := PushChoice354;
  PushChoiceProcs[355] := PushChoice355;
  PushChoiceProcs[356] := PushChoice356;
  PushChoiceProcs[357] := PushChoice357;
  PushChoiceProcs[358] := PushChoice358;
  PushChoiceProcs[359] := PushChoice359;
  PushChoiceProcs[360] := PushChoice360;
  PushChoiceProcs[361] := PushChoice361;
  PushChoiceProcs[362] := PushChoice362;
  PushChoiceProcs[363] := PushChoice363;
  PushChoiceProcs[364] := PushChoice364;
  PushChoiceProcs[365] := PushChoice365;
  PushChoiceProcs[366] := PushChoice366;
  PushChoiceProcs[367] := PushChoice367;
  PushChoiceProcs[368] := PushChoice368;
  PushChoiceProcs[369] := PushChoice369;
  PushChoiceProcs[370] := PushChoice370;
  PushChoiceProcs[371] := PushChoice371;
  PushChoiceProcs[372] := PushChoice372;
  PushChoiceProcs[373] := PushChoice373;
  PushChoiceProcs[374] := PushChoice374;
  PushChoiceProcs[375] := PushChoice375;
  PushChoiceProcs[376] := PushChoice376;
  PushChoiceProcs[377] := PushChoice377;
  PushChoiceProcs[378] := PushChoice378;
  PushChoiceProcs[379] := PushChoice379;
  PushChoiceProcs[380] := PushChoice380;
  PushChoiceProcs[381] := PushChoice381;
  PushChoiceProcs[382] := PushChoice382;
  PushChoiceProcs[383] := PushChoice383;
  PushChoiceProcs[384] := PushChoice384;
  PushChoiceProcs[385] := PushChoice385;
  PushChoiceProcs[386] := PushChoice386;
  PushChoiceProcs[387] := PushChoice387;
  PushChoiceProcs[388] := PushChoice388;
  PushChoiceProcs[389] := PushChoice389;
  PushChoiceProcs[390] := PushChoice390;
  PushChoiceProcs[391] := PushChoice391;
  PushChoiceProcs[392] := PushChoice392;
  PushChoiceProcs[393] := PushChoice393;
  PushChoiceProcs[394] := PushChoice394;
  PushChoiceProcs[395] := PushChoice395;
  PushChoiceProcs[396] := PushChoice396;
  PushChoiceProcs[397] := PushChoice397;
  PushChoiceProcs[398] := PushChoice398;
  PushChoiceProcs[399] := PushChoice399;
  PushChoiceProcs[400] := PushChoice400;
  PushChoiceProcs[401] := PushChoice401;
  PushChoiceProcs[402] := PushChoice402;
  PushChoiceProcs[403] := PushChoice403;
  PushChoiceProcs[404] := PushChoice404;
  PushChoiceProcs[405] := PushChoice405;
  PushChoiceProcs[406] := PushChoice406;
  PushChoiceProcs[407] := PushChoice407;
  PushChoiceProcs[408] := PushChoice408;
  PushChoiceProcs[409] := PushChoice409;
  PushChoiceProcs[410] := PushChoice410;
  PushChoiceProcs[411] := PushChoice411;
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiParser.GetExpectedString(
  ExpectedSymbol: TSepiSymbolClass): string;
begin
  Result := SymbolClassNames[ExpectedSymbol];
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiParser.GetParsingTable(NonTerminalClass,
  TerminalClass: TSepiSymbolClass): TRuleID;
begin
  Result := ParsingTable[NonTerminalClass, TerminalClass];
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiParser.GetNonTerminalClass(
  Symbol: TSepiSymbolClass): TSepiNonTerminalClass;
begin
  Result := NonTerminalClasses[Symbol];
end;

{*
  Initializes SymbolClassNames array
*}
procedure InitSymbolClassNames;
begin
  SymbolClassNames[ntSource] := 'ntSource';
  SymbolClassNames[ntInPreProcessorExpression] := 'ntInPreProcessorExpression';
  SymbolClassNames[ntInterface] := 'ntInterface';
  SymbolClassNames[ntImplementation] := 'ntImplementation';
  SymbolClassNames[ntIntfSection] := 'ntIntfSection';
  SymbolClassNames[ntImplSection] := 'ntImplSection';
  SymbolClassNames[ntInitFinit] := 'ntInitFinit';
  SymbolClassNames[ntIdentifier] := 'ntIdentifier';
  SymbolClassNames[ntUsesSection] := 'ntUsesSection';
  SymbolClassNames[ntCommaIdentList] := 'ntCommaIdentList';
  SymbolClassNames[ntCommaIdentDeclList] := 'ntCommaIdentDeclList';
  SymbolClassNames[ntQualifiedIdent] := 'ntQualifiedIdent';
  SymbolClassNames[ntIdentifierDecl] := 'ntIdentifierDecl';
  SymbolClassNames[ntInitializationExpression] := 'ntInitializationExpression';
  SymbolClassNames[ntArrayInitializationExpression] := 'ntArrayInitializationExpression';
  SymbolClassNames[ntArrayInitialization] := 'ntArrayInitialization';
  SymbolClassNames[ntRecordInitializationExpression] := 'ntRecordInitializationExpression';
  SymbolClassNames[ntRecordInitialization] := 'ntRecordInitialization';
  SymbolClassNames[ntGUIDInitializationExpression] := 'ntGUIDInitializationExpression';
  SymbolClassNames[ntGUIDInitialization] := 'ntGUIDInitialization';
  SymbolClassNames[ntOtherInitializationExpression] := 'ntOtherInitializationExpression';
  SymbolClassNames[ntOtherInitialization] := 'ntOtherInitialization';
  SymbolClassNames[ntExpression] := 'ntExpression';
  SymbolClassNames[ntExpressionNoEquals] := 'ntExpressionNoEquals';
  SymbolClassNames[ntConstExpression] := 'ntConstExpression';
  SymbolClassNames[ntConstExpressionNoEquals] := 'ntConstExpressionNoEquals';
  SymbolClassNames[ntConstOrType] := 'ntConstOrType';
  SymbolClassNames[ntConstOrTypeNoEquals] := 'ntConstOrTypeNoEquals';
  SymbolClassNames[ntSingleExpr] := 'ntSingleExpr';
  SymbolClassNames[ntUnaryOpExpr] := 'ntUnaryOpExpr';
  SymbolClassNames[ntParenthesizedExpr] := 'ntParenthesizedExpr';
  SymbolClassNames[ntNextExpr] := 'ntNextExpr';
  SymbolClassNames[ntUnaryOpModifier] := 'ntUnaryOpModifier';
  SymbolClassNames[ntDereferenceOp] := 'ntDereferenceOp';
  SymbolClassNames[ntParameters] := 'ntParameters';
  SymbolClassNames[ntIdentTestParam] := 'ntIdentTestParam';
  SymbolClassNames[ntArrayIndices] := 'ntArrayIndices';
  SymbolClassNames[ntExprList] := 'ntExprList';
  SymbolClassNames[ntFieldSelection] := 'ntFieldSelection';
  SymbolClassNames[ntSingleValue] := 'ntSingleValue';
  SymbolClassNames[ntIntegerConst] := 'ntIntegerConst';
  SymbolClassNames[ntFloatConst] := 'ntFloatConst';
  SymbolClassNames[ntStringConst] := 'ntStringConst';
  SymbolClassNames[ntIdentifierSingleValue] := 'ntIdentifierSingleValue';
  SymbolClassNames[ntInheritedSingleValue] := 'ntInheritedSingleValue';
  SymbolClassNames[ntNilValue] := 'ntNilValue';
  SymbolClassNames[ntSetValue] := 'ntSetValue';
  SymbolClassNames[ntSetRange] := 'ntSetRange';
  SymbolClassNames[ntBinaryOp] := 'ntBinaryOp';
  SymbolClassNames[ntBinaryOpNoEquals] := 'ntBinaryOpNoEquals';
  SymbolClassNames[ntUnaryOp] := 'ntUnaryOp';
  SymbolClassNames[ntAddressOfOp] := 'ntAddressOfOp';
  SymbolClassNames[ntConstSection] := 'ntConstSection';
  SymbolClassNames[ntConstKeyWord] := 'ntConstKeyWord';
  SymbolClassNames[ntConstDecl] := 'ntConstDecl';
  SymbolClassNames[ntInnerConstDecl] := 'ntInnerConstDecl';
  SymbolClassNames[ntVarSection] := 'ntVarSection';
  SymbolClassNames[ntGlobalVar] := 'ntGlobalVar';
  SymbolClassNames[ntInnerGlobalVar] := 'ntInnerGlobalVar';
  SymbolClassNames[ntTypeSection] := 'ntTypeSection';
  SymbolClassNames[ntTypeDecl] := 'ntTypeDecl';
  SymbolClassNames[ntTypeDesc] := 'ntTypeDesc';
  SymbolClassNames[ntTypeName] := 'ntTypeName';
  SymbolClassNames[ntPackedDesc] := 'ntPackedDesc';
  SymbolClassNames[ntArrayDesc] := 'ntArrayDesc';
  SymbolClassNames[ntPackedArrayDesc] := 'ntPackedArrayDesc';
  SymbolClassNames[ntArrayDims] := 'ntArrayDims';
  SymbolClassNames[ntArrayRange] := 'ntArrayRange';
  SymbolClassNames[ntTypeModifiers] := 'ntTypeModifiers';
  SymbolClassNames[ntCloneDesc] := 'ntCloneDesc';
  SymbolClassNames[ntRangeOrEnumDesc] := 'ntRangeOrEnumDesc';
  SymbolClassNames[ntRangeDesc] := 'ntRangeDesc';
  SymbolClassNames[ntEnumDesc] := 'ntEnumDesc';
  SymbolClassNames[ntSetDesc] := 'ntSetDesc';
  SymbolClassNames[ntStringDesc] := 'ntStringDesc';
  SymbolClassNames[ntPointerDesc] := 'ntPointerDesc';
  SymbolClassNames[ntRecordDesc] := 'ntRecordDesc';
  SymbolClassNames[ntPackedRecordDesc] := 'ntPackedRecordDesc';
  SymbolClassNames[ntRecordContents] := 'ntRecordContents';
  SymbolClassNames[ntRecordCaseBlock] := 'ntRecordCaseBlock';
  SymbolClassNames[ntRecordCase] := 'ntRecordCase';
  SymbolClassNames[ntCaseLabels] := 'ntCaseLabels';
  SymbolClassNames[ntRecordCaseContents] := 'ntRecordCaseContents';
  SymbolClassNames[ntNextRecordCaseContents] := 'ntNextRecordCaseContents';
  SymbolClassNames[ntNextRecordCaseContentsEx] := 'ntNextRecordCaseContentsEx';
  SymbolClassNames[ntRecordField] := 'ntRecordField';
  SymbolClassNames[ntRecordCaseField] := 'ntRecordCaseField';
  SymbolClassNames[ntField] := 'ntField';
  SymbolClassNames[ntClassDesc] := 'ntClassDesc';
  SymbolClassNames[ntClassExDesc] := 'ntClassExDesc';
  SymbolClassNames[ntClassContents] := 'ntClassContents';
  SymbolClassNames[ntClassHeritage] := 'ntClassHeritage';
  SymbolClassNames[ntClassMemberLists] := 'ntClassMemberLists';
  SymbolClassNames[ntVisibility] := 'ntVisibility';
  SymbolClassNames[ntClassMemberList] := 'ntClassMemberList';
  SymbolClassNames[ntInterfaceDesc] := 'ntInterfaceDesc';
  SymbolClassNames[ntInterfaceHeritage] := 'ntInterfaceHeritage';
  SymbolClassNames[ntDispInterfaceDesc] := 'ntDispInterfaceDesc';
  SymbolClassNames[ntDispInterfaceHeritage] := 'ntDispInterfaceHeritage';
  SymbolClassNames[ntInterfaceGUID] := 'ntInterfaceGUID';
  SymbolClassNames[ntInterfaceMemberList] := 'ntInterfaceMemberList';
  SymbolClassNames[ntClassMethodProp] := 'ntClassMethodProp';
  SymbolClassNames[ntClassMethodDecl] := 'ntClassMethodDecl';
  SymbolClassNames[ntIntfMethodProp] := 'ntIntfMethodProp';
  SymbolClassNames[ntIntfMethodRedirector] := 'ntIntfMethodRedirector';
  SymbolClassNames[ntIntfMethodRedirKind] := 'ntIntfMethodRedirKind';
  SymbolClassNames[ntMethodDecl] := 'ntMethodDecl';
  SymbolClassNames[ntPropertyDecl] := 'ntPropertyDecl';
  SymbolClassNames[ntPropertyKind] := 'ntPropertyKind';
  SymbolClassNames[ntPropertyNextDecl] := 'ntPropertyNextDecl';
  SymbolClassNames[ntRedefineMarker] := 'ntRedefineMarker';
  SymbolClassNames[ntPropInfo] := 'ntPropInfo';
  SymbolClassNames[ntPropReadAccess] := 'ntPropReadAccess';
  SymbolClassNames[ntPropWriteAccess] := 'ntPropWriteAccess';
  SymbolClassNames[ntPropIndex] := 'ntPropIndex';
  SymbolClassNames[ntPropDefaultValue] := 'ntPropDefaultValue';
  SymbolClassNames[ntPropStorage] := 'ntPropStorage';
  SymbolClassNames[ntIgnoredPropInfo] := 'ntIgnoredPropInfo';
  SymbolClassNames[ntPropertyModifier] := 'ntPropertyModifier';
  SymbolClassNames[ntDefaultMarker] := 'ntDefaultMarker';
  SymbolClassNames[ntIgnoredPropertyModifier] := 'ntIgnoredPropertyModifier';
  SymbolClassNames[ntEventDesc] := 'ntEventDesc';
  SymbolClassNames[ntEventModifiers] := 'ntEventModifiers';
  SymbolClassNames[ntEventIsOfObject] := 'ntEventIsOfObject';
  SymbolClassNames[ntRoutineDecl] := 'ntRoutineDecl';
  SymbolClassNames[ntRoutineKind] := 'ntRoutineKind';
  SymbolClassNames[ntMethodKind] := 'ntMethodKind';
  SymbolClassNames[ntClassMethodKind] := 'ntClassMethodKind';
  SymbolClassNames[ntMethodNameDeclaration] := 'ntMethodNameDeclaration';
  SymbolClassNames[ntRoutineModifier] := 'ntRoutineModifier';
  SymbolClassNames[ntMethodModifier] := 'ntMethodModifier';
  SymbolClassNames[ntCallingConvention] := 'ntCallingConvention';
  SymbolClassNames[ntMethodLinkKind] := 'ntMethodLinkKind';
  SymbolClassNames[ntAbstractMarker] := 'ntAbstractMarker';
  SymbolClassNames[ntOverloadMarker] := 'ntOverloadMarker';
  SymbolClassNames[ntIgnoredRoutineModifier] := 'ntIgnoredRoutineModifier';
  SymbolClassNames[ntIgnoredMethodModifier] := 'ntIgnoredMethodModifier';
  SymbolClassNames[ntMethodSignature] := 'ntMethodSignature';
  SymbolClassNames[ntPropertySignature] := 'ntPropertySignature';
  SymbolClassNames[ntMethodParamList] := 'ntMethodParamList';
  SymbolClassNames[ntParamList] := 'ntParamList';
  SymbolClassNames[ntReturnType] := 'ntReturnType';
  SymbolClassNames[ntPropType] := 'ntPropType';
  SymbolClassNames[ntParam] := 'ntParam';
  SymbolClassNames[ntParamKind] := 'ntParamKind';
  SymbolClassNames[ntParamNameList] := 'ntParamNameList';
  SymbolClassNames[ntParamName] := 'ntParamName';
  SymbolClassNames[ntParamTypeAndDefault] := 'ntParamTypeAndDefault';
  SymbolClassNames[ntComplexParamType] := 'ntComplexParamType';
  SymbolClassNames[ntParamIsArray] := 'ntParamIsArray';
  SymbolClassNames[ntParamArrayType] := 'ntParamArrayType';
  SymbolClassNames[ntParamType] := 'ntParamType';
  SymbolClassNames[ntParamDefault] := 'ntParamDefault';
  SymbolClassNames[ntMethodImpl] := 'ntMethodImpl';
  SymbolClassNames[ntMethodImplHeader] := 'ntMethodImplHeader';
  SymbolClassNames[ntForwardOrMethodBody] := 'ntForwardOrMethodBody';
  SymbolClassNames[ntMethodBody] := 'ntMethodBody';
  SymbolClassNames[ntInMethodSection] := 'ntInMethodSection';
  SymbolClassNames[ntForwardMarker] := 'ntForwardMarker';
  SymbolClassNames[ntUnitInitialization] := 'ntUnitInitialization';
  SymbolClassNames[ntUnitFinalization] := 'ntUnitFinalization';
  SymbolClassNames[ntLocalVarSection] := 'ntLocalVarSection';
  SymbolClassNames[ntLocalVar] := 'ntLocalVar';
  SymbolClassNames[ntInstructionList] := 'ntInstructionList';
  SymbolClassNames[ntInstruction] := 'ntInstruction';
  SymbolClassNames[ntNoInstruction] := 'ntNoInstruction';
  SymbolClassNames[ntBeginEndBlock] := 'ntBeginEndBlock';
  SymbolClassNames[ntIfThenElseInstruction] := 'ntIfThenElseInstruction';
  SymbolClassNames[ntElseBranch] := 'ntElseBranch';
  SymbolClassNames[ntWhileInstruction] := 'ntWhileInstruction';
  SymbolClassNames[ntRepeatInstruction] := 'ntRepeatInstruction';
  SymbolClassNames[ntForInstruction] := 'ntForInstruction';
  SymbolClassNames[ntForControlVar] := 'ntForControlVar';
  SymbolClassNames[ntForToDownTo] := 'ntForToDownTo';
  SymbolClassNames[ntForTo] := 'ntForTo';
  SymbolClassNames[ntForDownTo] := 'ntForDownTo';
  SymbolClassNames[ntTryInstruction] := 'ntTryInstruction';
  SymbolClassNames[ntNextTryInstruction] := 'ntNextTryInstruction';
  SymbolClassNames[ntExceptClause] := 'ntExceptClause';
  SymbolClassNames[ntNextExceptClause] := 'ntNextExceptClause';
  SymbolClassNames[ntMultiOn] := 'ntMultiOn';
  SymbolClassNames[ntOnClause] := 'ntOnClause';
  SymbolClassNames[ntExceptionVarAndType] := 'ntExceptionVarAndType';
  SymbolClassNames[ntMultiOnElseClause] := 'ntMultiOnElseClause';
  SymbolClassNames[ntFinallyClause] := 'ntFinallyClause';
  SymbolClassNames[ntRaiseInstruction] := 'ntRaiseInstruction';
  SymbolClassNames[ntExpressionInstruction] := 'ntExpressionInstruction';
  SymbolClassNames[ntNextExpressionInstruction] := 'ntNextExpressionInstruction';
  SymbolClassNames[ntCallInstruction] := 'ntCallInstruction';
  SymbolClassNames[ntAssignmentInstruction] := 'ntAssignmentInstruction';
  SymbolClassNames[ntPriv0] := 'ntPriv0';
  SymbolClassNames[ntPriv1] := 'ntPriv1';
  SymbolClassNames[ntPriv2] := 'ntPriv2';
  SymbolClassNames[ntPriv3] := 'ntPriv3';
  SymbolClassNames[ntPriv4] := 'ntPriv4';
  SymbolClassNames[ntPriv5] := 'ntPriv5';
  SymbolClassNames[ntPriv6] := 'ntPriv6';
  SymbolClassNames[ntPriv7] := 'ntPriv7';
  SymbolClassNames[ntPriv8] := 'ntPriv8';
  SymbolClassNames[ntPriv9] := 'ntPriv9';
  SymbolClassNames[ntPriv10] := 'ntPriv10';
  SymbolClassNames[ntPriv11] := 'ntPriv11';
  SymbolClassNames[ntPriv12] := 'ntPriv12';
  SymbolClassNames[ntPriv13] := 'ntPriv13';
  SymbolClassNames[ntPriv14] := 'ntPriv14';
  SymbolClassNames[ntPriv15] := 'ntPriv15';
  SymbolClassNames[ntPriv16] := 'ntPriv16';
  SymbolClassNames[ntPriv17] := 'ntPriv17';
  SymbolClassNames[ntPriv18] := 'ntPriv18';
  SymbolClassNames[ntPriv19] := 'ntPriv19';
  SymbolClassNames[ntPriv20] := 'ntPriv20';
  SymbolClassNames[ntPriv21] := 'ntPriv21';
  SymbolClassNames[ntPriv22] := 'ntPriv22';
  SymbolClassNames[ntPriv23] := 'ntPriv23';
  SymbolClassNames[ntPriv24] := 'ntPriv24';
  SymbolClassNames[ntPriv25] := 'ntPriv25';
  SymbolClassNames[ntPriv26] := 'ntPriv26';
  SymbolClassNames[ntPriv27] := 'ntPriv27';
  SymbolClassNames[ntPriv28] := 'ntPriv28';
  SymbolClassNames[ntPriv29] := 'ntPriv29';
  SymbolClassNames[ntPriv30] := 'ntPriv30';
  SymbolClassNames[ntPriv31] := 'ntPriv31';
  SymbolClassNames[ntPriv32] := 'ntPriv32';
  SymbolClassNames[ntPriv33] := 'ntPriv33';
  SymbolClassNames[ntPriv34] := 'ntPriv34';
  SymbolClassNames[ntPriv35] := 'ntPriv35';
  SymbolClassNames[ntPriv36] := 'ntPriv36';
  SymbolClassNames[ntPriv37] := 'ntPriv37';
  SymbolClassNames[ntPriv38] := 'ntPriv38';
  SymbolClassNames[ntPriv39] := 'ntPriv39';
  SymbolClassNames[ntPriv40] := 'ntPriv40';
  SymbolClassNames[ntPriv41] := 'ntPriv41';
  SymbolClassNames[ntPriv42] := 'ntPriv42';
  SymbolClassNames[ntPriv43] := 'ntPriv43';
  SymbolClassNames[ntPriv44] := 'ntPriv44';
  SymbolClassNames[ntPriv45] := 'ntPriv45';
  SymbolClassNames[ntPriv46] := 'ntPriv46';
  SymbolClassNames[ntPriv47] := 'ntPriv47';
  SymbolClassNames[ntPriv48] := 'ntPriv48';
  SymbolClassNames[ntPriv49] := 'ntPriv49';
  SymbolClassNames[ntPriv50] := 'ntPriv50';
  SymbolClassNames[ntPriv51] := 'ntPriv51';
  SymbolClassNames[ntPriv52] := 'ntPriv52';
  SymbolClassNames[ntPriv53] := 'ntPriv53';
  SymbolClassNames[ntPriv54] := 'ntPriv54';
  SymbolClassNames[ntPriv55] := 'ntPriv55';
  SymbolClassNames[ntPriv56] := 'ntPriv56';
  SymbolClassNames[ntPriv57] := 'ntPriv57';
end;

{*
  Initializes NonTerminalClasses array
*}
procedure InitNonTerminalClasses;
const
  ClassesToSimplify: array[0..97] of TSepiSymbolClass = (
    -1, 116, 117, 118, 121, 143, 149, 167, 170, 173, 175, 192, 195, 196, 201, 202, 206, 213, 215, 221, 223, 230, 239, 241, 242, 251, 252, 259, 260, 262, 263, 264, 267, 269, 276, 280, 285, 289, 291, 299, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359
  );
var
  I: TSepiSymbolClass;
begin
  for I := FirstNonTerminal to LastNonTerminal do
    NonTerminalClasses[I] := TSepiNonTerminal;

  for I := 1 to High(ClassesToSimplify) do
    NonTerminalClasses[ClassesToSimplify[I]] := TSepiChildThroughNonTerminal;
end;

initialization
  if Length(SymbolClassNames) < LastNonTerminal+1 then
    SetLength(SymbolClassNames, LastNonTerminal+1);

  InitSymbolClassNames;
  InitNonTerminalClasses;
end.

