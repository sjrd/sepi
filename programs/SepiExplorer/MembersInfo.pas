{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Informations sur les membres Sepi
  @author sjrd
  @version 1.0
*}
unit MembersInfo;

interface

uses
  SysUtils, Classes, TypInfo, ScUtils, ScCompilerMagic, SepiReflectionCore,
  SepiMembers;

type
  {*
    Écrivain dans une liste de chaînes
    @author sjrd
    @version 1.0
  *}
  TOutputWriter = class
  private
    FStrings: TStrings; /// Liste de chaînes de sortie
    FCurStr: string;    /// Current string
  public
    constructor Create(AStrings: TStrings);

    procedure BeforeDestruction; override;

    procedure Write(const Str: string = '');
    procedure WriteLn(const Str: string = '');
  end;

procedure PrintFieldInfo(Output: TOutputWriter; Field: TSepiField);
procedure PrintSignature(Output: TOutputWriter; Signature: TSepiSignature;
  const Name: string = '');
procedure PrintMethodInfo(Output: TOutputWriter; Method: TSepiMethod);
procedure PrintPropertyInfo(Output: TOutputWriter; Prop: TSepiProperty);

implementation

{---------------------}
{ TOutputWriter class }
{---------------------}

constructor TOutputWriter.Create(AStrings: TStrings);
begin
  inherited Create;

  FStrings := AStrings;
end;

procedure TOutputWriter.BeforeDestruction;
begin
  if FCurStr <> '' then
    FStrings.Add(FCurStr);

  inherited;
end;

procedure TOutputWriter.Write(const Str: string = '');
begin
  FCurStr := FCurStr + Str;
end;

procedure TOutputWriter.WriteLn(const Str: string = '');
begin
  FStrings.Add(FCurStr + Str);
  FCurStr := '';
end;

{-----------------}
{ Global routines }
{-----------------}

procedure PrintFieldInfo(Output: TOutputWriter; Field: TSepiField);
begin
  with Field do
    Output.WriteLn('  {' + IntToStr0(Offset, 2) + '}'#9 +
      Name + ': ' + FieldType.Name + ';');
end;

procedure PrintParamInfo(Output: TOutputWriter; Param: TSepiParam);
begin
  if Param.HiddenKind <> hpNormal then
    Output.Write('{');

  case Param.Kind of
    pkVar: Output.Write('var ');
    pkConst: Output.Write('const ');
    pkOut: Output.Write('out ');
  end;

  //if pfAddress in Param.Flags then Output.Write('{addr} ');
  //if pfReference in Param.Flags then Output.Write('{ref} ');

  Output.Write(Param.Name);

  if Param.ParamType <> nil then
  begin
    Output.Write(': ');
    if pfArray in Param.Flags then
      Output.Write('array of ');
    Output.Write(Param.ParamType.Name);
  end else if pfArray in Param.Flags then
    Output.Write(': array of const');

  if Param.HiddenKind <> hpNormal then
    Output.Write('}');
end;

procedure PrintSignature(Output: TOutputWriter; Signature: TSepiSignature;
  const Name: string = '');
var
  ParameterCount, I: Integer;
begin
  Output.Write('  ' + SignatureKindStrings[Signature.Kind]);
  if Name <> '' then
    Output.Write(' ' + Name);

  with Signature do
  begin
    ParameterCount := {Actual} ParamCount;
    if ParameterCount > 0 then
    begin
      Output.Write('(');
      for I := 0 to ParameterCount-1 do
      begin
        if I <> 0 then
          Output.Write('; ');
        PrintParamInfo(Output, {Actual} Params[I]);
      end;
      Output.Write(')');
    end;

    if ReturnType <> nil then
      Output.Write(': ' + ReturnType.Name);

    if CallingConvention <> ccRegister then
      Output.Write('; ' + LowerCase(Copy(
        GetEnumName(TypeInfo(TCallingConvention), Integer(CallingConvention)),
        3, MaxInt)));
  end;

  Output.Write(';');
end;

procedure PrintMethodInfo(Output: TOutputWriter; Method: TSepiMethod);
{var
  Code: Pointer;}
begin
  PrintSignature(Output, Method.Signature, Method.Name);

  if not Method.FirstDeclaration then
    Output.Write(' override;')
  else
  begin
    case Method.LinkKind of
      mlkVirtual: Output.Write(' virtual {' +
        IntToStr(Method.VMTOffset) + '};');
      mlkDynamic: Output.Write(' dynamic {' + IntToStr(Method.DMTIndex) + '};');
      mlkMessage: Output.Write(' message ' + IntToStr(Method.MsgID) + ';');
    end;
  end;

  if Method.IsAbstract then
    Output.Write(' abstract;')
  else if Method.Code <> nil then
  begin
    (*Code := Method.Code;
    if PByte(Code)^ = $E9 then
      Code := CompilerMagicRoutineAddress(Code);
    Output.Write(' {$' + IntToHex(Integer(Code), 8) + '}');*)
  end;

  Output.WriteLn;
end;

procedure PrintPropertyInfo(Output: TOutputWriter; Prop: TSepiProperty);
var
  I: Integer;
begin
  Output.Write('  property ' + Prop.Name);

  with Prop.Signature do
  begin
    if ParamCount > 0 then
    begin
      Output.Write('[');
      for I := 0 to ParamCount-1 do
      begin
        if I <> 0 then
          Output.Write('; ');
        PrintParamInfo(Output, Params[I]);
      end;
      Output.Write(']');
    end;
  end;

  Output.Write(': ' + Prop.PropType.Name);

  if Prop.ReadAccess.Kind <> pakNone then
    Output.Write(' read ' + Prop.ReadAccess.Meta.Name);
  if Prop.WriteAccess.Kind <> pakNone then
    Output.Write(' write ' + Prop.WriteAccess.Meta.Name);
  if Prop.Index <> NoIndex then
    Output.Write(' index ' + IntToStr(Prop.Index));
  if Prop.DefaultValue <> NoDefaultValue then
    Output.Write(' default ' + IntToStr(Prop.DefaultValue));

  if Prop.IsDefault then
    Output.Write('; default');

  Output.WriteLn(';');
end;

end.

