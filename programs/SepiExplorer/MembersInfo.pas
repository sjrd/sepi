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
  const Name: string = ''; const Brackets: string = '()');
procedure PrintMethodInfo(Output: TOutputWriter; Method: TSepiMethod);
procedure PrintPropertyInfo(Output: TOutputWriter; Prop: TSepiProperty);

implementation

uses
  TypesInfo;

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
      Name + ': ' + FieldType.DisplayName + ';');
end;

procedure PrintParamInfo(Output: TOutputWriter; Param: TSepiParam;
  const PreviousParams: string);
begin
  case Param.Kind of
    pkVar: Output.Write('var ');
    pkConst: Output.Write('const ');
    pkOut: Output.Write('out ');
  end;

  Output.Write(PreviousParams + Param.Name);

  if not Param.IsUntyped then
    Output.Write(': ' + Param.ParamType.DisplayName);

  if Param.HasDefaultValue then
  begin
    Output.Write(' = ');
    PrintValue(Output, Param.DefaultValuePtr, Param.ParamType);
  end;
end;

procedure PrintInnerParams(Output: TOutputWriter; Signature: TSepiSignature);
var
  I: Integer;
  PreviousParams: string;
  First: Boolean;
  Param: TSepiParam;
begin
  with Signature do
  begin
    PreviousParams := '';
    First := True;

    for I := 0 to ParamCount-1 do
    begin
      Param := Params[I];

      if (I < ParamCount-1) and (not Param.HasDefaultValue) and
        Param.Equals(Params[I+1], pcoAll - [pcoName]) then
      begin
        PreviousParams := Param.Name + ', ';
      end else
      begin
        if not First then
          Output.Write('; ');

        First := False;
        PrintParamInfo(Output, Param, PreviousParams);
        PreviousParams := '';
      end;
    end;
  end;
end;

procedure PrintSignature(Output: TOutputWriter; Signature: TSepiSignature;
  const Name: string = ''; const Brackets: string = '()');
begin
  Output.Write('  ' + SignatureKindStrings[Signature.Kind]);
  if Name <> '' then
    Output.Write(' ' + Name);

  with Signature do
  begin
    if ParamCount > 0 then
    begin
      Output.Write(Brackets[1]);
      PrintInnerParams(Output, Signature);
      Output.Write(Brackets[2]);
    end;

    if ReturnType <> nil then
      Output.Write(': ' + ReturnType.DisplayName);

    if CallingConvention <> ccRegister then
      Output.Write('; ' + LowerCase(Copy(
        GetEnumName(TypeInfo(TCallingConvention), Integer(CallingConvention)),
        3, MaxInt)));
  end;
end;

procedure PrintMethodInfo(Output: TOutputWriter; Method: TSepiMethod);
{var
  Code: Pointer;}
begin
  PrintSignature(Output, Method.Signature, Method.Name);

  Output.Write(';');

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
begin
  PrintSignature(Output, Prop.Signature, Prop.Name, '[]');

  if Prop.ReadAccess.Kind <> pakNone then
    Output.Write(' read ' + Prop.ReadAccess.Component.Name);
  if Prop.WriteAccess.Kind <> pakNone then
    Output.Write(' write ' + Prop.WriteAccess.Component.Name);
  if Prop.Index <> NoIndex then
    Output.Write(' index ' + IntToStr(Prop.Index));
  if Prop.DefaultValue <> NoDefaultValue then
    Output.Write(' default ' + IntToStr(Prop.DefaultValue));

  if (Prop.Storage.Kind = pskConstant) and (not Prop.Storage.Stored) then
    Output.Write(' stored False')
  else if Prop.Storage.Kind <> pskConstant then
    Output.Write(' stored '+Prop.Storage.Component.Name);

  if Prop.IsDefault then
    Output.Write('; default');

  Output.WriteLn(';');
end;

end.

