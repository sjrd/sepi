{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Importe l'unité System dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsSystem;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsTObject = class(TObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTInterfacedObject = class(TInterfacedObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTAggregatedObject = class(TAggregatedObject)
  private
    function GetController: IInterface;
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTContainedObject = class(TContainedObject)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

  TSepiImportsTClassHelperBase = class(TClassHelperBase)
  private
    class function SepiImport(Owner: TSepiUnit): TSepiClass;
  end;

{--------------}
{ TGUID import }
{--------------}

function SepiImportTGUID(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TGUID', True, True);

  with Result do
  begin
    AddField('D1', System.TypeInfo(LongWord));
    AddField('D2', System.TypeInfo(Word));
    AddField('D3', System.TypeInfo(Word));
    AddField('D4', '$1');

    Complete;
  end;
end;

{------------------------}
{ TInterfaceEntry import }
{------------------------}

function SepiImportTInterfaceEntry(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TInterfaceEntry', True, True);

  with Result do
  begin
    AddField('IID', 'TGUID');
    AddField('VTable', 'Pointer');
    AddField('IOffset', System.TypeInfo(Integer));
    AddField('ImplGetter', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------------}
{ TInterfaceTable import }
{------------------------}

function SepiImportTInterfaceTable(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TInterfaceTable', True, True);

  with Result do
  begin
    AddField('EntryCount', System.TypeInfo(Integer));
    AddField('Entries', '$2');

    Complete;
  end;
end;

{----------------}
{ TMethod import }
{----------------}

function SepiImportTMethod(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TMethod', False, True);

  with Result do
  begin
    AddField('Code', 'Pointer');
    AddField('Data', 'Pointer', True);

    Complete;
  end;
end;

{-------------------------}
{ TDispatchMessage import }
{-------------------------}

function SepiImportTDispatchMessage(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TDispatchMessage', False, True);

  with Result do
  begin
    AddField('MsgID', System.TypeInfo(Word));

    Complete;
  end;
end;

{----------------}
{ TObject import }
{----------------}

class function TSepiImportsTObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(Owner, TypeInfo(TObject));
  TSepiMetaClass.Create(Owner, 'TClass', Result, True);

  with Result do
  begin
    AddMethod('Create', @TSepiImportsTObject.Create,
      'constructor');
    AddMethod('Free', @TSepiImportsTObject.Free,
      'procedure');
    AddMethod('InitInstance', @TSepiImportsTObject.InitInstance,
      'class function(Instance: Pointer): TObject');
    AddMethod('CleanupInstance', @TSepiImportsTObject.CleanupInstance,
      'procedure');
    AddMethod('ClassType', @TSepiImportsTObject.ClassType,
      'function: TClass');
    AddMethod('ClassName', @TSepiImportsTObject.ClassName,
      'class function: ShortString');
    AddMethod('ClassNameIs', @TSepiImportsTObject.ClassNameIs,
      'class function(const Name: string): Boolean');
    AddMethod('ClassParent', @TSepiImportsTObject.ClassParent,
      'class function: TClass');
    AddMethod('ClassInfo', @TSepiImportsTObject.ClassInfo,
      'class function: Pointer');
    AddMethod('InstanceSize', @TSepiImportsTObject.InstanceSize,
      'class function: Longint');
    AddMethod('InheritsFrom', @TSepiImportsTObject.InheritsFrom,
      'class function(AClass: TClass): Boolean');
    AddMethod('MethodAddress', @TSepiImportsTObject.MethodAddress,
      'class function(const Name: ShortString): Pointer');
    AddMethod('MethodName', @TSepiImportsTObject.MethodName,
      'class function(Address: Pointer): ShortString');
    AddMethod('FieldAddress', @TSepiImportsTObject.FieldAddress,
      'function(const Name: ShortString): Pointer');
    AddMethod('GetInterface', @TSepiImportsTObject.GetInterface,
      'function(const IID: TGUID; out Obj): Boolean');
    AddMethod('GetInterfaceEntry', @TSepiImportsTObject.GetInterfaceEntry,
      'class function(const IID: TGUID): PInterfaceEntry');
    AddMethod('GetInterfaceTable', @TSepiImportsTObject.GetInterfaceTable,
      'class function: PInterfaceTable');
    AddMethod('SafeCallException', @TSepiImportsTObject.SafeCallException,
      'function(ExceptObject: TObject; ExceptAddr: Pointer ) : HResult',
      mlkVirtual);
    AddMethod('AfterConstruction', @TSepiImportsTObject.AfterConstruction,
      'procedure',
      mlkVirtual);
    AddMethod('BeforeDestruction', @TSepiImportsTObject.BeforeDestruction,
      'procedure',
      mlkVirtual);
    AddMethod('Dispatch', @TSepiImportsTObject.Dispatch,
      'procedure(var Message)',
      mlkVirtual);
    AddMethod('DefaultHandler', @TSepiImportsTObject.DefaultHandler,
      'procedure(var Message)',
      mlkVirtual);
    AddMethod('NewInstance', @TSepiImportsTObject.NewInstance,
      'class function: TObject',
      mlkVirtual);
    AddMethod('FreeInstance', @TSepiImportsTObject.FreeInstance,
      'procedure',
      mlkVirtual);
    AddMethod('Destroy', @TSepiImportsTObject.Destroy,
      'destructor',
      mlkVirtual);

    Complete;
  end;
end;

{-------------------}
{ IInterface import }
{-------------------}

function SepiImportIInterface(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInterface));

  with Result do
  begin
    AddMethod('QueryInterface',
      'function(const IID: TGUID; out Obj): HResult', ccStdCall);
    AddMethod('_AddRef',
      'function: Integer', ccStdCall);
    AddMethod('_Release',
      'function: Integer', ccStdCall);

    Complete;
  end;
end;

{-------------------}
{ IInvokable import }
{-------------------}

function SepiImportIInvokable(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IInvokable));

  with Result do
  begin

    Complete;
  end;
end;

{------------------}
{ IDispatch import }
{------------------}

function SepiImportIDispatch(Owner: TSepiUnit): TSepiInterface;
begin
  Result := TSepiInterface.RegisterTypeInfo(
    Owner, TypeInfo(IDispatch));

  with Result do
  begin
    AddMethod('GetTypeInfoCount',
      'function(out Count: Integer): HResult', ccStdCall);
    AddMethod('GetTypeInfo',
      'function(Index, LocaleID: Integer; out TypeInfo): HResult', ccStdCall);
    AddMethod('GetIDsOfNames',
      'function(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer ; DispIDs: Pointer ) : HResult',
      ccStdCall);
    AddMethod('Invoke',
      'function(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word ; var Params ; VarResult, ExcepInfo, ArgErr: Pointer ) : HResult', ccStdCall);

    Complete;
  end;
end;

{--------------------------}
{ TInterfacedObject import }
{--------------------------}

class function TSepiImportsTInterfacedObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TInterfacedObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    CurrentVisibility := mvProtected;

    AddField('FRefCount', System.TypeInfo(Integer));

    AddMethod('QueryInterface', @TSepiImportsTInterfacedObject.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTInterfacedObject._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTInterfacedObject._Release,
      'function: Integer',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('AfterConstruction',
      @TSepiImportsTInterfacedObject.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('BeforeDestruction',
      @TSepiImportsTInterfacedObject.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('NewInstance', @TSepiImportsTInterfacedObject.NewInstance,
      'class function: TObject',
      mlkOverride);

    AddProperty('RefCount', 'property: Integer',
      'FRefCount', '');

    Complete;
  end;
end;

{--------------------------}
{ TAggregatedObject import }
{--------------------------}

function TSepiImportsTAggregatedObject.GetController: IInterface;
begin
  Result := Controller;
end;

class function TSepiImportsTAggregatedObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TAggregatedObject));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FController', 'Pointer');

    AddMethod('GetController', @TSepiImportsTAggregatedObject.GetController,
      'function: IInterface');

    CurrentVisibility := mvProtected;

    AddMethod('QueryInterface', @TSepiImportsTAggregatedObject.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      ccStdCall);
    AddMethod('_AddRef', @TSepiImportsTAggregatedObject._AddRef,
      'function: Integer',
      ccStdCall);
    AddMethod('_Release', @TSepiImportsTAggregatedObject._Release,
      'function: Integer',
      ccStdCall);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTAggregatedObject.Create,
      'constructor(const Controller: IInterface)');

    AddProperty('Controller', 'property: IInterface',
      'GetController', '');

    Complete;
  end;
end;

{-------------------------}
{ TContainedObject import }
{-------------------------}

class function TSepiImportsTContainedObject.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TContainedObject));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    CurrentVisibility := mvProtected;

    AddMethod('QueryInterface', @TSepiImportsTContainedObject.QueryInterface,
      'function(const IID: TGUID; out Obj): HResult',
      mlkVirtual, False, 0, ccStdCall);

    Complete;
  end;
end;

{-------------------------}
{ TClassHelperBase import }
{-------------------------}

class function TSepiImportsTClassHelperBase.SepiImport(
  Owner: TSepiUnit): TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TClassHelperBase));

  with Result do
  begin
    AddInterface(System.TypeInfo(IInterface));

    CurrentVisibility := mvProtected;

    AddField('FInstance', System.TypeInfo(TObject));

    AddMethod('_Create', @TSepiImportsTClassHelperBase._Create,
      'constructor(Instance: TObject)');

    Complete;
  end;
end;

{-----------------------}
{ TVarArrayBound import }
{-----------------------}

function SepiImportTVarArrayBound(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TVarArrayBound', True, True);

  with Result do
  begin
    AddField('ElementCount', System.TypeInfo(Integer));
    AddField('LowBound', System.TypeInfo(Integer));

    Complete;
  end;
end;

{------------------}
{ TVarArray import }
{------------------}

function SepiImportTVarArray(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TVarArray', True, True);

  with Result do
  begin
    AddField('DimCount', System.TypeInfo(Word));
    AddField('Flags', System.TypeInfo(Word));
    AddField('ElementSize', System.TypeInfo(Integer));
    AddField('LockCount', System.TypeInfo(Integer));
    AddField('Data', 'Pointer');
    AddField('Bounds', 'TVarArrayBoundArray');

    Complete;
  end;
end;

{-----------------}
{ TVarData import }
{-----------------}

function SepiImportTVarData(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TVarData', True, True);

  with Result do
  begin
    AddFieldAfter('VType', System.TypeInfo(TVarType), '');
    AddFieldAfter('Reserved1', System.TypeInfo(Word), 'VType');
    AddFieldAfter('Reserved2', System.TypeInfo(Word), 'Reserved1');
    AddField('Reserved3', System.TypeInfo(Word), True);
    AddFieldAfter('VSmallInt', System.TypeInfo(Smallint), 'Reserved3');
    AddFieldAfter('VInteger', System.TypeInfo(Integer), 'Reserved3');
    AddFieldAfter('VSingle', System.TypeInfo(Single), 'Reserved3');
    AddFieldAfter('VDouble', System.TypeInfo(Double), 'Reserved3');
    AddFieldAfter('VCurrency', System.TypeInfo(Currency), 'Reserved3');
    AddFieldAfter('VDate', System.TypeInfo(TDateTime), 'Reserved3');
    AddFieldAfter('VOleStr', 'PWideChar', 'Reserved3');
    AddFieldAfter('VDispatch', 'Pointer', 'Reserved3');
    AddFieldAfter('VError', System.TypeInfo(HRESULT), 'Reserved3');
    AddFieldAfter('VBoolean', System.TypeInfo(WordBool), 'Reserved3');
    AddFieldAfter('VUnknown', 'Pointer', 'Reserved3');
    AddFieldAfter('VShortInt', System.TypeInfo(Shortint), 'Reserved3');
    AddFieldAfter('VByte', System.TypeInfo(Byte), 'Reserved3');
    AddFieldAfter('VWord', System.TypeInfo(Word), 'Reserved3');
    AddFieldAfter('VLongWord', System.TypeInfo(LongWord), 'Reserved3');
    AddFieldAfter('VInt64', System.TypeInfo(Int64), 'Reserved3');
    AddFieldAfter('VString', 'Pointer', 'Reserved3');
    AddFieldAfter('VAny', 'Pointer', 'Reserved3');
    AddFieldAfter('VArray', 'PVarArray', 'Reserved3');
    AddFieldAfter('VPointer', 'Pointer', 'Reserved3');
    AddFieldAfter('VLongs', '$3', 'Reserved1');
    AddFieldAfter('VWords', '$4', 'VType');
    AddFieldAfter('VBytes', '$5', 'VType');
    AddFieldAfter('RawData', '$6', '');

    Complete;
  end;
end;

{------------------}
{ TCallDesc import }
{------------------}

function SepiImportTCallDesc(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TCallDesc', True, True);

  with Result do
  begin
    AddField('CallType', System.TypeInfo(Byte));
    AddField('ArgCount', System.TypeInfo(Byte));
    AddField('NamedArgCount', System.TypeInfo(Byte));
    AddField('ArgTypes', '$7');

    Complete;
  end;
end;

{------------------}
{ TDispDesc import }
{------------------}

function SepiImportTDispDesc(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TDispDesc', True, True);

  with Result do
  begin
    AddField('DispID', System.TypeInfo(Integer));
    AddField('ResType', System.TypeInfo(Byte));
    AddField('CallDesc', 'TCallDesc');

    Complete;
  end;
end;

{-----------------}
{ TFileRec import }
{-----------------}

function SepiImportTFileRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TFileRec', True, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(Integer));
    AddField('Mode', System.TypeInfo(Word));
    AddField('Flags', System.TypeInfo(Word));
    AddFieldAfter('RecSize', System.TypeInfo(Cardinal), 'Flags');
    AddFieldAfter('BufSize', System.TypeInfo(Cardinal), 'Flags');
    AddFieldAfter('BufPos', System.TypeInfo(Cardinal), 'BufSize');
    AddFieldAfter('BufEnd', System.TypeInfo(Cardinal), 'BufPos');
    AddFieldAfter('BufPtr', 'PChar', 'BufEnd');
    AddFieldAfter('OpenFunc', 'Pointer', 'BufPtr');
    AddFieldAfter('InOutFunc', 'Pointer', 'OpenFunc');
    AddFieldAfter('FlushFunc', 'Pointer', 'InOutFunc');
    AddFieldAfter('CloseFunc', 'Pointer', 'FlushFunc');
    AddFieldAfter('UserData', '$8', 'CloseFunc');
    AddFieldAfter('Name', '$9', 'UserData');

    Complete;
  end;
end;

{-----------------}
{ TTextRec import }
{-----------------}

function SepiImportTTextRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TTextRec', True, True);

  with Result do
  begin
    AddField('Handle', System.TypeInfo(Integer));
    AddField('Mode', System.TypeInfo(Word));
    AddField('Flags', System.TypeInfo(Word));
    AddField('BufSize', System.TypeInfo(Cardinal));
    AddField('BufPos', System.TypeInfo(Cardinal));
    AddField('BufEnd', System.TypeInfo(Cardinal));
    AddField('BufPtr', 'PChar');
    AddField('OpenFunc', 'Pointer');
    AddField('InOutFunc', 'Pointer');
    AddField('FlushFunc', 'Pointer');
    AddField('CloseFunc', 'Pointer');
    AddField('UserData', '$10');
    AddField('Name', '$11');
    AddField('Buffer', 'TTextBuf');

    Complete;
  end;
end;

{----------------------}
{ TResStringRec import }
{----------------------}

function SepiImportTResStringRec(Owner: TSepiUnit): TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TResStringRec', True, True);

  with Result do
  begin
    AddField('Module', '$12');
    AddField('Identifier', System.TypeInfo(Integer));

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

procedure Write(const S: string);
begin
  System.Write(S);
end;

procedure Writeln(const S: string);
begin
  System.Writeln(S);
end;

procedure Readln(var S: string);
begin
  System.Readln(S);
end;

procedure ChDir_0(const S: string);
begin
  ChDir(S);
end;

procedure ChDir_1(P: PChar);
begin
  ChDir(P);
end;

procedure MkDir_0(const S: string);
begin
  MkDir(S);
end;

procedure MkDir_1(P: PChar);
begin
  MkDir(P);
end;

procedure RmDir_0(const S: string);
begin
  RmDir(S);
end;

procedure RmDir_1(P: PChar);
begin
  RmDir(P);
end;

function Random_0(const ARange: Integer): Integer;
begin
  Result := Random(ARange);
end;

function Random_1: Extended;
begin
  Result := Random;
end;

function UnicodeToUtf8_0(Dest: PChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := UnicodeToUtf8(Dest, MaxDestBytes, Source, SourceChars);
end;

function Utf8ToUnicode_0(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PChar; SourceBytes: Cardinal): Cardinal;
begin
  Result := Utf8ToUnicode(Dest, MaxDestChars, Source, SourceBytes);
end;

function Pos_0(const substr, str: AnsiString): Integer;
begin
  Result := Pos(substr, str);
end;

function Pos_1(const substr, str: WideString): Integer;
begin
  Result := Pos(substr, str);
end;

function StringOfChar_0(ch: AnsiChar; Count: Integer): AnsiString;
begin
  Result := StringOfChar(ch, Count);
end;

function StringOfChar_1(ch: WideChar; Count: Integer): WideString;
begin
  Result := StringOfChar(ch, Count);
end;

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, SystemUnitName, []);

  // Integer types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Integer));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Cardinal));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Shortint));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Smallint));
  TSepiTypeAlias.Create(Result, 'Longint', TypeInfo(Longint));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Int64));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Byte));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Word));
  TSepiTypeAlias.Create(Result, 'Longword', TypeInfo(LongWord));

  // Character types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Char));
  TSepiTypeAlias.Create(Result, 'AnsiChar', TypeInfo(AnsiChar));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideChar));

  // Boolean types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Boolean));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ByteBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WordBool));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(LongBool));

  // Float types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Single));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Double));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Extended));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Comp));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Currency));

  // String types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(string));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(ShortString));
  TSepiTypeAlias.Create(Result, 'AnsiString', TypeInfo(AnsiString));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(WideString));

  // Variant types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(Variant));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(OleVariant));

  // Pointer types
  TSepiPointerType.Create(Result, 'Pointer', TSepiType(nil), True);
  TSepiPointerType.Create(Result, 'PChar', TypeInfo(Char), True);
  TSepiPointerType.Create(Result, 'PAnsiChar', TypeInfo(AnsiChar), True);
  TSepiPointerType.Create(Result, 'PWideChar', TypeInfo(WideChar), True);

  { System constants }
  TSepiConstant.Create(Result, 'RTLVersion', RTLVersion);
  TSepiConstant.Create(Result, 'CompilerVersion', CompilerVersion);
  {$IF DECLARED(GPL)}
  TSepiConstant.Create(Result, 'GPL', GPL);
  {$IFEND}
  TSepiConstant.Create(Result, 'True', True);
  TSepiConstant.Create(Result, 'False', False);
  TSepiConstant.Create(Result, 'MaxInt', MaxInt);
  TSepiConstant.Create(Result, 'MaxLongint', MaxLongint);
  { The pseudo-constant nil isn't declared here, for it has many different
    types, depending on the situation. Each compiler should understand the nil
    value for what it is: a special value, not a simple constant. }

  { Variant type codes }
  TSepiConstant.Create(Result, 'varEmpty', varEmpty);
  TSepiConstant.Create(Result, 'varNull', varNull);
  TSepiConstant.Create(Result, 'varSmallint', varSmallint);
  TSepiConstant.Create(Result, 'varInteger', varInteger);
  TSepiConstant.Create(Result, 'varSingle', varSingle);
  TSepiConstant.Create(Result, 'varDouble', varDouble);
  TSepiConstant.Create(Result, 'varCurrency', varCurrency);
  TSepiConstant.Create(Result, 'varDate', varDate);
  TSepiConstant.Create(Result, 'varOleStr', varOleStr);
  TSepiConstant.Create(Result, 'varDispatch', varDispatch);
  TSepiConstant.Create(Result, 'varError', varError);
  TSepiConstant.Create(Result, 'varBoolean', varBoolean);
  TSepiConstant.Create(Result, 'varVariant', varVariant);
  TSepiConstant.Create(Result, 'varUnknown', varUnknown);
  TSepiConstant.Create(Result, 'varShortInt', varShortInt);
  TSepiConstant.Create(Result, 'varByte', varByte);
  TSepiConstant.Create(Result, 'varWord', varWord);
  TSepiConstant.Create(Result, 'varLongWord', varLongWord);
  TSepiConstant.Create(Result, 'varInt64', varInt64);

  TSepiConstant.Create(Result, 'varStrArg', varStrArg);
  TSepiConstant.Create(Result, 'varString', varString);
  TSepiConstant.Create(Result, 'varAny', varAny);

  TSepiConstant.Create(Result, 'varTypeMask', varTypeMask);
  TSepiConstant.Create(Result, 'varArray', varArray);
  TSepiConstant.Create(Result, 'varByRef', varByRef);

  { TVarRec.VType values }
  TSepiConstant.Create(Result, 'vtInteger', vtInteger);
  TSepiConstant.Create(Result, 'vtBoolean', vtBoolean);
  TSepiConstant.Create(Result, 'vtChar', vtChar);
  TSepiConstant.Create(Result, 'vtExtended', vtExtended);
  TSepiConstant.Create(Result, 'vtString', vtString);
  TSepiConstant.Create(Result, 'vtPointer', vtPointer);
  TSepiConstant.Create(Result, 'vtPChar', vtPChar);
  TSepiConstant.Create(Result, 'vtObject', vtObject);
  TSepiConstant.Create(Result, 'vtClass', vtClass);
  TSepiConstant.Create(Result, 'vtWideChar', vtWideChar);
  TSepiConstant.Create(Result, 'vtPWideChar', vtPWideChar);
  TSepiConstant.Create(Result, 'vtAnsiString', vtAnsiString);
  TSepiConstant.Create(Result, 'vtCurrency', vtCurrency);
  TSepiConstant.Create(Result, 'vtVariant', vtVariant);
  TSepiConstant.Create(Result, 'vtInterface', vtInterface);
  TSepiConstant.Create(Result, 'vtWideString', vtWideString);
  TSepiConstant.Create(Result, 'vtInt64', vtInt64);

  { Virtual method table entries }
  TSepiConstant.Create(Result, 'vmtSelfPtr', vmtSelfPtr);
  TSepiConstant.Create(Result, 'vmtIntfTable', vmtIntfTable);
  TSepiConstant.Create(Result, 'vmtAutoTable', vmtAutoTable);
  TSepiConstant.Create(Result, 'vmtInitTable', vmtInitTable);
  TSepiConstant.Create(Result, 'vmtTypeInfo', vmtTypeInfo);
  TSepiConstant.Create(Result, 'vmtFieldTable', vmtFieldTable);
  TSepiConstant.Create(Result, 'vmtMethodTable', vmtMethodTable);
  TSepiConstant.Create(Result, 'vmtDynamicTable', vmtDynamicTable);
  TSepiConstant.Create(Result, 'vmtClassName', vmtClassName);
  TSepiConstant.Create(Result, 'vmtInstanceSize', vmtInstanceSize);
  TSepiConstant.Create(Result, 'vmtParent', vmtParent);

  { Types declared in System.pas }
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(HRESULT));
  TSepiPointerType.Create(Result, 'PGUID', 'TGUID', True);
  TSepiStaticArrayType.Create(Result, '$1', TypeInfo(Integer),
    0, 7, TypeInfo(Byte), True);
  SepiImportTGUID(Result);
  TSepiPointerType.Create(Result, 'PInterfaceEntry', 'TInterfaceEntry', True);
  SepiImportTInterfaceEntry(Result);
  TSepiPointerType.Create(Result, 'PInterfaceTable', 'TInterfaceTable', True);
  TSepiStaticArrayType.Create(Result, '$2', 'Integer',
    0, 9999, 'TInterfaceEntry', True);
  SepiImportTInterfaceTable(Result);
  SepiImportTMethod(Result);
  SepiImportTDispatchMessage(Result);

  { TObject class }
  TSepiImportsTObject.SepiImport(Result);

  { Result code constants }
  TSepiConstant.Create(Result, 'S_OK', S_OK);
  TSepiConstant.Create(Result, 'S_FALSE', S_FALSE);
  TSepiConstant.Create(Result, 'E_NOINTERFACE',
    E_NOINTERFACE, TypeInfo(HRESULT));
  TSepiConstant.Create(Result, 'E_UNEXPECTED', E_UNEXPECTED,
    TypeInfo(HRESULT));
  TSepiConstant.Create(Result, 'E_NOTIMPL', E_NOTIMPL, TypeInfo(HRESULT));

  { Write, Writeln and Readln routines}
  TSepiMethod.Create(Result, 'Write', @Write,
    'procedure(const S: string)');
  TSepiMethod.Create(Result, 'Writeln', @Writeln,
    'procedure(const S: string)');
  TSepiMethod.Create(Result, 'Readln', @Readln,
    'procedure(var S: string)');

  { Classes and interfaces }
  SepiImportIInterface(Result);
  TSepiTypeAlias.Create(Result, 'IUnknown', TypeInfo(IInterface));
  SepiImportIInvokable(Result);
  SepiImportIDispatch(Result);
  TSepiImportsTInterfacedObject.SepiImport(Result);
  TSepiMetaClass.Create(Result, 'TInterfacedClass',
    TypeInfo(TInterfacedObject), True);
  TSepiImportsTAggregatedObject.SepiImport(Result);
  TSepiImportsTContainedObject.SepiImport(Result);
  TSepiImportsTClassHelperBase.SepiImport(Result);

  { Additionnal types }
  TSepiPointerType.Create(Result, 'PShortString', TypeInfo(ShortString), True);
  TSepiPointerType.Create(Result, 'PAnsiString', TypeInfo(AnsiString), True);
  TSepiPointerType.Create(Result, 'PWideString', TypeInfo(WideString), True);
  TSepiTypeAlias.Create(Result, 'PString', 'PAnsiString');
  TSepiTypeAlias.Create(Result, 'UCS2Char', TypeInfo(WideChar));
  TSepiTypeAlias.Create(Result, 'PUCS2Char', 'PWideChar');
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(UCS4Char));
  TSepiPointerType.Create(Result, 'PUCS4Char', TypeInfo(UCS4Char), True);
  TSepiStaticArrayType.Create(Result, 'TUCS4CharArray', TypeInfo(Integer),
    0, $effffff, TypeInfo(UCS4Char), True);
  TSepiPointerType.Create(Result, 'PUCS4CharArray', 'TUCS4CharArray', True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(UCS4String));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(UTF8String));
  TSepiPointerType.Create(Result, 'PUTF8String', TypeInfo(UTF8String), True);
  TSepiStaticArrayType.Create(Result, 'IntegerArray', TypeInfo(Integer),
    0, $effffff, TypeInfo(Integer), True);
  TSepiPointerType.Create(Result, 'PIntegerArray', 'IntegerArray', True);
  TSepiStaticArrayType.Create(Result, 'PointerArray', 'Integer',
    0, 512*1024*1024 - 2, 'Pointer', True);
  TSepiPointerType.Create(Result, 'PPointerArray', 'PointerArray', True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TBoundArray));
  TSepiStaticArrayType.Create(Result, 'TPCharArray', 'Integer',
    0, (MaxLongint div SizeOf(PChar))-1, 'PChar', True);
  TSepiPointerType.Create(Result, 'PPCharArray', 'TPCharArray', True);
  TSepiPointerType.Create(Result, 'PLongint', TypeInfo(Longint), True);
  TSepiPointerType.Create(Result, 'PInteger', TypeInfo(Integer), True);
  TSepiPointerType.Create(Result, 'PCardinal', TypeInfo(Cardinal), True);
  TSepiPointerType.Create(Result, 'PWord', TypeInfo(Word), True);
  TSepiPointerType.Create(Result, 'PSmallInt', TypeInfo(Smallint), True);
  TSepiPointerType.Create(Result, 'PByte', TypeInfo(Byte), True);
  TSepiPointerType.Create(Result, 'PShortInt', TypeInfo(Shortint), True);
  TSepiPointerType.Create(Result, 'PInt64', TypeInfo(Int64), True);
  TSepiPointerType.Create(Result, 'PLongWord', TypeInfo(LongWord), True);
  TSepiPointerType.Create(Result, 'PSingle', TypeInfo(Single), True);
  TSepiPointerType.Create(Result, 'PDouble', TypeInfo(Double), True);
  TSepiPointerType.Create(Result, 'PDate', TypeInfo(Double), True);
  TSepiPointerType.Create(Result, 'PDispatch', TypeInfo(IDispatch), True);
  TSepiPointerType.Create(Result, 'PPDispatch', 'PDispatch', True);
  TSepiPointerType.Create(Result, 'PError', TypeInfo(LongWord), True);
  TSepiPointerType.Create(Result, 'PWordBool', TypeInfo(WordBool), True);
  TSepiPointerType.Create(Result, 'PUnknown', TypeInfo(IUnknown), True);
  TSepiPointerType.Create(Result, 'PPUnknown', 'PUnknown', True);
  TSepiPointerType.Create(Result, 'PPWideChar', 'PWideChar', True);
  TSepiPointerType.Create(Result, 'PPChar', 'PChar', True);
  TSepiTypeAlias.Create(Result, 'PPAnsiChar', 'PPChar');
  TSepiPointerType.Create(Result, 'PExtended', TypeInfo(Extended), True);
  TSepiPointerType.Create(Result, 'PComp', TypeInfo(Comp), True);
  TSepiPointerType.Create(Result, 'PCurrency', TypeInfo(Currency), True);
  TSepiPointerType.Create(Result, 'PVariant', TypeInfo(Variant), True);
  TSepiPointerType.Create(Result, 'POleVariant', TypeInfo(OleVariant), True);
  TSepiPointerType.Create(Result, 'PPointer', 'Pointer', True);
  TSepiPointerType.Create(Result, 'PBoolean', TypeInfo(Boolean), True);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDateTime));
  TSepiPointerType.Create(Result, 'PDateTime', TypeInfo(TDateTime), True);
  TSepiTypeAlias.Create(Result, 'THandle', TypeInfo(LongWord));
  SepiImportTVarArrayBound(Result);
  TSepiStaticArrayType.Create(Result, 'TVarArrayBoundArray', 'Integer',
    0, 0, 'TVarArrayBound', True);
  TSepiPointerType.Create(Result, 'PVarArrayBoundArray',
    'TVarArrayBoundArray', True);
  TSepiStaticArrayType.Create(Result, 'TVarArrayCoorArray', TypeInfo(Integer),
    0, 0, TypeInfo(Integer), True);
  TSepiPointerType.Create(Result, 'PVarArrayCoorArray',
    'TVarArrayCoorArray', True);
  TSepiPointerType.Create(Result, 'PVarArray', 'TVarArray', True);
  SepiImportTVarArray(Result);
  TSepiTypeAlias.Create(Result, 'TVarType', TypeInfo(Word));
  TSepiPointerType.Create(Result, 'PVarData', 'TVarData', True);
  TSepiStaticArrayType.Create(Result, '$3', TypeInfo(Integer),
    0, 2, TypeInfo(Longint), True);
  TSepiStaticArrayType.Create(Result, '$4', TypeInfo(Integer),
    0, 6, TypeInfo(Word), True);
  TSepiStaticArrayType.Create(Result, '$5', TypeInfo(Integer),
    0, 13, TypeInfo(Byte), True);
  TSepiStaticArrayType.Create(Result, '$6', TypeInfo(Integer),
    0, 3, TypeInfo(Longint), True);
  SepiImportTVarData(Result);
  TSepiTypeAlias.Create(Result, 'TVarOp', TypeInfo(Integer));

  { Operations values }
  TSepiConstant.Create(Result, 'opAdd', opAdd);
  TSepiConstant.Create(Result, 'opSubtract', opSubtract);
  TSepiConstant.Create(Result, 'opMultiply', opMultiply);
  TSepiConstant.Create(Result, 'opDivide', opDivide);
  TSepiConstant.Create(Result, 'opIntDivide', opIntDivide);
  TSepiConstant.Create(Result, 'opModulus', opModulus);
  TSepiConstant.Create(Result, 'opShiftLeft', opShiftLeft);
  TSepiConstant.Create(Result, 'opShiftRight', opShiftRight);
  TSepiConstant.Create(Result, 'opAnd', opAnd);
  TSepiConstant.Create(Result, 'opOr', opOr);
  TSepiConstant.Create(Result, 'opXor', opXor);
  TSepiConstant.Create(Result, 'opCompare', opCompare);
  TSepiConstant.Create(Result, 'opNegate', opNegate);
  TSepiConstant.Create(Result, 'opNot', opNot);
  TSepiConstant.Create(Result, 'opCmpEQ', opCmpEQ);
  TSepiConstant.Create(Result, 'opCmpNE', opCmpNE);
  TSepiConstant.Create(Result, 'opCmpLT', opCmpLT);
  TSepiConstant.Create(Result, 'opCmpLE', opCmpLE);
  TSepiConstant.Create(Result, 'opCmpGT', opCmpGT);
  TSepiConstant.Create(Result, 'opCmpGE', opCmpGE);

  // Types
  TSepiPointerType.Create(Result, 'PCallDesc', 'TCallDesc', True);
  TSepiStaticArrayType.Create(Result, '$7', TypeInfo(Integer),
    0, 255, TypeInfo(Byte), True);
  SepiImportTCallDesc(Result);
  TSepiPointerType.Create(Result, 'PDispDesc', 'TDispDesc', True);
  SepiImportTDispDesc(Result);

  // Routines
  TSepiMethod.Create(Result, 'AcquireExceptionObject', @AcquireExceptionObject,
    'function: Pointer');
  TSepiMethod.Create(Result, 'ReleaseExceptionObject', @ReleaseExceptionObject,
    'procedure');
  TSepiMethod.Create(Result, 'ExceptObject', @ExceptObject,
    'function: TObject');
  TSepiMethod.Create(Result, 'ExceptAddr', @ExceptAddr,
    'function: Pointer');

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTextLineBreakStyle));

  // Global variables
  TSepiVariable.Create(Result, 'DefaultTextLineBreakStyle',
    DefaultTextLineBreakStyle, TypeInfo(TTextLineBreakStyle));

  // Constants
  TSepiConstant.Create(Result, 'sLineBreak', sLineBreak);

  // Types
  TSepiTypeAlias.Create(Result, 'HRSRC', TypeInfo(THandle));
  TSepiTypeAlias.Create(Result, 'TResourceHandle', TypeInfo(HRSRC));
  TSepiTypeAlias.Create(Result, 'HINST', TypeInfo(THandle));
  TSepiTypeAlias.Create(Result, 'HMODULE', TypeInfo(HINST));
  TSepiTypeAlias.Create(Result, 'HGLOBAL', TypeInfo(THandle));

  // Constants
  TSepiConstant.Create(Result, 'fmClosed', fmClosed);
  TSepiConstant.Create(Result, 'fmInput', fmInput);
  TSepiConstant.Create(Result, 'fmOutput', fmOutput);
  TSepiConstant.Create(Result, 'fmInOut', fmInOut);

  // Types
  TSepiStaticArrayType.Create(Result, '$8', TypeInfo(Integer),
    1, 32, TypeInfo(Byte), True);
  TSepiStaticArrayType.Create(Result, '$9', TypeInfo(Integer),
    0, 259, TypeInfo(Char), True);
  SepiImportTFileRec(Result);
  TSepiPointerType.Create(Result, 'PTextBuf', 'TTextBuf', True);
  TSepiStaticArrayType.Create(Result, 'TTextBuf', TypeInfo(Integer),
    0, 127, TypeInfo(Char), True);
  TSepiStaticArrayType.Create(Result, '$10', TypeInfo(Integer),
    1, 32, TypeInfo(Byte), True);
  TSepiStaticArrayType.Create(Result, '$11', TypeInfo(Integer),
    0, 259, TypeInfo(Char), True);
  SepiImportTTextRec(Result);
  TSepiMethodRefType.Create(Result, 'TTextIOFunc',
    'function(var F: TTextRec): Integer');
  TSepiMethodRefType.Create(Result, 'TFileIOFunc',
    'function(var F: TFileRec): Integer');

  // Routines
  TSepiMethod.CreateOverloaded(Result, 'ChDir', @ChDir_0,
    'procedure(const S: string)');
  TSepiMethod.CreateOverloaded(Result, 'ChDir', @ChDir_1,
    'procedure(P: PChar)');
  TSepiMethod.Create(Result, 'IOResult', @IOResult,
    'function: Integer');
  TSepiMethod.CreateOverloaded(Result, 'MkDir', @MkDir_0,
    'procedure(const S: string)');
  TSepiMethod.CreateOverloaded(Result, 'MkDir', @MkDir_1,
    'procedure(P: PChar)');
  TSepiMethod.Create(Result, 'Move', @Move,
    'procedure(const Source; var Dest; Count: Integer)');
  TSepiMethod.Create(Result, 'ParamCount', @ParamCount,
    'function: Integer');
  TSepiMethod.Create(Result, 'ParamStr', @ParamStr,
    'function(Index: Integer): string');
  TSepiMethod.CreateOverloaded(Result, 'RmDir', @RmDir_0,
    'procedure(const S: string)');
  TSepiMethod.CreateOverloaded(Result, 'RmDir', @RmDir_1,
    'procedure(P: PChar)');
  TSepiMethod.Create(Result, 'UpCase', @UpCase,
    'function(Ch: Char): Char');
  TSepiMethod.Create(Result, 'Randomize', @Randomize,
    'procedure');
  TSepiMethod.CreateOverloaded(Result, 'Random', @Random_0,
    'function(const ARange: Integer): Integer');
  TSepiMethod.CreateOverloaded(Result, 'Random', @Random_1,
    'function: Extended');
  TSepiMethod.Create(Result, 'WideCharToString', @WideCharToString,
    'function(Source: PWideChar): string');
  TSepiMethod.Create(Result, 'WideCharLenToString', @WideCharLenToString,
    'function(Source: PWideChar; SourceLen: Integer): string');
  TSepiMethod.Create(Result, 'WideCharToStrVar', @WideCharToStrVar,
    'procedure(Source: PWideChar; var Dest: string)');
  TSepiMethod.Create(Result, 'WideCharLenToStrVar', @WideCharLenToStrVar,
    'procedure(Source: PWideChar; SourceLen: Integer; var Dest: string )');
  TSepiMethod.Create(Result, 'StringToWideChar', @StringToWideChar,
    'function(const Source: string; Dest: PWideChar; DestSize: Integer ) : PWideChar');
  TSepiMethod.Create(Result, 'PUCS4Chars', @PUCS4Chars,
    'function(const S: UCS4String): PUCS4Char');
  TSepiMethod.Create(Result, 'WideStringToUCS4String', @WideStringToUCS4String,
    'function(const S: WideString): UCS4String');
  TSepiMethod.Create(Result, 'UCS4StringToWideString', @UCS4StringToWideString,
    'function(const S: UCS4String): WideString');
  TSepiMethod.CreateOverloaded(Result, 'UnicodeToUtf8', @UnicodeToUtf8_0,
    'function(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal');
  TSepiMethod.CreateOverloaded(Result, 'Utf8ToUnicode', @Utf8ToUnicode_0,
    'function(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal');
  TSepiMethod.Create(Result, 'UTF8Encode', @UTF8Encode,
    'function(const WS: WideString): UTF8String');
  TSepiMethod.Create(Result, 'UTF8Decode', @UTF8Decode,
    'function(const S: UTF8String): WideString');
  TSepiMethod.Create(Result, 'AnsiToUtf8', @AnsiToUtf8,
    'function(const S: string): UTF8String');
  TSepiMethod.Create(Result, 'Utf8ToAnsi', @Utf8ToAnsi,
    'function(const S: UTF8String): string');
  TSepiMethod.Create(Result, 'OleStrToString', @OleStrToString,
    'function(Source: PWideChar): string');
  TSepiMethod.Create(Result, 'OleStrToStrVar', @OleStrToStrVar,
    'procedure(Source: PWideChar; var Dest: string)');
  TSepiMethod.Create(Result, 'StringToOleStr', @StringToOleStr,
    'function(const Source: string): PWideChar');

  // Types
  TSepiPointerType.Create(Result, 'PResStringRec', 'TResStringRec', True);
  TSepiPointerType.Create(Result, '$12', TypeInfo(Cardinal), True);
  SepiImportTResStringRec(Result);

  // Routines
  TSepiMethod.Create(Result, 'LoadResString', @LoadResString,
    'function(ResStringRec: PResStringRec): string');
  TSepiMethod.Create(Result, 'Int', @Int,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Frac', @Frac,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Exp', @Exp,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Cos', @Cos,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Sin', @Sin,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Ln', @Ln,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'ArcTan', @ArcTan,
    'function(const X: Extended): Extended');
  TSepiMethod.Create(Result, 'Sqrt', @Sqrt,
    'function(const X: Extended): Extended');
  TSepiMethod.CreateOverloaded(Result, 'Pos', @Pos_0,
    'function(const substr, str: AnsiString): Integer');
  TSepiMethod.CreateOverloaded(Result, 'Pos', @Pos_1,
    'function(const substr, str: WideString): Integer');
  TSepiMethod.CreateOverloaded(Result, 'StringOfChar', @StringOfChar_0,
    'function(ch: AnsiChar; Count: Integer): AnsiString');
  TSepiMethod.CreateOverloaded(Result, 'StringOfChar', @StringOfChar_1,
    'function(ch: WideChar; Count: Integer): WideString');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('System', ImportUnit);
end.

