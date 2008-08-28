unit ScInterfaces;

interface

uses
  SysUtils, ScUtils, ScConsts;

type
  {*
    Erreur liée à des interfaces liées dynamiquement
    @author sjrd
    @version 1.0
  *}
  EDynamicIntfError = class(Exception);

  {*
    Interface liable dynamiquement à un contrôleur
    Les objets implémentant cette interface peuvent être liés dynamiquement à
    un contrôleur, pour une interface identifiée donnée. Ce contrôleur
    implémentera dès ce moment l'interface identifiée, au travers de l'objet
    lié.
    Les méthodes AttachTo et DetachFrom ne doivent pas être appelées depuis le
    code qui utilise de telles interfaces, mais plutôt par l'implémentation de
    l'objet contrôleur.
    Lorsqu'un objet est liée dynamique à un contrôleur, le compteur de
    références de celui-ci doit comprendre le compteur de références de l'objet
    lié. Par ailleurs, le contrôleur doit garantir que le compteur de référence
    de l'objet lié ne tombera pas à 0 tant qu'il est lié.
    @author sjrd
    @version 1.0
  *}
  IDynamicallyLinkable = interface(IInterface)
    ['{89755CAD-5153-48CE-AC51-DE0241C3440B}']

    {*
      Attache cet objet à un contrôleur
      Cette méthode doit lier les compteurs de références de l'interface et de
      son contrôleur de telle façon que le contrôleur restera en vie tant que
      l'objet lié est en vie.
      @throws EDynamicIntfError L'objet a déjà un contrôleur
    *}
    procedure AttachTo(const Controller: IInterface);

    {*
      Détache cet objet de son contrôleur
      @throws EDynamicIntfError L'objet n'a pas de contrôleur
    *}
    procedure DetachFromController;
  end;

  {*
    Classe de base pour objets dont les interfaces seront liées dynamiquement
    @author sjrd
    @version 1.0
  *}
  TDynamicallyLinkedObject = class(TInterfacedObject, IInterface,
    IDynamicallyLinkable)
  private
    FController: Pointer; /// Référence faible au contrôleur

    function GetController: IInterface;
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure AttachTo(const Controller: IInterface); virtual;
    procedure DetachFromController; virtual;

    property Controller: IInterface read GetController;
  end;

  {*
    Élément interface dynamique d'un contrôleur
    @author sjrd
    @version 1.0
  *}
  TDynamicInterface = record
    IID: TGUID;
    Intf: IDynamicallyLinkable;
  end;

  {*
    Classe de base pour contrôleurs d'interfaces dynamiques
    @author sjrd
    @version 1.0
  *}
  TDynamicIntfController = class(TInterfacedObject, IInterface)
  private
    FDestroying: Boolean;
    FInterfaces: array of TDynamicInterface; /// Interfaces liées

    function Find(const IID: TGUID): Integer;
    procedure InternalAttach(Index: Integer; const IID: TGUID;
      const Intf: IDynamicallyLinkable);
    procedure InternalDetach(Index: Integer);

    function GetDynamicIntfCount: Integer;
    function GetDynamicInterfaces(Index: Integer): TGUID;
  protected
    function QueryInterface(const IID: TGUID;
      out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure Attach(const IID: TGUID; const Intf: IDynamicallyLinkable);
    procedure Detach(const IID: TGUID);

    property Destroying: Boolean read FDestroying;
    property DynamicIntfCount: Integer read GetDynamicIntfCount;
    property DynamicInterfaces[Index: Integer]: TGUID read GetDynamicInterfaces;
  public
    procedure BeforeDestruction; override;
  end;

implementation

{--------------------------------}
{ TDynamicallyLinkedObject class }
{--------------------------------}

{*
  Contrôleur
  @return Contrôleur
*}
function TDynamicallyLinkedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

{*
  [@inheritDoc]
*}
function TDynamicallyLinkedObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> 0) and (FController <> nil) then
    Result := IInterface(FController).QueryInterface(IID, Obj);
end;

{*
  [@inheritDoc]
*}
function TDynamicallyLinkedObject._AddRef: Integer;
begin
  Result := inherited _AddRef;

  if FController <> nil then
    IInterface(FController)._AddRef;
end;

{*
  [@inheritDoc]
*}
function TDynamicallyLinkedObject._Release: Integer;
begin
  Result := inherited _Release;

  if FController <> nil then
    IInterface(FController)._Release;
end;

{*
  [@inheritDoc]
*}
procedure TDynamicallyLinkedObject.AttachTo(const Controller: IInterface);
var
  I: Integer;
begin
  if FController <> nil then
    raise EDynamicIntfError.Create(SAlreadyHasController);

  FController := Pointer(Controller);

  for I := RefCount-1 downto 0 do
    Controller._AddRef;
end;

{*
  [@inheritDoc]
*}
procedure TDynamicallyLinkedObject.DetachFromController;
var
  I: Integer;
begin
  if FController = nil then
    raise EDynamicIntfError.Create(SHasNoController);

  for I := RefCount-1 downto 0 do
    IInterface(FController)._Release;

  FController := nil;
end;

{------------------------------}
{ TDynamicIntfController class }
{------------------------------}

{*
  Cherche un enregistrement d'interface pour un GUID donné
  @param IID     ID de l'interface recherchée
  @param Index   En sortie, si trouvé : index de l'enregistrement
  @return True si trouvé, False sinon
*}
function TDynamicIntfController.Find(const IID: TGUID): Integer;
begin
  Result := 0;

  while Result < Length(FInterfaces) do
    if SameGUID(IID, FInterfaces[Result].IID) then
      Exit;

  Result := -1;
end;

{*
  Attache une interface dynamique
  @param Index   Index où stocker l'interface dynamique
  @param IID     ID de l'interface à attacher
  @param Intf    Interface à lier
*}
procedure TDynamicIntfController.InternalAttach(Index: Integer;
  const IID: TGUID; const Intf: IDynamicallyLinkable);
begin
  FInterfaces[Index].IID := IID;
  FInterfaces[Index].Intf := Intf;

  try
    Intf.AttachTo(Self);
    _Release;
  except
    FInterfaces[Index].Intf := nil;
    raise;
  end;
end;

{*
  Détache une interface dynamique identifiée par son index
  @param Index   Index où est sockée l'interface dynamique
*}
procedure TDynamicIntfController.InternalDetach(Index: Integer);
begin
  _AddRef;
  try
    FInterfaces[Index].Intf.DetachFromController;
  except
    _Release;
    raise;
  end;

  FInterfaces[Index].Intf := nil;
end;

{*
  Nombre d'interfaces dynamiques recensées
  @return Nombre d'interfaces dynamiques recensées
*}
function TDynamicIntfController.GetDynamicIntfCount: Integer;
begin
  Result := Length(FInterfaces);
end;

{*
  Tableau zero-based des interfaces dynamiques recensées
  @param Index   Index de l'interface
  @return ID de l'interface
*}
function TDynamicIntfController.GetDynamicInterfaces(Index: Integer): TGUID;
begin
  Result := FInterfaces[Index].IID;
end;

{*
  [@inheritDoc]
*}
function TDynamicIntfController.QueryInterface(const IID: TGUID;
  out Obj): HResult;
var
  Index: Integer;
begin
  Result := inherited QueryInterface(IID, Obj);

  if Result <> 0 then
  begin
    Index := Find(IID);
    if Index >= 0 then
      Result := FInterfaces[Index].Intf.QueryInterface(IID, Obj);
  end;
end;

{*
  [@inheritDoc]
*}
function TDynamicIntfController._AddRef: Integer;
begin
  if Destroying then
    Result := 0
  else
    Result := inherited _AddRef;
end;

{*
  [@inheritDoc]
*}
function TDynamicIntfController._Release: Integer;
begin
  if Destroying then
    Result := 0
  else
    Result := inherited _Release;
end;

{*
  Attache une interface dynamique
  @param IID    ID de l'interface à attacher
  @param Intf   Interface à lier
*}
procedure TDynamicIntfController.Attach(const IID: TGUID;
  const Intf: IDynamicallyLinkable);
var
  Index: Integer;
begin
  if not Supports(Intf, IID) then
    Error(reIntfCastError);

  Index := Find(IID);

  if Index < 0 then
  begin
    Index := Length(FInterfaces);
    SetLength(FInterfaces, Index+1);
  end else
  begin
    if FInterfaces[Index].Intf = Intf then
      Exit;

    InternalDetach(Index);
  end;

  InternalAttach(Index, IID, Intf);
end;

{*
  Détache une interface dynamique identifiée par son ID
  @param IID   ID de l'interface à détacher
*}
procedure TDynamicIntfController.Detach(const IID: TGUID);
var
  Index, Len: Integer;
begin
  Index := Find(IID);
  if Index < 0 then
    Exit;

  Len := Length(FInterfaces);

  InternalDetach(Index);

  Move(FInterfaces[Index+1], FInterfaces[Index],
    (Len-Index-1) * SizeOf(TDynamicInterface));

  Pointer(FInterfaces[Len-1].Intf) := nil;
  SetLength(FInterfaces, Len-1);
end;

{*
  [@inheritDoc]
*}
procedure TDynamicIntfController.BeforeDestruction;
var
  I: Integer;
begin
  inherited;

  FDestroying := True;

  for I := 0 to Length(FInterfaces)-1 do
    InternalDetach(I);
  SetLength(FInterfaces, 0);
end;

end.

