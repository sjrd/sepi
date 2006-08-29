unit SvLabels;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Graphics, StdCtrls, Menus, ClipBrd,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QGraphics, QStdCtrls, QMenus, QClipBrd,
{$ENDIF}
  Classes, ScUtils;

type
  TRunURLEvent = procedure(Sender : TObject; const URL : string) of object;

  TSvCustomURLLabel = class(TCustomLabel)
  private
    FURL : string;
    Menu : TPopupMenu;
    FOnRunURL : TRunURLEvent;
    function RealURL : string;
    procedure MenuCopyClick(Sender : TObject);
    procedure MenuRunClick(Sender : TObject);
    function IsFontStored : boolean;
    function IsPopupMenuStored : boolean;
  protected
    property Cursor default crHandPoint;
    property Font stored IsFontStored;
    property PopupMenu stored IsPopupMenuStored;
    property URL : string read FURL write FURL;

    property OnRunURL : TRunURLEvent read FOnRunURL write FOnRunURL;
      // CLX uniquement : doit lancer l'URL passée en paramètre
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end platform;

  TSvURLLabel = class(TSvCustomURLLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;

    property URL;
    property OnRunURL;
  end;

implementation

////////////////////////////////
/// Classe TScCustomURLLabel ///
////////////////////////////////

constructor TSvCustomURLLabel.Create(AOwner : TComponent);
var TempMenu : TMenuItem;
begin
   inherited Create(AOwner);
   Cursor := crHandPoint;
   with Font do
   begin
     Color := clBlue;
     Style := [fsUnderline];
   end;
   Menu := TPopupMenu.Create(Self);
   Menu.Name := 'PopupMenu';
   TempMenu := TMenuItem.Create(Menu);
   with TempMenu do
   begin
     Name := 'MenuCopy';
     Caption := 'Copier l''URL';
     Hint := 'Copier l''URL dans le presse-papier';
     OnClick := MenuCopyClick;
   end;
   Menu.Items.Add(TempMenu);
   TempMenu := TMenuItem.Create(Menu);
   with TempMenu do
   begin
     Name := 'MenuRun';
     Caption := 'Lancer';
     Hint := 'Lancer l''URL';
     OnClick := MenuRunClick;
   end;
   Menu.Items.Add(TempMenu);
   PopupMenu := Menu;
end;

function TSvCustomURLLabel.RealURL : string;
begin
  if URL = '' then Result := Caption else Result := URL;
end;

destructor TSvCustomURLLabel.Destroy;
begin
   Menu.Free;
   inherited;
end;

procedure TSvCustomURLLabel.MenuCopyClick;
begin
  Clipboard.AsText := RealURL;
end;

procedure TSvCustomURLLabel.MenuRunClick(Sender : TObject);
begin
  Click;
end;

function TSvCustomURLLabel.IsFontStored : boolean;
begin
  Result := True;
end;

function TSvCustomURLLabel.IsPopupMenuStored : boolean;
begin
  Result := PopupMenu <> Menu;
end;

procedure TSvCustomURLLabel.Click;
begin
  inherited Click;
  {$IFDEF MSWINDOWS}
    RunURL(RealURL);
  {$ENDIF}
  {$IFDEF LINUX}
    if Assigned(FOnRunURL) then FOnRunURL(Self, RealURL);
  {$ENDIF}
end;

end.

