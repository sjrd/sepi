{*
  Composants de type Label
  @author sjrd
  @version 1.0
*}
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

resourcestring
  sCopyCaption = 'Copier';
  sCopyHint = 'Copier l''URL dans le presse-papier';
  sRunCaption = 'Lancer';
  sRunHint = 'Lancer l''URL';

type
  {*
    Classe de base pour les label renvoyant � une URL
    @author sjrd
    @version 1.0
  *}
  TSvCustomURLLabel = class(TCustomLabel)
  private
    FURL : string;     /// URL vers laquelle renvoyer (vide utilise Caption)
    Menu : TPopupMenu; /// Menu contextuel par d�faut

    function RealURL : string;

    procedure MenuCopyClick(Sender : TObject);
    procedure MenuRunClick(Sender : TObject);

    function IsFontStored : boolean;
    function IsPopupMenuStored : boolean;
  protected
    /// [@inheritDoc]
    property Cursor default crHandPoint;
    /// [@inheritDoc]
    property Font stored IsFontStored;
    /// [@inheritDoc]
    property PopupMenu stored IsPopupMenuStored;
    property URL : string read FURL write FURL;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Click; override;
  end platform;

  {*
    Label renvoyant � une URL
    @author sjrd
    @version 1.0
  *}
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
  end platform;

implementation

{--------------------------}
{ Classe TScCustomURLLabel }
{--------------------------}

{*
  Cr�e une instance de TSvCustomURLLabel
  @param AOwner   Propri�taire
*}
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
   Menu.Name := 'PopupMenu'; {don't localize}

   TempMenu := TMenuItem.Create(Menu);
   with TempMenu do
   begin
     Name := 'MenuCopy'; {don't localize}
     Caption := sCopyCaption;
     Hint := sCopyHint;
     OnClick := MenuCopyClick;
   end;
   Menu.Items.Add(TempMenu);

   TempMenu := TMenuItem.Create(Menu);
   with TempMenu do
   begin
     Name := 'MenuRun'; {don't localize}
     Caption := sRunCaption;
     Hint := sRunHint;
     OnClick := MenuRunClick;
   end;
   Menu.Items.Add(TempMenu);

   PopupMenu := Menu;
end;

{*
  D�truit l'instance
*}
destructor TSvCustomURLLabel.Destroy;
begin
   Menu.Free;
   inherited;
end;

{*
  D�termine l'URL exacte vers laquelle renvoyer
  @return URL exacte vers laquelle renvoyer
*}
function TSvCustomURLLabel.RealURL : string;
begin
  if URL = '' then Result := Caption else Result := URL;
end;

{*
  Ex�cut� lorsque le menu Copier a �t� s�lectionn�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TSvCustomURLLabel.MenuCopyClick(Sender : TObject);
begin
  Clipboard.AsText := RealURL;
end;

{*
  Ex�cut� lorsque le menu Lancher a �t� s�lectionn�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TSvCustomURLLabel.MenuRunClick(Sender : TObject);
begin
  Click;
end;

{*
  Indique si la fonte doit �tre stock�e dans un flux dfm
  @return True : il faut toujours sauvegarder la fonte
*}
function TSvCustomURLLabel.IsFontStored : boolean;
begin
  Result := True;
end;

{*
  Indique si le menu popup doit �tre stock� dans un flux dfm
  @return True si le menu popup est diff�rent de celui par d�faut, False sinon
*}
function TSvCustomURLLabel.IsPopupMenuStored : boolean;
begin
  Result := PopupMenu <> Menu;
end;

{*
  Ex�cut� lorsque l'utilisateur clique sur le lien
*}
procedure TSvCustomURLLabel.Click;
begin
  inherited Click;
  {$IFDEF MSWINDOWS}
    RunURL(RealURL);
  {$ENDIF}
  {$IFDEF LINUX}
    assert(False); {to-do}
  {$ENDIF}
end;

end.

