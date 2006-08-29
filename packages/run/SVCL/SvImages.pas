unit SvImages;

interface

uses
  Windows, Forms, Classes, Controls, ExtCtrls;

type
  /// Types des évènements
  TDropImageEvent = procedure(Sender: TObject; X, Y : integer) of object;

  /// Composants
  TSvDropImage = class(TImage)
  private
    ImageBis : TImage;
    FDropControl : TControl;
    FOnDrop : TDropImageEvent;
    procedure MoveBisAt(X, Y : integer);
  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : integer); override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property DropControl : TControl read FDropControl write FDropControl default nil;
    property OnDrop : TDropImageEvent read FOnDrop write FOnDrop default nil;
  end;

implementation

///////////////////////////
/// Classe TScDropImage ///
///////////////////////////

constructor TSvDropImage.Create(AOwner : TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
  ImageBis := nil;
  FDropControl := nil;
  FOnDrop := nil;
end;

procedure TSvDropImage.MoveBisAt(X, Y : integer);
var L, T, ParentWidth, ParentHeight : integer;
begin
  if Parent is TForm then
  begin
    ParentWidth  := (Parent as TForm).ClientWidth ;
    ParentHeight := (Parent as TForm).ClientHeight;
  end else
  begin
    ParentWidth  := Parent.Width ;
    ParentHeight := Parent.Height;
  end;
  L := Left + X - Width  div 2;
  T := Top  + Y - Height div 2;
  if L > (ParentWidth -Width ) then L := ParentWidth -Width ;
  if L < 0 then L := 0;
  if T > (ParentHeight-Height) then T := ParentHeight-Height;
  if T < 0 then T := 0;
  ImageBis.Left := L;
  ImageBis.Top  := T;
end;

procedure TSvDropImage.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : integer);
begin
  inherited;
  if not Enabled then exit;
  if (Button <> mbLeft) or Assigned(ImageBis) then exit;
  ImageBis := TImage.Create(Self);
  ImageBis.Parent := Parent;
  ImageBis.Width := Width;
  ImageBis.Height := Height;
  ImageBis.Stretch := Stretch;
  ImageBis.Picture.Assign(Picture);
  MoveBisAt(X, Y);
end;

procedure TSvDropImage.MouseMove(Shift : TShiftState; X, Y : integer);
begin
  inherited;
  if Assigned(ImageBis) then MoveBisAt(X, Y);
end;

procedure TSvDropImage.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : integer);
var PutPoint : TPoint;
begin
  inherited;
  if (Button <> mbLeft) or (not Assigned(ImageBis)) then exit;
  ImageBis.Free;
  ImageBis := nil;
  if not Assigned(FOnDrop) then exit;
  if Assigned(FDropControl) then
    PutPoint := FDropControl.ScreenToClient(ClientToScreen(Point(X, Y)))
  else
    PutPoint := ClientToParent(Point(X, Y));
  FOnDrop(Self, PutPoint.X, PutPoint.Y);
end;

end.

