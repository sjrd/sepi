Around line 1413 in Delphi 2007
----------
  TCustomFormHelper = class helper for TCustomForm
  private
    function GetGlassFrame: TGlassFrame;
    procedure ReadGlassFrameBottom(Reader: TReader);
    procedure ReadGlassFrameEnabled(Reader: TReader);
    procedure ReadGlassFrameLeft(Reader: TReader);
    procedure ReadGlassFrameRight(Reader: TReader);
    procedure ReadGlassFrameSheetOfGlass(Reader: TReader);
    procedure ReadGlassFrameTop(Reader: TReader);
    procedure SetGlassFrame(const Value: TGlassFrame);
    procedure WriteGlassFrameBottom(Writer: TWriter);
    procedure WriteGlassFrameEnabled(Writer: TWriter);
    procedure WriteGlassFrameLeft(Writer: TWriter);
    procedure WriteGlassFrameRight(Writer: TWriter);
    procedure WriteGlassFrameSheetOfGlass(Writer: TWriter);
    procedure WriteGlassFrameTop(Writer: TWriter);
  public
    procedure UpdateGlassFrame(Sender: TObject);
    property GlassFrame: TGlassFrame read GetGlassFrame write SetGlassFrame;
  end;

  TApplicationHelper = class helper for TApplication
  private
    procedure SetEnumAllWindowsOnActivateHint(Flag: Boolean);
    function GetEnumAllWindowsOnActivateHint: Boolean;
    procedure SetMainFormOnTaskBar(const Value: Boolean);
    function GetMainFormOnTaskBar: Boolean;
    procedure InternalRestore;
  public
    property EnumAllWindowsOnActivateHint: Boolean read GetEnumAllWindowsOnActivateHint write SetEnumAllWindowsOnActivateHint;
    property MainFormOnTaskBar: Boolean read GetMainFormOnTaskBar write SetMainFormOnTaskBar;
  end;
----------
----------
