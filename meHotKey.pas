unit meHotKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  LCLType, StdCtrls;

type
  TmeHotKey = class(TCustomEdit)
  private
    FShortCut: TShortCut;
    FOnExecute: TNotifyEvent;
    FActive: Boolean;
    FDescription: string;
    FWinKey: Boolean;
    FIsEditing: Boolean; // Флаг для отслеживания процесса ввода
    procedure SetShortCut(const Value: TShortCut);
    procedure SetActive(const Value: Boolean);
    procedure SetWinKey(const Value: Boolean);
    procedure UpdateDisplay;
  protected
    procedure DoExecute;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function GetShortCutAsText: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    class function TextToShortCut(const Text1: string): TShortCut;
    class function ShortCutToText(ShortCut: TShortCut): string;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    property Active: Boolean read FActive write SetActive default True;
    property Description: string read FDescription write FDescription;
    property WinKey: Boolean read FWinKey write SetWinKey default False;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;

    // Наследуем стандартные свойства TEdit
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

uses
  LCLProc;

procedure Register;
begin
  RegisterComponents('Morgunov', [TmeHotKey]);
end;

{ TmeHotKey }

constructor TmeHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShortCut := Menus.ShortCut(Ord('A'), [ssCtrl]);
  FActive := True;
  FDescription := '';
  FWinKey := False;
  FIsEditing := False;
  ReadOnly := True;
  Anchors := [akLeft, akTop]; // Устанавливаем якоря по умолчанию (левый верхний угол)
  UpdateDisplay;
end;

procedure TmeHotKey.Assign(Source: TPersistent);
begin
  if Source is TmeHotKey then
  begin
    FShortCut := TmeHotKey(Source).ShortCut;
    FActive := TmeHotKey(Source).Active;
    FDescription := TmeHotKey(Source).Description;
    FWinKey := TmeHotKey(Source).WinKey;
    FOnExecute := TmeHotKey(Source).OnExecute;
    UpdateDisplay;
  end
  else
    inherited Assign(Source);
end;

procedure TmeHotKey.SetShortCut(const Value: TShortCut);
begin
  if FShortCut <> Value then
  begin
    FShortCut := Value;
    UpdateDisplay;
  end;
end;

procedure TmeHotKey.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
  end;
end;

procedure TmeHotKey.SetWinKey(const Value: Boolean);
begin
  if FWinKey <> Value then
  begin
    FWinKey := Value;
    UpdateDisplay;
  end;
end;

procedure TmeHotKey.UpdateDisplay;
begin
  if FShortCut = 0 then
    inherited Text := ''
  else
    inherited Text := GetShortCutAsText;
end;

procedure TmeHotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewShortCut: TShortCut;
begin
  // Игнорируем служебные клавиши
  if Key in [VK_SHIFT, VK_CONTROL, VK_MENU, VK_LWIN, VK_RWIN] then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  // Если только начали ввод - очищаем старое значение
  if not FIsEditing then
  begin
    FShortCut := 0;
    inherited Text := '';
    FIsEditing := True;
  end;

  NewShortCut := Menus.ShortCut(Key, Shift);
  SetShortCut(NewShortCut);
  Key := 0;  // Подавляем дальнейшую обработку клавиши
end;

procedure TmeHotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  // Сбрасываем флаг редактирования при отпускании всех клавиш
  if Shift = [] then
    FIsEditing := False;
end;

function TmeHotKey.GetShortCutAsText: string;
var
  Key: Word;
  KeyChar: Char;
begin
  Result := '';
  Key := FShortCut and $FF;

  if FShortCut and scCtrl <> 0 then Result := Result + 'Ctrl+';
  if FShortCut and scAlt <> 0 then Result := Result + 'Alt+';
  if FShortCut and scShift <> 0 then Result := Result + 'Shift+';
  if (FShortCut and scMeta <> 0) and FWinKey then Result := Result + 'Win+';

  case Key of
    VK_BACK: Result := Result + 'Backspace';
    VK_TAB: Result := Result + 'Tab';
    VK_RETURN: Result := Result + 'Enter';
    VK_ESCAPE: Result := Result + 'Esc';
    VK_SPACE: Result := Result + 'Space';
    VK_PRIOR: Result := Result + 'PgUp';
    VK_NEXT: Result := Result + 'PgDn';
    VK_END: Result := Result + 'End';
    VK_HOME: Result := Result + 'Home';
    VK_LEFT: Result := Result + 'Left';
    VK_UP: Result := Result + 'Up';
    VK_RIGHT: Result := Result + 'Right';
    VK_DOWN: Result := Result + 'Down';
    VK_INSERT: Result := Result + 'Ins';
    VK_DELETE: Result := Result + 'Del';
    VK_ADD: Result := Result + 'Num +';
    VK_SUBTRACT: Result := Result + 'Num -';
    VK_DIVIDE: Result := Result + 'Num /';
    VK_MULTIPLY: Result := Result + 'Num *';
    VK_F1..VK_F24: Result := Result + 'F' + IntToStr(Key - VK_F1 + 1);
  else
    KeyChar := Chr(Key);
    if (KeyChar in ['A'..'Z', '0'..'9']) then
      Result := Result + KeyChar
    else if Key <> 0 then
      Result := Result + '[' + IntToStr(Key) + ']';
  end;
end;

class function TmeHotKey.TextToShortCut(const Text1: string): TShortCut;
begin
  Result := Menus.ShortCut(Ord(Text1[1]), []);
end;

class function TmeHotKey.ShortCutToText(ShortCut: TShortCut): string;
begin
  // Используем нашу функцию GetShortCutAsText
  Result := '';
  if ShortCut <> 0 then
    Result := TmeHotKey.Create(nil).GetShortCutAsText;
end;

procedure TmeHotKey.DoExecute;
begin
  if FActive and Assigned(FOnExecute) then
    FOnExecute(Self);
end;

initialization
  {$I meHotKey.lrs}   

end.
