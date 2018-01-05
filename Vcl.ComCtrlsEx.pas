//########################################################################//
//                                                                        //
//  ComCtrls 功能加強模組                                                 //
//                                                                        //
//########################################################################//
// 類型：VCL元件功能加強                                                  //
// 編寫：Wei-Lun Huang                                                    //
// 版權：Copyright (c) 2018 Wei-Lun Huang                                 //
// 說明：改進或增加 ComCtrls 中 VCL 元件的功能。                          //
//                                                                        //
// 作用：                                                                 //
//   1. TListViewEdit                                                     //
//      使 TListView 增加 TListItem.SubItems 使用者直接編輯功能。         //
// 歷程：                                                                 //
//   2018年01月01日 初版 TListViewEdit - TListView 加入子項編輯功能       //
//                                                                        //
// 其他：                                                                 //
//                                                                        //
// 最後變更日期：2018年01月01日                                           //
//                                                                        //
//########################################################################//

//
//  ComCtrls function enhancement module.
//
// Type: VCL Component Enhancements.
// Author: 2018 Wei-Lun Huang
// Description: Improve or add function to VCL components in ComCtrls.
//
// Features:
//   1. TListViewEdit
//      Add the TListItem.SubItems edit box feature in TListView.
// History:
//   Jan 01, 2018 first edition TListViewEdit - TListView Add the subkey edit feature.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: Jan 01, 2018.

unit Vcl.ComCtrlsEx;

interface

uses
  System.SysUtils, System.Classes,
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls;

type
  // Edit 內容複製至 ListView 的時機
  // Content synchronization mode.
  TSyncTextMode  = (
    STM_Non,    // Non-auto, disable automatic synchronization.
    STM_Change, // Sync when edit box content changes.
    STM_Exit,   // Sync when the edit box focus exits.
    STM_Return  // Sync when the return key press.
    );

  // 啟動編輯模式的行為方式
  // edit box action condition,
  // The mouse button event is based on the left/right button of the system mouse.
  TMouseBehavior = (
    MB_Click,   // Mouse click.
    MB_DblClick // Mouse double-click.
    );

  // Edit 的移動方向
  // Move direction, used in internal direction control event.
  TMoveDirec  = (MD_Non, MD_Left, MD_Up, MD_Right, MD_Down);

  TMoveStatus = record
    Force: Boolean;
    Direc: TMoveDirec;
  end;

  TFieldIndex = record
    Column, Row: Integer;
  end;

  TLVEActiveEvent =
    procedure(Sender: TObject; const FieldId: TFieldIndex; var AllowEdit: Boolean) of object;

  // 編輯框需要 ListView 屬性 ViewStyle = vsReport 與 ReadOnly = True 才會啟動。
  // 而當 ListView.RowSelect 為 True 時可編輯子項目，反之只能編輯 Caption。
  // The edit mode requires the ListView property ViewStyle = vsReport and ReadOnly = True to be used.
  // When ListView.RowSelect is True, the SubItems can be edited, and only Caption can be edited if False.
  TListViewEdit = class(TComponent)
  private type
    TWinProc = record
      Edit    : TWndMethod;
      ListView: TWndMethod;
    end;
  private
    FIsOwner: Boolean;
    FEnable: Boolean;
    FEditChanged: Boolean;
    FSelectFocused: Boolean;
    FSyncMode: TSyncTextMode;
    FMouseBehavior: TMouseBehavior;
    FSelect: TFieldIndex;
    FListView: TListView;
    FHeaderHeight: Integer;
    FEdit: TEdit;
    FOriginalWinProc: TWinProc;
    FOnActive: TLVEActiveEvent;
    FForceKey: Word;
    FMoveStatus: TMoveStatus;
    procedure HookWindowProc(var Message: TMessage);
    procedure EditWindowProc(var Message: TMessage);
    procedure OnEditActiveEvent(const FieldId: TFieldIndex; var AllowEdit: Boolean); inline;
    procedure InitialStatus; inline;
    procedure HeaderChanged;
    procedure SyncTextToListView;
    procedure SetEnable(Enable: Boolean);
    procedure SetListView(Control: TListView);
    procedure SetSyncMode(Mode: TSyncTextMode);
    procedure ScrollListToVisibleArea(Item: TListItem);
    function ActivateEdit: Boolean; inline;
    function MoveEdit(Direc: TMoveDirec; Force: Boolean = False): Boolean;
    function GetColumnLeft(Index: Integer): Integer; inline;
    function GetHeaderHeight: Integer; inline;
    function GetEditSelection: TSelection; inline;
    function GetEditTextLength: Integer; inline;
    function ActivityEditByPos: Boolean; inline;
    function CheckIndex(Index: TFieldIndex): Boolean; inline;
    function InVisibleArea(Item: TListItem): Boolean; inline;
    function FollowScroll(Item: TListItem): Boolean; overload;
    function FollowScroll: Boolean; overload;
    function ShowEditByPos(X, Y: Integer): Boolean; overload;
    function ShowEditByPos(const Pos: TPoint): Boolean; overload; inline;
    function ShowEditByIndex(const FieldIndex: TFieldIndex): Boolean; overload;
    function ShowEditByIndex(Column, Row: Integer): Boolean; overload; inline;
    function ShowEditByItem(Item: TListItem; Column: Integer = -1): Boolean; inline;
  public
    // AOwner 為要關聯的 TListView，在建立本元件時將 AOwner 為 nil，可之後再指定要關聯的 TListView。
    // 但本元件建立時已指派關聯的 TListView，則之後不可再變更。
    // If AOwner is nil to create, the target TListView component can then be set to ListView property.
    // If the specified target to create, then it cannot be changed.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // 是否啟用編輯模式
    // Enable or disable edit mode.
    property Enable: Boolean read FEnable write SetEnable default True;

    // 啟動編輯模式的行為方式。
    // Behavior mode to start editing mode.
    property MouseBehavior: TMouseBehavior read FMouseBehavior write FMouseBehavior default MB_DblClick;

    // 編輯的內容設定至 ListView 的時機
    // Behavior of synchronizing edit box contents to TListItem content.
    property SyncMode: TSyncTextMode read FSyncMode write SetSyncMode default STM_Exit;

    // 可略過編輯框字元游標檢查直接移動編輯框的附加按鍵，預設 [Alt]
    // Can move directly the edit box when the arrow key are pressed with this key. (Default [Alt])
    property ForceKey: Word read FForceKey write FForceKey default VK_MENU;

    // 顯示編輯框的前置事件，會傳入 FieldId，可以透過 AllowEdit 來控制是否可編輯。
    // Displays the predecessor event of the edit box,FieldID is the index of the field
    // currently being edit. can use the change AllowEdit value to control continuing editing.
    property OnActive: TLVEActiveEvent read FOnActive;

    // 取得編輯框元件，編輯框會自動建立與釋放。
    // Get edit box component.
    property Edit: TEdit read FEdit;

    // 取得目前欄位索引
    // Currently selected field index.
    property FieldIndex: TFieldIndex read FSelect;

    // 若在建立時未指派關聯的 TListView，則可使用此 ListView 屬性設定指派關聯。
    // If target is nil to create, can set TListView component to implant.
    property ListView: TListView read FListView write SetListView;
  end;

function GetDirecFormKey(Key: NativeUInt): TMoveDirec; inline;

implementation

uses
  Debug;

resourcestring
  _CannotSetListView = 'Can''t change the component, that has been set to owner.';

function GetDirecFormKey(Key: NativeUInt): TMoveDirec; inline;
begin
  if Key in [VK_LEFT..VK_DOWN] then
    Result := TMoveDirec(Key - (VK_LEFT - 1))
  else
    Result := MD_Non;
end;

{ TListViewEdit }

constructor TListViewEdit.Create(AOwner: TComponent);
begin
  inherited;
  FIsOwner := False;
  FEdit := nil;
  FListView := nil;
  FOnActive := nil;
  FillChar(FOriginalWinProc, SizeOf(FOriginalWinProc), 0);

  // Set setting default.
  FEnable := True;
  FSyncMode := STM_Exit;
  FMouseBehavior := MB_DblClick;
  FForceKey := VK_MENU;

  if Assigned(AOwner) then
  begin
    if AOwner is TListView then
    begin
      FEdit := TEdit.Create(AOwner);
      { In function SetListView, FIsOwner is checked for true,
        false to allow component FListView to be changed,
        so the FIsOwner flag needs to be false.

        In SetListView function, if the flag FIsOwner is false,
        the InitialStatus function is executed.
        So there's no need to execute the SetListView function again.
      }
      SetListView(TListView(AOwner));
      FIsOwner := True;
    end
    else
    begin
      InitialStatus;
    end;
  end
  else
  begin
    InitialStatus;
  end;
end;

destructor TListViewEdit.Destroy;
begin
  if Assigned(FListView) then
  begin
    FEdit.WindowProc := FOriginalWinProc.Edit;
    FListView.WindowProc := FOriginalWinProc.ListView;
  end;
  if not FIsOwner then
  begin
    if Assigned(FEdit) then
      FEdit.Free;
  end;
  inherited;
end;

procedure TListViewEdit.InitialStatus;
begin
  FEditChanged := False;
  FSelectFocused := False;
  FSelect.Column := -1;
  FSelect.Row := -1;
  FillChar(FMoveStatus, SizeOf(FMoveStatus), 0);
end;

procedure TListViewEdit.HeaderChanged;
begin
  FHeaderHeight := GetHeaderHeight;
end;

function TListViewEdit.ActivateEdit: Boolean;
begin
  Result := FEnable and
            FListView.ReadOnly and
            (FListView.ViewStyle = vsReport) and
            (FListView.SelCount = 1);
end;

procedure TListViewEdit.OnEditActiveEvent(const FieldId: TFieldIndex; var AllowEdit: Boolean);
begin
  if Assigned(FOnActive) then
    FOnActive(FListView, FieldIndex, AllowEdit);
end;

function TListViewEdit.GetColumnLeft(Index: Integer): Integer;
begin
  Result := 0;
  while Index > 0 do
  begin
    Dec(Index);
    Inc(Result, FListView.Column[Index].Width);
  end;
end;

function TListViewEdit.GetEditSelection: TSelection;
begin
  SendMessage(FEdit.Handle, EM_GETSEL, WPARAM(@Result.StartPos), LPARAM(@Result.EndPos));
end;

function TListViewEdit.GetEditTextLength: Integer;
begin
  Result := GetWindowTextLength(FEdit.Handle);
end;

function TListViewEdit.GetHeaderHeight: Integer;
var
  h: THandle;
  Rc:TRect;
begin
  h := ListView_GetHeader(FListView.Handle);
  if GetWindowRect(h, rc) then
    Result := rc.Height
  else
    Result := -1;
end;

function TListViewEdit.ActivityEditByPos: Boolean;
var
  CursorPos: TPoint;
begin
  if ActivateEdit then
  begin
    if GetCursorPos(CursorPos) then
    begin
      CursorPos := FListView.ScreenToClient(CursorPos);
      Exit(ShowEditByPos(CursorPos));
    end;
  end;
  Result := False;
end;

function TListViewEdit.CheckIndex(Index: TFieldIndex): Boolean;
begin
  if Index.Column or Index.Row > -1 then
    if Index.Column < FListView.Columns.Count then
      if Index.Row < FListView.Items.Count then
        Exit(True);
  Result := False;
end;

function TListViewEdit.InVisibleArea(Item: TListItem): Boolean;
var
  Y: Integer;
begin
  Y := Item.Top;
  Result := ((Y + Item.DisplayRect(drBounds).Height) > FHeaderHeight) and
            (Y < FListView.ClientHeight);
end;

function TListViewEdit.FollowScroll(Item: TListItem): Boolean;
const
  _FieldOffsetX = 2;
  _FieldOffsetY = 1;
var
  I, X: Integer;
  p: TPoint;
begin
  if not FSelectFocused then
    Exit(False);
  if not Assigned(Item) then
    Exit(False);
  Result := InVisibleArea(Item);
  if Result then
    OnEditActiveEvent(FSelect, Result);
  if Result then
  begin
    I := FSelect.Column;
    X := GetColumnLeft(I);
    p := Item.GetPosition;
    p.Offset(X +_FieldOffsetX, _FieldOffsetY);
    X := FListView.Column[I].Width - (_FieldOffsetX + 12);
    I := Item.DisplayRect(drSelectBounds).Height - (_FieldOffsetY + 2);
    FEdit.SetBounds(p.X, p.Y, X, I);
    if not FEdit.Visible then
    begin
      FEdit.Show;
      if FListView.Focused then
        FEdit.SetFocus;
    end;
  end
  else
  begin
    if FEdit.Visible then
    begin
      if FEdit.Focused then
        if FListView.Visible then
          FListView.SetFocus;
      FEdit.Hide;
    end;
  end;
end;

function TListViewEdit.FollowScroll: Boolean;
begin
  Result := FollowScroll(FListView.Selected);
end;

procedure TListViewEdit.HookWindowProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KILLFOCUS:
      if not FEdit.Focused then
        FSelectFocused := False;
  end;
  FOriginalWinProc.ListView(Message); // Call backup of WindowProc.
  case Message.Msg of
    WM_HSCROLL, WM_VSCROLL, WM_MOUSEWHEEL:
      if ActivateEdit then
          FollowScroll;
    WM_LBUTTONDOWN:
      if Message.WParam = MK_LBUTTON then
        if FMouseBehavior = MB_Click then
          ActivityEditByPos
        else
          FSelectFocused := False;
    WM_LBUTTONDBLCLK:
      if Message.WParam = MK_LBUTTON then
        if FMouseBehavior = MB_DblClick then
          ActivityEditByPos;
    LVM_SETCOLUMN:
      HeaderChanged;
  end;
end;

function TListViewEdit.MoveEdit(Direc: TMoveDirec; Force: Boolean): Boolean;
var
  SelItem, Item: TListItem;
  FieldIndex: TFieldIndex;
  Selection: TSelection;
  I, J: Integer;
begin
  if Direc = MD_Non then
    Exit(False);

  SelItem := FListView.Selected;
  if Assigned(SelItem) then
  begin
    FieldIndex.Column := FSelect.Column;
    FieldIndex.Row    := FSelect.Row;
    Result := False;
    case Direc of
      MD_Left:
      begin
        Selection := GetEditSelection;
        if Force or (Selection.StartPos = 0) and (Selection.EndPos = 0) then
        begin
          I := FieldIndex.Column;
          if I > 0 then
          begin
            Dec(I);
            FieldIndex.Column := I;

            I := GetColumnLeft(I);
            J := GetScrollPos(FListView.Handle, SB_HORZ);
            FListView.Scroll(I - J, 0);

            Result := True;
          end;
        end;
      end;
      MD_Up:
      begin
        I := FieldIndex.Row;
        if I > 0 then
        begin
          Dec(I);
          Item := FListView.Items.Item[I];
          FieldIndex.Row := I;

          if FListView.MultiSelect then
            SelItem.Selected := False;
          Item.Selected := True;

          if not InVisibleArea(Item) then
            Item.MakeVisible(True);

          Result := True;
        end;
      end;
      MD_Right:
      begin
        Selection := GetEditSelection;
        J := GetEditTextLength;
        if Force or (Selection.StartPos = J) and (Selection.EndPos = J) then
          begin
          I := FieldIndex.Column;
          if I < FListView.Columns.Count - 1 then
          begin
            Inc(I);
            FieldIndex.Column := I;

            I := GetColumnLeft(I);
            J := GetScrollPos(FListView.Handle, SB_HORZ);
            FListView.Scroll(I - J, 0);

            Result := True;
          end;
        end;
      end;
      MD_Down:
      begin
        I := FieldIndex.Row;
        if I < (FListView.Items.Count - 1) then
        begin
          Inc(I);
          Item := FListView.Items.Item[I];
          FieldIndex.Row := I;

          if FListView.MultiSelect then
            SelItem.Selected := False;
          Item.Selected := True;

          if not InVisibleArea(Item) then
            Item.MakeVisible(True);

          Result := True;
        end;
      end;
    end;
    if Result then
      ShowEditByIndex(FieldIndex);
  end
  else
  begin
    Result := False;
  end;
end;

procedure TListViewEdit.EditWindowProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KILLFOCUS:
      if not FListView.Focused then
        FSelectFocused := False;
    WM_KEYDOWN, WM_SYSKEYDOWN:
    begin
      case Message.WParam of
        VK_ESCAPE:
        begin
          if Edit.Visible then
          begin
            if FSyncMode = STM_Exit then
              SyncTextToListView;
            Edit.Hide;
            Exit;
          end;
        end;
        VK_RETURN:
        begin
          if FSyncMode = STM_Return then
            SyncTextToListView;
        end
        else
        begin
          if Message.WParam = FForceKey then
            FMoveStatus.Force := True
          else if FMoveStatus.Direc = MD_Non then
            FMoveStatus.Direc := GetDirecFormKey(Message.WParam);
          if MoveEdit(FMoveStatus.Direc, FMoveStatus.Force) then
            Exit;
        end;
      end;
    end;
    WM_KEYUP, WM_SYSKEYUP:
    begin
      if Message.WParam = VK_ESCAPE then
        Exit
      else if Message.WParam = FForceKey then
        FMoveStatus.Force := False
      else if GetDirecFormKey(Message.WParam) = FMoveStatus.Direc then
        FMoveStatus.Direc := MD_Non;
    end;
  end;
  FOriginalWinProc.Edit(Message); // Call backup of WindowProc.
  case Message.Msg of
    WM_KILLFOCUS:
    begin
      if FSyncMode = STM_Exit then
        SyncTextToListView;
      Edit.Hide;
    end;
    WM_IME_SETCONTEXT:
    begin
      FEditChanged := True;
      if FSyncMode = STM_Change then
        SyncTextToListView;
    end;
  end;
end;

procedure TListViewEdit.SyncTextToListView;
var
  Item: TListItem;
  Index: Integer;
begin
  if FEditChanged and CheckIndex(FSelect) then
  begin
    Item := FListView.Items.Item[FSelect.Row];
    Index := FListView.Columns.Items[FSelect.Column].ID;
    if Index > 0 then
    begin
      if Index < Item.SubItems.Count then
      begin
        Dec(Index);
        Item.SubItems.Strings[Index] := FEdit.Text;
      end
      else
      begin
        Dec(Index);
        Item.SubItems.BeginUpdate;
        try
          while Item.SubItems.Count < Index do
            Item.SubItems.Add('');
          Item.SubItems.Add(FEdit.Text);
        finally
          Item.SubItems.EndUpdate;
        end;
      end;
    end
    else if Index = 0 then
    begin
      Item.Caption := FEdit.Text;
    end;
    FEditChanged := False;
  end;
end;

procedure TListViewEdit.SetEnable(Enable: Boolean);
begin
  if FEnable <> Enable then
  begin
    FEnable := Enable;
    if not Enable then
    begin
      if Assigned(FEdit) then
      begin
        if FSelectFocused then
          SyncTextToListView;
        if FEdit.Visible then
          FEdit.Hide;
      end;
    end;
  end;
end;

procedure TListViewEdit.SetListView(Control: TListView);
begin
  if FIsOwner then
  begin
    raise Exception.Create(_CannotSetListView);
    Exit;
  end;
  if Assigned(FListView) then
  begin
    FEdit.WindowProc := FOriginalWinProc.Edit;
    FListView.WindowProc  := FOriginalWinProc.ListView;
  end;
  if Assigned(FEdit) then
  begin
    FEdit.Parent := nil;
  end;
  InitialStatus;
  if Assigned(Control) then
  begin
    FListView := Control;
    HeaderChanged;
    if not Assigned(FEdit) then
      FEdit := TEdit.Create(nil);
    FEdit.Hide;
    FEdit.Parent := FListView;
    FEdit.Ctl3D := False;
    FEdit.BorderStyle := bsNone;
    FEdit.Font.Assign(Control.Font);
    FEdit.DoubleBuffered := FListView.DoubleBuffered;
    FOriginalWinProc.Edit := FEdit.WindowProc;
    FEdit.WindowProc := EditWindowProc;

    FOriginalWinProc.ListView := FListView.WindowProc;
    FListView.WindowProc := HookWindowProc;
  end
  else
  begin
    FEdit.Hide;
    FListView := nil;
    FillChar(FOriginalWinProc, SizeOf(FOriginalWinProc), 0);
  end;
end;

procedure TListViewEdit.SetSyncMode(Mode: TSyncTextMode);
begin
  case Mode of
    STM_Non             : FEdit.ReadOnly := True;
    STM_Change, STM_Exit: FEdit.ReadOnly := False;
  end;
  FSyncMode := Mode;
end;

procedure TListViewEdit.ScrollListToVisibleArea(Item: TListItem);
var
  I, J: Integer;
begin
  I := Item.Top;
  J := I + FHeaderHeight;
  if J < 0 then
  begin
    FListView.Scroll(0, J)
  end
  else
  begin
    J := Item.DisplayRect(drBounds).Height;
    Inc(I, J);
    J := J * FListView.VisibleRowCount + FHeaderHeight;
    J := I - J;
    if J > 0 then
      FListView.Scroll(0, J);
  end;
end;

function TListViewEdit.ShowEditByPos(X, Y: Integer): Boolean;
var
  Item: TListItem;
  I, J: Integer;
begin
  Item := FListView.GetItemAt(X, Y);
  Result := Assigned(Item);
  if Result then
  begin
    I := 0;
    Inc(X, GetScrollPos(FListView.Handle, SB_HORZ));
    J := FListView.Column[I].Width;
    while J < X do
    begin
      Inc(I);
      Inc(J, FListView.Column[I].Width);
    end;
    FSelect.Column := I;
    FSelect.Row := Item.Index;

    OnEditActiveEvent(FSelect, Result);
    if Result then
    begin
      ScrollListToVisibleArea(Item);

      FEdit.Alignment := FListView.Column[I].Alignment;
      J := FListView.Column[I].ID;
      if J = 0 then
        FEdit.Text := Item.Caption
      else
        if J <= Item.SubItems.Count then
          FEdit.Text := Item.SubItems.Strings[J - 1]
        else
          FEdit.Text := '';
      FSelectFocused := True;
      if FollowScroll(Item) then
      begin
        if not FEdit.Visible then
          FEdit.Show;
        if not FEdit.Focused then
          FEdit.SetFocus;
      end;
    end;
  end;
end;

function TListViewEdit.ShowEditByPos(const Pos: TPoint): Boolean;
begin
  Result := ShowEditByPos(Pos.X, Pos.Y);
end;

function TListViewEdit.ShowEditByIndex(const FieldIndex: TFieldIndex): Boolean;
var
  Item: TListItem;
  I: Integer;
begin
  if ActivateEdit then
  begin
    Result := (FieldIndex.Row >= 0) and (FieldIndex.Row < FListView.Items.Count);
    if Result then
    begin
      FEdit.Alignment := FListView.Column[FieldIndex.Column].Alignment;
      Item := FListView.Items.Item[FieldIndex.Row];
      OnEditActiveEvent(FieldIndex, Result);
      if Result then
      begin
        ScrollListToVisibleArea(Item);

        I := FListView.Column[FieldIndex.Column].ID;
        if I = 0 then
          FEdit.Text := Item.Caption
        else
          if I <= Item.SubItems.Count then
            FEdit.Text := Item.SubItems.Strings[I - 1]
          else
            FEdit.Clear;

        Move(FieldIndex, FSelect, SizeOf(TFieldIndex));

        FSelectFocused := True;
        if FollowScroll(Item) then
        begin
          if not FEdit.Visible then
            FEdit.Show;
          if not FEdit.Focused then
            FEdit.SetFocus;
          FEdit.SelectAll;
        end;
      end;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function TListViewEdit.ShowEditByIndex(Column, Row: Integer): Boolean;
var
  FieldIndex: TFieldIndex;
begin
  FieldIndex.Column := Column;
  FieldIndex.Row    := Row;
  Result := ShowEditByIndex(FieldIndex);
end;

function TListViewEdit.ShowEditByItem(Item: TListItem; Column: Integer): Boolean;
begin
  Result := ShowEditByIndex(Column, Item.Index);
end;

end.
