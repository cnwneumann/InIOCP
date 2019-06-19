unit frmInSQLMgrEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, ActnList, iocp_sqlMgr;

type
  TFormInSQLManager = class(TForm)
    lvSQLNames: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    pgSQLMgr: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    Splitter1: TSplitter;
    memSQL: TMemo;
    memResFile: TMemo;
    btnSave: TBitBtn;
    btnSaveAs: TBitBtn;
    btnCancel: TBitBtn;
    btnOpen: TBitBtn;
    alActions: TActionList;
    actSaveMemSQL: TAction;
    actOpen: TAction;
    actSaveAs: TAction;
    actSaveResFile: TAction;
    procedure pgSQLMgrChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvSQLNamesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveMemSQLExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveResFileExecute(Sender: TObject);
    procedure pgSQLMgrChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
    FSQLManager: TInSQLManager;
    FCurrentSQL: TStrings;
    FCurrentName: String;
    procedure SetSQLManager(const Value: TInSQLManager);
    procedure UpdateFormLayout(const SQLs: TStrings);
  public
    { Public declarations }
    property SQLManager: TInSQLManager read FSQLManager write SetSQLManager; 
  end;

function EditInSQLManager(InSQLManager: TInSQLManager): Boolean;

implementation

{$R *.dfm}

function EditInSQLManager(InSQLManager: TInSQLManager): Boolean;
begin
  with TFormInSQLManager.Create(Application) do
  try
    try
      SQLManager := InSQLManager;
      Result := ShowModal = mrOK;
    except
      Result := False;
    end;
  finally
    Free;
  end;
end;

{ TFormInSQLManager }

procedure TFormInSQLManager.actOpenExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    try
      DefaultExt := '*.sql';
      Filter := '*.sql|*.sql';
      Options := Options + [ofFileMustExist]; 
      if Execute then
      begin
        memResFile.Lines.LoadFromFile(FileName);
        FSQLManager.SQLs := memResFile.Lines;
        UpdateFormLayout(memResFile.Lines);
        memResFile.Modified := False;
      end;
    finally
      Free;
    end;
end;

procedure TFormInSQLManager.actSaveAsExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
    try
      DefaultExt := '*.sql';
      Filter := '*.sql|*.sql';
      Options := Options + [ofOverwritePrompt];
      if Execute then
        memResFile.Lines.SaveToFile(FileName);
    finally
      Free;
    end;
end;

procedure TFormInSQLManager.actSaveMemSQLExecute(Sender: TObject);
var
  i, iStart, iEnd: Integer;
  S: String;
begin
  FCurrentSQL.Assign(memSQL.Lines);

  iStart := 0;
  iEnd := memResFile.Lines.Count;

  // Section 范围
  for i := 0 to memResFile.Lines.Count - 1 do
  begin
    S := Trim(memResFile.Lines[i]);
    if (FCurrentName = S) then      // 区分大小写
      iStart := i
    else
    if (iStart > 0) and (i > iStart) and
       (Length(S) > 0) and (S[1] = '[') then
      iEnd := i;
  end;

  if (iStart = 0) then             // 直接加入
    memResFile.Lines.AddStrings(memSQL.Lines)
  else begin
    // 删除 Section 内容，插入新内容
    for i := iEnd - 1 downto iStart + 1 do
      memResFile.Lines.Delete(i);

    Inc(iStart);
    for i := 0 to memSQL.Lines.Count - 1 do
      memResFile.Lines.Insert(iStart + i, memSQL.Lines[i]);

    i := memSQL.Lines.Count - 1;
    if (memSQL.Lines[i] <> '') then
      memResFile.Lines.Insert(iStart + i, #13#10);
  end;

  FSQLManager.SQLs := memResFile.Lines;  // 不要用 Assign()
    
  memSQL.Modified := False;
  memResFile.Modified := False;
end;

procedure TFormInSQLManager.actSaveResFileExecute(Sender: TObject);
begin
  FSQLManager.SQLs := memResFile.Lines;  // 不要用 Assign()
  UpdateFormLayout(memResFile.Lines);
  memResFile.Modified := False;
end;

procedure TFormInSQLManager.alActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actOpen.Enabled := (pgSQLMgr.ActivePageIndex = 1) and (memResFile.Modified = False);
  actSaveMemSQL.Enabled := memSQL.Modified;
  actSaveResFile.Enabled := memResFile.Modified;
  actSaveAs.Enabled := pgSQLMgr.ActivePageIndex = 1;
end;

procedure TFormInSQLManager.FormCreate(Sender: TObject);
begin
  pgSQLMgr.ActivePageIndex := 0;
  pgSQLMgrChange(nil);
end;

procedure TFormInSQLManager.FormDestroy(Sender: TObject);
begin
  lvSQLNames.Items.Clear;
  memSQL.Lines.Clear;
  memResFile.Lines.Clear;
end;

procedure TFormInSQLManager.lvSQLNamesClick(Sender: TObject);
begin
  if Assigned(lvSQLNames.Selected) and not memSQL.Modified then
  begin
    FCurrentSQL := TStrings(lvSQLNames.Selected.Data);
    FCurrentName := lvSQLNames.Selected.Caption;
    Delete(FCurrentName, 1, Pos('>', FCurrentName));
    FCurrentName := '[' + FCurrentName + ']';
    memSQL.Lines.Assign(FCurrentSQL);
    memSQL.Modified := False;
  end;
end;

procedure TFormInSQLManager.pgSQLMgrChange(Sender: TObject);
begin
  btnOpen.Enabled := pgSQLMgr.ActivePageIndex = 1;
  btnSaveAs.Enabled := btnOpen.Enabled;
  if btnOpen.Enabled then
    btnSave.Action := actSaveResFile
  else
    btnSave.Action := actSaveMemSQL
end;

procedure TFormInSQLManager.pgSQLMgrChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := btnSave.Enabled = False;
end;

procedure TFormInSQLManager.SetSQLManager(const Value: TInSQLManager);
begin
  FSQLManager := Value;
  memResFile.Lines := FSQLManager.SQLs;
  UpdateFormLayout(memResFile.Lines);
end;

procedure TFormInSQLManager.UpdateFormLayout(const SQLs: TStrings);
var
  i: Integer;
  Obj: TSQLObject;
begin
  lvSQLNames.Items.BeginUpdate;
  memResFile.Lines.BeginUpdate;
  try
    lvSQLNames.Items.Clear;
    memSQL.Lines.Clear;

    for i := 0 to FSQLManager.Count - 1 do
    begin
      Obj := FSQLManager.Items[i];
      with lvSQLNames.Items.Add do
      begin
        Caption := IntToStr(i) + '->' + Obj.SQLName;
        Data := Pointer(Obj.SQL);
      end;
    end;
    
  finally
    memResFile.Modified := False;
    memResFile.Lines.EndUpdate;
    lvSQLNames.Items.EndUpdate;
  end;
end;

end.
