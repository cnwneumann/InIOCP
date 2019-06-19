unit frmInIOCPFileSvrClient;

interface

{$I in_iocp.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iocp_base, iocp_clients, iocp_msgPacks, StdCtrls, ExtCtrls, ComCtrls;

type
  TFormInIOCPFileSvrClient = class(TForm)
    Memo1: TMemo;
    InConnection1: TInConnection;
    InCertifyClient1: TInCertifyClient;
    InFileClient1: TInFileClient;
    btnLogin: TButton;
    edtLoginUser: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnLogout: TButton;
    btnUpload: TButton;
    btnDownload: TButton;
    btnQueryFiles: TButton;
    btnSetDir: TButton;
    ListView1: TListView;
    BtnCancel: TButton;
    btnRestart: TButton;
    btnClearList: TButton;
    btnUpChunk: TButton;
    btnDownChunk: TButton;
    Button1: TButton;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure InCertifyClient1Certify(Sender: TObject; Action: TActionType;
      ActResult: Boolean);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure InFileClient1ReturnResult(Sender: TObject; Result: TResultParams);
    procedure btnQueryFilesClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSetDirClick(Sender: TObject);
    procedure InFileClient2ReturnResult(Sender: TObject; Result: TResultParams);
    procedure InConnection1Error(Sender: TObject; const Msg: string);
    procedure InFileClient1ListFiles(Sender: TObject; ActResult: TActionResult;
      No: Integer; Result: TCustomPack);
    procedure InConnection1AddWork(Sender: TObject; Msg: TClientParams);
    procedure ListView1Click(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure btnRestartClick(Sender: TObject);
    procedure InConnection1DataSend(Sender: TObject; MsgId, MsgSize,
      CurrentSize: Int64);
    procedure InConnection1DataReceive(Sender: TObject; MsgId, MsgSize,
      CurrentSize: Int64);
    procedure btnClearListClick(Sender: TObject);
    procedure btnUpChunkClick(Sender: TObject);
    procedure btnDownChunkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FToUserName: string;
    FSvrSocket: Cardinal;
    FFileName: String;
    FFromClient: String;
    FMsgId: String;
    FFileList: TStrings;
    procedure ShowProgress(MsgId, MsgSize, CurrentSize: Int64);
  public
    { Public declarations }
  end;

var
  FormInIOCPFileSvrClient: TFormInIOCPFileSvrClient;

implementation

uses
  iocp_utils, http_utils, iocp_md5;
  
{$R *.dfm}

procedure TFormInIOCPFileSvrClient.BtnCancelClick(Sender: TObject);
begin
  // 取消指定消息编号的任务，编号是 64 位
  //   如果不是断点续传，则无法恢复操作，只能重新执行
  //   如果是 atFileDownload，请求发出后也服务端不断发生数据，
  //   客户端无法干预，也就无法取消，只有断开连接了
  InConnection1.CancelWork(StrToInt64(FMsgId)); // 内有说明
end;

procedure TFormInIOCPFileSvrClient.btnClearListClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  ListView1.Items.Clear;
  ListView1Click(Self);
end;

procedure TFormInIOCPFileSvrClient.btnConnectClick(Sender: TObject);
begin
  InConnection1.Active := True;
end;

procedure TFormInIOCPFileSvrClient.btnDisconnectClick(Sender: TObject);
begin
  InConnection1.Active := False;
end;

procedure TFormInIOCPFileSvrClient.btnDownChunkClick(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // TInFileClient 没公开方法，用 TMessagePack

  InConnection1.OnDataReceive := InConnection1DataReceive;
  InConnection1.OnDataSend := nil;

  Msg := TMessagePack.Create(InFileClient1);
  Msg.FileName := 'jdk-8u77-windows-i586.exe'; // 'WIN-7-20190228.GHO';
  Msg.LocalPath := 'temp';
  
//  Msg.CheckType := ctMurmurHash;  // 可以加校验码，分块传输不支持压缩！
  Msg.Post(atFileDownChunk);
  
end;

procedure TFormInIOCPFileSvrClient.btnDownloadClick(Sender: TObject);
begin
  // 下载文件，存放路径: InConnection1.LocalPath
  InConnection1.OnDataReceive := InConnection1DataReceive;
  InConnection1.OnDataSend := nil;
  
  InFileClient1.Download('jdk-8u77-windows-i586.exe');
end;

procedure TFormInIOCPFileSvrClient.btnLoginClick(Sender: TObject);
begin
  InCertifyClient1.UserName := edtLoginUser.Text;
  InCertifyClient1.Password := 'pppp';
  InCertifyClient1.Login;
end;

procedure TFormInIOCPFileSvrClient.btnLogoutClick(Sender: TObject);
begin
  InCertifyClient1.Logout;
end;

procedure TFormInIOCPFileSvrClient.btnQueryFilesClick(Sender: TObject);
var
  i: Integer;
  Files: TStrings;
begin
  // 查询服务端当前目录的文件

  // 1. 不带参数，在 InFileClient1ListFiles 显示
  InFileClient1.ListFiles; 

  // 2. 带参数，阻塞式下载（未实现）
{  InConnection1.BlockMode := True;

  Files := TStringList.Create;
  InFileClient1.ListFiles(Files);

  for i := 0 to Files.Count - 1 do
    InFileClient1.Download(Files.Strings[i]);

  Files.Free;     }
  
end;

procedure TFormInIOCPFileSvrClient.btnRestartClick(Sender: TObject);
var
  Action: TActionType;
  Item: TListItem;
begin
  // 断点续传，恢复操作，否则全新开始
  //  TInFileClient 没公开续传方法，用 TMessagePack
  //  断点续传时要使用唯一的 MsgId（可以修改）。
  Item := ListView1.Selected;
  Action := TActionType(StrToInt(Item.SubItems[2]));
  case Action of
    atFileUpload:     // 只能重新上传！
      with TMessagePack.Create(InFileClient1) do
      begin
        LoadFromFile(Item.SubItems[0]);
        Post(atFileUpload);
      end;

    atFileUpChunk:    // 续传上传
      with TMessagePack.Create(InFileClient1) do
      begin
        MsgId := StrToInt64(Item.SubItems[1]);  // 保证 MsgId 唯一不变
        LoadFromFile(Item.SubItems[0]);
        Post(atFileUpChunk);
      end;

    atFileDownload:   // 只能重新下载！
      with TMessagePack.Create(InFileClient1) do
      begin
        FileName := Item.SubItems[0];
        Post(atFileDownload);
      end;

    atFileDownChunk: // 续传下载
      with TMessagePack.Create(InFileClient1) do
      begin
        MsgId := StrToInt64(Item.SubItems[1]); // 保证 MsgId 唯一不变
        FileName := Item.SubItems[0];
        Post(atFileDownChunk);
      end;

    else
      Memo1.Lines.Add('不是续传, 无法重启任务.');
  end;
end;

procedure TFormInIOCPFileSvrClient.btnUpChunkClick(Sender: TObject);
var
  Msg: TMessagePack;
begin
  // TInFileClient 没公开方法，用 TMessagePack
  InConnection1.OnDataReceive := nil;
  InConnection1.OnDataSend := InConnection1DataSend;

  Msg := TMessagePack.Create(InFileClient1);
  Msg.LoadFromFile('F:\Backup\jdk-8u77-windows-i586.exe'); // 大唐西域记全译.pdf'); // Ghost\WIN-7-20190228.GHO');
//  Msg.CheckType := ctMurmurHash;  // 可以加校验码，分块传输不支持压缩！

  Msg.Post(atFileUpChunk);
  
end;

procedure TFormInIOCPFileSvrClient.btnUploadClick(Sender: TObject);
begin
  // 上传文件，存放在服务端的用户数据路径
  //   续传例子见：all_in_one
  InConnection1.OnDataReceive := nil;
  InConnection1.OnDataSend := InConnection1DataSend;
  
  InFileClient1.Upload('F:\Backup\jdk-8u77-windows-i586.exe'); // F:\Backup\Ghost\WIN-7-20190228.GHO
end;

procedure TFormInIOCPFileSvrClient.Button1Click(Sender: TObject);
var
  Msg: TCustomPack;
begin
{  Msg := TCustomPack.Create;
  Msg.Initialize('test.txt');

  Msg.AsString['aaaa'] := 'aaaaaaaaaaaaaaaaaaaa22';
  Msg.AsInt64['_offset'] := 123546;
  Msg.AsInt64['_offsetHigh'] := 923546;

  Msg.SaveToFile('test.txt'); }  
end;

procedure TFormInIOCPFileSvrClient.Button2Click(Sender: TObject);
begin
  // 查询文件，保存文件名到列表
  // 带参数时内部自动保存到 FFileList，
  //   否则可以在 InFileClient1ListFiles 中保存文件名到 FFileList
  // 在 InFileClient1ReturnResult 中逐一下载，之后释放 FFileList
  FFileList := TStringlist.Create;
  InFileClient1.ListFiles(FFileList);
end;

procedure TFormInIOCPFileSvrClient.Button3Click(Sender: TObject);
var
  CLientMd5, ServerMd5: String;
begin
  // 检查上传、下载的文件的 MD5
  // client_data\user_a\data\jdk-8u77-windows-i586.exe F:\Backup\jdk-8u77-windows-i586.exe
  // temp\jdk-8u77-windows-i586.exe  WIN-7-20190228.GHO

  CLientMd5 := MD5Print(MD5File(Edit1.Text));
  ServerMd5 := MD5Print(MD5File(Edit2.Text));

  Memo1.Lines.Add('Client: ' + CLientMd5);
  Memo1.Lines.Add('Server: ' + ServerMd5);

  if (CLientMd5 = ServerMd5) then
    Memo1.Lines.Add('MD5=')
  else
    Memo1.Lines.Add('<>');

end;

procedure TFormInIOCPFileSvrClient.btnSetDirClick(Sender: TObject);
begin
  if InFileClient1.Tag = 0 then  // 设置子目录
  begin
    InFileClient1.SetDir('sub'); // 进入 sub 子目录
    InFileClient1.Tag := 1;
    btnSetDir.Caption := 'cd ..';
  end else
  begin
    InFileClient1.SetDir('..');  // 返回父母录
    InFileClient1.Tag := 0;
    btnSetDir.Caption := 'cd sub';    
  end;
end;

procedure TFormInIOCPFileSvrClient.FormCreate(Sender: TObject);
begin
  iocp_utils.IniDateTimeFormat;        // 设置日期时间格式
  
  // 复制自身为 upload_me.exe                    
  CopyFile(PChar(Application.ExeName), PChar('upload_me.exe'), False);

  MyCreateDir(InConnection1.LocalPath); // 下载文件存放路径
end;

procedure TFormInIOCPFileSvrClient.InCertifyClient1Certify(Sender: TObject;
  Action: TActionType; ActResult: Boolean);
begin
  case Action of
    atUserLogin:       // 登录
      if ActResult then begin
        Memo1.Lines.Add(InConnection1.UserName + '登录成功');
    //    Timer1.Enabled := True;
      end else
        Memo1.Lines.Add(InConnection1.UserName + '登录失败');
    atUserLogout:      // 登出
      if ActResult then
        Memo1.Lines.Add(InConnection1.UserName + '登出成功')
      else
        Memo1.Lines.Add(InConnection1.UserName + '登出失败');
  end;
end;

procedure TFormInIOCPFileSvrClient.InConnection1AddWork(Sender: TObject; Msg: TClientParams);
  function CheckWorkExists: Boolean;
  var
    i: Integer;
    Item: TListItem;
  begin
    // 找相应的消息 MsgId
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      Item := ListView1.Items[i];
      if Item.SubItems[1] = IntToStr(Msg.MsgId) then  // 找到对应编号
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
var
  Item: TListItem;
begin
  // 加入任务时，会调用此过程，可以把 Msg 信息加入列表显示
  if (Msg.Action in [atFileList..atFileUpShare]) and not CheckWorkExists then
  begin
    Item := ListView1.Items.Add;
    Item.Caption := IntToStr(Item.Index + 1);

    if Msg.Action in [atFileUpload, atFileUpChunk] then  // 上传，断点上传
      Item.SubItems.Add(Msg.AttachFileName)
    else
      Item.SubItems.Add(Msg.FileName);

    Item.SubItems.Add(IntToStr(Msg.MsgId));  // 消息号
    Item.SubItems.Add(IntToStr(Integer(Msg.Action)));  // 操作类型
    Item.SubItems.Add('...');  // 执行结果标志
  end;
end;

procedure TFormInIOCPFileSvrClient.InConnection1DataReceive(Sender: TObject;
  MsgId, MsgSize, CurrentSize: Int64);
begin
  // 在这里显示接收进程
  ShowProgress(MsgId, MsgSize, CurrentSize);
end;

procedure TFormInIOCPFileSvrClient.InConnection1DataSend(Sender: TObject; MsgId,
  MsgSize, CurrentSize: Int64);
begin
  // 在这里显示接发出进程
  ShowProgress(MsgId, MsgSize, CurrentSize);
end;

procedure TFormInIOCPFileSvrClient.InConnection1Error(Sender: TObject;
  const Msg: string);
begin
  memo1.Lines.Add(Msg);
end;

procedure TFormInIOCPFileSvrClient.InFileClient1ListFiles(Sender: TObject;
  ActResult: TActionResult; No: Integer; Result: TCustomPack);
begin
  // 查询文件的返回结果
  // atFileList, atTextGetFiles 操作会先执行本事件,再执行 OnReturnResult  
  case ActResult of
    arFail:
      Memo1.Lines.Add('目录不存在.');
    arEmpty:
      Memo1.Lines.Add('目录为空.');
    arExists: begin // 列出服务端当前工作路径下的文件
//      if Assigned(FFileList) then  // 不带参数调用 -> 保存到列表，以便下载
//        FFileList.Add(Result.AsString['name']);
      Memo1.Lines.Add(IntToStr(No) + ': ' +
                      Result.AsString['name'] + ', ' +
                      IntToStr(Result.AsInt64['size']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['CreationTime']) + ', ' +
                      DateTimeToStr(Result.AsDateTime['LastWriteTime']) + ', ' +
                      Result.AsString['dir']);
    end;
  end;
end;

procedure TFormInIOCPFileSvrClient.InFileClient1ReturnResult(Sender: TObject; Result: TResultParams);
  procedure MarkListView(Flag: string);
  var
    i: Integer;
    Item: TListItem;
  begin
    // 返回上传下载结果，标志一下列表
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      Item := ListView1.Items[i];
      if Item.SubItems[1] = IntToStr(Result.MsgId) then  // 找到对应编号
      begin
        Item.SubItems[3] := Flag;
        Break;
      end;
    end;
  end;
  procedure DownloadFiles;
  var
    i: Integer;  
  begin
    // 下载列表中的文件
    for i := 0 to FFileList.Count - 1 do
      InFileClient1.Download(FFileList.Strings[i]);
  end;
begin
  case Result.Action of
    atFileSetDir:
      case Result.ActResult of
        arOK:
          Memo1.Lines.Add('设置目录成功.');
        arMissing:
          Memo1.Lines.Add('目录不存在.');
        arFail:
          Memo1.Lines.Add('目录名称错误.');
      end;

    atFileDownload, atFileDownChunk:
      case Result.ActResult of    // 文件不存在
        arFail:
          Memo1.Lines.Add('服务器：打开当前路径的文件失败.');
        arMissing:
          Memo1.Lines.Add('服务器：当前路径的文件不存在/丢失.');
        arOK: begin
//          MarkListView('V');
          Memo1.Lines.Add('下载文件完毕.');
//          lblFileName.Cursor := crDefault;
        end;
      end;
                          
    atFileUpload, atFileUpChunk:
      case Result.ActResult of  // 2.0 不会有 arExists 的结果了
        arFail: begin
//          MarkListView('X');
          Memo1.Lines.Add('服务器建文件异常.');
        end;
        arOK: begin
//          MarkListView('V');
          Memo1.Lines.Add('上传文件完毕.');
//          lblFileName.Cursor := crDefault;
        end;
      end;

    atFileList:  // 查询文件，逐一下载文件
      if Assigned(FFileList) then
        try
          DownloadFiles;
        finally
          FreeAndNil(FFileList);
        end;
  end;
end;

procedure TFormInIOCPFileSvrClient.InFileClient2ReturnResult(Sender: TObject;
  Result: TResultParams);
begin
  if Result.Action = atFileDownload then
    case Result.ActResult of    // 文件不存在
      arMissing:
        Memo1.Lines.Add('服务器文件不存在/丢失.');
      arOK: begin
        Memo1.Lines.Add('下载文件完毕.');
//        lblFileName.Cursor := crDefault;
      end;
    end;
end;

procedure TFormInIOCPFileSvrClient.ListView1Click(Sender: TObject);
begin
  BtnCancel.Enabled := ListView1.Selected <> nil;
  BtnRestart.Enabled := BtnCancel.Enabled;
  if BtnCancel.Enabled  then
    FMsgId := ListView1.Selected.SubItems[1];
end;

procedure TFormInIOCPFileSvrClient.ShowProgress(MsgId, MsgSize, CurrentSize: Int64);
var
  i: Integer;
  Item: TListItem;
begin
  // 返回上传下载结果，标志一下列表
  
  // 直接设置会 超大, 用千分数
  ProgressBar1.Position := CurrentSize * 100 div MsgSize;

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    Item := ListView1.Items[i];
    if Item.SubItems[1] = IntToStr(MsgId) then  // 找到对应编号
    begin
      Item.SubItems[3] := FormatFloat('00.00', CurrentSize * 100 / MsgSize) + '%';
      Break;
    end;
  end;

  Memo1.Lines.Add(IntToStr(CurrentSize) + '/' + IntToStr(MsgSize));

end;

end.
