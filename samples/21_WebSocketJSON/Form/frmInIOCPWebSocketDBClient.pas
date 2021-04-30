unit frmInIOCPWebSocketDBClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fmIOCPSvrInfo, iocp_managers, iocp_base,
  http_objects, iocp_server, iocp_sockets, iocp_wsClients, DB, DBClient, Grids,
  DBGrids, ExtCtrls, iocp_clientBase;

type
  TFormInIOCPWsDBClient = class(TForm)
    InWSConnection1: TInWSConnection;          
    btnConnect: TButton;
    btnSend: TButton;
    btnDBQuery: TButton;
    Memo1: TMemo;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Image1: TImage;
    btnDBUpdate: TButton;
    btnListFiles: TButton;
    Button3: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure InWSConnection1AfterConnect(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
    procedure InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
    procedure InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
    procedure btnDBQueryClick(Sender: TObject);
    procedure ClientDataSet1AfterScroll(DataSet: TDataSet);
    procedure btnDBUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnListFilesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FSourceTable: string;
  public
    { Public declarations }
  end;

var
  FormInIOCPWsDBClient: TFormInIOCPWsDBClient;

implementation

uses
  MidasLib, iocp_varis, jpeg, iocp_utils, iocp_log,
  iocp_msgPacks, dm_iniocp_test;

{$R *.dfm}

procedure TFormInIOCPWsDBClient.btnConnectClick(Sender: TObject);
begin
  InWSConnection1.Active := not InWSConnection1.Active;
end;

procedure TFormInIOCPWsDBClient.btnSendClick(Sender: TObject);
var
  JSON: TJSONMessage;
begin

  // 方法 1，使用 InWSConnection1 的属性 JSON：
{  with InWSConnection1.JSON do
  begin
    S['aaa'] := 'WebSocket test + 中文.';
    S['BBB'] := 'ba +中文';
    Post;
  end;     }

  // 方法 2：
  JSON := TJSONMessage.Create(InWSConnection1);
  JSON.S['aaa'] := '测试 InIOCP 的 WebSocket 客户端，中文.';
  JSON.S['BBB'] := 'bbb 中文';

  JSON.Attachment := TFileStream.Create('doc\InIOCPWebSocketJSON.7z', fmOpenRead);
  JSON.S['attach'] := 'InIOCPWebSocketJSON.7z';  // 给附件命名，方便服务端保存

  JSON.Post;

end;

procedure TFormInIOCPWsDBClient.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TFormInIOCPWsDBClient.btnDBQueryClick(Sender: TObject);
begin
  // 数据库查询
  //   为与其他操作区别开来，用 Action 属性
  with InWSConnection1.JSON do
  begin
    Action := 11;  // 服务端要返回这个操作码供客户端判断使用
    S['aaa'] := 'WebSocket 数据库查询.';
//    Text := '{"Id":123,"Name":"我","Boolean":True,"Stream":Null,"_Variant":' +
//            '{"Length":5,"Data":"abcde"},"_zzz":2345,"KKK":"aaabbbccc"}'; 
    Post;
  end;
end;

procedure TFormInIOCPWsDBClient.btnDBUpdateClick(Sender: TObject);
begin
  // 数据库更新
  //   为与其他操作区别开来，用 Action 属性
  with InWSConnection1.JSON do
  begin
    Action := 12; // 服务端要返回这个操作码供客户端判断使用
    SetRemoteTable(ClientDataSet1, FSourceTable); // 设置要更新的数据表（服务端已经关闭）

    // 用 Variant 类型，自动压缩，服务端取 V['_delta'] 更新，自动解压
    V['_delta'] := ClientDataSet1.Delta;
    Post;
  end;
end;

procedure TFormInIOCPWsDBClient.btnListFilesClick(Sender: TObject);
begin
  // 先查询服务端 Form 下的文件
  // InIOCP-JSON 支持记录对象 R[]，每文件当中一条记录，
  //   见单元 iocp_WsJSON，iocp_managers 的 TInFileManager.ListFiles
  with InWSConnection1.JSON do
  begin
    Action := 20;
    S['Path'] := 'form\';
    Post;
  end;
end;

procedure TFormInIOCPWsDBClient.ClientDataSet1AfterScroll(DataSet: TDataSet);
var
  Field: TField;
  Stream: TMemoryStream;
  JpegPic: TJpegImage;
begin
  if ClientDataSet1.Active then
  begin
    Field := ClientDataSet1.FieldByName('picture');
    if Field.IsNull then
      Image1.Picture.Graphic := nil
    else begin
      Stream := TMemoryStream.Create;
      JpegPic := TJpegImage.Create;
      try
        TBlobField(Field).SaveToStream(Stream);
        Stream.Position := 0;           // 必须
        JpegPic.LoadFromStream(Stream);
        Image1.Picture.Graphic := JpegPic;
      finally
        JpegPic.Free;
        Stream.Free;
      end;
    end;
  end;
end;

procedure TFormInIOCPWsDBClient.FormCreate(Sender: TObject);
begin
  InWSConnection1.ServerAddr := 'localhost'; //'192.168.1.196';
end;

procedure TFormInIOCPWsDBClient.InWSConnection1AfterConnect(Sender: TObject);
begin
  if InWSConnection1.Active then
  begin
    btnConnect.Caption := '断开';
    btnSend.Enabled := True;
    btnListFiles.Enabled := True;
    btnDBQuery.Enabled := True;
    btnDBUpdate.Enabled := True;
  end else
  begin
    btnConnect.Caption := '连接';
    btnSend.Enabled := False;
    btnListFiles.Enabled := False;
    btnDBQuery.Enabled := False;
    btnDBUpdate.Enabled := False;
  end;
end;

procedure TFormInIOCPWsDBClient.InWSConnection1ReceiveData(Sender: TObject; const Msg: string);
begin
  // Sender: 是 TInWSConnection
  //    Msg：收到消息文本
  // 这里收到的不是 InIOCP-JSON 消息（可能未封装的数据）
  // 测试方法：用浏览器发送一条消息，在服务端广播它
end;

procedure TFormInIOCPWsDBClient.InWSConnection1ReceiveMsg(Sender: TObject; Msg: TJSONResult);
begin
  //  Sender: 是 TInWSConnection
  //     Msg: JSON 消息，系统会自动释放
  // 这里收到的是其他客户端推送来到 InIOCP-JSON 消息（被动接收）
  // 测试方法：用 TJSONMessage 发送消息，在服务端广播它
end;

procedure TFormInIOCPWsDBClient.InWSConnection1ReturnResult(Sender: TObject; Result: TJSONResult);
var
  i: Integer;
begin
  // Sender: 是 TInWSConnection
  // Result: JSON 消息，系统会自动释放
  
  // Result.MsgType: 消息类型
  //   1. mtJSON：Result 是 JSON 消息
  //   2. mtAttachment：Result 是附件流（系统自动释放，不要 Result.Attachment.Free）

  // 测试方法：服务端收到消息后，设置 Socket.Result，用 Socket.SendResult 发送回来。

  if Result.MsgType = mtJSON then
    case Result.Action of
      11: begin
        // 服务端返回的数据集变量，要压缩
        FSourceTable := Result.S['_table'];  // 保存要更新的数据表名称
        ClientDataSet1.Data := Result.V['_data'];
      end;

      12: begin
        // 服务端返回 更新结果
        // 本地要合并修改过的内容
        ClientDataSet1.MergeChangeLog;
        Memo1.Lines.Add(Result.S['result']);
      end;

      20:  // 服务端返回 文件列表
        if Result.I['count'] = -1 then  // 路径错误
          Memo1.Lines.Add('路径错误.')
        else
          for i := 1 to Result.I['count'] do  
            with Result.R[IntToStr(i)] do  // 逐一取记录
            begin
              Memo1.Lines.Add(S['name']);  // 可以见请求消息，下载
              Free; // 释放记录 R[IntToStr(i)]
            end;

      else begin
        if Result.HasAttachment then  // 有附件流，接收（可以不接收）
          Result.Attachment := TFileStream.Create('temp\客户端收到' + Result.S['attach'], fmCreate);
        Memo1.Lines.Add('服务端返回给自己的 JSON 消息: ' + Result.S['return']);
      end;

    end
    
  else  // 是附件流
  if Assigned(Result.Attachment) then  // 收到文件流
  begin
    Memo1.Lines.Add('接收文件完毕（系统会自动关闭附件流）');
  end else
    Memo1.Lines.Add('没有接收文件');
end;

end.
