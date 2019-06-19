unit frmInIOCPWebQueryScores;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iocp_sockets, iocp_managers, iocp_server,
  http_base, http_objects, fmIOCPSvrInfo;

type
  TFormInIOCPWebQueryScores = class(TForm)
    InIOCPServer1: TInIOCPServer;
    InHttpDataProvider1: TInHttpDataProvider;
    btnStart: TButton;
    btnStop: TButton;
    FrameIOCPSvrInfo1: TFrameIOCPSvrInfo;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure InHttpDataProvider1Get(Sender: TObject; Request: THttpRequest;
      Respone: THttpRespone);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InHttpDataProvider1Accept(Sender: TObject; Request: THttpRequest;
      var Accept: Boolean);
    procedure InIOCPServer1AfterOpen(Sender: TObject);
  private
    { Private declarations }
    FAppDir: String;
    FWebSitePath: String;
  public
    { Public declarations }
  end;

var
  FormInIOCPWebQueryScores: TFormInIOCPWebQueryScores;

implementation

uses
  iocp_log, iocp_utils, iocp_msgPacks, http_utils;

type

  // 准考证号
  TExamNumber = string[10];

  // 成绩记录
  PStudentScores = ^TStudentScores;
  TStudentScores = record
    ExamNo: TExamNumber;   // 准考证号
    Scores: AnsiString;    // JSON 格式的成绩单
  end;

  // 三个字符的类型
  PThreadChars = ^TThreadChars;
  TThreadChars = array[0..2] of AnsiChar;

var
  // 分数数组
  FScores: array of TStudentScores;

procedure LoadFromTextFile(const FileName: string);
  procedure ExtractString(var Scores, LeftValue: AnsiString);
  var
    i: Integer;
  begin
    // 提取左侧内容
    i := Pos(#9, Scores);
    if (i > 0) then
    begin
      LeftValue := Copy(Scores, 1, i - 1);
      Delete(Scores, 1, i);
    end else
    begin
      LeftValue := Scores;
      Scores := '';
    end;
  end;
  procedure WriteData(const S: AnsiString; var ToBuf: PAnsiChar; ItemType: Integer);
  begin
    // 写字段或字段值
    if (Length(S) > 0) then
    begin
      System.Move(S[1], ToBuf^, Length(S));
      Inc(ToBuf, Length(S));
    end;
    case ItemType of
      0:  // 字段
        PThreadChars(ToBuf)^ := AnsiString('":"');
      1:  // 值
        PThreadChars(ToBuf)^ := AnsiString('","')
      else  // 结束
        PThreadChars(ToBuf)^ := AnsiString('"}]');
    end;
    Inc(ToBuf, 3);  // 前移 3 字节
  end;
var
  i: Integer;
  LeftVal, S: AnsiString;
  Student: PStudentScores;
  p: PAnsiChar;
  Strs: TStrings;
begin
  // 从以 TAB 分隔的文本文件读入成绩（不带标题）
  // 字段：准考证号	姓名	语文	数学	总分	等级
  // JSON: [{"ExamNo":"123456789","name":"..","chinese":"80","maths":"90","total":"170","level":"AAAA"}]
  Strs := TStringList.Create;
  try
    Strs.LoadFromFile(FileName);

    // 设置数组长度（每行一条记录）
    SetLength(FScores, Strs.Count);

    for i := 0 to Strs.Count - 1 do
    begin
      S := Trim(Strs[i]);
      if (Length(S) = 0) or (S[1] = '/') then  // 为空或注解
        Continue;
        
      Student := @FScores[i];
      SetLength(Student^.Scores, Length(S) * 5);  // 预设 JSON 空间
      p := PAnsiChar(Student^.Scores); // 写入的地址

      // 开始
      PThreadChars(p)^ := AnsiString('[{"');
      Inc(p, 3);  // 前移 3 字节

      // 内容不多，逐一提取字段内容，加入
      
      // 1. 准考证号
      WriteData('examNo', p, 0);
      ExtractString(S, LeftVal);

      Student^.ExamNo := LeftVal;
      WriteData(LeftVal, p, 1);

      // 2. 姓名
      WriteData('name', p, 0);
      ExtractString(S, LeftVal);
      WriteData(LeftVal, p, 1);

      // 3. 语文
      WriteData('chinese', p, 0);
      ExtractString(S, LeftVal);
      WriteData(LeftVal, p, 1);

      // 4. 数学
      WriteData('maths', p, 0);
      ExtractString(S, LeftVal);
      WriteData(LeftVal, p, 1);

      // 5. 总分
      WriteData('total', p, 0);
      ExtractString(S, LeftVal);
      WriteData(LeftVal, p, 1);

      // 6. 等级
      WriteData('level', p, 0);
      ExtractString(S, LeftVal);
      WriteData(LeftVal, p, 2);  // 结束

      // 删除多余的空间
      Delete(FScores[i].Scores, Integer(p - PAnsiChar(FScores[i].Scores)) + 1, 999);
      
    end;
  finally
    Strs.Free;
  end;
end;

{$R *.dfm}

procedure TFormInIOCPWebQueryScores.btnStartClick(Sender: TObject);
begin
  iocp_log.TLogThread.InitLog;  // 开启日志

  // 从以 TAB 分隔的文本文件读入成绩
  // 考生数量、成绩单都相对固定，不用数据库，直接使用数组保存成绩。
  LoadFromTextFile(FWebSitePath + 'scores.txt');

  InIOCPServer1.Active := True;  // 开启服务
  FrameIOCPSvrInfo1.Start(InIOCPServer1);  // 开始统计
end;

procedure TFormInIOCPWebQueryScores.btnStopClick(Sender: TObject);
begin
  InIOCPServer1.Active := False;   // 停止服务
  FrameIOCPSvrInfo1.Stop;       // 停止统计
  iocp_log.TLogThread.StopLog;  // 停止日志
  if (FScores <> nil) then  // 释放数组
    SetLength(FScores, 0);
end;

procedure TFormInIOCPWebQueryScores.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  btnStopClick(nil);
end;

procedure TFormInIOCPWebQueryScores.FormCreate(Sender: TObject);
begin
  FAppDir := ExtractFilePath(Application.ExeName); // 本地路径
  FWebSitePath := FAppDir + AddBackslash(InHttpDataProvider1.RootDirectory);  // 网站路径
  MyCreateDir(FAppDir + 'log');  // 日志路径
end;

procedure TFormInIOCPWebQueryScores.InHttpDataProvider1Accept(Sender: TObject;
  Request: THttpRequest; var Accept: Boolean);
begin
  // 在此判断是否接受请求:
  //   Request.Method: 方法
  //      Request.URI：路径/资源
  IF (Request.Method = hmGet) then
    Accept := True   // Request.URI = '/QueryScores';  // 允许
  else
    Accept := False; // 拒绝
end;

procedure TFormInIOCPWebQueryScores.InHttpDataProvider1Get(Sender: TObject;
  Request: THttpRequest; Respone: THttpRespone);
var
  i: Integer;
  ExamNo: TExamNumber;
  Scores: PStudentScores;
begin
  // Get: 查询准考证号，返回 JSON 数据（这种情况无需加锁）
  if (Request.URI = '/queryScores.do') then
  begin
    // 查询成绩
    ExamNo := Request.Params.AsString['exam_no'];
    if (Length(ExamNo) > 0) then
      for i := 0 to High(FScores) do
      begin
        Scores := @FScores[i];
        if (Scores^.ExamNo = ExamNo) then // 找到记录
        begin
          Respone.SetContent(Scores^.Scores);  // 发送 JSON 成绩
          Exit;
        end;
      end;
    // 考生不存在！
    Respone.SetContent('NOT_EXISTS');
  end else
  if (Request.URI = '/return') then
  begin
    // 重定位
    Respone.Redirect('/');
  end else
  if (Request.URI = '/query.htm') then
  begin
    // 返回查询页
    Respone.TransmitFile(FWebSitePath + 'query.htm');
  end else
  if (Request.URI = '/favicon.ico') then
  begin
    Respone.StatusCode := 204;  // 没有东西
  end else
  begin
    // 返回首页
    Respone.TransmitFile(FWebSitePath + 'index.htm');
  end;
end;

procedure TFormInIOCPWebQueryScores.InIOCPServer1AfterOpen(Sender: TObject);
begin
  btnStart.Enabled := not InIOCPServer1.Active;
  btnStop.Enabled := InIOCPServer1.Active;
end;

end.
