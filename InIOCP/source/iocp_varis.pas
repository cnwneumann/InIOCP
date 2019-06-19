(*
 * icop 全局变量单元
 *)
unit iocp_varis;

interface

 uses
   SysUtils, iocp_utils;

var
       gAppPath: String;  // 程序路径
      gTempPath: String;  // 临时路径
  gUserDataPath: String;  // 服务端用户数据路径

     WriteInLog: procedure(const Msg: AnsiString) = nil;

implementation

initialization
   gAppPath := ExtractFilePath(ParamStr(0));
  gTempPath := AddBackslash(GetSystemTempDir());

finalization

end.
