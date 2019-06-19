unit iocp_reg;

interface

{$I in_iocp.inc}

uses
  {$IFDEF DELPHI_XE7}System.Classes,{$ELSE}Classes,{$ENDIF}
  DesignIntf, DesignEditors, ToolIntf, ToolsAPI, EditIntf, ExptIntf,
  iocp_Server, iocp_managers, iocp_clients, iocp_sqlMgr,
  iocp_wsClients, iocp_baseModule;
                               
type

  { TInSQLManager 编辑器 }

  TInSQLManagerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  frmInSQLMgrEditor;  // SQL 编辑器单元，请设定搜索路径
  
procedure Register;
begin
  RegisterComponents('InIOCP服务组件库(服务端)', [TInIOCPServer, TInIOCPBroker,
                     TInClientManager, TInMessageManager, TInFileManager,
                     TInDatabaseManager, TInCustomManager, TInRemoteFunctionGroup,
                     TInHttpDataProvider, TInWebSocketManager, TInSQLManager]);

  RegisterComponents('InIOCP服务组件库(客户端)', [TInConnection,
                     TInEchoClient, TInCertifyClient, TInMessageClient,
                     TInFileClient, TInCustomClient, TInFunctionClient,
                     TInDBConnection, TInDBQueryClient, TInDBSQLClient,
                     TInWSConnection]);

  RegisterComponentEditor(TInSQLManager, TInSQLManagerEditor);
  RegisterCustomModule(TInIOCPDataModule, TCustomModule);
end;

{ TInSQLManagerEditor }

procedure TInSQLManagerEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:
      if EditInSQLManager(TInSQLManager(Component)) then
        Designer.Modified;
  end;
end;

function TInSQLManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'SQL 编辑器(&E)...';
    1: Result := 'SQL 编辑器(&E)...';
  end;
end;

function TInSQLManagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

