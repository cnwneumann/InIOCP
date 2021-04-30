object dmInIOCPTest: TdmInIOCPTest
  OldCreateOrder = False
  OnCreate = InIOCPDataModuleCreate
  OnDestroy = InIOCPDataModuleDestroy
  OnApplyUpdates = InIOCPDataModuleApplyUpdates
  OnExecQuery = InIOCPDataModuleExecQuery
  OnExecSQL = InIOCPDataModuleExecSQL
  OnExecStoredProcedure = InIOCPDataModuleExecStoredProcedure
  OnHttpExecQuery = InIOCPDataModuleHttpExecQuery
  OnHttpExecSQL = InIOCPDataModuleHttpExecSQL
  OnWebSocketQuery = InIOCPDataModuleWebSocketQuery
  OnWebSocketUpdates = InIOCPDataModuleWebSocketUpdates
  Height = 229
  Width = 531
  object DataSetProvider1: TDataSetProvider
    Left = 192
    Top = 24
  end
  object InSQLManager1: TInSQLManager
    Left = 64
    Top = 24
  end
end
