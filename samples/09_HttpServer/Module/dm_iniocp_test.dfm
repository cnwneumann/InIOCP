object dmInIOCPTest: TdmInIOCPTest
  OldCreateOrder = False
  OnCreate = InIOCPDataModuleCreate
  OnDestroy = InIOCPDataModuleDestroy
  OnHttpExecQuery = InIOCPDataModuleHttpExecQuery
  OnHttpExecSQL = InIOCPDataModuleHttpExecSQL
  Height = 229
  Width = 531
  object DataSetProvider1: TDataSetProvider
    Left = 168
    Top = 24
  end
  object InSQLManager1: TInSQLManager
    Left = 64
    Top = 24
  end
end
