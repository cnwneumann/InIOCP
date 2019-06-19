object dmInIOCPTest: TdmInIOCPTest
  OldCreateOrder = False
  OnCreate = InIOCPDataModuleCreate
  OnDestroy = InIOCPDataModuleDestroy
  OnWebSocketQuery = InIOCPDataModuleWebSocketQuery
  OnWebSocketUpdates = InIOCPDataModuleWebSocketUpdates
  Height = 213
  Width = 531
  object DataSetProvider1: TDataSetProvider
    Left = 88
    Top = 40
  end
end
