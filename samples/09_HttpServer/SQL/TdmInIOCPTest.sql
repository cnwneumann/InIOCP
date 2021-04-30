//
// InIOCP TInSQLManager 资源文件(文本格式)
//

// 每一段内容为一个 SQL 命令，
//   可以带 Delphi 的参数指示符号“:”（此时的请求命令中要带相应的参数和参数值）；
//   命令行的首尾字符不能为“[”和“]”；
//   每段中可以用行首注解“//”。

[Select_tbl_xzqh]

SELECT *
FROM tbl_xzqh

[Select_tbl_xzqh2]

SELECT code, detail
FROM tbl_xzqh WHERE code<:code

[Select_tbl_xzqh4]

SELECT code, detail
FROM tbl_xzqh
WHERE detail=:detail

[Update_xzqh]

UPDATE tbl_xzqh SET code = 001 WHERE code IS NULL

[Stored_select]

SELECT * FROM tbl_xzqh WHERE code < '110105' ORDER BY code
