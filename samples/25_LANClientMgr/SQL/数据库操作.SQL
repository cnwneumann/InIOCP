
// CREATE TABLE TBL_USERS (
//   USER_CODE CHAR(6) NOT NULL UNIQUE,
//   USER_NAME VARCHAR(20) NOT NULL PRIMARY KEY,
//   USER_PASSWORD CHAR(10) NOT NULL,
//   USER_LEVEL INTEGER NOT NULL,
//   USER_REAL_NAME VARCHAR(20),
//   USER_TELEPHONE VARCHAR(30),
//   USER_LOGIN_TIME TIMESTAMP,
//   USER_LOGOUT_TIME TIMESTAMP,
//   ACT_EXECUTOR VARCHAR(20)
// )

[USER_DELETE]
DELETE FROM tbl_users WHERE user_name = :user_name

[USER_LOGIN]
SELECT user_level FROM tbl_users
WHERE user_name = :user_name AND user_password = :user_password

[USER_LOGIN_UPDATE]
UPDATE tbl_users
SET user_login_time = DATETIME('NOW')
WHERE user_name = :user_name

[USER_LOGOUT]
UPDATE tbl_users
SET user_logout_time = DATETIME('now')
WHERE user_name = :user_name

[USER_MODIFY]
UPDATE tbl_users SET
 user_password = :user_password, user_level = :user_level,
 user_real_name = :user_real_name, user_telephone = :user_telephone,
 act_executor = :_UserName
WHERE user_name = :_ToUser

[USER_QUERY_INFO]
SELECT * FROM tbl_users WHERE user_name = :user_name

[USER_REGISTER]
// _UserName -> ×Ö¶Î act_executor
// _ToUser   -> ×Ö¶Î user_name
INSERT INTO tbl_users
 (user_code, user_name, user_password, user_level, act_executor, user_register_time) VALUES
 (:user_code, :_ToUser, :user_password, :user_level, :_UserName, DATETIME('now'))

[USER_QUERY_ALL]
SELECT * FROM tbl_users ORDER BY user_code
