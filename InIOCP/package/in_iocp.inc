
{$DEFINE DEBUG_MODE}     // 调试模式（写调试日志）
{$DEFINE TRANSMIT_FILE}  // 服务端用 TransmitFile 发送数据（对中大文件传输有优势）

{$IF CompilerVersion >= 32}   
    {$DEFINE DELPHI_XE7UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE}
{$ELSE}

{$IF CompilerVersion >= 30}   
    {$DEFINE DELPHI_XE7UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE}
{$ELSE}

{$IF CompilerVersion >= 26}
    {$DEFINE DELPHI_XE7UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE}
{$ELSE}

{$IF CompilerVersion >= 22}   
    {$DEFINE DELPHI_XE}
{$ELSE}

{$IF CompilerVersion >= 18.5}
    {$DEFINE DELPHI_2007}
{$ELSE}

{$IF CompilerVersion >= 15}
    {$DEFINE DELPHI_7}

{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}


{$IF CompilerVersion >= 21}
     {$DEFINE USE_UNICODE}
{$ELSE}
     {$DEFINE NO_UNICODE}
{$IFEND}


{$IF CompilerVersion >= 17}
     {$DEFINE USE_INLINE}
{$ELSE}
     {$DEFINE NO_INLINE}
{$IFEND}


(*
VER320	XE10  
VER310	XE9
VER290	XE8  
VER280	XE7
VER270	XE6         
VER260	XE5
VER250    XE4    
VER240    XE3  
VER230    XE2
VER220    XE
VER210    2010
VER200    2009
VER190    2007
VER180 or VER185    2007
VER180    2006
VER170    2005
VER160    8
VER150    7
*)
