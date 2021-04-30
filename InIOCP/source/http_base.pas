(*
 * http 服务基本类型常量等
 *)
unit http_base;

interface

{$I in_iocp.inc}

type

  // 请求方法

  THttpMethod = (
    hmUnknown, hmGet, hmPost, hmHead,
    hmConnect, hmDelete, hmPut, hmOptions, hmTrace
  );

  // POST 文件的状态

  THttpPostState = (
    hpsUnknown,        // 未知
    hpsRequest,        // 请求
    hpsRecvData        // 数据
  );

  // HTTP 表单类型

  THttpContentType = (
    hctUnknown,        // 原始数据流
    hctUrlEncoded,     // application/x-www-form-urlencoded
    hctTextPlain,      // text/plain
    hctMultiPart       // multipart/form-data; boundary=...
  );

  // 字符集（格式）

  THttpCharSet = (
    hcsDefault,        // 默认: CharSet=gb2312
    hcsUTF8,           // UTF-8
    hcsURLEncode       // UTF-8 URL Encode
  );

  // form-data 的元素类型

  TFormElementType  = (fdtUnknown, fdtName, fdtFileName);

  // 变量类型

  THttpFieldType = (hftString, hftInteger, hftFloat);

  // 请求头分类：30+5 个
  // 为加快对 REQUEST_HEADERS 的查找，把常用的放前
  TRequestHeaderType = (
    rqhUnknown, rqhHost, rqhContentType, rqhContentLength,  // 0..3
    rqhConnection, rqhUserAgent, rqhCookie, rqhReferer, rqhRange, // 4..8
    rqhIfMatch, rqhIfModifiedSince, rqhIfNoneMatch, rqhIfRange, // 9..12
    rqhIfUnmodifiedSince, rqhDate, rqhUpgrade, rqhAccept, // 13..16
    rqhAcceptCharset, rqhAcceptEncoding, rqhAcceptLang, rqhAcceptRanges, // 17..20
    rqhAuthor, rqhCacheControl, rqhExpect, rqhFrom, rqhMaxForwards, // 21..25
    rqhPragma, rqhProxyAuthor, rqhTE, rqhVia, rqhWarning, // 26..30
    rqhWebSocketKey, rqhWebSocketProtocol,  // webSocket, 31..32
    rqhWebSocketVersion, rqhWebSocketExten, // webSocket, 33..34
    rqhOrigin  // webSocket, 33..34
  );

  // 响应头分类
  TResponseHeaderType = (
    rshUnknown, rshAcceptRanges, rshAge, rshAllow, rshCacheControl, // 0..4
    rshConnection, rshContentEncoding, rshContentLang, rshContentLength, // 5..8
    rshContentLocation, rshContentMD5, rshContentRange, rshContentType, // 9..12
    rshDate, rshETag, rshExpires, rshLastModified, rshLocation, rshPragma, // 13..18
    rshProxyAuthenticate, rshRefresh, rshRetryAfter, rshServer, // 19.22
    rshSetCookie, rshTrailer, rshTransferEncoding, rshVary, rshVia, // 23..27
    rshWarning, rshWWWAuthenticate,  // 28..29
    rshUpgrade, rshWebSocketAccept, rshWebSocketProtocol // webSocket
  );

  // 文件/内容类型
  PContentType = ^TContentType;
  TContentType = packed record
    Extension: String[10];      // 是单字节
    ContentType: AnsiString;
  end;

  // 回车换行类型
  TStrCRLF  = array[0..1] of AnsiChar;
  PStrCRLF  = ^TStrCRLF;

  TStrCRLF2 = array[0..3] of AnsiChar;
  PStrCRLF2 = ^TStrCRLF2;

  // Http 分块发送时的长度描述空间
  TChunkSize = array[0..5] of AnsiChar;
  PChunkSize = ^TChunkSize;

const
  CHAR_CR   = AnsiChar(#13);               // 回车
  CHAR_LF   = AnsiChar(#10);               // 换行
  CHAR_SP   = AnsiChar(#32);               // 空格
  CHAR_TAB  = AnsiChar(#9);                // 制表符
  CHAR_SC   = AnsiChar(':');               // 分号
  CHAR_SC2  = AnsiString(':'#32);          // 分号 + 空格
  QUE_MARK  = AnsiChar('?');               // 问号
  SEMICOLON = AnsiChar(';');               // 分号
                                   
  STR_CRLF  = AnsiString(#13#10);         // 回车换行
  STR_CRLF2 = AnsiString(#13#10#13#10);   // 两个回车换行
  HTTP_VER  = AnsiString('HTTP/1.1');     // http 版本
  HTTP_VER1 = AnsiString('HTTP/1.0');     // http 版本     

  {$IFDEF TRANSMIT_FILE}
  HTTP_SERVER_NAME     = AnsiString('InIOCP/2.8t');
  {$ELSE}
  HTTP_SERVER_NAME     = AnsiString('InIOCP/2.8');
  {$ENDIF}

  HTTP_SESSION_ID      = AnsiString('InIOCP_SID');
  HTTP_INVALID_SESSION = AnsiString('INVALID_SESSION');

  // http/1.1 代理响应
  HTTP_PROXY_RESPONSE = 'HTTP/1.1 200 Connection Established'#13#10'' +
                        'Proxy-agent: ' + HTTP_SERVER_NAME + #13#10#13#10;

  // 实体长度限制 20m
  MAX_CONTENT_LENGTH  = 20480000;

  // 实体最大发送长度 2G
  MAX_TRANSMIT_LENGTH = 2147483646;

  // 字符集
  HTTP_CHAR_SETS: array[THttpCharSet] of AnsiString = (
    '; CharSet=gb2312', '; CharSet=utf-8', '; CharSet=utf-8'
  );

  // http/1.1 命令表
  METHOD_LIST: array[THttpMethod] of AnsiString = (
    'NONE', 'GET', 'POST', 'HEAD', 'CONNECT',
    'DELETE', 'PUT', 'OPTIONS', 'TRACE'
  );

  // 请求头名称（与 TRequestHeaderType 对应，用于查找 Header 类型）
  REQUEST_HEADERS: array[TRequestHeaderType] of AnsiString = (
    'NONE', 'HOST', 'CONTENT-TYPE', 'CONTENT-LENGTH',
    'CONNECTION', 'USER-AGENT', 'COOKIE', 'REFERER', 'RANGE',
    'IF-MATCH', 'IF-MODIFIED-SINCE', 'IF-NONE-MATCH', 'IF-RANGE',
    'IF-UNMODIFIED-SINCE', 'DATE', 'UPGRADE', 'ACCEPT', 'ACCEPT-CHARSET',
    'ACCEPT-ENCODING', 'ACCEPT-LANGUAGE', 'ACCEPT-RANGES',
    'AUTHORIZATION', 'CACHE-CONTROL', 'EXPECT', 'FROM',
    'MAX-FORWARDS',  'PRAGMA', 'PROXY-AUTHORIZATION',
    'TE', 'VIA', 'WARNING',
    'SEC-WEBSOCKET-KEY', 'SEC-WEBSOCKET-PROTOCOL',  // WebSocket Protocol
    'SEC-WEBSOCKET-VERSION', 'SEC-WEBSOCKET-EXTENSIONS', // WebSocket Protocol
    'ORIGIN' // WebSocket Protocol
   );

  // 响应头名称（与 TResponseHeaderType 对应，用于返回给客户端）
  RESPONSE_HEADERS: array[TResponseHeaderType] of AnsiString = (
    'None', 'Accept-Ranges', 'Age', 'Allow', 'Cache-Control',
    'Connection', 'Content-Encoding', 'Content-Language', 'Content-Length',
    'Content-Location', 'Content-MD5', 'Content-Range', 'Content-Type',
    'Date', 'Etag', 'Expires', 'Last-Modified', 'Location', 'Pragma',
    'Proxy-Authenticate', 'Refresh', 'Retry-After', 'Server', 'Set-Cookie',
    'Trailer', 'Transfer-Encoding', 'Vary', 'Via',
    'Warning', 'WWW-Authenticate',
    'Upgrade', 'Sec-WebSocket-Accept', 'Sec-WebSocket-Protocol'  // WebSocket
  );

  // 响应状态代码、含义

  HTTP_STATES_100: array[0..1] of AnsiString = (
    ' 100 Continue', ' 101 Switching Protocols'
  );

  HTTP_STATES_200: array[0..6] of AnsiString = (
    ' 200 OK', ' 201 Created', ' 202 Accepted',
    ' 203 Non-Authoritative Information', ' 204 No Content',
    ' 205 Reset Content', ' 206 Partial Content'
  );

  HTTP_STATES_300: array[0..7] of AnsiString = (
    ' 300 Multiple Choices', ' 301 Moved Permanently', ' 302 Found',
    ' 303 See Other', ' 304 Not Modified', ' 305 Use Proxy', ' 306 None',
    ' 307 Temporary Redirect'
  );

  HTTP_STATES_400: array[0..17] of AnsiString = (
    ' 400 Bad Request', ' 401 Unauthorized', ' 402 Payment Required',
    ' 403 Forbidden', ' 404 Not Found', ' 405 Method Not Allowed',
    ' 406 Not Acceptable', ' 407 Proxy Authentication Required',
    ' 408 Request Time-out', ' 409 Conflict', ' 410 Gone', ' 411 Length Required',
    ' 412 Precondition Failed', ' 413 Request Entity Too Large', ' 414 Request-URI Too Large',
    ' 415 Unsupported Media Type', ' 416 Requested range not satisfiable',
    ' 417 Expectation Failed'
  );

  HTTP_STATES_500: array[0..5] of AnsiString = (
    ' 500 Internal Server Error', ' 501 Not Implemented', ' 502 Bad Gateway',
    ' 503 Service Unavailable', ' 504 Gateway Time-out', ' 505 HTTP Version Not Supported'
  );

  // 文件/内容类型（常用的置前）
  CONTENT_TYPES: array[0..321] of TContentType = (
    (Extension: '.*';       ContentType: 'application/octet-stream'),

    (Extension: '.htm';     ContentType: 'text/html'),
    (Extension: '.html';    ContentType: 'text/html'),
    (Extension: '.css';     ContentType: 'text/css'),
    (Extension: '.jpg';     ContentType: 'image/jpeg'),
    (Extension: '.jpeg';    ContentType: 'image/jpeg'),
    (Extension: '.js';      ContentType: 'application/x-javascript'),
    (Extension: '.jsp';     ContentType: 'text/html'),

    (Extension: '.mp3';     ContentType: 'audio/mp3'),
    (Extension: '.mp4';     ContentType: 'video/mpeg4'),
    (Extension: '.mpeg';    ContentType: 'video/mpg'),
    (Extension: '.mpg';     ContentType: 'video/mpg'),
    
    (Extension: '.a11';     ContentType: 'application/x-a11'),
    (Extension: '.acp';     ContentType: 'audio/x-mei-aac'),
    (Extension: '.ai';      ContentType: 'application/postscript'),
    (Extension: '.aif';     ContentType: 'audio/aiff'),
    (Extension: '.aifc';    ContentType: 'audio/aiff'),
    (Extension: '.aiff';    ContentType: 'audio/aiff'),
    (Extension: '.anv';     ContentType: 'application/x-anv'),
    (Extension: '.apk';     ContentType: 'application/vnd.android.package-archive'),
    (Extension: '.asa';     ContentType: 'text/asa'),
    (Extension: '.asf';     ContentType: 'video/x-ms-asf'),
    (Extension: '.asp';     ContentType: 'text/asp'),
    (Extension: '.asx';     ContentType: 'video/x-ms-asf'),
    (Extension: '.au';      ContentType: 'audio/basic'),
    (Extension: '.avi';     ContentType: 'video/avi'),
    (Extension: '.awf';     ContentType: 'application/vnd.adobe.workflow'),
    (Extension: '.biz';     ContentType: 'text/xml'),
    (Extension: '.bmp';     ContentType: 'application/x-bmp'),
    (Extension: '.bot';     ContentType: 'application/x-bot'),
    (Extension: '.c4t';     ContentType: 'application/x-c4t'),
    (Extension: '.c90';     ContentType: 'application/x-c90'),
    (Extension: '.cal';     ContentType: 'application/x-cals'),
    (Extension: '.cat';     ContentType: 'application/vnd.ms-pki.seccat'),
    (Extension: '.cdf';     ContentType: 'application/x-netcdf'),
    (Extension: '.cdr';     ContentType: 'application/x-cdr'),
    (Extension: '.cel';     ContentType: 'application/x-cel'),
    (Extension: '.cer';     ContentType: 'application/x-x509-ca-cert'),
    (Extension: '.cg4';     ContentType: 'application/x-g4'),
    (Extension: '.cgm';     ContentType: 'application/x-cgm'),
    (Extension: '.cit';     ContentType: 'application/x-cit'),
    (Extension: '.class';   ContentType: 'java/*'),
    (Extension: '.cml';     ContentType: 'text/xml'),
    (Extension: '.cmp';     ContentType: 'application/x-cmp'),
    (Extension: '.cmx';     ContentType: 'application/x-cmx'),
    (Extension: '.cot';     ContentType: 'application/x-cot'),
    (Extension: '.crl';     ContentType: 'application/pkix-crl'),
    (Extension: '.crt';     ContentType: 'application/x-x509-ca-cert'),
    (Extension: '.csi';     ContentType: 'application/x-csi'),
    (Extension: '.cut';     ContentType: 'application/x-cut'),
    (Extension: '.dbf';     ContentType: 'application/x-dbf'),
    (Extension: '.dbm';     ContentType: 'application/x-dbm'),
    (Extension: '.dbx';     ContentType: 'application/x-dbx'),
    (Extension: '.dcd';     ContentType: 'text/xml'),
    (Extension: '.dcx';     ContentType: 'application/x-dcx'),
    (Extension: '.der';     ContentType: 'application/x-x509-ca-cert'),
    (Extension: '.dgn';     ContentType: 'application/x-dgn'),
    (Extension: '.dib';     ContentType: 'application/x-dib'),
    (Extension: '.dll';     ContentType: 'application/x-msdownload'),
    (Extension: '.doc';     ContentType: 'application/msword'),
    (Extension: '.dot';     ContentType: 'application/msword'),
    (Extension: '.drw';     ContentType: 'application/x-drw'),
    (Extension: '.dtd';     ContentType: 'text/xml'),
    (Extension: '.dwf';     ContentType: 'Model/vnd.dwf'),
    (Extension: '.dwg';     ContentType: 'application/x-dwg'),
    (Extension: '.dxb';     ContentType: 'application/x-dxb'),
    (Extension: '.dxf';     ContentType: 'application/x-dxf'),
    (Extension: '.edn';     ContentType: 'application/vnd.adobe.edn'),
    (Extension: '.emf';     ContentType: 'application/x-emf'),
    (Extension: '.eml';     ContentType: 'message/rfc822'),
    (Extension: '.ent';     ContentType: 'text/xml'),
    (Extension: '.epi';     ContentType: 'application/x-epi'),
    (Extension: '.eps';     ContentType: 'application/postscript'),
    (Extension: '.etd';     ContentType: 'application/x-ebx'),
    (Extension: '.exe';     ContentType: 'application/x-msdownload'),
    (Extension: '.fax';     ContentType: 'image/fax'),
    (Extension: '.fdf';     ContentType: 'application/vnd.fdf'),
    (Extension: '.fif';     ContentType: 'application/fractals'),
    (Extension: '.fo';      ContentType: 'text/xml'),
    (Extension: '.frm';     ContentType: 'application/x-frm'),
    (Extension: '.g4';      ContentType: 'application/x-g4'),
    (Extension: '.gbr';     ContentType: 'application/x-gbr'),
    (Extension: '.gif';     ContentType: 'image/gif'),
    (Extension: '.gl2';     ContentType: 'application/x-gl2'),
    (Extension: '.gp4';     ContentType: 'application/x-gp4'),
    (Extension: '.hgl';     ContentType: 'application/x-hgl'),
    (Extension: '.hmr';     ContentType: 'application/x-hmr'),
    (Extension: '.hpg';     ContentType: 'application/x-hpgl'),
    (Extension: '.hpl';     ContentType: 'application/x-hpl'),
    (Extension: '.hqx';     ContentType: 'application/mac-binhex40'),
    (Extension: '.hrf';     ContentType: 'application/x-hrf'),
    (Extension: '.hta';     ContentType: 'application/hta'),
    (Extension: '.htc';     ContentType: 'text/x-component'),
    (Extension: '.htt';     ContentType: 'text/webviewhtml'),
    (Extension: '.htx';     ContentType: 'text/html'),
    (Extension: '.icb';     ContentType: 'application/x-icb'),
    (Extension: '.ico';     ContentType: 'image/x-icon'),
    (Extension: '.iff';     ContentType: 'application/x-iff'),
    (Extension: '.ig4';     ContentType: 'application/x-g4'),
    (Extension: '.igs';     ContentType: 'application/x-igs'),
    (Extension: '.iii';     ContentType: 'application/x-iphone'),
    (Extension: '.img';     ContentType: 'application/x-img'),
    (Extension: '.ins';     ContentType: 'application/x-internet-signup'),
    (Extension: '.ipa';     ContentType: 'application/vnd.iphone'),
    (Extension: '.isp';     ContentType: 'application/x-internet-signup'),
    (Extension: '.IVF';     ContentType: 'video/x-ivf'),
    (Extension: '.java';    ContentType: 'java/*'),
    (Extension: '.jfif';    ContentType: 'image/jpeg'),
    (Extension: '.jpe';     ContentType: 'image/jpeg'),
    (Extension: '.la1';     ContentType: 'audio/x-liquid-file'),
    (Extension: '.lar';     ContentType: 'application/x-laplayer-reg'),
    (Extension: '.latex';   ContentType: 'application/x-latex'),
    (Extension: '.lavs';    ContentType: 'audio/x-liquid-secure'),
    (Extension: '.lbm';     ContentType: 'application/x-lbm'),
    (Extension: '.lmsff';   ContentType: 'audio/x-la-lms'),
    (Extension: '.ls';      ContentType: 'application/x-javascript'),
    (Extension: '.ltr';     ContentType: 'application/x-ltr'),
    (Extension: '.m1v';     ContentType: 'video/x-mpeg'),
    (Extension: '.m2v';     ContentType: 'video/x-mpeg'),
    (Extension: '.m3u';     ContentType: 'audio/mpegurl'),
    (Extension: '.m4e';     ContentType: 'video/mpeg4'),
    (Extension: '.mac';     ContentType: 'application/x-mac'),
    (Extension: '.man';     ContentType: 'application/x-troff-man'),
    (Extension: '.math';    ContentType: 'text/xml'),
    (Extension: '.mdb';     ContentType: 'application/msaccess'),
    (Extension: '.mfp';     ContentType: 'application/x-shockwave-flash'),
    (Extension: '.mht';     ContentType: 'message/rfc822'),
    (Extension: '.mhtml';   ContentType: 'message/rfc822'),
    (Extension: '.mi';      ContentType: 'application/x-mi'),
    (Extension: '.mid';     ContentType: 'audio/mid'),
    (Extension: '.midi';    ContentType: 'audio/mid'),
    (Extension: '.mil';     ContentType: 'application/x-mil'),
    (Extension: '.mml';     ContentType: 'text/xml'),
    (Extension: '.mnd';     ContentType: 'audio/x-musicnet-download'),
    (Extension: '.mns';     ContentType: 'audio/x-musicnet-stream'),
    (Extension: '.mocha';   ContentType: 'application/x-javascript'),
    (Extension: '.movie';   ContentType: 'video/x-sgi-movie'),
    (Extension: '.mp1';     ContentType: 'audio/mp1'),
    (Extension: '.mp2';     ContentType: 'audio/mp2'),
    (Extension: '.mp2v';    ContentType: 'video/mpeg'),

    (Extension: '.mpa';     ContentType: 'video/x-mpg'),
    (Extension: '.mpd';     ContentType: 'application/vnd.ms-project'),
    (Extension: '.mpe';     ContentType: 'video/x-mpeg'),

    (Extension: '.mpga';    ContentType: 'audio/rn-mpeg'),
    (Extension: '.mpp';     ContentType: 'application/vnd.ms-project'),
    (Extension: '.mps';     ContentType: 'video/x-mpeg'),
    (Extension: '.mpt';     ContentType: 'application/vnd.ms-project'),
    (Extension: '.mpv';     ContentType: 'video/mpg'),
    (Extension: '.mpv2';    ContentType: 'video/mpeg'),
    (Extension: '.mpw';     ContentType: 'application/vnd.ms-project'),
    (Extension: '.mpx';     ContentType: 'application/vnd.ms-project'),
    (Extension: '.mtx';     ContentType: 'text/xml'),
    (Extension: '.mxp';     ContentType: 'application/x-mmxp'),
    (Extension: '.net';     ContentType: 'image/pnetvue'),
    (Extension: '.nrf';     ContentType: 'application/x-nrf'),
    (Extension: '.nws';     ContentType: 'message/rfc822'),
    (Extension: '.odc';     ContentType: 'text/x-ms-odc'),
    (Extension: '.out';     ContentType: 'application/x-out'),
    (Extension: '.p10';     ContentType: 'application/pkcs10'),
    (Extension: '.p12';     ContentType: 'application/x-pkcs12'),
    (Extension: '.p7b';     ContentType: 'application/x-pkcs7-certificates'),
    (Extension: '.p7c';     ContentType: 'application/pkcs7-mime'),
    (Extension: '.p7m';     ContentType: 'application/pkcs7-mime'),
    (Extension: '.p7r';     ContentType: 'application/x-pkcs7-certreqresp'),
    (Extension: '.p7s';     ContentType: 'application/pkcs7-signature'),
    (Extension: '.pc5';     ContentType: 'application/x-pc5'),
    (Extension: '.pci';     ContentType: 'application/x-pci'),
    (Extension: '.pcl';     ContentType: 'application/x-pcl'),
    (Extension: '.pcx';     ContentType: 'application/x-pcx'),
    (Extension: '.pdf';     ContentType: 'application/pdf'),
    (Extension: '.pdx';     ContentType: 'application/vnd.adobe.pdx'),
    (Extension: '.pfx';     ContentType: 'application/x-pkcs12'),
    (Extension: '.pgl';     ContentType: 'application/x-pgl'),
    (Extension: '.pic';     ContentType: 'application/x-pic'),
    (Extension: '.pko';     ContentType: 'application/vnd.ms-pki.pko'),
    (Extension: '.pl';      ContentType: 'application/x-perl'),
    (Extension: '.plg';     ContentType: 'text/html'),
    (Extension: '.pls';     ContentType: 'audio/scpls'),
    (Extension: '.plt';     ContentType: 'application/x-plt'),
    (Extension: '.png';     ContentType: 'image/png'),
    (Extension: '.pot';     ContentType: 'application/vnd.ms-powerpoint'),
    (Extension: '.ppa';     ContentType: 'application/vnd.ms-powerpoint'),
    (Extension: '.ppm';     ContentType: 'application/x-ppm'),
    (Extension: '.pps';     ContentType: 'application/vnd.ms-powerpoint'),
    (Extension: '.ppt';     ContentType: 'application/vnd.ms-powerpoint'),
    (Extension: '.pr';      ContentType: 'application/x-pr'),
    (Extension: '.prf';     ContentType: 'application/pics-rules'),
    (Extension: '.prn';     ContentType: 'application/x-prn'),
    (Extension: '.prt';     ContentType: 'application/x-prt'),
    (Extension: '.ps';      ContentType: 'application/postscript'),
    (Extension: '.ptn';     ContentType: 'application/x-ptn'),
    (Extension: '.pwz';     ContentType: 'application/vnd.ms-powerpoint'),
    (Extension: '.r3t';     ContentType: 'text/vnd.rn-realtext3d'),
    (Extension: '.ra';      ContentType: 'audio/vnd.rn-realaudio'),
    (Extension: '.ram';     ContentType: 'audio/x-pn-realaudio'),
    (Extension: '.ras';     ContentType: 'application/x-ras'),
    (Extension: '.rat';     ContentType: 'application/rat-file'),
    (Extension: '.rdf';     ContentType: 'text/xml'),
    (Extension: '.rec';     ContentType: 'application/vnd.rn-recording'),
    (Extension: '.red';     ContentType: 'application/x-red'),
    (Extension: '.rgb';     ContentType: 'application/x-rgb'),
    (Extension: '.rjs';     ContentType: 'application/vnd.rn-realsystem-rjs'),
    (Extension: '.rjt';     ContentType: 'application/vnd.rn-realsystem-rjt'),
    (Extension: '.rlc';     ContentType: 'application/x-rlc'),
    (Extension: '.rle';     ContentType: 'application/x-rle'),
    (Extension: '.rm';      ContentType: 'application/vnd.rn-realmedia'),
    (Extension: '.rmf';     ContentType: 'application/vnd.adobe.rmf'),
    (Extension: '.rmi';     ContentType: 'audio/mid'),
    (Extension: '.rmj';     ContentType: 'application/vnd.rn-realsystem-rmj'),
    (Extension: '.rmm';     ContentType: 'audio/x-pn-realaudio'),
    (Extension: '.rmp';     ContentType: 'application/vnd.rn-rn_music_package'),
    (Extension: '.rms';     ContentType: 'application/vnd.rn-realmedia-secure'),
    (Extension: '.rmvb';    ContentType: 'application/vnd.rn-realmedia-vbr'),
    (Extension: '.rmx';     ContentType: 'application/vnd.rn-realsystem-rmx'),
    (Extension: '.rnx';     ContentType: 'application/vnd.rn-realplayer'),
    (Extension: '.rp';      ContentType: 'image/vnd.rn-realpix'),
    (Extension: '.rpm';     ContentType: 'audio/x-pn-realaudio-plugin'),
    (Extension: '.rsml';    ContentType: 'application/vnd.rn-rsml'),
    (Extension: '.rt';      ContentType: 'text/vnd.rn-realtext'),
    (Extension: '.rtf';     ContentType: 'application/msword'),
    (Extension: '.rv';      ContentType: 'video/vnd.rn-realvideo'),
    (Extension: '.sam';     ContentType: 'application/x-sam'),
    (Extension: '.sat';     ContentType: 'application/x-sat'),
    (Extension: '.sdp';     ContentType: 'application/sdp'),
    (Extension: '.sdw';     ContentType: 'application/x-sdw'),
    (Extension: '.sis';     ContentType: 'application/vnd.symbian.install'),
    (Extension: '.sisx';    ContentType: 'application/vnd.symbian.install'),
    (Extension: '.sit';     ContentType: 'application/x-stuffit'),
    (Extension: '.slb';     ContentType: 'application/x-slb'),
    (Extension: '.sld';     ContentType: 'application/x-sld'),
    (Extension: '.slk';     ContentType: 'drawing/x-slk'),
    (Extension: '.smi';     ContentType: 'application/smil'),
    (Extension: '.smil';    ContentType: 'application/smil'),
    (Extension: '.smk';     ContentType: 'application/x-smk'),
    (Extension: '.snd';     ContentType: 'audio/basic'),
    (Extension: '.sol';     ContentType: 'text/plain'),
    (Extension: '.sor';     ContentType: 'text/plain'),
    (Extension: '.spc';     ContentType: 'application/x-pkcs7-certificates'),
    (Extension: '.spl';     ContentType: 'application/futuresplash'),
    (Extension: '.spp';     ContentType: 'text/xml'),
    (Extension: '.ssm';     ContentType: 'application/streamingmedia'),
    (Extension: '.sst';     ContentType: 'application/vnd.ms-pki.certstore'),
    (Extension: '.stl';     ContentType: 'application/vnd.ms-pki.stl'),
    (Extension: '.stm';     ContentType: 'text/html'),
    (Extension: '.sty';     ContentType: 'application/x-sty'),
    (Extension: '.svg';     ContentType: 'text/xml'),
    (Extension: '.swf';     ContentType: 'application/x-shockwave-flash'),
    (Extension: '.tdf';     ContentType: 'application/x-tdf'),
    (Extension: '.tg4';     ContentType: 'application/x-tg4'),
    (Extension: '.tga';     ContentType: 'application/x-tga'),
    (Extension: '.tif';     ContentType: 'image/tiff'),
    (Extension: '.tiff';    ContentType: 'image/tiff'),
    (Extension: '.tld';     ContentType: 'text/xml'),
    (Extension: '.top';     ContentType: 'drawing/x-top'),
    (Extension: '.torrent'; ContentType: 'application/x-bittorrent'),
    (Extension: '.tsd';     ContentType: 'text/xml'),
    (Extension: '.txt';     ContentType: 'text/plain'),
    (Extension: '.uin';     ContentType: 'application/x-icq'),
    (Extension: '.uls';     ContentType: 'text/iuls'),
    (Extension: '.vcf';     ContentType: 'text/x-vcard'),
    (Extension: '.vda';     ContentType: 'application/x-vda'),
    (Extension: '.vdx';     ContentType: 'application/vnd.visio'),
    (Extension: '.vml';     ContentType: 'text/xml'),
    (Extension: '.vpg';     ContentType: 'application/x-vpeg005'),
    (Extension: '.vsd';     ContentType: 'application/vnd.visio'),
    (Extension: '.vss';     ContentType: 'application/vnd.visio'),
    (Extension: '.vst';     ContentType: 'application/vnd.visio'),
    (Extension: '.vsw';     ContentType: 'application/vnd.visio'),
    (Extension: '.vsx';     ContentType: 'application/vnd.visio'),
    (Extension: '.vtx';     ContentType: 'application/vnd.visio'),
    (Extension: '.vxml';    ContentType: 'text/xml'),
    (Extension: '.wav';     ContentType: 'audio/wav'),
    (Extension: '.wax';     ContentType: 'audio/x-ms-wax'),
    (Extension: '.wb1';     ContentType: 'application/x-wb1'),
    (Extension: '.wb2';     ContentType: 'application/x-wb2'),
    (Extension: '.wb3';     ContentType: 'application/x-wb3'),
    (Extension: '.wbmp';    ContentType: 'image/vnd.wap.wbmp'),
    (Extension: '.wiz';     ContentType: 'application/msword'),
    (Extension: '.wk3';     ContentType: 'application/x-wk3'),
    (Extension: '.wk4';     ContentType: 'application/x-wk4'),
    (Extension: '.wkq';     ContentType: 'application/x-wkq'),
    (Extension: '.wks';     ContentType: 'application/x-wks'),
    (Extension: '.wm';      ContentType: 'video/x-ms-wm'),
    (Extension: '.wma';     ContentType: 'audio/x-ms-wma'),
    (Extension: '.wmd';     ContentType: 'application/x-ms-wmd'),
    (Extension: '.wmf';     ContentType: 'application/x-wmf'),
    (Extension: '.wml';     ContentType: 'text/vnd.wap.wml'),
    (Extension: '.wmv';     ContentType: 'video/x-ms-wmv'),
    (Extension: '.wmx';     ContentType: 'video/x-ms-wmx'),
    (Extension: '.wmz';     ContentType: 'application/x-ms-wmz'),
    (Extension: '.wp6';     ContentType: 'application/x-wp6'),
    (Extension: '.wpd';     ContentType: 'application/x-wpd'),
    (Extension: '.wpg';     ContentType: 'application/x-wpg'),
    (Extension: '.wpl';     ContentType: 'application/vnd.ms-wpl'),
    (Extension: '.wq1';     ContentType: 'application/x-wq1'),
    (Extension: '.wr1';     ContentType: 'application/x-wr1'),
    (Extension: '.wri';     ContentType: 'application/x-wri'),
    (Extension: '.wrk';     ContentType: 'application/x-wrk'),
    (Extension: '.ws';      ContentType: 'application/x-ws'),
    (Extension: '.ws2';     ContentType: 'application/x-ws'),
    (Extension: '.wsc';     ContentType: 'text/scriptlet'),
    (Extension: '.wsdl';    ContentType: 'text/xml'),
    (Extension: '.wvx';     ContentType: 'video/x-ms-wvx'),
    (Extension: '.x_b';     ContentType: 'application/x-x_b'),
    (Extension: '.x_t';     ContentType: 'application/x-x_t'),
    (Extension: '.xap';     ContentType: 'application/x-silverlight-app'),
    (Extension: '.xdp';     ContentType: 'application/vnd.adobe.xdp'),
    (Extension: '.xdr';     ContentType: 'text/xml'),
    (Extension: '.xfd';     ContentType: 'application/vnd.adobe.xfd'),
    (Extension: '.xfdf';    ContentType: 'application/vnd.adobe.xfdf'),
    (Extension: '.xhtml';   ContentType: 'text/html'),
    (Extension: '.xls';     ContentType: 'application/vnd.ms-excel'),
    (Extension: '.xlw';     ContentType: 'application/x-xlw'),
    (Extension: '.xml';     ContentType: 'text/xml'),
    (Extension: '.xpl';     ContentType: 'audio/scpls'),
    (Extension: '.xq';      ContentType: 'text/xml'),
    (Extension: '.xql';     ContentType: 'text/xml'),
    (Extension: '.xquery';  ContentType: 'text/xml'),
    (Extension: '.xsd';     ContentType: 'text/xml'),
    (Extension: '.xsl';     ContentType: 'text/xml'),
    (Extension: '.xslt';    ContentType: 'text/xml'),
    (Extension: '.xwd';     ContentType: 'application/x-xwd')
  );

implementation

end.
