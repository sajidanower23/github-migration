Left (HTTPError (HttpExceptionRequest Request 
  { host                 = "api.github.com"
  , port                 = 443
  , secure               = True
  , requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","github.hs/0.7.4"),("Accept","application/vnd.github.preview")]
  , path                 = "/repos/axman6/test-to/issues/41/comments"
  , queryString          = ""
  , method               = "POST"
  , proxy                = Nothing
  , rawBody              = False
  , redirectCount        = 10
  , responseTimeout      = ResponseTimeoutDefault
  , requestVersion       = HTTP/1.1
}
 (StatusCodeException 
  (Response {responseStatus = Status {statusCode = 403,
  statusMessage = "Forbidden"},
  responseVersion = HTTP/1.1,
  responseHeaders = 
    [("Date","Wed, 05 Dec 2018 08:03:54 GMT")
    ,("Content-Type","application/json; charset=utf-8")
    ,("Transfer-Encoding","chunked")
    ,("Server","GitHub.com")
    ,("Status","403 Forbidden")
    ,("X-RateLimit-Limit","5000")
    ,("X-RateLimit-Remaining","4698")
    ,("X-RateLimit-Reset","1543997702")
    ,("X-OAuth-Scopes","repo")
    ,("X-Accepted-OAuth-Scopes","")
    ,("X-GitHub-Media-Type","github.v3; param=preview")
    ,("Access-Control-Expose-Headers","ETag,Link,Location,Retry-After,X-GitHub-OTP,X-RateLimit-Limit,X-RateLimit-Remaining,X-RateLimit-Reset,X-OAuth-Scopes,X-Accepted-OAuth-Scopes,X-Poll-Interval,X-GitHub-Media-Type")
    ,("Access-Control-Allow-Origin","*")
    ,("Strict-Transport-Security","max-age=31536000; includeSubdomains; preload")
    ,("X-Frame-Options","deny")
    ,("X-Content-Type-Options","nosniff")
    ,("X-XSS-Protection","1; mode=block")
    ,("Referrer-Policy","origin-when-cross-origin,strict-origin-when-cross-origin")
    ,("Content-Security-Policy","default-src 'none'")
    ,("Content-Encoding","gzip")
    ,("X-GitHub-Request-Id","DFB2:9C76:152A2C5:1A3C0EC:5C078669")
    ],
  responseBody = (),
  responseCookieJar = CJ {expose = []},
  responseClose' = ResponseClose}) 
  "{\"message\":\"You have triggered an abuse detection mechanism and have been temporarily blocked from content creation. Please retry your request again later.\",\"documentation_url\":\"https://developer.github.com/v3/#abuse-rate-limits\"}"))
  )