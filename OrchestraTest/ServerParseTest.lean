import OrchestraTest.TestM
import Orchestra.Server

open Lean (Json)
open Orchestra
open Orchestra.Server

-- parseToolCall: simple tools

@[test]
def parseToolCall_health : Test := do
  match parseToolCall "health" (Json.mkObj []) with
  | .health => TestM.assert true
  | _ => TestM.fail "expected .health"

@[test]
def parseToolCall_refreshToken : Test := do
  match parseToolCall "refresh_token" (Json.mkObj []) with
  | .refreshToken => TestM.assert true
  | _ => TestM.fail "expected .refreshToken"

@[test]
def parseToolCall_getTaskInput : Test := do
  match parseToolCall "get_task_input" (Json.mkObj []) with
  | .getTaskInput => TestM.assert true
  | _ => TestM.fail "expected .getTaskInput"

@[test]
def parseToolCall_unknown : Test := do
  match parseToolCall "nonexistent_tool" (Json.mkObj []) with
  | .unknown name => TestM.assertEqual name "nonexistent_tool" (msg := "unknown tool name")
  | _ => TestM.fail "expected .unknown"

-- parseToolCall: submit_task_output

@[test]
def parseToolCall_submitTaskOutput : Test := do
  let args := Json.mkObj [("value", .str "result")]
  match parseToolCall "submit_task_output" args with
  | .submitTaskOutput v => TestM.assertEqual v.compress "\"result\"" (msg := "value")
  | _ => TestM.fail "expected .submitTaskOutput"

@[test]
def parseToolCall_submitTaskOutput_missingValue : Test := do
  match parseToolCall "submit_task_output" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing value"

-- parseToolCall: create_pr

@[test]
def parseToolCall_createPr : Test := do
  let args := Json.mkObj [
    ("title", .str "My PR"),
    ("body",  .str "PR body"),
    ("head",  .str "feature-branch"),
    ("base",  .str "develop")
  ]
  match parseToolCall "create_pr" args with
  | .createPr title body head base .upstream =>
    TestM.assertEqual title "My PR"          (msg := "title")
    TestM.assertEqual body  "PR body"        (msg := "body")
    TestM.assertEqual head  "feature-branch" (msg := "head")
    TestM.assertEqual base  "develop"        (msg := "base")
  | _ => TestM.fail "expected .createPr"

@[test]
def parseToolCall_createPr_defaults : Test := do
  let args := Json.mkObj [("head", .str "my-branch")]
  match parseToolCall "create_pr" args with
  | .createPr title body head base .upstream =>
    TestM.assertEqual title "Agent PR"   (msg := "default title")
    TestM.assertEqual body  ""           (msg := "default body")
    TestM.assertEqual head  "my-branch"  (msg := "head")
    TestM.assertEqual base  "main"       (msg := "default base")
  | _ => TestM.fail "expected .createPr with defaults"

@[test]
def parseToolCall_createPr_missingHead : Test := do
  let args := Json.mkObj [("title", .str "My PR")]
  match parseToolCall "create_pr" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing head"

@[test]
def parseToolCall_createPr_emptyHead : Test := do
  let args := Json.mkObj [("head", .str "")]
  match parseToolCall "create_pr" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for empty head"

-- parseToolCall: get_pr_comments

@[test]
def parseToolCall_getPrComments : Test := do
  let args := Json.mkObj [
    ("pr_number",       .num ⟨42, 0⟩),
    ("unresolved_only", .bool true),
    ("exclude_outdated", .bool true)
  ]
  match parseToolCall "get_pr_comments" args with
  | .getPrComments prNum unresolvedOnly excludeOutdated =>
    TestM.assertEqual prNum          42   (msg := "pr_number")
    TestM.assertEqual unresolvedOnly true (msg := "unresolved_only")
    TestM.assertEqual excludeOutdated true (msg := "exclude_outdated")
  | _ => TestM.fail "expected .getPrComments"

@[test]
def parseToolCall_getPrComments_defaults : Test := do
  let args := Json.mkObj [("pr_number", .num ⟨7, 0⟩)]
  match parseToolCall "get_pr_comments" args with
  | .getPrComments prNum unresolvedOnly excludeOutdated =>
    TestM.assertEqual prNum           7     (msg := "pr_number")
    TestM.assertEqual unresolvedOnly  false (msg := "unresolved_only default")
    TestM.assertEqual excludeOutdated false (msg := "exclude_outdated default")
  | _ => TestM.fail "expected .getPrComments"

@[test]
def parseToolCall_getPrComments_missingPrNumber : Test := do
  match parseToolCall "get_pr_comments" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing pr_number"

@[test]
def parseToolCall_getPrComments_zeroPrNumber : Test := do
  let args := Json.mkObj [("pr_number", .num ⟨0, 0⟩)]
  match parseToolCall "get_pr_comments" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for pr_number = 0"

@[test]
def parseToolCall_getPrComments_nonInteger : Test := do
  let args := Json.mkObj [("pr_number", .str "abc")]
  match parseToolCall "get_pr_comments" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for non-integer pr_number"

-- parseToolCall: comment — issue mode

@[test]
def parseToolCall_comment_issue : Test := do
  let args := Json.mkObj [("body", .str "Hello!")]
  match parseToolCall "comment" args with
  | .comment (.issue body) .upstream => TestM.assertEqual body "Hello!" (msg := "body")
  | _ => TestM.fail "expected .comment (.issue ...) .upstream"

@[test]
def parseToolCall_comment_missingBody : Test := do
  match parseToolCall "comment" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing body"

@[test]
def parseToolCall_comment_emptyBody : Test := do
  let args := Json.mkObj [("body", .str "")]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for empty body"

-- parseToolCall: comment — review mode

@[test]
def parseToolCall_comment_review : Test := do
  let args := Json.mkObj [("body", .str "LGTM"), ("review", .bool true)]
  match parseToolCall "comment" args with
  | .comment (.review body inlineComments) .upstream =>
    TestM.assertEqual body "LGTM"           (msg := "body")
    TestM.assertEqual inlineComments.size 0 (msg := "no inline comments")
  | _ => TestM.fail "expected .comment (.review ...) .upstream"

@[test]
def parseToolCall_comment_reviewWithInline : Test := do
  let item := Json.mkObj [
    ("path", .str "src/Foo.lean"),
    ("line", .num ⟨10, 0⟩),
    ("body", .str "Fix this"),
    ("side", .str "LEFT")
  ]
  let args := Json.mkObj [
    ("body",            .str "Review body"),
    ("review",          .bool true),
    ("inline_comments", .arr #[item])
  ]
  match parseToolCall "comment" args with
  | .comment (.review body inlineComments) .upstream =>
    TestM.assertEqual body "Review body"    (msg := "body")
    TestM.assertEqual inlineComments.size 1 (msg := "inline comment count")
    match inlineComments[0]? with
    | none => TestM.fail "no inline comment at index 0"
    | some ic =>
      TestM.assertEqual ic.path "src/Foo.lean" (msg := "inline path")
      TestM.assertEqual ic.line 10             (msg := "inline line")
      TestM.assertEqual ic.body "Fix this"     (msg := "inline body")
      TestM.assertEqual ic.side "LEFT"         (msg := "inline side")
  | _ => TestM.fail "expected .comment (.review ...) .upstream with inline"

@[test]
def parseToolCall_comment_reviewWithInline_defaultSide : Test := do
  let item := Json.mkObj [
    ("path", .str "Foo.lean"),
    ("line", .num ⟨1, 0⟩),
    ("body", .str "Note")
  ]
  let args := Json.mkObj [
    ("body",            .str "r"),
    ("review",          .bool true),
    ("inline_comments", .arr #[item])
  ]
  match parseToolCall "comment" args with
  | .comment (.review _ inlineComments) .upstream =>
    match inlineComments[0]? with
    | none => TestM.fail "no inline comment"
    | some ic => TestM.assertEqual ic.side "RIGHT" (msg := "default side")
  | _ => TestM.fail "expected .comment (.review ...) .upstream"

@[test]
def parseToolCall_comment_reviewMutualExclusion_replyId : Test := do
  let args := Json.mkObj [
    ("body",                .str "body"),
    ("review",              .bool true),
    ("reply_to_comment_id", .num ⟨5, 0⟩)
  ]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError: review + reply_to_comment_id"

@[test]
def parseToolCall_comment_reviewMutualExclusion_path : Test := do
  let args := Json.mkObj [
    ("body",   .str "body"),
    ("review", .bool true),
    ("path",   .str "Foo.lean"),
    ("line",   .num ⟨1, 0⟩)
  ]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError: review + path/line"

-- parseToolCall: comment — replyInline mode

@[test]
def parseToolCall_comment_replyInline : Test := do
  let args := Json.mkObj [
    ("body",                .str "Got it"),
    ("reply_to_comment_id", .num ⟨99, 0⟩)
  ]
  match parseToolCall "comment" args with
  | .comment (.replyInline body cid) .upstream =>
    TestM.assertEqual body "Got it" (msg := "body")
    TestM.assertEqual cid  99       (msg := "comment id")
  | _ => TestM.fail "expected .comment (.replyInline ...) .upstream"

@[test]
def parseToolCall_comment_replyInline_withPath : Test := do
  let args := Json.mkObj [
    ("body",                .str "x"),
    ("reply_to_comment_id", .num ⟨1, 0⟩),
    ("path",                .str "Foo.lean")
  ]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError: reply_to_comment_id + path"

-- parseToolCall: comment — newInline mode

@[test]
def parseToolCall_comment_newInline : Test := do
  let args := Json.mkObj [
    ("body", .str "Fix here"),
    ("path", .str "Foo.lean"),
    ("line", .num ⟨42, 0⟩),
    ("side", .str "LEFT")
  ]
  match parseToolCall "comment" args with
  | .comment (.newInline ic) .upstream =>
    TestM.assertEqual ic.body "Fix here" (msg := "body")
    TestM.assertEqual ic.path "Foo.lean" (msg := "path")
    TestM.assertEqual ic.line 42         (msg := "line")
    TestM.assertEqual ic.side "LEFT"     (msg := "side")
  | _ => TestM.fail "expected .comment (.newInline ...) .upstream"

@[test]
def parseToolCall_comment_newInline_defaultSide : Test := do
  let args := Json.mkObj [
    ("body", .str "Note"),
    ("path", .str "Bar.lean"),
    ("line", .num ⟨1, 0⟩)
  ]
  match parseToolCall "comment" args with
  | .comment (.newInline ic) .upstream =>
    TestM.assertEqual ic.side "RIGHT" (msg := "default side RIGHT")
  | _ => TestM.fail "expected .comment (.newInline ...) .upstream"

@[test]
def parseToolCall_comment_pathWithoutLine : Test := do
  let args := Json.mkObj [("body", .str "x"), ("path", .str "Foo.lean")]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError: path without line"

@[test]
def parseToolCall_comment_lineWithoutPath : Test := do
  let args := Json.mkObj [("body", .str "x"), ("line", .num ⟨5, 0⟩)]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError: line without path"

-- parseToolCall: comment — target parameter

@[test]
def parseToolCall_comment_targetUpstreamExplicit : Test := do
  let args := Json.mkObj [("body", .str "hi"), ("target", .str "upstream")]
  match parseToolCall "comment" args with
  | .comment (.issue _) .upstream => TestM.assert true
  | _ => TestM.fail "expected .comment ... .upstream"

@[test]
def parseToolCall_comment_targetFork : Test := do
  let args := Json.mkObj [("body", .str "hi"), ("target", .str "fork")]
  match parseToolCall "comment" args with
  | .comment (.issue _) .fork => TestM.assert true
  | _ => TestM.fail "expected .comment ... .fork"

@[test]
def parseToolCall_comment_targetInvalid : Test := do
  let args := Json.mkObj [("body", .str "hi"), ("target", .str "bad")]
  match parseToolCall "comment" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for invalid target"

@[test]
def parseToolCall_comment_targetFork_review : Test := do
  let args := Json.mkObj [("body", .str "LGTM"), ("review", .bool true), ("target", .str "fork")]
  match parseToolCall "comment" args with
  | .comment (.review _ _) .fork => TestM.assert true
  | _ => TestM.fail "expected .comment (.review ...) .fork"

-- parseRequest

@[test]
def parseRequest_initialize : Test := do
  let msg := Json.mkObj [
    ("jsonrpc", .str "2.0"),
    ("id",      .num ⟨1, 0⟩),
    ("method",  .str "initialize")
  ]
  match parseRequest msg with
  | some (.initialize id) => TestM.assertEqual id.compress "1" (msg := "id")
  | _ => TestM.fail "expected .initialize"

@[test]
def parseRequest_initialized : Test := do
  let msg := Json.mkObj [("method", .str "notifications/initialized")]
  match parseRequest msg with
  | some .initialized => TestM.assert true
  | _ => TestM.fail "expected .initialized"

@[test]
def parseRequest_toolsList : Test := do
  let msg := Json.mkObj [("id", .num ⟨2, 0⟩), ("method", .str "tools/list")]
  match parseRequest msg with
  | some (.toolsList id) => TestM.assertEqual id.compress "2" (msg := "id")
  | _ => TestM.fail "expected .toolsList"

@[test]
def parseRequest_toolsCall_health : Test := do
  let msg := Json.mkObj [
    ("id",     .num ⟨3, 0⟩),
    ("method", .str "tools/call"),
    ("params", Json.mkObj [("name", .str "health"), ("arguments", Json.mkObj [])])
  ]
  match parseRequest msg with
  | some (.toolsCall id .health) => TestM.assertEqual id.compress "3" (msg := "id")
  | _ => TestM.fail "expected .toolsCall with .health"

@[test]
def parseRequest_toolsCall_withArgs : Test := do
  let msg := Json.mkObj [
    ("id",     .num ⟨4, 0⟩),
    ("method", .str "tools/call"),
    ("params", Json.mkObj [
      ("name",      .str "get_pr_comments"),
      ("arguments", Json.mkObj [("pr_number", .num ⟨7, 0⟩)])
    ])
  ]
  match parseRequest msg with
  | some (.toolsCall _ (.getPrComments prNum _ _)) =>
    TestM.assertEqual prNum 7 (msg := "pr_number parsed through request")
  | _ => TestM.fail "expected .toolsCall with .getPrComments"

@[test]
def parseRequest_unknown : Test := do
  let msg := Json.mkObj [("id", .num ⟨5, 0⟩), ("method", .str "ping")]
  match parseRequest msg with
  | some (.unknown id method) =>
    TestM.assertEqual id.compress "5" (msg := "id")
    TestM.assertEqual method "ping"   (msg := "method")
  | _ => TestM.fail "expected .unknown"

@[test]
def parseRequest_noMethod : Test := do
  let msg := Json.mkObj [("id", .num ⟨6, 0⟩), ("result", .str "ok")]
  match parseRequest msg with
  | none => TestM.assert true
  | _ => TestM.fail "expected none for message without method field"

@[test]
def parseRequest_missingId_defaultsToNull : Test := do
  let msg := Json.mkObj [("method", .str "tools/list")]
  match parseRequest msg with
  | some (.toolsList id) => TestM.assertEqual id.compress "null" (msg := "missing id defaults to null")
  | _ => TestM.fail "expected .toolsList"

@[test]
def parseRequest_missingParams_defaultsToEmpty : Test := do
  let msg := Json.mkObj [("id", .num ⟨7, 0⟩), ("method", .str "tools/call")]
  match parseRequest msg with
  | some (.toolsCall _ (.unknown name)) =>
    TestM.assertEqual name "" (msg := "empty tool name when params absent")
  | _ => TestM.fail "expected .toolsCall with .unknown (empty name)"
