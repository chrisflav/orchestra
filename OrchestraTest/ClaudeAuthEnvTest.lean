import OrchestraTest.TestM

/-!
# The environment a claude auth source becomes

One source, one set of environment variables. What is worth pinning down is the pair that is not
a credential: a `setup-token` never fetches the account profile Claude Code learns the
subscription from, so the client holds no plan, and an entitlement check that cannot confirm the
plan covers a model refuses it — a Max account is told Fable needs usage credits
(anthropics/claude-code#79597). In this mode the client reads the plan from the environment
instead, so orchestra states it beside the token.
-/

namespace Orchestra

open Orchestra.AgentDef

private def claudeEnv (src : AuthSource) : Array (String × String) :=
  AgentDef.claude.envVarsOfAuthSource src

@[test]
def oauthSource_carriesThePlanAlongsideTheToken : Test := do
  let env := claudeEnv { label := "work", kind := .oauthToken "sk-ant-oat-x" }
  TestM.assert (env.contains ("CLAUDE_CODE_OAUTH_TOKEN", "sk-ant-oat-x"))
    (msg := "the token is passed as before")
  TestM.assert (env.contains ("CLAUDE_CODE_SUBSCRIPTION_TYPE", "max"))
    (msg := "and the plan the client cannot look up for itself")
  TestM.assert (env.contains ("CLAUDE_CODE_RATE_LIMIT_TIER", "default_claude_max_20x"))
    (msg := "and the tier that goes with it")

@[test]
def apiKeySource_getsNoPlan : Test := do
  -- An API key bills an organisation per token: there is no subscription for a plan to describe,
  -- and the client reads these two only on the OAuth-token path anyway.
  let env := claudeEnv { label := "org", kind := .apiKey "sk-ant-api-x" }
  TestM.assert (env.contains ("ANTHROPIC_API_KEY", "sk-ant-api-x")) (msg := "the key is set")
  TestM.assert (!env.any (·.1 == "CLAUDE_CODE_SUBSCRIPTION_TYPE"))
    (msg := "no plan rides with an API key")
  TestM.assert (!env.any (·.1 == "CLAUDE_CODE_RATE_LIMIT_TIER"))
    (msg := "and no tier either")

@[test]
def apiKeySource_stillCarriesItsBaseUrl : Test := do
  -- The plan variables are additions to the OAuth arm alone; the API-key arm is untouched.
  let env := claudeEnv { label := "local", kind := .apiKey "k" (some "http://127.0.0.1:8080") }
  TestM.assert (env.contains ("ANTHROPIC_BASE_URL", "http://127.0.0.1:8080"))
    (msg := "base_url still reaches the agent")

end Orchestra
