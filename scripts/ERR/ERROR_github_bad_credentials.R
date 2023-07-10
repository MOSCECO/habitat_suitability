# Réponse à l'erreur ci-dessous
> install_github("StatsWithR/statsr")
Using GitHub PAT from envvar GITHUB_PAT
Downloading GitHub repo StatsWithR/statsr@master
from URL https://api.github.com/repos/StatsWithR/statsr/zipball/master
Installation failed: Bad credentials (401)

> install_github
function (repo, username = NULL, ref = "master", subdir = NULL,
          auth_token = github_pat(quiet), host = "https://api.github.com",
          quiet = FALSE, ...)
  <Snip - snip>
  }
<environment: namespace:devtools>
  
  > github_pat(1)
[1] "bf<snip><snip>88"

> github_pat

function (quiet = FALSE)
{
  pat <- Sys.getenv("GITHUB_PAT")
  <snip - snip>
    return(NULL)
}
<environment: namespace:devtools>
  
  > Sys.getenv("GITHUB_PAT")
[1] "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" #clé de MOsceco

> Sys.unsetenv("GITHUB_PAT")
> Sys.getenv("GITHUB_PAT")

[1] ""

> install_github("StatsWithR/statsr")
Downloading GitHub repo StatsWithR/statsr@master
from URL https://api.github.com/repos/StatsWithR/statsr/zipball/master
Installing statsr                     <<< Bob's your uncle