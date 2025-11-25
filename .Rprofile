is_github_actions <- function() {
  # GitHub Actions sets GITHUB_ACTIONS == "true"
  gha <- tolower(Sys.getenv("GITHUB_ACTIONS", unset = ""))
  if (nzchar(gha) && gha %in% c("true", "1")) return(TRUE)

  # Additional GH Actions hints
  if (nzchar(Sys.getenv("GITHUB_WORKFLOW", unset = "")) ||
      nzchar(Sys.getenv("GITHUB_RUN_ID", unset = "")) ||
      nzchar(Sys.getenv("GITHUB_ACTION", unset = ""))) {
    return(TRUE)
  }

  FALSE
}

is_devcontainer <- function() {
  # 1. Primary - remote containers
  remote_containers <- tolower(Sys.getenv("REMOTE_CONTAINERS", unset = ""))
  if (nzchar(remote_containers) && remote_containers %in% c("true", "1")) {
    return(TRUE)
  }
  
  # 2. Codespaces (GitHub)
  if (nzchar(Sys.getenv("CODESPACES", unset = "")) || nzchar(Sys.getenv("CODESPACE_NAME", unset = ""))) {
    return(TRUE)
  }
  
  # 3. VS Code environment hints (not exclusive)
  vscode_vars <- c("VSCODE_IPC_HOOK_CLI", "VSCODE_GIT_ASKPASS_NODE", "VSCODE_GIT_IPC_HANDLE")
  if (any(nzchar(vapply(vscode_vars, Sys.getenv, "", USE.NAMES = FALSE)))) {
    # if VS Code vars *plus* something else (like REMOTE_CONTAINERS or container), treat as devcontainer
    if (nzchar(remote_containers) || file.exists("/.dockerenv")) {
      return(TRUE)
    }
  }
  
  # 4. RStudio or Bioconductor images hints:
  if (nzchar(Sys.getenv("RSTUDIO_VERSION", unset = "")) ||
      nzchar(Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION", unset = "")) ||
      nzchar(Sys.getenv("BIOCONDUCTOR_NAME", unset = ""))) {
    # Image-based inference; be cautious
    return(TRUE)
  }
  FALSE
}
if (is_devcontainer() && !is_github_actions()) {
  source("renv/activate.R")
}

# Ensure that Rplots.pdf is treated as unchanged by git in the repo root.
# We only run this locally (skip on GitHub Actions) and only if git is available
try({
  if (zchar(Sys.which("git"))) {
    git_top <- suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = TRUE))
    if (length(git_top) == 1 && !grepl("^fatal:", git_top)) {
      # run update-index silently; ignore errors if file is untracked
      try(suppressWarnings(system2("git", c("-C", git_top, "update-index", "--assume-unchanged", "Rplots.pdf"), stdout = FALSE, stderr = FALSE)), silent = TRUE)
    }
  }
}, silent = TRUE)
