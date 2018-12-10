# github-migration

Transfers issues, comments, labels and milestones from one Github repository (Github Enterprise or github.com)
to another.

## Installation

The easiest way to run this is to download the script, install `stack`,
and then compile and run it with `stack`.

### Download the script

```
git clone https://github.com:sajidanower23/github-migration
cd github-migration
```

### Install stack

`stack` is also available for install via `apt` but it may be a very old version.

```
sudo apt install haskell-stack
```

To install the latest version of `stack`, run:

```
wget -qO- https://get.haskellstack.org/ | sh
```

The latest version of `stack` the program has been tested on is:

```
$ stack --version
Version 1.9.1, Git revision f9d0042c141660e1d38f797e1d426be4a99b2a3c (6168 commits) x86_64 hpack-0.31.0
```

### Run the program

### Via compiled executable

```
stack build
stack exec github-migration -- <args>
```

## Usage

```
$ stack exec github-migration -- -h
Usage: github-migration [--info] [--long-info] [-v|--version] [--license]
                        [-?|-h|--help] [--print-config]
                        ([--config-https-insecure] |
                        [--no-config-https-insecure])
                        [--config-https-allow-cert HOSTNAME:PORT:FINGERPRINT]
                        [--config-file FILE] [-f|--from-host ARG]
                        [-k|--from-api-key ARG] [-r|--from-repo ARG]
                        [-t|--to-host ARG] [-l|--to-api-key ARG]
                        [-s|--to-repo ARG] [-c|--user-map-file ARG]
  github-migration

Available options:
  --info                   Print program info message and exit
  --long-info              Print detailed program info message and exit
  -v,--version             Print version string and exit
  --license                Print license of the program and exit
  -?,-h,--help             Show this help message
  --print-config           Print the parsed configuration to standard out and
                           exit
  --config-https-insecure  Bypass certificate validation for all HTTPS
                           connections to all services. ONLY USE THIS WHEN YOU
                           UNDERSTAND WHAT YOU DO.
  --no-config-https-insecure
                           unset flag config-https-insecure
  --config-https-allow-cert HOSTNAME:PORT:FINGERPRINT
                           Unconditionally trust the certificate for connecting
                           to the service. ONLY USE THIS WHEN YOU ARE SURE THAT
                           THE CERTIFICATE CAN BE TRUSTED.
  --config-file FILE       Configuration file in YAML or JSON format. If more
                           than a single config file option is present files are
                           loaded in the order in which they appear on the
                           command line.
  -f,--from-host ARG       From Host
  -k,--from-api-key ARG    From API Key
  -r,--from-repo ARG       Source Repo
  -t,--to-host ARG         To Host
  -l,--to-api-key ARG      To API Key
  -s,--to-repo ARG         Dest Repo
  -c,--user-map-file ARG   CSV File containing user maps

Configurations are loaded in order from the following sources:
  1. Configuration files from locations provided through --config-file options
     in the order as they appear.
  2. Command line options.

Configuration file locations can be either local file system paths or remote
HTTP or HTTPS URLs. Remote URLs must start with either "http://" or "https://".

Configuration settings that are loaded later overwrite settings that were loaded
before.

```

### Command line options

-f,--from-host: From Host. Use `api.github.com` if it's github.com.
Defaults to Enterprise Github.

-t,--to-host: To Host.  Similar to `-f`. Use `api.github.com` if it's github.com.
Defaults to Enterprise Github.

-k,--from-api-key: An API key generated from the `from-host`.
-r,--from-repo: Source Repo, in the form `<owner>/<reponame>`. For example, `Microsoft/vscode`.

-l,--to-api-key: Similar to `k`, but in the `to-host`. The `from-api-key` and `to-api-key`
must belong to the same person.

-s,--to-repo: Similar to the source repo name, in the form `<owner>/<reponame>`.

-c,--user-map-file: Path to a CSV file containing information about all users
relevant to the repository (ies).

### User mapping

If you would like user mapping between source and destination repository
(relevant for a migration from Github Enterprise to github.com or vice-versa),
then you want to pass in a CSV file with the `-c` option which includes user
information in the following format:

> username_source, username_dest, useremail_source, useremail_dest, accesstoken_dest

Note that if you do not provide this file, then all the github events (issues, comments, etc)
will be attributed to the user running the migration. However, issues
and issue comments will have an attribution at the bottom saying who was the
original author.

## Assumptions

The following assumptions are made when the program runs, and will likely result
in an error (and halting of execution) if they are not met:

- The users in destination repo are already added as collaborators
- The user running has read access to everything in the source repo, and has full write-access to destination repo.
- Each access key (given via the CSV file) has write-access to the destination repo
- No milestones were deleted in source repo.
- No issues were deleted in source repo.

## Known Issues / Future features

- Transfers Pull Requests as an Issue instead of a Pull Request.
- Releases are not transferred.
- If some milestones were deleted in the source repo, the milestones in the dest repo will not have the same order, and by extension, the milestone assignments will be incorrect.
- If an issue was deleted (a relatively new feature exclusive to github.com), the order of issues will be incorrect in the destination repo.
