# github-migration

Transfers issues, comments, labels and milestones from one Github repository (Github Enterprise or github.com)
to another.

## Installation

The easiest way to run this is to install `stack`, compile it and run it.

```console
git clone https://github.com:sajidanower23/github-migration
cd github-migration
sudo apt install haskell-stack # to install stack
stack build
stack exec github-migration -- <args>
```


## Usage

```console
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

If you would like user mapping between source and destination repository
(relevant for a migration from Github Enterprise to github.com or vice-versa),
then you want to pass in a CSV file with the `-c` option which includes user
information in the following format:

> username_source, username_dest, useremail_source, useremail_dest, accesstoken_dest

Note that if you do not provide this file, then all the github events (issues, comments, etc)
will be attributed to the user running the migration. However, issues
and issue comments will have an attribution at the bottom saying who was the
original author.

## Known Issues

- Transfers Pull Requests as an Issue instead of a Pull Request, which means that the Pull Request diff is lost.
- If transferring between different domains (i.e, between Github Enterprise and github.com or vice-versa), issue assignees are lost.
- Releases are not transferred.
