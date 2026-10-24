#!/usr/bin/env bash
# Before/after demo of remora --acl2-infer; see README.md.
#
# Usage: demo.sh [WORK_DIR]
#   Copies the programs in this directory to WORK_DIR (default: a new
#   temporary directory) and runs remora on them there, printing each command
#   before its output.
# Environment:
#   REMORA  remora built from this branch (default: remora)
#   ACL2    the command that starts ACL2 with kestrel/remora/elaborator,
#           e.g. "docker run --rm -i remora-acl2" (default: acl2)
set -u
here=$(cd "$(dirname "$0")" && pwd)
REMORA=${REMORA:-remora}

# Turn REMORA into a full path, since the script changes directory below.
# (As a path, it also can't be mistaken for the function remora below.)
if ! remora_path=$(command -v "$REMORA"); then
  if [ "$REMORA" = remora ]; then
    cat >&2 << 'EOF'
demo.sh: there is no remora on the search path, probably because nix isn't
set up in this shell. Run this in the repository root, then run demo.sh again:
  nix shell .#remora-wrapped
(Or set REMORA to a remora built from this branch; see README.md.)
EOF
  else
    echo "demo.sh: REMORA is set to \"$REMORA\", but there is no such program." >&2
  fi
  exit 1
fi
case $remora_path in
  /*) REMORA=$remora_path ;;
  *) REMORA=$PWD/$remora_path ;;
esac

if ! "$REMORA" interpret --help 2> /dev/null | grep -q -e --acl2-infer; then
  cat >&2 << EOF
demo.sh: this remora has no --acl2-infer option, so it wasn't built from this
branch (see README.md):
  $REMORA
EOF
  exit 1
fi
if [ -z "${ACL2:-}" ] && ! command -v acl2 > /dev/null; then
  cat >&2 << 'EOF'
demo.sh: ACL2 isn't set, and there is no acl2 on the search path. Once the
remora-acl2 image is built (setup step 2 in README.md), run this, then run
demo.sh again:
  export ACL2="docker run --rm -i remora-acl2"
EOF
  exit 1
fi

work=${1:-$(mktemp -d)}
mkdir -p "$work"
cp "$here"/*.remora "$work/"
cd "$work" || exit 1
rm -f ./*-typed.remora

show() { # print a command, then run it
  echo
  echo "\$ $*"
  "$@"
}

remora() { "$REMORA" "$@"; }

echo "# 1. Without --acl2-infer, a missing type or index application is an error."
show cat reverse.remora
show remora interpret -f reverse.remora

echo
echo "# 2. With --acl2-infer, ACL2 attempts to infer the applications and, if"
echo "#    successful, passes the elaborated program back to remora; remora writes"
echo "#    the elaborated program next to the original as reverse-typed.remora, and"
echo "#    then interprets/compiles that elaborated program (which is ordinary"
echo "#    Remora code)."
show remora interpret --acl2-infer -f reverse.remora
show cat reverse-typed.remora

echo
echo "# 3. The same through the compiler: Futhark, then C."
show cat my-last.remora
show remora futhark --acl2-infer --run -f my-last.remora
show cat my-last-typed.remora

echo
echo "# 4. Or just the Futhark IR."
show remora futhark --acl2-infer -f reverse.remora

echo
echo "# 5. When ACL2 cannot infer the applications, remora's own error stands."
show remora interpret --acl2-infer -f reduce.remora

exit 0 # steps 1 and 5 fail on purpose
