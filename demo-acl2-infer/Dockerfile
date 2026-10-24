# ACL2 for remora --acl2-infer (see README.md).
#
# This starts from Kestrel's nightly image of ACL2 with its books certified,
# which must include kestrel/remora/elaborator, and adds acl2-remora: an ACL2
# executable with that book already loaded, so that each call doesn't spend
# about 30 seconds loading books first.
#
# Build, from the repository root:
#   docker build --pull -t remora-acl2 demo-acl2-infer
# Use:
#   export ACL2="docker run --rm -i remora-acl2"
#
# To start from another image that has the book certified:
#   docker build -t remora-acl2 --build-arg ACL2_IMAGE=IMAGE demo-acl2-infer
ARG ACL2_IMAGE=ghcr.io/kestrelinstitute/kestrel-allcerts-java:latest
FROM ${ACL2_IMAGE}

RUN test -f /root/acl2/books/kestrel/remora/elaborator.cert \
 || { echo "This ACL2 image does not have kestrel/remora/elaborator certified." >&2; \
      echo "If that book is in ACL2 now, docker build --pull gets the newest image." >&2; \
      exit 1; }

RUN printf '%s\n' \
      '(include-book "kestrel/remora/elaborator" :dir :system)' \
      ':q' \
      '(save-exec "/root/acl2/bin/acl2-remora" "ACL2 with kestrel/remora/elaborator loaded")' \
    | acl2

# Check that acl2-remora elaborates a program.  (Not grep -q: the base image
# runs RUN with pipefail, so grep must read all of ACL2's output.)
RUN printf '%s\n' \
      '(remora::elaborate-program-string "(entry (main) (reverse [3 1 4 1]))" state)' \
      '(good-bye 0)' \
    | acl2-remora | grep '<<<remora-acl2 elaborated>>>' > /dev/null

CMD ["acl2-remora"]
