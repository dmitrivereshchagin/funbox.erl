ARG WORKSPACE_OTP_VERSION
FROM erlang:$WORKSPACE_OTP_VERSION-alpine

ARG WORKSPACE_UID
ARG WORKSPACE_GID

RUN set -x \
    && addgroup -g "$WORKSPACE_GID" workspace \
    && adduser -G workspace -D -u "$WORKSPACE_UID" workspace

USER workspace:workspace
