FROM ubuntu:focal

# Install packages
RUN apt-get update && apt-get install -y \
      software-properties-common \
      bash \
      libtinfo5 \
    --no-install-recommends \
 && rm -r /var/lib/apt/lists/*

# Switch to bash
SHELL ["/bin/bash", "-c"]

# Add GHC to the path
ENV PATH="/opt/ghc/bin:${PATH}"

# Create student user
ARG GNAME=student
ARG GID=1000
ARG UNAME=student
ARG UID=1000
ARG UHOME=/user_code
RUN set -o errexit -o nounset \
 && addgroup --gid ${GID} "${GNAME}" \
 && adduser --home "${UHOME}" --disabled-password --uid ${UID} --ingroup "${GNAME}" "${UNAME}"

# Install libraries and test runner
# Do this in one go to avoid creation of large intermediate images
# that contain the whole Cabal index
WORKDIR ${UHOME}
ENV HOME ${UHOME}
COPY --chown=${UNAME} testrunner/ ${UHOME}
RUN apt-get update && apt-get install -y \
      ghc \
      cabal-install \
      agda-bin \
      libghc-agda-dev \
    --no-install-recommends \
 && rm -r /var/lib/apt/lists/* \
 && cabal new-update \
 && cabal install --enable-executable-dynamic xml \
 && cabal install --enable-executable-dynamic \
 && rm ~/.cabal/packages/hackage.haskell.org/*-index.* \
 && rm -r /lib/ghc/Cabal-* \
 && find ~/.cabal -type f -name '*.a' -exec rm {} + \
 && find /lib/ghc -type f -name '*.a' -exec rm {} + \
 && find /lib/ghc -type f -name '*.hi' -exec rm {} + \
 && find /lib/ghc -name '*-prof' -exec rm -r {} +

RUN chown -R ${UNAME}:${GNAME} ${UHOME}/.cabal/

# Copy user code
COPY --chown=${UNAME} user_code/ ${UHOME}

# Remove write permission on truncate_output.sh
RUN chmod u-w truncate_output.sh

# Run
USER ${UNAME}
RUN mkdir ${UHOME}/output
CMD ( ~/.cabal/bin/TestRunner | tee output/stdout.txt ) 3>&1 1>&2 2>&3 | tee output/stderr.txt \
 && ./truncate_output.sh
 
