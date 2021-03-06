# This is my Bash login shell configuration. This file contains some
# useful aliases, environment variables and also configures the shell
# prompt to show git branch information.

# Don't suggest using zsh in macOS Catalina and newer versions of macOS.
export BASH_SILENCE_DEPRECATION_WARNING=1

# Custom Homebrew directory.
export HOMEBREW_HOME="/usr/local"

# My GPG key.
export GPGKEY=19993523339747A9

# Java SDK.
export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"

# Android SDK.
export ANDROID_SDK_ROOT="$HOMEBREW_HOME/share/android-sdk"

# Android NDK.
export ANDROID_NDK_HOME="$HOMEBREW_HOME/share/android-ndk"

# Use CCache for Android NDK.
export USE_CCACHE=0
export NDK_CCACHE="$HOMEBREW_HOME/bin/ccache"

# LLVM source root.
export LLVM_SOURCE_ROOT=~/Projects/llvm-project/llvm/

# Swift source root.
export SWIFT_SOURCE_ROOT=~/Projects/swift-source/

# Default Swift build dir.
export SWIFT_BUILD_DIR=~/Projects/swift-source/build/Ninja-RelWithDebInfoAssert+swift-DebugAssert/swift-macosx-x86_64/

# Prepend /usr/local to PATH for Homebrew executables.
export PATH="/usr/local/bin:/usr/local/sbin:~/bin:$PATH"

# We want to install Casks without root privileges.
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ll='ls -alF'

source ~/.git-prompt.sh
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$(__git_ps1)$ "

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1

source ~/git-completion.bash

# Convenient aliases to switch between Xcode and Xcode beta.
alias xcode='sudo xcode-select -s /Applications/Xcode.app/Contents/Developer/ && xcode-select -p'
alias xcodeb='sudo xcode-select -s /Applications/Xcode-beta.app/Contents/Developer/ && xcode-select -p'

# Convenient aliases to run lit.py.
alias lit='${LLVM_SOURCE_ROOT}/utils/lit/lit.py -sv --param swift_site_config=${SWIFT_BUILD_DIR}/test-macosx-x86_64/lit.site.cfg'
alias litsimulator='${LLVM_SOURCE_ROOT}/utils/lit/lit.py -sv --param swift_site_config=${SWIFT_BUILD_DIR}/test-iphonesimulator-x86_64/lit.site.cfg'

# PSPDFKit changelog management.
alias cla='bundle exec pspdfkit changelog add'
alias clg='bundle exec pspdfkit changelog generate'

# Add Cargo (Rust) and Yarn (JS) packages to PATH.
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# Add Arcanist to PATH.
export PATH="$HOME/Projects/arcanist/bin:$PATH"

# ASDF configuration.
source $HOMEBREW_HOME/opt/asdf/asdf.sh
