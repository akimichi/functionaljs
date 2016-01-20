# ~/.profile: executed by Bourne-compatible login shells.

if [ "$BASH" ]; then
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi
fi

mesg n
# The Following loads nvm, and install Node.js which version is assigned to $NODE_ENV
source ~/.nvm/nvm.sh
echo "Installing node@${NODE_VERSION}, this may take several minutes..."
# nvm install ${NODE_VERSION}
nvm use v${NODE_VERSION}
nvm alias default v${NODE_VERSION}
alias gulp='node --harmony `which gulp`'

# sbt
export PATH=~/scala-$SCALA_VERSION/bin:$PATH
