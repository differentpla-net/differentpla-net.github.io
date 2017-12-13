# Installing `robot-detect`

Linux Mint 18.3. Yak shaving.

## Install Python3

    sudo apt-get install python3 python3-dev

## Install Python3 with virtualenv

    # Installs to $HOME/.local
    pip install --upgrade pip
    pip install --upgrade virtualenv

    PYTHON_VERSION=$(python3  -c 'import platform; print(platform.python_version())')

    # I have a habit of using a 'direnv'-compatible layout.
    virtualenv -p python3 .direnv/python-$PYTHON_VERSION
    source ./.direnv/python-$PYTHON_VERSION/bin/activate

## Install required Python packages

    pip install gmpy2
    pip install cryptography

## Install `robot-detect`

    wget https://raw.githubusercontent.com/robotattackorg/robot-detect/master/robot-detect
    chmod +x robot-detect

## Run `robot-detect`

    ./robot-detect example.com
