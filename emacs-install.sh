sudo apt update
sudo apt install -y build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo autoconf libgccjit-12-dev gnutls-bin

git clone --depth 1 --branch emacs-29 https://git.savannah.gnu.org/git/emacs.git

cd emacs
git checkout emacs-29

./autogen.sh

CFLAGS='-I/usr/lib/gcc/x86_64-linux-gnu/12/include -L/usr/lib/gcc/x86_64-linux-gnu/12' ./configure --prefix=/usr/local --with-native-compilation --with-gnutls=ifavailable --without-pop --with-pgtk

make -j$(nproc)
sudo make install

# Start Emacs and run M-x describe-variable RET system-configuration-features RET.
# If Its value is contains NATIVE_COMP as shown in the following image, it is enabled.
