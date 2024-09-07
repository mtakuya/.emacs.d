sudo apt update
sudo apt install -y build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo autoconf

git clone --depth 1 --branch emacs-29 https://git.savannah.gnu.org/git/emacs.git

cd emacs
git checkout emacs-29

./autogen.sh

./configure --with-pgtk --with-native-compilation

make -j$(nproc)
sudo make install



