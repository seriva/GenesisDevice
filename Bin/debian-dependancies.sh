apt-get install libsdl2-dev libopenal-dev libmpg123-dev
apt-get install build-essential cmake libtinyxml-dev libglfw3-dev
wget https://github.com/MADEAPPS/newton-dynamics/archive/newton-3.14.zip; unzip newton-3.14.zip; rm newton-3.14.zip
cd newton-dynamics-newton-3.14
mkdir build && cd build
cmake -DNEWTON_DEMOS_SANDBOX=OFF .. && make
sudo cp -rf ./lib/libNewton.so /usr/lib/
cd ../../
rm -rf newton-dynamics-newton-3.14
