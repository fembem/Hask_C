#! /bin/sh -e

./Hcat *.hs | ./Hsort | ./Huniq | more
