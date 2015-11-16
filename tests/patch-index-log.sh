. lib                         
rm -rf R
darcs init --repo R
cd R

[[ $(darcs log FOO --count | tail -n 1) -eq 0 ]]

touch f0
darcs record -lam 'p0'

mkdir d1
darcs record -lam 'p1'

touch d1/f1
darcs record -lam 'p2'

darcs move f0 d1/f0
darcs record -am 'p3'

touch d1/f2
darcs record -lam 'p4'

rm d1/f1
darcs record -am 'p5'

darcs move d1/f0 f0
darcs record -am 'p6'

darcs move d1 d2
darcs record -am 'p7'

mkdir d1
darcs record -lam 'p8'

touch d1/f3
darcs record -lam 'p9'

echo "f0" > f0
darcs record -am 'p10'

# log on directories gives all patches
# that touched any sub path (including itself)

# Note that the path of the directory varies
# as you move backward in history.

# The sub path comparision is with the path
# the directory had when the patch which is is being
# tested has been just applied.

# d1 <-> p8-p9
[[ $(darcs log d1 --count | tail -n 1) -eq 2 ]]
for i in 8 9
do
   darcs log d1 | grep p$i
done


# d2 <-> p1-p7
[[ $(darcs log d2 --count | tail -n 1) -eq 7 ]]
for i in {1..7}
do
   darcs log d2 | grep p$i
done

darcs optimize disable-patch-index
[[ $(darcs log FOO --count | tail -n 1) -eq 0 ]]
[[ $(darcs log d1 --count | tail -n 1) -eq 2 ]]
for i in 8 9
do
   darcs log d1 | grep p$i
done
[[ $(darcs log d2 --count | tail -n 1) -eq 7 ]]
for i in {1..7}
do
   darcs log d2 | grep p$i
done
