. lib
rm -rf R
darcs init --repo R
cd R

echo 'revision 1' > f
darcs record -lam 'p1'

echo 'revision 2' > f
darcs record -am 'p2'

echo 'revision 3' > f
darcs record -am 'p3'


darcs annotate f > out   # creates patch index on annotate
grep p3 out
not grep p2 out
not grep p1 out

darcs annotate f --patch 'p2' > out
grep p2 out
not grep p3 out
not grep p1 out

darcs annotate f --patch 'p1' > out
grep p1 out
not grep p2 out
not grep p3 out

darcs optimize disable-patch-index

darcs annotate f > out
grep p3 out
not grep p2 out
not grep p1 out

darcs annotate f --patch 'p2' > out
grep p2 out
not grep p3 out
not grep p1 out

darcs annotate f --patch 'p1' > out
grep p1 out
not grep p2 out
not grep p3 out
