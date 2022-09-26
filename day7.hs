-- run the following regex
-- %s/\(.*\) -> \(.*\)/\2 = \1  -- Move the -> to be = at the end
-- %s/OR/doOr/
-- %s/AND/doAnd/
-- %s/NOT/doNot/
-- %s/LSHIFT/doLshift/
-- %s/RSHIFT/doRshift/
-- %s/\(\w\+\) \(\w\+\) \(\w\+\)/\2 \1 \3/
-- s/ = \(\d\+\)/ = \1 :: Word16
-- %s/in/inn/
-- %s/if /iif /
-- %s/do /doo /


import Data.Bits
import Data.Word



doAnd x y= (.&.) x y

doOr x y= (.|.) x y

doLshift x y = x `shiftL` y
doRshift x y = x `shiftR` y
doNot x = complement x


ls = doAnd lf lq
jn = doRshift iu 1
bv = doOr bo bu
hc = doRshift gj 1
eu = doRshift et 2
by = doAnd bv bx
iu = doOr is it
o = doOr b n
gg = doOr gf ge
ku = doNot kt
ed = doAnd ea eb
ks = doOr kl kr
hl = doAnd hi hk
ax = doAnd au av
lg = doRshift lf 2
df = doRshift dd 3
fc = doAnd eu fa
di = doAnd df dg
it = doLshift ip 15
em = doNot el
ff = doOr et fe
fn = doLshift fj 15
u = doOr t s
ma = doOr ly lz
kr = doAnd ko kq
fy = doNot fx
fm = doRshift et 1
fb = doOr eu fa
de = doRshift dd 2
gp = doNot go
ke = doAnd kb kd
hi = doOr hg hh
kg = doLshift jm 1
co = doNot cn
jq = doRshift jp 2
js = doRshift jp 5
ip = doAnd 1 io
es = doLshift eo 15
jk = doAnd 1 jj
j = doAnd g i
ck = doRshift ci 3
gq = doAnd gn gp
fv = doAnd fs fu
lm = doAnd lj ll
jo = doLshift jk 15
iw = doRshift iu 3
ij = doNot ii
cd = doAnd 1 cc
bp = doRshift bn 3
gx = doNot gw
fu = doNot ft
jp = doOr jn jo
jc = doOr iv jb
hw = doOr hv hu
b = 16076 :: Word16
-- old bb = 19138 :: Word16
gm = doRshift gj 5
ht = doAnd hq hs
er = doRshift dy 1
ap = doOr ao an
lf = doOr ld le
ce = doLshift bk 1
cc = doAnd bz cb
bm = doLshift bi 15
io = doAnd il inn
ai = doAnd af ah
bl = doRshift as 1
lh = doRshift lf 3
et = doOr er es
ay = doNot ax
db = doRshift ci 1
fg = doAnd et fe
ln = doOr lg lm
n = doAnd k m
ia = doRshift hz 2
lb = doLshift kh 1
ez = doNot ey
dj = doNot di
eg = doOr dz ef
a = lx
ja = doNot iz
hd = doLshift gz 15
cf = doOr ce cd
ft = doAnd fq fr
bb = doAnd at az
hb = doOr ha gz
fx = doAnd fp fv
gc = doNot gb
ii = doAnd ia ig
gn = doOr gl gm
c = 0 :: Word16
cb = doNot ca
cg = doRshift bn 1
t = doLshift c 1
iy = doOr iw ix
kh = doOr kg kf
ek = doOr dy ej
kp = doAnd km kn
fd = doNot fc
ib = doRshift hz 3
dr = doNot dq
fh = doNot fg
dz = doRshift dy 2
kl = doRshift kk 2
fj = doAnd 1 fi
hs = doNot hr
ki = doRshift jp 1
bn = doOr bl bm
gz = doAnd 1 gy
gu = doAnd gr gt
dd = doOr db dc
dl = doOr de dk
av = doRshift as 5
li = doRshift lf 5
hp = doAnd hm ho
ci = doOr cg ch
gw = doAnd gj gu
gi = doLshift ge 15
g = doOr e f
fw = doOr fp fv
fe = doAnd fb fd
ch = doLshift cd 15
v = doRshift b 1
ba = doOr at az
bo = doRshift bn 2
lk = doAnd lh li
doo= doAnd dl dn
ej = doAnd eg ei
fa = doAnd ex ez
kq = doNot kp
ll = doNot lk
ak = doAnd x ai
kb = doOr jp ka
je = doNot jd
jb = doAnd iy ja
jr = doRshift jp 3
ga = doOr fo fz
dh = doOr df dg
gk = doRshift gj 2
gv = doOr gj gu
ji = doNot jh
bj = doLshift ap 1
lt = doNot ls
jl = doLshift ir 1
ca = doAnd bn by
lz = doLshift lv 15
bd = doAnd ba bc
dc = doLshift cy 15
lq = doAnd ln lp
aq = doRshift x 1
gr = doOr gk gq
ky = doNot kx
jj = doAnd jg ji
bz = doOr bn by
gf = doLshift fl 1
br = doOr bp bq
hq = doOr he hp
ew = doRshift et 5
iv = doRshift iu 2
go = doAnd gl gm
aj = doOr x ai
he = doOr hc hd
lo = doAnd lg lm
lj = doOr lh li
du = doLshift da 1
fp = doRshift fo 2
gs = doAnd gk gq
bk = doOr bj bi
lr = doOr lf lq
cr = doAnd cj cp
hy = doLshift hu 15
bi = doAnd 1 bh
fq = doRshift fo 3
lp = doNot lo
iq = doLshift hw 1
dw = doRshift dd 1
dx = doLshift dt 15
el = doAnd dy ej
ar = doLshift an 15
as = doOr aq ar
s = doAnd 1 r
fz = doAnd fw fy
inn = doNot im
ev = doRshift et 3
dt = doAnd 1 ds
ef = doAnd ec ee
al = doNot ak
jm = doOr jl jk
eo = doAnd 1 en
lc = doOr lb la
jh = doAnd iu jf
ix = doRshift iu 5
bw = doAnd bo bu
da = doOr cz cy
jd = doAnd iv jb
iz = doAnd iw ix
ly = doRshift lf 1
jg = doOr iu jf
dn = doNot dm
lx = doOr lw lv
ha = doLshift gg 1
lu = doAnd lr lt
fo = doOr fm fn
hg = doRshift he 3
am = doAnd aj al
la = doAnd 1 kz
eb = doRshift dy 5
jf = doAnd jc je
cp = doAnd cm co
gy = doAnd gv gx
ex = doOr ev ew
kc = doAnd jp ka
fl = doOr fk fj
ea = doRshift dy 3
bt = doNot bs
ah = doNot ag
eh = doAnd dz ef
cz = doLshift cf 1
cw = doNot cv
cy = doAnd 1 cx
dm = doAnd de dk
cn = doAnd ck cl
aa = doRshift x 5
ep = doLshift dv 1
hf = doRshift he 2
bx = doNot bw
cm = doOr ck cl
bs = doAnd bp bq
be = doOr as bd
hr = doAnd he hp
ey = doAnd ev ew
lv = doAnd 1 lu
km = doRshift kk 3
p = doAnd b n
kd = doNot kc
lw = doLshift lc 1
ko = doOr km kn
ig = doAnd ida iif
ik = doAnd ih ij
ju = doAnd jr js
cl = doRshift ci 5
is = doRshift hz 1
kf = doAnd 1 ke
gt = doNot gs
az = doAnd aw ay
y = doRshift x 2
ae = doAnd ab ad
fi = doAnd ff fh
cv = doAnd ci ct
fk = doLshift eq 1
gl = doRshift gj 3
ao = doLshift u 1
bc = doNot bb
hk = doNot hj
kz = doAnd kw ky
bf = doAnd as bd
dy = doOr dw dx
bu = doAnd br bt
kx = doAnd kk kv
eq = doOr ep eo
hx = doRshift he 1
kk = doOr ki kj
jv = doNot ju
en = doAnd ek em
kn = doRshift kk 5
ei = doNot eh
hz = doOr hx hy
ec = doOr ea eb
w = doLshift s 15
gh = doRshift fo 1
kw = doOr kk kv
bq = doRshift bn 5
ee = doNot ed
hu = doAnd 1 ht
cx = doAnd cu cw
f = doRshift b 5
kt = doAnd kl kr
ir = doOr iq ip
cj = doRshift ci 2
cq = doOr cj cp
r = doAnd o q
dg = doRshift dd 5
d = doRshift b 2
kv = doAnd ks ku
e = doRshift b 3
k = doOr d j
q = doNot p
cs = doNot cr
dv = doOr du dt
kj = doLshift kf 15
ad = doNot ac
fr = doRshift fo 5
il = doOr hz ik
ka = doAnd jx jz
gj = doOr gh gi
ld = doRshift kk 1
ic = doRshift hz 5
at = doRshift as 2
jz = doNot jy
an = doAnd 1 am
cu = doOr ci ct
hj = doAnd hg hh
jx = doOr jq jw
x = doOr v w
le = doLshift la 15
dk = doAnd dh dj
ds = doAnd dp dr
jy = doAnd jq jw
aw = doOr au av
bg = doNot bf
ab = doOr z aa
gd = doAnd ga gc
im = doAnd hz ik
jw = doAnd jt jv
ac = doAnd z aa
jt = doOr jr js
hv = doLshift hb 1
hm = doOr hf hl
ida = doOr ib ic
fs = doOr fq fr
ct = doAnd cq cs
ih = doOr ia ig
dp = doOr dd doo
l = doAnd d j
ie = doAnd ib ic
au = doRshift as 3
bh = doAnd be bg
dq = doAnd dd doo
m = doNot l
ge = doAnd 1 gd
ag = doAnd y ae
gb = doAnd fo fz
iif= doNot ie
h = doAnd e f
z = doRshift x 3
af = doOr y ae
hn = doAnd hf hl
i = doNot h
ho = doNot hn
hh = doRshift he 5



