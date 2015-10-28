NB. Universal gas constant
NB. J/(mol K)
R=: 8.31

NB. The performance depends on heat consumption as
NB. gF = b*q - a*q^2, where
NB. b is the "reversible efficiency"
NB. and a is the "irreversibility coefficient"

NB. Gibbs reversible separation work
NB. J/mol
NB. TD - temperature in reflux drum, K
NB. xF - molar ratio of low-boiling component, mol/mol
AG=: 3 : 0
  'TD xF' =. y
  -R*TD*((xF* ^.xF)+(1-xF)*(^. 1-xF))
)

NB. Reversible efficiency
NB. mol/J
NB. TD - temperature in reflux drum, K
NB. TB - temperature in reboiler, K
NB. xF - molar ratio of low-boiling component, mol/mol
b=: 3 : 0
  'TD TB xF'=. y
  AG=. AG TD;xF
  (TB-TD)%(TB*AG)
)

NB. Mass transfer coefficient
NB. mol^2 K / (J s)
NB. Shows the irreversibility of
NB. the mass transfer in the column
NB. gF - given performance, mol/s
NB. xF - molar ratio of low-boiling component, mol/mol
NB. q  - given heat consumption, W
NB. r  - molar vaporization heat, J/mol
NB. alpha - relative volatility, Pa/Pa
k=: 3 : 0
  'gF xF q r alpha' =. y
  I1=. (^. alpha)%(1-alpha)
  V=. q%r
  gB=. gF*(1-xF)
  gD=. gF*xF
  gDV=. gD%V
  I2=. (xF*(^. (V+gB)*xF%V))-xF
  m=. gD+(V-gD)*xF
  l=. ^. (gD+(V-gD)*xF)%V
  s=. (V-gD)+(gD-V)*xF
  md=. gD-V
  I3=. (s+m*l)%md
  q%(R*r*(I1-(I2+I3)))
)

NB. Irreversibility coefficient
NB. mol s / J^2
NB. bB - heat transfer coefficient in reboiler, J/K
NB. bD - heat transfer coefficient in reflux drum, J/K
NB. k  - mass transfer coefficient, mol^2 K / (J s)
NB. TD - temperature in reflux drum, K
NB. TB - temperature in reboiler, K
NB. r  - molar vaporization heat, J/mol
NB. xF - molar ratio of low-boiling component, mol/mol
a=: 3 : 0
  'bB bD k TD TB r xF' =. y
  S1=. % (bB*TB*(TB+15))
  S2=. % (bD*TD*(TD-15))
  S3=. 2 % (k * r * r)
  AG=. AG TD;xF
  (S1+S2+S3)*TD%AG
)

NB. Limiting efficiency coefficient
NB. r  - molar vaporization heat, J/mol
NB. xF - molar ratio of low-boiling component, mol/mol
NB. alpha - relative volatility, Pa/Pa
elim=: 3 : 0
  'r xF alpha' =. y
  (alpha-1)%(r*(1+(alpha-1)*xF))
)

NB. Irreversible estimate of heat consumption for
NB. the given performance
NB. W
NB. bB - heat transfer coefficient in reboiler, J/K
NB. bD - heat transfer coefficient in reflux drum, J/K
NB. k  - mass transfer coefficient, mol^2 K / (J s)
NB. TD - temperature in reflux drum, K
NB. TB - temperature in reboiler, K
NB. r  - molar vaporization heat, J/mol
NB. xF - molar ratio of low-boiling component, mol/mol
NB. gF - given performance, mol/s
Q=: 3 : 0
  'bB bD k TD TB r xF gF' =. y
  b=. b TD;TB;xF
  a=. a bB;bD;k;TD;TB;r;xF
  D=. (b*b) -  4*a*gF
  (b - %:D)%(2*a)
)

gF=. 1.85
r1=: 74100
r2=: 88800
r3=: 100400
r4=: 100750
q=:  150000.0
alpha12=: 1.78
alpha23=: 1.18
alpha34=: 1.9
x1=: 0.23
x2=: 0.48
x3=: 0.14
x4=: 0.15
T1=: 542.0
T2=: 571.0
T3=: 599.0
T4=: 625.0
bB=: 140000
bD=: 142500

k11=: k gF;x1;q;r1;alpha12

b1=. b T1;T2;x1
b2=. b T2;T3;(x2 % (x2+x3+x4))
b3=. b T3;T4;(x3 % (x3+x4))
qRev=. (gF%b1) + (gF*(x2+x3+x4)%b2) + (gF*(x3+x4)%b3)
q1=. Q bB;bD;k11;T1;T2;r1;x1;gF
q2=. Q bB;bD;k11;T2;T3;r2;(x2%(x2+x3+x4));gF*(x2+x3+x4)
q3=. Q bB;bD;k11;T3;T4;r3;(x3%(x3+x4));gF*(x3+x4)
q123=. q1+q2+q3

q1=. Q bB;bD;k11;T1;T2;r1;x1;gF
q2=. Q bB;bD;k11;T3;T4;(((r2*x2)+r3*x3)%(x2+x3));((x2+x3)%(1-x1));(gF*(1-x1))
q3=. Q bB;bD;k11;T2;T3;r2;(x2%(x2+x3));(gF*(x2+x3))
q132=. q1+q2+q3
smoutput ('Q reversible';qRev),('Order 1-2-3';q123),:('Order 1-3-2';q132)

smoutput elim r1;x1;alpha12
smoutput b T1;T2;x1
exit''