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
  'xF r alpha' =. y
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

NB. Calculate distillation for some mixture
NB. Feed:
NB.  xs - array of molar concentrations, [mol/mol]
NB.  Ts - array of evaporation temperatures, [K]
NB.  rs - array of molar vaporization heats, [J/mol]
NB. bB - heat transfer coefficient in reboiler, J/K
NB. bD - heat transfer coefficient in reflux drum, J/K
NB. k  - mass transfer coefficient, mol^2 K / (J s)
NB. gF - given performance, mol/s
NB. i  - point of separation (i.e 1 if the mixture is separated
NB.  between the first and the second components, etc)
NB. Result:
NB. Upper product; Lower product; b; a; gD; gB; heat consumption
NB. gD - flow from reflux drum, gB - flow from reboiler
dist=: 3 : 0
  'xs Ts rs bB bD k gF i' =. y
  xs1=. i {. xs
  xs2=. i }. xs
  xF=. +/xs1
  TD=. Ts{~<:i
  TB=. i{Ts
  r=. (+/(i{.rs)*xs1) (%+/) xs1
  cb=. b TD;TB;xF
  ca=. a bB;bD;k;TD;TB;r;xF
  D=. (cb*cb)-4*ca*gF
  q=. (cb-%:D)%(2*ca)
  xs1=. (%+/)xs1
  xs2=. (%+/)xs2
  (xs1;(i{.Ts);(i{.rs));(xs2;(i}.Ts);(i}.rs));cb;ca;(gF*xF);(gF*(1-xF));q
)

NB. Calculate distillation for some mixture
NB. in maximum power mode
NB. Feed:
NB.  xs - array of molar concentrations, [mol/mol]
NB.  Ts - array of evaporation temperatures, [K]
NB.  rs - array of molar vaporization heats, [J/mol]
NB. bB - heat transfer coefficient in reboiler, J/K
NB. bD - heat transfer coefficient in reflux drum, J/K
NB. k  - mass transfer coefficient, mol^2 K / (J s)
NB. i  - point of separation (i.e 1 if the mixture is separated
NB.  between the first and the second components, etc)
NB. Result:
NB. Upper product; Lower product; b; a; gD; gB; heat consumption
NB. gD - flow from reflux drum, gB - flow from reboiler
mdist=: 3 : 0
  'xs Ts rs bB bD k i' =. y
  xs1=. i {. xs
  xs2=. i }. xs
  xF=. +/xs1
  TD=. Ts{~<:i
  TB=. i{Ts
  r=. (+/(i{.rs)*xs1) (%+/) xs1
  cb=. b TD;TB;xF
  ca=. a bB;bD;k;TD;TB;r;xF
  gF=. (*:cb)%(4*ca)
  q=. cb%(2*ca)
  xs1=. (%+/)xs1
  xs2=. (%+/)xs2
  (xs1;(i{.Ts);(i{.rs));(xs2;(i}.Ts);(i}.rs));cb;ca;(gF*xF);(gF*(1-xF));gF;q
)

NB. Summary heat consumption for some distillation order
NB. One-column version
NB. W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. gF - given performance
NB. is - distillation order, the tree:
NB.  [point [order] [order]], or 0 if there is no further separation
sumq=: 3 : 0
  'xs Ts rs bB bD k gF is' =. y
  i=. >{.is
  res=. dist xs;Ts;rs;bB;bD;k;gF;i
  'm1 m2 cb ca gD gB q'=. res
  q1=. 0
  if. (0 ~: >1{is) do.
    q1=. sumq m1,bB;bD;k;gD;1{is
  end.
  q2=. 0
  if. (0 ~: >2{is) do.
    q2=. sumq m2,bB;bD;k;gB;2{is
  end.
  q+q1+q2
)

NB. Maximum performance and heat consumption for some distillation order
NB. One-column version
NB. mol/s;W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. is - distillation order, the tree:
NB.  [point [order] [order]], or 0 if there is no further separation
maxgq=: 3 : 0
  'xs Ts rs bB bD k is' =. y
  i=. >{.is
  res=. mdist xs;Ts;rs;bB;bD;k;i
  'm1 m2 cb ca gD gB gF q'=. res
  q1=. 0
  if. (0 ~: >1{is) do.
    'g1 q1'=. maxgq m1,bB;bD;k;1{is
    if. g1 < gD do.
      gF=. g1%gD%gF
      q=. >_1{dist xs;Ts;rs;bB;bD;k;gF;i
    end.
  end.
  q2=. 0
  if. (0 ~: >2{is) do.
    'g2 q2'=. maxgq m2,bB;bD;k;2{is
    if. g2 < gB do.
      gF=. g2%gB%gF
      q=. >_1{dist xs;Ts;rs;bB;bD;k;gF;i
    end.
  end.
  gF;(q+q1+q2)
)

NB. Approximate a and b coefficients for the cascade
NB. One-column version
NB. mol/s;W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. is - distillation order, the tree:
NB.  [point [order] [order]], or 0 if there is no further separation
cascab=: 3 : 0
  'xs Ts rs bB bD k is' =. y
  'g1 q1'=. maxgq y
  g2=. -:g1
  q2=. sumq xs;Ts;rs;bB;bD;k;g2;<is
  ca=. (g2 - g1*q2%q1)%((q1*q2)-*:q2)
  cb=. (g1%q1)+ca*q1
  ca;cb
)

NB. Summary heat consumption for some distillation order
NB. Multi-column version
NB. W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. gF - given performance
NB. is - distillation order, the tree:
NB.  [point column [order] [order]], or 0 if there is no further separation
NB.  column is the index in column array
NB. cols - column array [bB;bD;k]
sumqc=: 3 : 0
  'xs Ts rs gF is cols' =. y
  i=. >{.is
  coli=. >1{is
  col=. >>(>1{is){cols
  bB=. 0{col
  bD=. 1{col
  k=. 2{col
  res=. dist xs;Ts;rs;bB;bD;k;gF;i
  'm1 m2 cb ca gD gB q'=. res
  q1=. 0
  if. (0 ~: >2{is) do.
    q1=. sumqc m1,gD;(>2{is);<cols
  end.
  q2=. 0
  if. (0 ~: >3{is) do.
    q2=. sumqc m2,gB;(>3{is);<cols
  end.
  q+q1+q2
)

NB. Maximum performance and heat consumption for some distillation order
NB. Multi-column version
NB. mol/s;W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. gF - given performance
NB. is - distillation order, the tree:
NB.  [point column [order] [order]], or 0 if there is no further separation
NB.  column is the index in column array
NB. cols - column array [bB;bD;k]
maxgqc=: 3 : 0
  'xs Ts rs is cols' =. y
  i=. >{.is
  coli=. >1{is
  col=. >>(>1{is){cols
  bB=. 0{col
  bD=. 1{col
  k=. 2{col
  res=. mdist xs;Ts;rs;bB;bD;k;i
  'm1 m2 cb ca gD gB gF q'=. res
  q1=. 0
  if. (0 ~: >2{is) do.
    'g1 q1'=. maxgqc m1,(>2{is);<cols
    if. g1 < gD do.
      gF=. g1%gD%gF
      q=. >_1{dist xs;Ts;rs;bB;bD;k;gF;i
    end.
  end.
  q2=. 0
  if. (0 ~: >3{is) do.
    'g2 q2'=. maxgqc m2,(>3{is);<cols
    if. g2 < gB do.
      gF=. g2%gB%gF
      q=. >_1{dist xs;Ts;rs;bB;bD;k;gF;i
    end.
  end.
  gF;(q+q1+q2)
)

NB. Approximate a and b coefficients for the cascade
NB. Multi-column version
NB. mol/s;W
NB. Feed:
NB.  xs - array of molar concentrations [mol/mol]
NB.  Ts - array of evaporation temperatures [K]
NB.  rs - array of molar vaporization heats [J/mol]
NB. bB - heat transfer coefficient in reboiler
NB. bD - heat transfer coefficient in reflux drum
NB. k  - mass transfer coefficient
NB. is - distillation order, the tree:
NB.  [point [order] [order]], or 0 if there is no further separation
cascabc=: 3 : 0
  'xs Ts rs is cols' =. y
  'g1 q1'=. maxgqc y
  g2=. -:g1
  q2=. sumqc xs;Ts;rs;g2;is;<cols
  ca=. (g2 - g1*q2%q1)%((q1*q2)-*:q2)
  cb=. (g1%q1)+ca*q1
  ca;cb
)

NB. Minimum heat consumption separation order
NB. One-column version
minq=: 3 : 0
  'xs Ts rs bB bD k gF' =. y
  if. (1 -: #xs) do.
    0;0
    return.
  elseif. (2 -: #xs) do.
    (>_1{dist xs;Ts;rs;bB;bD;k;gF;1);(<1;0;0)
    return.
  end.
  qm=. _
  ord=. 0$0
  for_j. }.xs do.
    idx=. >:j_index
    res=. dist xs;Ts;rs;bB;bD;k;gF;idx
    'm1 m2 cb ca gD gB q'=. res
    'q1 ord1'=. minq m1,bB;bD;k;gD
    'q2 ord2'=. minq m2,bB;bD;k;gB
    q=. q+q1+q2
    if. q < qm do.
      qm=. q
      ord=. idx;ord1;<ord2
    end.
  end.
  qm;<ord
)

NB. Preliminary optimal separation order calculation
NB. using temperature coefficients
NB. Ts - evaporation temperatures of mixture components
tcoeff=: 3 : 0
  Ts =. y
  if. 1 -: #Ts do.
    0
    return.
  elseif. 2 -: #Ts do.
    1;0;0
    return.
  end.
  T=. {. Ts
  mk=. _
  i=. 0
  k=. 0
  for_j. }.Ts do.
    k=. (T*j)%(j - T)
    if. k < mk do.
      mk=. k
      i=. >:j_index
    end.
    T=. j
  end.
  ord1=. tcoeff i {. Ts
  ord2=. tcoeff i }. Ts
  i;ord1;<ord2
)

elims=: 3 : 0
  'xs rs as' =. y
  if. (1 -: #xs) do.
    0
  elseif. (2 -: #xs) do.
    1;0;0
  end.
  for_a. as do.
    xs1=. (>:a_index) {. xs
    xs2=. (>:a_index) }. xs
    xF=. +/xs1
    r=. (+/((>:a_index){.rs)*xs1) (%+/) xs1
    e=. elim xF;r;a
    smoutput e;xF;r;a
  end.
)

minqch=: 3 : 0
  'xs Ts rs gF cols used' =. y
  if. (1 -: #xs) do.
    0;0;used
    return.
  elseif. (2 -: #xs) do.
    qm=. _
    orc=. _
    for_j. >cols do.
      if. (used i. j_index) -: #used do.
	bB=. 0{>j
	bD=. 1{>j
	k=. 2{>j
	q=. >_1{dist xs;Ts;rs;bB;bD;k;gF;1
	if. q < qm do.
	  qm=. q
	  orc=. j_index
	end.
      end.
    end.
    qm;(<1;orc;0;0);(<used,(orc{>cols))
    return.
  end.
  qm=. _
  ord=. 0$0
  for_j. }.xs do.
    for_x. cols do.
      for_y. cols do.
        idx=. >:j_index
	if. (x_index ~: y_index) *. ((used i. x_index) -: #used) *. ((used i. y_index) -: #used) do.
	  col=. 
	 smoutput 'gere'
          res=. dist xs;Ts;rs;bB;bD;k;gF;idx
          'm1 m2 cb ca gD gB q'=. res
          NB.'q1 ord1'=. minq m1,bB;bD;k;gD
          NB.'q2 ord2'=. minq m2,bB;bD;k;gB
          NB.q=. q+q1+q2
	  q=.0
	  ord1=.0
	  ord2=.0
          if. q < qm do.
            qm=. q
            ord=. idx;ord1;<ord2
          end.
	end.
      end.
    end.
  end.
  qm;<ord
)

gF=. 1.85
r1=: 74100
r2=: 88800
r3=: 100400
r4=: 100750
q=:  150000
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

NB.smoutput elim r1;x1;alpha12
NB.smoutput b T1;T2;x1
xs=. x1,x2,x3,x4
Ts=. T1,T2,T3,T4
rs=. r1,r2,r3,r4
as=. alpha12,alpha23,alpha34

cols=: (140000;142500;k11);(<110000;200000;30)

NB. smoutput dist xs;Ts;rs;bB;bD;k11;gF;2
is1=: 1;0;(<1;0;(<1;0;0))
is=: 1;0;0;(<1;0;0;(<1;0;0;0))
NB.smoutput is
smoutput sumqc xs;Ts;rs;gF;is;(<cols)
NB.smoutput maxgq xs;Ts;rs;bB;bD;k11;<is1
NB.smoutput maxgqc xs;Ts;rs;is;<cols
'q ord'=. minq xs;Ts;rs;bB;bD;k11;gF
smoutput q;ord
smoutput sumq xs;Ts;rs;bB;bD;k11;gF;<ord
smoutput 'g q'=. maxgq xs;Ts;rs;bB;bD;k11;<is1
smoutput  cascabc xs;Ts;rs;is;<cols
smoutput tcoeff Ts
smoutput elims xs;rs;as

exit''