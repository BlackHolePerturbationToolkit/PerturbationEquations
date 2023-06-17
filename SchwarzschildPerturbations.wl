(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["SchwarzschildPerturbations`"]
Unprotect @@ Names["SchwarzschildPerturbations`*"]
ClearAll @@ Names["SchwarzschildPerturbations`*"]



(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Rules*)


CarterToBLShRule::usage = "Rule that Expresses Carter tertrad modes of the metric perturbation in terms of Barack--Lousto--Sago modes."
CarterToBLS::usage = "Rule that takes a quantity expressed in terms of its Carter modes with its inputs also in terms of Carter modes, and rexpresses the quantity in terms of its the Barack--Lousto--Sago modes with Barack--Lousto--Sago mode inputs. The resulting list is the BLS mode number from 1 to 10."

BLStotrhRule::usage = "Rule that Expresses Barack--Lousto--Sago modes of the metric perturbation in terms of trTensor basis modes."
BLStotr::usage = "Rule that takes a quantity expressed in terms of its Barack--Lousto--Sago modes, and rexpresses the quantity in terms of its the tr modes. The resulting list is the tr mode number in the following order {\!\(\*SubscriptBox[\(S\), \(tt\)]\),\!\(\*SubscriptBox[\(S\), \(tr\)]\),\!\(\*SubscriptBox[\(S\), \(rr\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\[EmptyCircle]\)]\),\!\(\*SubscriptBox[\(S\), \(+\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(-\)]\)}."


CarterToKinnersleyhRule::usage = "Rule that takes Expresses Carter tertrad modes of the metric perturbation in terms of the Kinnersley tetrad modes."
CarterToKinnersley::usage = "Rule that takes a quantity expressed in terms of its Carter modes with its inputs also in terms of Carter modes, and rexpresses the quantity in terms of its the Kinnersley tetrad modes with the Kinnersley tetrad mode inputs."


FrequencyDomainConversion::usage = "Rule for converting t derivatives to frequency domain. Assumes the time dependence is Exp[-i\[Omega]t]"


RWGaugeConditionNPform::usage = "Rule that imposes the Regge--Wheeler gauge in the Carter tetrad."
RWGaugeConditionVectorHarmonicdecompform::usage = "Rule that imposes the Regge--Wheeler gauge in the Vector harmonic decomposition."

f::usage = "Schwarzschild's function, f[R[]]=1-2M/R[], see ftoMrule"
M::usage = "Mass"
mu::usage = "coefficient in terms of l, see mutolambdarule"
(***r::usage = "Boyer\[Dash]Lindquist radial coordinates" ***)
t::usage = "Boyer\[Dash]Lindquist time coordinates"

Coupling::usage = "Coupling[xAct`xTensor`LI[l], xAct`xTensor`LI[m], xAct`xTensor`LI[s], -xAct`xTensor`LI[l'], -xAct`xTensor`LI[m'], -xAct`xTensor`LI[s'], -xAct`xTensor`LI[l''], -xAct`xTensor`LI[m''], -xAct`xTensor`LI[s'']]=\!\(\*SubscriptBox[\(\[Integral]\), \(S\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(Y\), \(_\)], \(lm\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s'\)]\)\!\(\*SuperscriptBox[\(Y\), \(l' m'\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s''\)]\)\!\(\*SuperscriptBox[\(Y\), \(l'' m''\)]\) \[DifferentialD]\[CapitalOmega]"
CInt::usage = "CInt[xAct`xTensor`LI[l], xAct`xTensor`LI[m], xAct`xTensor`LI[s], -xAct`xTensor`LI[l'], -xAct`xTensor`LI[m'], -xAct`xTensor`LI[s'], -xAct`xTensor`LI[l''], -xAct`xTensor`LI[m''], -xAct`xTensor`LI[s'']]=\!\(\*SubscriptBox[\(\[Integral]\), \(S\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(Y\), \(_\)], \(lm\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s'\)]\)\!\(\*SuperscriptBox[\(Y\), \(l' m'\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s''\)]\)\!\(\*SuperscriptBox[\(Y\), \(l'' m''\)]\) \[DifferentialD]\[CapitalOmega]"

\[Sigma]m::usage = "see sigmarule"
\[Sigma]p::usage = "see sigmarule"
\[Sigma]::usage = "see sigmarule"


ftoMrule::usage = "Rule for f[a_]\[RuleDelayed]1-2M/a"
mutolrule::usage = "Rule for mu[LI[l_],LI[s_]]:>Sqrt[(l-s)(l+1+s)]"
lambdatolrule::usage = "Rule for {\[Lambda]1[l_]\[Rule]Sqrt[(l)(l+1)],\[Lambda]2[l_]\[Rule]Sqrt[(l-1)(l+2)]}"
sigmarule::usage = "Rule for {\[Sigma]\[Rule](-1)^(\[ScriptQ]+\[ScriptL]+\[ScriptP]),\[Sigma]p\[Rule]\[Sigma]+1,\[Sigma]m\[Rule]\[Sigma]-1}"




R::usage ="radial coordinate"
BL::usage ="Boyer--Lindquist coordinates"
h::usage ="metric perturbation"
NP::usage ="Newman--Penrose tetrad basis"
l::usage ="angular number l"
l1::usage ="angular number l1"
l2::usage ="angular number l2"
m::usage ="magnetic number m"
m1::usage ="magnetic number m1"
m2::usage ="magnetic number m2"

hBS::usage ="metric perturbation in the Barack--Sago basis"


hab::usage ="metric perturbation in tr basis"
haA::usage ="metric perturbation in tr and angular basis"
hAB::usage ="metric perturbation in angular basis"
hablm::usage ="modes of metric perturbation in tr basis"
htrab::usage ="trace of metric perturbation in tr basis"
hap::usage ="metric perturbation in tr basis plus angular part"
ham::usage ="metric perturbation in tr basis minus angular part"
htrAB::usage ="trace of metric perturbation in angular basis"
hp::usage ="metric perturbation plus angular part"
hm::usage ="metric perturbation minus angular part"

htt::usage ="tt component of the metric perturbation"
htr::usage ="tr component of the metric perturbation"
hrr::usage ="rr component of the metric perturbation"
htp::usage ="t component of the plus angular part of the metric perturbation"
hrp::usage ="r component of the plus angular part of the metric perturbation"
htm::usage ="t component of the minus angular part of the metric perturbation"
hrm::usage ="r component of the minus angular part of the metric perturbation"



(* ::Subsection:: *)
(*Functions*)


SchwarzschildQuadraticOperator::usage = "Function for generating decomposed, Boyer\[Dash]Lindquist coordinate form, quadratic opperators for second-order Schwarzschild sources. The functions are quadratic in the metric perturbation. 

SchwarzschildQuadraticOperator[Source_,Gauge_:\"Generic\",OutputBasis_:\"Carter\",InputBasis_:\"Carter\"]

Arguments:
			Source: The type of source {\"d2G\", \"d2R\", \"S4d2G\", \"S0d2G\"}.

			Gauge: The Infinitesimal gauge the metric perturbation will be in {\"Generic\", \"Lorenz\", \"ReggeWheeler\", \"IngoingRadiationGauge\", \"TraceFreeIngoingRadiationGauge\", \"OutgoingRadiationGauge\", \"TraceFreeOutgoingRadiationGauge\"}.

			OutputBasis: The basis the output will be in {\"Carter\",\"BLS\",\"tr\",\"Kinnersley\"}. Note, for a source of type {\"S4d2G\", \"S0d2G\"} the output can only be in the \"Carter\" basis.

			InputBasis: The basis the input metric perturbation will be in {\"Carter\",\"BLS\",\"tr\",\"Kinnersley\"}.

"


SchwarzschildLinearOperator::usage = "Function for generating decomposed, Boyer\[Dash]Lindquist coordinate form, linear opperators in Schwarzschild. 

SchwarzschildLinearOperator[Source_,Gauge_:\"Generic\",OutputBasis_:\"Carter\",InputBasis_:\"Carter\"]

Arguments:
			Source: The type of source {\"dG\", \"dR\"}.

			Gauge: The Infinitesimal gauge the metric perturbation will be in {\"Generic\", \"Lorenz\", \"ReggeWheeler\", \"IngoingRadiationGauge\", \"TraceFreeIngoingRadiationGauge\", \"OutgoingRadiationGauge\", \"TraceFreeOutgoingRadiationGauge\"}.

			OutputBasis: The basis the output will be in {\"Carter\",\"BLS\",\"tr\",\"Kinnersley\"}. 

			InputBasis: The basis the input metric perturbation will be in {\"Carter\",\"BLS\",\"tr\",\"Kinnersley\"}.

"


SchwarzschildQuadraticCovariantSource::usage = "Function for generating d2G, a second-order Schwarzschild source, in the tr covariant manifold. SchwarzschildCovariantSource[Source_,Gauge_:\"Generic\"]

Arguments:
			Source: The type of source {\"d2G\"}.

			Gauge: The Infinitesimal gauge the metric perturbation will be in {\"Generic\", \"ReggeWheeler\"}.

"


(* ::Subsection:: *)
(*Error Messages*)


SchwarzschildSource::argserror = "Argument `1` is unknown"

SchwarzschildSource::argserror2 = "Cannot put `1` in output basis `2`"



SchwarzschildCovariantSource::argserror = "Argument `1` is unknown"


(* ::Section:: *)
(*Begin Private part of package*)


Begin["`Private`"]


(* ::Section:: *)
(*Definitions*)


<<xAct`xTensor`;
<<xAct`xCoba`;
$DefInfoQ=False;
$CVVerbose=False;
$PrePrint=ScreenDollarIndices;

DefManifold[S2,2,{A,B,F,G,H,J,P,Q}];
DefMetric[1,\[CapitalOmega][-A,-B],CDS2,SymbolOfCovD->{"|","D "}];

DefManifold[R2,2,{a,b,c,d,i,j,k,p}];
DefMetric[-1,q[-a,-b],cd,SymbolOfCovD->{"|","\[Delta] "}];

DefManifold[M4,{R2,S2},{\[Alpha],\[Beta],\[Gamma],\[Mu],\[Nu],\[Rho],\[Upsilon],\[Iota],\[Chi]}];
DefProductMetric[g[-\[Alpha],-\[Beta]],{{TangentR2,1},{TangentS2,r[]}},CD,SymbolOfCovD->{";","\[Del]"}];

DefConstantSymbol[M];
DefScalarFunction[f];
DefTensor[r[],R2];
DefTensor[R[],M4,PrintAs->"r"];

$Assumptions=f[R[]]>0;

DefConstantSymbol[l1,PrintAs->"\!\(\*SubscriptBox[\(l\), \(1\)]\)"];
DefConstantSymbol[l2,PrintAs->"\!\(\*SubscriptBox[\(l\), \(2\)]\)"];
DefConstantSymbol[l];
DefConstantSymbol[m1,PrintAs->"\!\(\*SubscriptBox[\(m\), \(1\)]\)"];
DefConstantSymbol[m2,PrintAs->"\!\(\*SubscriptBox[\(m\), \(2\)]\)"];
DefConstantSymbol[m];
DefConstantSymbol[s];


DefConstantSymbol[\[Sigma]];DefConstantSymbol[\[Sigma]p,PrintAs->"\!\(\*SubscriptBox[\(\[Sigma]\), \(+\)]\)"];DefConstantSymbol[\[Sigma]m,PrintAs->"\!\(\*SubscriptBox[\(\[Sigma]\), \(-\)]\)"];

DefTensor[n[-a],{}];
DefTensor[dt[-a],{}];

DefTensor[h[-\[Alpha],-\[Beta]],M4,Symmetric[{-\[Alpha],-\[Beta]}]];
DefTensor[hab[-a,-b],{R2,S2},Symmetric[{-a,-b}],PrintAs->"h"];
DefTensor[haA[-a,-A],{R2,S2},PrintAs->"h"];
DefTensor[hAB[-A,-B],{R2,S2},Symmetric[{-A,-B}],PrintAs->"h"];

DefTensor[hablm[LI[l],LI[\[ScriptM]],a,b],R2,Symmetric[{a,b}],PrintAs->"h"];
DefTensor[htrab[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[FilledCircle]\)]\)"];
DefTensor[hhat[LI[l],LI[\[ScriptM]],a,b],R2,Symmetric[{a,b}],PrintAs->"\!\(\*OverscriptBox[\(h\), \(^\)]\)"];
DefTensor[hap[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(+\)]\)"];
DefTensor[ham[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(-\)]\)"];
DefTensor[htrAB[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[EmptyCircle]\)]\)"];
DefTensor[hp[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(+\)]\)"];
DefTensor[hm[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(-\)]\)"];

DefTensor[hilm[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"h"];
DefTensor[hilmdot[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"\!\(\*OverscriptBox[\(h\), \(.\)]\)"];
DefTensor[hilmddot[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"\!\(\*OverscriptBox[\(h\), \(..\)]\)"];
DefTensor[hilmprime[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SuperscriptBox[\(h\), \(\[Prime]\)]\)"];
DefTensor[hilmprimedot[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SuperscriptBox[OverscriptBox[\(h\), \(.\)], \(\[Prime]\)]\)"];
DefTensor[hilmpprime[-LI[i],-LI[l],-LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SuperscriptBox[\(h\), \(\[Prime]\[Prime]\)]\)"];

DefTensor[hbarablm[LI[l],LI[\[ScriptM]],a,b],R2,Symmetric[{a,b}],PrintAs->"\!\(\*OverscriptBox[\(h\), \(_\)]\)"];
DefTensor[hbartrab[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(h\), \(_\)], \(\[FilledCircle]\)]\)"];
DefTensor[hbartrAB[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(h\), \(_\)], \(\[EmptyCircle]\)]\)"];

DefTensor[hodot[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleDot]\)]\)"];DefTensor[hocross[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleTimes]\)]\)"];DefTensor[hoplus[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CirclePlus]\)]\)"];DefTensor[hominus[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleMinus]\)]\)"];
DefTensor[haodot[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleDot]\)]\)"];DefTensor[haocross[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleTimes]\)]\)"];DefTensor[haoplus[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CirclePlus]\)]\)"];DefTensor[haominus[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[CircleMinus]\)]\)"];
DefTensor[hup[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[FilledUpTriangle]\)]\)"];DefTensor[hdown[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[FilledDownTriangle]\)]\)"];
DefTensor[haup[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[FilledUpTriangle]\)]\)"];DefTensor[hadown[LI[l],LI[\[ScriptM]],a],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\[FilledDownTriangle]\)]\)"];

DefScalarFunction[\[Lambda]1,PrintAs->"\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\)"];DefScalarFunction[\[Lambda]2,PrintAs->"\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\)"];DefScalarFunction[\[Lambda]3,PrintAs->"\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\)"];DefScalarFunction[\[Lambda]4,PrintAs->"\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\)"];

DefConstantSymbol[\[ScriptL]];DefConstantSymbol[\[ScriptP],PrintAs->"\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)"];DefConstantSymbol[\[ScriptQ], PrintAs->"\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\[Prime]\)]\)"];DefConstantSymbol[\[ScriptM]];DefConstantSymbol[\[ScriptM]p,PrintAs->"\!\(\*SuperscriptBox[\(\[ScriptM]\), \(\[Prime]\)]\)"];DefConstantSymbol[\[ScriptM]pp, PrintAs->"\!\(\*SuperscriptBox[\(\[ScriptM]\), \(\[Prime]\[Prime]\)]\)"];DefConstantSymbol[L]; DefConstantSymbol[Lp,PrintAs->"\!\(\*SuperscriptBox[\(L\), \(\[Prime]\)]\)"];
DefTensor[Coupling[LI[\[ScriptQ]],LI[\[ScriptM]pp],LI[spp],-LI[\[ScriptL]],-LI[\[ScriptM]],-LI[s],-LI[\[ScriptP]],-LI[\[ScriptM]p],-LI[sp]],{},PrintAs->"C"];


DefBasis[NP,TangentM4,{1,2,3,4},BasisColor->RGBColor[0,0,1]];


DefTensor[hBS[LI[i],LI[l],LI[m]],M4]
DefTensor[hK[-\[Alpha],-\[Beta]],M4,Symmetric[{-\[Alpha],-\[Beta]}],PrintAs->"\!\(\*SubscriptBox[\(h\), \(K\)]\)"];

DefTensor[htt[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(tt\)]\)"];
DefTensor[htr[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(tr\)]\)"];
DefTensor[hrr[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(rr\)]\)"];
DefTensor[htp[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\(t\)\(+\)\)]\)"];
DefTensor[htm[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\(t\)\(-\)\)]\)"];
DefTensor[hrp[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\(r\)\(+\)\)]\)"];
DefTensor[hrm[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*SubscriptBox[\(h\), \(\(r\)\(-\)\)]\)"];

DefTensor[lNP[a],M4,PrintAs->"l"]
DefTensor[nNP[a],M4,PrintAs->"n"]
DefTensor[mNP[a],M4,Dagger->Complex,PrintAs->"m"]
PrintAs@mNP\[Dagger]^="\!\(\*OverscriptBox[\(m\), \(_\)]\)";


SetDaggerMatrix[NP,{{1,0,0,0},{0,1,0,0},{0,0,0,1},{0,0,1,0}}]
xTensorFormStop[];
FormatBasis[PDNP[{1,-NP}],"D"];
FormatBasis[PDNP[{2,-NP}],"\[CapitalDelta]"];
FormatBasis[PDNP[{3,-NP}],"\[Delta]"];
FormatBasis[PDNP[{4,-NP}],"\!\(\*OverscriptBox[\(\[Delta]\), \(_\)]\)"];
xTensorFormStart[];


DefMetricPerturbation[g,h,\[Epsilon]]


DefTensor[CInt[],M4,PrintAs->"C "];
DefTensor[mu[],M4,PrintAs->"\[Mu] "];


Unprotect[PD];
PD[{0,-BL}][PD[{1,-BL}][a_]]:=PD[{1,-BL}][PD[{0,-BL}][a]];
PD[{0,-BL}][rs[]]=0;
PD[{0,-BL}][mu[__]]=0;
PD[{1,-BL}][mu[__]]=0;
PD[{0,-BL}][R[]]=0;
PD[{1,-BL}][R[]]=1;
PD[{0,-BL}][F]=0;
PD[{0,-BL}][f[R[]]]=0;
Protect[PD];

DefConstantSymbol[\[Omega]]
DefConstantSymbol[\[Omega]1]
DefConstantSymbol[\[Omega]2]


(* ::Section:: *)
(*Frequency domain decomposition*)


FrequencyDomainConversion={PD[{0,-BL}][h[{A_,-NP},{B_,-NP},LI[C_],LI[l1],LI[m1]]]:>-I \[Omega]1 h[{A,-NP},{B,-NP},LI[C],LI[l1],LI[m1]],
PD[{0,-BL}][h[{A_,-NP},{B_,-NP},LI[C_],LI[l2],LI[m2]]]:>-I \[Omega]2 h[{A,-NP},{B,-NP},LI[C],LI[l2],LI[m2]],
PD[{0,-BL}][h[{A_,-NP},{B_,-NP},LI[C_],LI[l],LI[m]]]:>-I \[Omega] h[{A,-NP},{B,-NP},LI[C],LI[l],LI[m]],

PD[{0,-BL}][hBS[LI[C_],LI[l1],LI[m1]]]:>-I \[Omega]1 hBS[LI[C],LI[l1],LI[m1]],
PD[{0,-BL}][hBS[LI[C_],LI[l2],LI[m2]]]:>-I \[Omega]2 hBS[LI[C],LI[l2],LI[m2]],
PD[{0,-BL}][hBS[LI[C_],LI[l],LI[m]]]:>-I \[Omega] hBS[LI[C],LI[l],LI[m]],

PD[{0,-BL}][hK[{A_,-NP},{B_,-NP},LI[C_],LI[l1],LI[m1]]]:>-I \[Omega]1 hK[{A,-NP},{B,-NP},LI[C],LI[l1],LI[m1]],
PD[{0,-BL}][hK[{A_,-NP},{B_,-NP},LI[C_],LI[l2],LI[m2]]]:>-I \[Omega]2 hK[{A,-NP},{B,-NP},LI[C],LI[l2],LI[m2]],
PD[{0,-BL}][hK[{A_,-NP},{B_,-NP},LI[C_],LI[l],LI[m]]]:>-I \[Omega] hK[{A,-NP},{B,-NP},LI[C],LI[l],LI[m]],

PD[{0,-BL}][htt[LI[l1],LI[m1]]]:>-I \[Omega]1 htt[LI[l1],LI[m1]],
PD[{0,-BL}][htt[LI[l2],LI[m2]]]:>-I \[Omega]2 htt[LI[l2],LI[m2]],
PD[{0,-BL}][htt[LI[l],LI[m]]]:>-I \[Omega] htt[LI[l],LI[m]],

PD[{0,-BL}][htr[LI[l1],LI[m1]]]:>-I \[Omega]1 htr[LI[l1],LI[m1]],
PD[{0,-BL}][htr[LI[l2],LI[m2]]]:>-I \[Omega]2 htr[LI[l2],LI[m2]],
PD[{0,-BL}][htr[LI[l],LI[m]]]:>-I \[Omega] htr[LI[l],LI[m]],

PD[{0,-BL}][hrr[LI[l1],LI[m1]]]:>-I \[Omega]1 hrr[LI[l1],LI[m1]],
PD[{0,-BL}][hrr[LI[l2],LI[m2]]]:>-I \[Omega]2 hrr[LI[l2],LI[m2]],
PD[{0,-BL}][hrr[LI[l],LI[m]]]:>-I \[Omega] hrr[LI[l],LI[m]],

PD[{0,-BL}][hrp[LI[l1],LI[m1]]]:>-I \[Omega]1 hrp[LI[l1],LI[m1]],
PD[{0,-BL}][hrp[LI[l2],LI[m2]]]:>-I \[Omega]2 hrp[LI[l2],LI[m2]],
PD[{0,-BL}][hrp[LI[l],LI[m]]]:>-I \[Omega] hrp[LI[l],LI[m]],

PD[{0,-BL}][hrm[LI[l1],LI[m1]]]:>-I \[Omega]1 hrm[LI[l1],LI[m1]],
PD[{0,-BL}][hrm[LI[l2],LI[m2]]]:>-I \[Omega]2 hrm[LI[l2],LI[m2]],
PD[{0,-BL}][hrm[LI[l],LI[m]]]:>-I \[Omega] hrm[LI[l],LI[m]],

PD[{0,-BL}][htp[LI[l1],LI[m1]]]:>-I \[Omega]1 htp[LI[l1],LI[m1]],
PD[{0,-BL}][htp[LI[l2],LI[m2]]]:>-I \[Omega]2 htp[LI[l2],LI[m2]],
PD[{0,-BL}][htp[LI[l],LI[m]]]:>-I \[Omega] htp[LI[l],LI[m]],

PD[{0,-BL}][htm[LI[l1],LI[m1]]]:>-I \[Omega]1 htm[LI[l1],LI[m1]],
PD[{0,-BL}][htm[LI[l2],LI[m2]]]:>-I \[Omega]2 htm[LI[l2],LI[m2]],
PD[{0,-BL}][htm[LI[l],LI[m]]]:>-I \[Omega] htm[LI[l],LI[m]],

PD[{0,-BL}][htrAB[LI[l1],LI[m1]]]:>-I \[Omega]1 htrAB[LI[l1],LI[m1]],
PD[{0,-BL}][htrAB[LI[l2],LI[m2]]]:>-I \[Omega]2 htrAB[LI[l2],LI[m2]],
PD[{0,-BL}][htrAB[LI[l],LI[m]]]:>-I \[Omega] htrAB[LI[l],LI[m]]
}


(* ::Section:: *)
(*Load expressions for dG, dR, d2G, d2R, S0d2G and S4d2G*)


(* ::Subsection:: *)
(*dG*)


Get["dGCarter.wl"];


Get["dGLorenzCarter.wl"];


(* ::Subsection:: *)
(*dR*)


Get["dRCarter.wl"];


Get["dRLorenzCarter.wl"];


(* ::Subsection:: *)
(*d2R*)


Get["d2RCarter.wl"];


Get["d2RLorenzCarter.wl"];


(* ::Subsection:: *)
(*d2G*)


Get["d2GCarter.wl"];


Get["d2GLorenzCarter.wl"];


(* ::Subsection:: *)
(*d2G Vector Harmonics*)


Get["d2GVectorHarmonics.wl"];


(* ::Subsection:: *)
(*Sd2G*)


Get["S0d2GCarter.wl"];


Get["S0d2GLorenzCarter.wl"];


Get["S4d2GCarter.wl"];


Get["S4d2GLorenzCarter.wl"];


(* ::Section:: *)
(*Regge-Wheeler gauge condition rule*)


RWGaugeConditionNPform={h[{3,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{4,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>h[{1,-NP},{3,-NP},LI[-s],LI[l],LI[m]],h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>h[{2,-NP},{4,-NP},LI[-s],LI[l],LI[m]]}

RWGaugeConditionVectorHarmonicdecompform={hap[___]->0,hp[___]->0,hm[___]->0};


(* ::Section:: *)
(*Radiation gauge condition rule*)


OutgoingRadiationGauge={h[{1,-NP},{1,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{1,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>0};
IngoingRadiationGauge={h[{2,-NP},{1,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{2,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{2,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>0};

TraceFreeGauge={h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{3,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>0};


(* ::Section:: *)
(*From Carter to Barack--Lousto--Sago rule and function*)


F=f[R[]]
CarterToBLShRule={h[ {1, -NP}, {1, -NP},LI[0],LI[l_],LI[m_]]:>1/(2R[]f[R[]])(hBS[LI[1],LI[l],LI[m]]+hBS[LI[2],LI[l],LI[m]]),

h[ {2, -NP}, {2, -NP},LI[0],LI[l_],LI[m_]]:>1/(2R[]f[R[]])(hBS[LI[1],LI[l],LI[m]]-hBS[LI[2],LI[l],LI[m]]),

h[ {1, -NP}, {2, -NP},LI[0],LI[l_],LI[m_]]:>1/(2R[])hBS[LI[3],LI[l],LI[m]],

h[ {3, -NP}, {4, -NP},LI[0],LI[l_],LI[m_]]:>1/(2R[])hBS[LI[6],LI[l],LI[m]],

h[ {1, -NP}, {3, -NP},LI[1],LI[l_],LI[m_]]:>-1/(4R[]Sqrt[f[R[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]+hBS[LI[5],LI[l],LI[m]]-I(hBS[LI[8],LI[l],LI[m]]+hBS[LI[9],LI[l],LI[m]])),

h[ {1, -NP}, {4, -NP},LI[-1],LI[l_],LI[m_]]:>1/(4R[]Sqrt[f[R[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]+hBS[LI[5],LI[l],LI[m]]+I(hBS[LI[8],LI[l],LI[m]]+hBS[LI[9],LI[l],LI[m]])),

h[ {2, -NP}, {3, -NP},LI[1],LI[l_],LI[m_]]:>-1/(4R[]Sqrt[f[R[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]-hBS[LI[5],LI[l],LI[m]]-I(hBS[LI[8],LI[l],LI[m]]-hBS[LI[9],LI[l],LI[m]])),

h[ {2, -NP}, {4, -NP},LI[-1],LI[l_],LI[m_]]:>1/(4R[]Sqrt[f[R[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]-hBS[LI[5],LI[l],LI[m]]+I(hBS[LI[8],LI[l],LI[m]]-hBS[LI[9],LI[l],LI[m]])),

h[ {3, -NP}, {3, -NP},LI[2],LI[l_],LI[m_]]:>1/(2R[]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])(hBS[LI[7],LI[l],LI[m]]-I hBS[LI[10],LI[l],LI[m]]),

h[ {4, -NP}, {4, -NP},LI[-2],LI[l_],LI[m_]]:>1/(2R[]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])(hBS[LI[7],LI[l],LI[m]]+I hBS[LI[10],LI[l],LI[m]])};

ail={1/Sqrt[2],1/Sqrt[2],1/Sqrt[2],1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/Sqrt[2],1/(Sqrt[2]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])}

CarterToBLS[x_]:={ail[[1]]/R[](f[R[]] x[[1]] R[]+f[R[]] x[[5]] R[]),

ail[[2]]/R[](f[R[]] x[[1]] R[]-f[R[]] x[[5]] R[]),

ail[[3]]/R[](2 x[[2]] R[]),

ail[[4]]/R[](-Sqrt[f[R[]]] x[[3]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[4]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]-Sqrt[f[R[]]] x[[6]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[7]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]),

ail[[5]]/R[](-Sqrt[f[R[]]] x[[3]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[4]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[6]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]-Sqrt[f[R[]]] x[[7]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]),

ail[[6]]/R[](2 x[[9]] R[]),

ail[[7]]/R[](x[[8]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[2]] R[]+x[[10]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[2]] R[]),

ail[[8]]/R[](-I (Sqrt[f[R[]]] x[[3]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[4]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[6]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[7]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[])),

ail[[9]]/R[](-I (Sqrt[f[R[]]] x[[3]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]+Sqrt[f[R[]]] x[[4]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]-Sqrt[f[R[]]] x[[6]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[]-Sqrt[f[R[]]] x[[7]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] R[])),

ail[[10]]/R[](I (x[[8]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[2]] R[]-x[[10]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[1]] mu[xAct`xTensor`LI[l], -xAct`xTensor`LI[2]] R[]))};


(* ::Section:: *)
(*BLS to tr basis rule and function*)


BLStotrhRule={hBS[LI[1],LI[l_],LI[m_]]:>R[](f[R[]]^2 htt[LI[l],LI[m]]+hrr[LI[l],LI[m]]),
hBS[LI[2],LI[l_],LI[m_]]:>2R[]f[R[]] htr[LI[l],LI[m]],
hBS[LI[3],LI[l_],LI[m_]]:>R[]/f[R[]](f[R[]]^2 htt[LI[l],LI[m]]-hrr[LI[l],LI[m]]),
hBS[LI[4],LI[l_],LI[m_]]:>2 mu[LI[l],-LI[1]]^2 htp[LI[l],LI[m]],
hBS[LI[5],LI[l_],LI[m_]]:>2 mu[LI[l],-LI[1]]^2 f[R[]] hrp[LI[l],LI[m]],
hBS[LI[8],LI[l_],LI[m_]]:>-2 mu[LI[l],-LI[1]]^2 htm[LI[l],LI[m]],
hBS[LI[9],LI[l_],LI[m_]]:>-2 mu[LI[l],-LI[1]]^2 f[R[]] hrm[LI[l],LI[m]],
hBS[LI[6],LI[l_],LI[m_]]:>2/R[] htrAB[LI[l],LI[m]],
hBS[LI[7],LI[l_],LI[m_]]:>mu[LI[l],-LI[1]]^2 mu[LI[l],-LI[2]]^2/R[] hp[LI[l],LI[m]],
hBS[LI[10],LI[l_],LI[m_]]:>-mu[LI[l],-LI[1]]^2 mu[LI[l],-LI[2]]^2/R[] hm[LI[l],LI[m]]};


BLStotr[x_]:={1/Sqrt[2](f[R[]]x[[3]]+x[[1]]),1/(Sqrt[2]f[R[]])x[[2]],1/(Sqrt[2]f[R[]]^2)(-f[R[]]x[[3]]+x[[1]]),R[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[4]],R[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[5]],R[]^2/(Sqrt[2])x[[6]],Sqrt[2]R[]^2/(mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])x[[7]],-R[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[8]],-R[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[9]],-Sqrt[2]R[]^2/(mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])x[[10]]};


(* ::Section:: *)
(*From Carter to Kinnersley tetrad rule and function*)


CarterToKinnersleyhRule={h[{1,-NP},{1,-NP},LI[s_],LI[l_],LI[m_]]:>f[R[]]/2hK[{1,-NP},{1,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{1,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[f[R[]]/2]hK[{1,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[f[R[]]/2]hK[{1,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>2/f[R[]]hK[{2,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[2/f[R[]]]hK[{2,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[2/f[R[]]]hK[{2,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{4,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{4,-NP},{4,-NP},LI[s],LI[l],LI[m]]};

CarterToKinnersley[x_]:={2/f[R[]]x[[1]],x[[2]],Sqrt[2/f[R[]]]x[[3]],Sqrt[2/f[R[]]]x[[4]],f[R[]]/2  x[[5]],Sqrt[f[R[]]/2]x[[6]],Sqrt[f[R[]]/2]x[[7]],x[[8]],x[[9]],x[[10]]}


(* ::Section:: *)
(*Rules for quantities in my notation*)


ftoMrule=f->Function[{r},1-2M/r];

Derivative[1][f][R[]]=2M/R[]^2

lambdatolrule={\[Lambda]1[l_]->Sqrt[(l)(l+1)],\[Lambda]2[l_]->Sqrt[(l-1)(l+2)]};
mutolrule=mu[LI[l_],LI[s_]]:>Sqrt[(l-s)(l+1+s)];
sigmarule={\[Sigma]->(-1)^(\[ScriptQ]+\[ScriptL]+\[ScriptP]),\[Sigma]p->\[Sigma]+1,\[Sigma]m->\[Sigma]-1};


(* ::Section:: *)
(*SchwarzschildSource Function*)


SchwarzschildQuadraticOperator[Source_,Gauge_:"Generic",OutputBasis_:"Carter",InputBasis_:"Carter"]:=Module[{func},



If[!MemberQ[{"d2G","d2R","S4d2G","S0d2G"},Source],Message[SchwarzschildSource::argserror,Source]];
If[!MemberQ[{"Generic","Lorenz","ReggeWheeler","OutgoingRadiationGauge","TraceFreeOutgoingRadiationGauge","IngoingRadiationGauge","TraceFreeIngoingRadiationGauge"},Gauge],Message[SchwarzschildSource::argserror,Gauge]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley"},OutputBasis],Message[SchwarzschildSource::argserror,OutputBasis]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley"},InputBasis],Message[SchwarzschildSource::argserror,InputBasis]];

If[MemberQ[{"S4d2G","S0d2G"},Source]&&MemberQ[{"BLS","trTensor","Kinnersley"},OutputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]];


If[Source=="d2G"&&Gauge=="Generic",func=d2GCarter];
If[Source=="d2G"&&Gauge=="ReggeWheeler",func=d2GCarter/.RWGaugeConditionNPform];
If[Source=="d2G"&&Gauge=="Lorenz",func=d2GLorenzCarter];
If[Source=="d2G"&&Gauge=="OutgoingRadiationGauge",func=d2GCarter/.OutgoingRadiationGauge];
If[Source=="d2G"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=d2GCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="d2G"&&Gauge=="IngoingRadiationGauge",func=d2GCarter/.IngoingRadiationGauge];
If[Source=="d2G"&&Gauge=="TraceFreeIngoingRadiationGauge",func=d2GCarter/.IngoingRadiationGauge/.TraceFreeGauge];

If[Source=="d2R"&&Gauge=="Generic",func=d2RCarter];
If[Source=="d2R"&&Gauge=="ReggeWheeler",func=d2RCarter/.RWGaugeConditionNPform];
If[Source=="d2R"&&Gauge=="Lorenz",func=d2RLorenzCarter];
If[Source=="d2R"&&Gauge=="OutgoingRadiationGauge",func=d2RCarter/.OutgoingRadiationGauge];
If[Source=="d2R"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=d2RCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="d2R"&&Gauge=="IngoingRadiationGauge",func=d2RCarter/.IngoingRadiationGauge];
If[Source=="d2R"&&Gauge=="TraceFreeIngoingRadiationGauge",func=d2RCarter/.IngoingRadiationGauge/.TraceFreeGauge];

If[Source=="S4d2G"&&Gauge=="Generic",func=S4d2GCarter];
If[Source=="S4d2G"&&Gauge=="ReggeWheeler",func=S4d2GCarter/.RWGaugeConditionNPform];
If[Source=="S4d2G"&&Gauge=="Lorenz",func=S4d2GLorenzCarter];
If[Source=="S4d2G"&&Gauge=="OutgoingRadiationGauge",func=S4d2GCarter/.OutgoingRadiationGauge];
If[Source=="S4d2G"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=S4d2GCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="S4d2G"&&Gauge=="IngoingRadiationGauge",func=S4d2GCarter/.IngoingRadiationGauge];
If[Source=="S4d2G"&&Gauge=="TraceFreeIngoingRadiationGauge",func=S4d2GCarter/.IngoingRadiationGauge/.TraceFreeGauge];

If[Source=="S0d2G"&&Gauge=="Generic",func=S0d2GCarter];
If[Source=="S0d2G"&&Gauge=="ReggeWheeler",func=S0d2GCarter/.RWGaugeConditionNPform];
If[Source=="S0d2G"&&Gauge=="Lorenz",func=S0d2GLorenzCarter];
If[Source=="S0d2G"&&Gauge=="OutgoingRadiationGauge",func=S0d2GCarter/.OutgoingRadiationGauge];
If[Source=="S0d2G"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=S0d2GCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="S0d2G"&&Gauge=="IngoingRadiationGauge",func=S0d2GCarter/.IngoingRadiationGauge];
If[Source=="S0d2G"&&Gauge=="TraceFreeIngoingRadiationGauge",func=S0d2GCarter/.IngoingRadiationGauge/.TraceFreeGauge];


If[OutputBasis=="BLS"&&MemberQ[{"d2G","d2R"},Source],func=CarterToBLS[func]];
If[OutputBasis=="trTensor"&&MemberQ[{"d2G","d2R"},Source],func=BLStotr[CarterToBLS[func]]];
If[OutputBasis=="Kinnersley"&&MemberQ[{"d2G","d2R"},Source],func=CarterToKinnersley[func]];

If[InputBasis=="BLS",func=func/.CarterToBLShRule];
If[InputBasis=="trTensor",func=(func/.CarterToBLShRule)/.BLStotrhRule];
If[InputBasis=="Kinnersley",func=func/.CarterToKinnersleyhRule];

Return[func]];




SchwarzschildLinearOperator[Source_,Gauge_:"Generic",OutputBasis_:"Carter",InputBasis_:"Carter"]:=Module[{func},



If[!MemberQ[{"dG","dR"},Source],Message[SchwarzschildSource::argserror,Source]];
If[!MemberQ[{"Generic","Lorenz","ReggeWheeler","OutgoingRadiationGauge","TraceFreeOutgoingRadiationGauge","IngoingRadiationGauge","TraceFreeIngoingRadiationGauge"},Gauge],Message[SchwarzschildSource::argserror,Gauge]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley"},OutputBasis],Message[SchwarzschildSource::argserror,OutputBasis]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley"},InputBasis],Message[SchwarzschildSource::argserror,InputBasis]];



If[Source=="dG"&&Gauge=="Generic",func=dGCarter];
If[Source=="dG"&&Gauge=="ReggeWheeler",func=dGCarter/.RWGaugeConditionNPform];
If[Source=="dG"&&Gauge=="Lorenz",func=dGLorenzCarter];
If[Source=="dG"&&Gauge=="OutgoingRadiationGauge",func=dGCarter/.OutgoingRadiationGauge];
If[Source=="dG"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=dGCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="dG"&&Gauge=="IngoingRadiationGauge",func=dGCarter/.IngoingRadiationGauge];
If[Source=="dG"&&Gauge=="TraceFreeIngoingRadiationGauge",func=dGCarter/.IngoingRadiationGauge/.TraceFreeGauge];

If[Source=="dR"&&Gauge=="Generic",func=dRCarter];
If[Source=="dR"&&Gauge=="ReggeWheeler",func=dRCarter/.RWGaugeConditionNPform];
If[Source=="dR"&&Gauge=="Lorenz",func=dRLorenzCarter];
If[Source=="dR"&&Gauge=="OutgoingRadiationGauge",func=dRCarter/.OutgoingRadiationGauge];
If[Source=="dR"&&Gauge=="TraceFreeOutgoingRadiationGauge",func=dRCarter/.OutgoingRadiationGauge/.TraceFreeGauge];
If[Source=="dR"&&Gauge=="IngoingRadiationGauge",func=dRCarter/.IngoingRadiationGauge];
If[Source=="dR"&&Gauge=="TraceFreeIngoingRadiationGauge",func=dRCarter/.IngoingRadiationGauge/.TraceFreeGauge];


If[OutputBasis=="BLS"&&MemberQ[{"dG","dR"},Source],func=CarterToBLS[func]];
If[OutputBasis=="trTensor"&&MemberQ[{"dG","dR"},Source],func=BLStotr[CarterToBLS[func]]];
If[OutputBasis=="Kinnersley"&&MemberQ[{"dG","dR"},Source],func=CarterToKinnersley[func]];

If[InputBasis=="BLS",func=func/.CarterToBLShRule];
If[InputBasis=="trTensor",func=(func/.CarterToBLShRule)/.BLStotrhRule];
If[InputBasis=="Kinnersley",func=func/.CarterToKinnersleyhRule];

Return[func]];




SchwarzschildQuadraticCovariantSource[Source_,Gauge_:"Generic"]:=Module[{func},

If[!MemberQ[{"d2G"},Source],Message[SchwarzschildCovariantSource::argserror,Source]];
If[!MemberQ[{"Generic","ReggeWheeler"},Gauge],Message[SchwarzschildCovariantSource::argserror,Gauge]];

If[Source=="d2G"&&Gauge=="Generic",func=d2GVectorHarmonics];
If[Source=="d2G"&&Gauge=="ReggeWheeler",func=d2GVectorHarmonics/.RWGaugeConditionVectorHarmonicdecompform];

Return[func]];


(* ::Section:: *)
(*End Package*)


End[];
Protect @@ Names["SchwarzschildPerturbations`*"];
EndPackage[];




