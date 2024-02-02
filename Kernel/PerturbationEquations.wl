(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


xAct`HelloWorldxTension`$xTensorVersionExpected={"1.2.0",{2021,10,17}};
xAct`PerturbationEquations`$Version={"0.2.0",{2023,10,11}}


With[{xAct`PerturbationEquations`Private`PerturbationEquationsSymbols=DeleteCases[Join[Names["xAct`PerturbationEquations`*"],Names["xAct`PerturbationEquations`Private`*"]],"$Version"|"xAct`PerturbationEquations`$Version"|"$PerturbationEquationsVersionExpected"|"xAct`PerturbationEquations`$xTensorVersionExpected"]},
Unprotect/@xAct`PerturbationEquations`Private`PerturbationEquationsSymbols;
Clear/@xAct`PerturbationEquations`Private`PerturbationEquationsSymbols;
]


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`PerturbationEquations`"];


BeginPackage["xAct`PerturbationEquations`", {"xAct`xCoba`","xAct`xTensor`","xAct`xPerm`","xAct`xCore`"}]


If[Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],Throw@Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected]]


Print[xAct`xCore`Private`bars];
Print["Package xAct`PerturbationEquations`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2023, Andrew Spiers, Adam Pound and Barry Wardell."];


Off[General::shdw]
xAct`PerturbationEquations`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`PerturbationEquations`",
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


ReportSet[$PrePrint,ScreenDollarIndices];
ReportSet[$DefInfoQ,False];
ReportSet[$CVVerbose,False];


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Rules*)


CarterToBLShRule::usage = "Rule that expresses Carter tetrad modes of the metric perturbation in terms of Barack-Lousto-Sago modes.";
CarterToBLS::usage = "Rule that takes a quantity expressed in terms of its Carter output modes, and reexpresses the quantity in terms of its the Barack-Lousto-Sago output modes. The resulting list is the BLS mode number from 1 to 10.";


BLStotrhRule::usage = "Rule that expresses Barack-Lousto-Sago modes of the metric perturbation in terms of trTensor basis modes.";
BLStotr::usage = "Rule that takes a quantity expressed in terms of its Barack-Lousto-Sago output modes, and reexpresses the quantity in terms of its tr output modes. The resulting list is the tr mode number in the following order {\!\(\*SubscriptBox[\(S\), \(tt\)]\),\!\(\*SubscriptBox[\(S\), \(tr\)]\),\!\(\*SubscriptBox[\(S\), \(rr\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\[EmptyCircle]\)]\),\!\(\*SubscriptBox[\(S\), \(+\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(-\)]\)}.";

CarterTotrhRule::usage = "Rule that expresses Carter tetrad modes of the metric perturbation in terms of trTensor basis modes.";
CarterTotr::usage = "Rule that takes a quantity expressed in terms of its Carter output modes and reexpresses the quantity in terms of its tr output modes. The resulting list is the tr mode number in the following order {\!\(\*SubscriptBox[\(S\), \(tt\)]\),\!\(\*SubscriptBox[\(S\), \(tr\)]\),\!\(\*SubscriptBox[\(S\), \(rr\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(+\)\)]\),\!\(\*SubscriptBox[\(S\), \(\[EmptyCircle]\)]\),\!\(\*SubscriptBox[\(S\), \(+\)]\),\!\(\*SubscriptBox[\(S\), \(\(t\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(\(r\)\(-\)\)]\),\!\(\*SubscriptBox[\(S\), \(-\)]\)}.";


CarterToKinnersleyhRule::usage = "Rule that expresses Carter tetrad modes of the metric perturbation in terms of the Kinnersley tetrad modes.";
CarterToKinnersley::usage = "Rule that takes a quantity expressed in terms of its Carter output modes, and reexpresses the quantity in terms of Kinnersley tetrad output modes.";


CarterToHartleHawkinghRule::usage = "Rule that expresses Carter tetrad modes of the metric perturbation in terms of the Hartle--Hawking tetrad modes.";
CarterToHartleHawkingKinnersley::usage = "Rule that takes a quantity expressed in terms of its Carter output modes, and reexpresses the quantity in terms of Hartle--Hawking tetrad output modes."


FrequencyDomainConversion::usage = "Rule for converting t derivatives to frequency domain. Assumes the time dependence is Exp[-i\[Omega]t].";


RWGaugeConditionNPform::usage = "Rule that imposes the Regge-Wheeler gauge in the Carter tetrad.";
RWGaugeConditionVectorHarmonicdecompform::usage = "Rule that imposes the Regge-Wheeler gauge in the vector harmonic decomposition.";


ftoMrule::usage = "Rule for f(r)=1-2M/r.";
mutolrule::usage = "Rule for \[Mu]=\!\(\*SqrtBox[\(\((l + 1 - s)\) \((l + s)\)\)]\).";
lambdatolrule::usage = "Rule for \[Lambda]1=\!\(\*SqrtBox[\(l \((l + 1)\)\)]\) and \[Lambda]2=\!\(\*SqrtBox[\(\((l - 1)\) \((l + 2)\)\)]\).";
sigmarule::usage = "Rule for \[Sigma]=(-1)^(\[ScriptQ]+\[ScriptL]+\[ScriptP]).";




(* ::Subsection:: *)
(*Symbols*)


(* ::Subsubsection:: *)
(*Basis*)


Kinnersley::usage="The Kinnersley tetrad basis. The ordering of the output terms is {ll,ln,lm,l\!\(\*OverscriptBox[\(m\), \(_\)]\),nn,nm,n\!\(\*OverscriptBox[\(m\), \(_\)]\),mm,m\!\(\*OverscriptBox[\(m\), \(_\)]\),\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)}=={11,12,13,14,22,23,24,33,34,44}."
Carter::usage="The Carter tetrad basis. The ordering of the output terms is {ll,ln,lm,l\!\(\*OverscriptBox[\(m\), \(_\)]\),nn,nm,n\!\(\*OverscriptBox[\(m\), \(_\)]\),mm,m\!\(\*OverscriptBox[\(m\), \(_\)]\),\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)}=={11,12,13,14,22,23,24,33,34,44}."
BLS::usage="The Barack--Lousto--Sago basis."
trTensor::usage="The Boyer--Lindquist coordinate t r basis with the angular dependence expressed using scalar, vector and tenser spherical harmonics.  
The ordering of the output terms is {\!\(\*SubscriptBox[\(h\), \(tt\)]\),\!\(\*SubscriptBox[\(h\), \(tr\)]\),\!\(\*SubscriptBox[\(h\), \(rr\)]\),\!\(\*SubscriptBox[\(h\), \(\(t\)\(+\)\)]\),\!\(\*SubscriptBox[\(h\), \(\(r\)\(+\)\)]\),\!\(\*SubscriptBox[\(h\), \(\[EmptyCircle]\)]\),\!\(\*SubscriptBox[\(h\), \(p\)]\),\!\(\*SubscriptBox[\(h\), \(\(t\)\(-\)\)]\),\!\(\*SubscriptBox[\(h\), \(rm\)]\),\!\(\*SubscriptBox[\(h\), \(-\)]\)}"



(* ::Subsubsection:: *)
(*Gauges*)


Lorenz::usage="The Lorenz gauge, \!\(\*SuperscriptBox[\(\[Del]\), \(a\)]\)\!\(\*SubscriptBox[OverscriptBox[\(h\), \(_\)], \(ab\)]\)=0, where \!\(\*SubscriptBox[OverscriptBox[\(h\), \(_\)], \(ab\)]\)=\!\(\*SubscriptBox[\(h\), \(ab\)]\)-\!\(\*FractionBox[\(1\), \(2\)]\)\!\(\*SuperscriptBox[\(g\), \(ab\)]\)\!\(\*SubscriptBox[\(h\), \(ab\)]\)."
ReggeWheeler::usage="The Regge--Wheeler gauge, {\!\(\*SubscriptBox[\(h\), \(+\)]\)==0,\!\(\*SubscriptBox[\(h\), \(-\)]\)==0,\!\(\*SuperscriptBox[SubscriptBox[\(h\), \(+\)], \(a\)]\)==0}."
IngoingRadiationGauge::usage="The Ingoing Radiation Gauge, \!\(\*SubscriptBox[\(h\), \(la\)]\)=0."
TraceFreeIngoingRadiationGauge::usage="The Trace free Ingoing Radiation Gauge, \!\(\*SubscriptBox[\(h\), \(la\)]\)=0=\!\(\*SubscriptBox[\(h\), \(m \*OverscriptBox[\(m\), \(_\)]\)]\)."
OutgoingRadiationGauge::usage="The Outgoing Radiation Gauge, \!\(\*SubscriptBox[\(h\), \(na\)]\)=0."
TraceFreeOutgoingRadiationGauge::usage="The Trace free Outgoing Radiation Gauge, \!\(\*SubscriptBox[\(h\), \(na\)]\)=0=\!\(\*SubscriptBox[\(h\), \(m \*OverscriptBox[\(m\), \(_\)]\)]\)."


(* ::Subsubsection:: *)
(*Coupling coefficients*)


CInt::usage = "CInt[LI[l], LI[m], LI[s], -LI[l'], -LI[m'], -LI[s'], -LI[l''], -LI[m''], -LI[s'']]=\!\(\*SubscriptBox[\(\[Integral]\), \(S\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(Y\), \(_\)], \(lm\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s'\)]\)\!\(\*SuperscriptBox[\(Y\), \(l' m'\)]\)\!\(\*SubscriptBox[\(\\\ \), \(s''\)]\)\!\(\*SuperscriptBox[\(Y\), \(l'' m''\)]\) \[DifferentialD]\[CapitalOmega]";


CIntrule::usage = "Converts CInt to ThreeJSymbols: {SchwarzschildPerturbations`CInt[xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`l, Blank[]]], xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`m, Blank[]]], xAct`xTensor`LI[Pattern[s, Blank[]]], -xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`l1, Blank[]]], -xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`m1, Blank[]]], -xAct`xTensor`LI[Pattern[s1, Blank[]]], -xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`l2, Blank[]]], -xAct`xTensor`LI[Pattern[SchwarzschildPerturbations`m2, Blank[]]], -xAct`xTensor`LI[Pattern[s2, Blank[]]]]\[RuleDelayed](-1\!\(\*SuperscriptBox[\()\), \(\*InterpretationBox[
StyleBox[\"m\",\nShowAutoStyles->False,\nAutoSpacing->False],
SchwarzschildPerturbations`m,\nEditable->False] + s\)]\) \!\(\*SqrtBox[FractionBox[
RowBox[{
RowBox[{\"(\", 
RowBox[{
RowBox[{\"2\", \" \", InterpretationBox[
StyleBox[\"l\",\nShowAutoStyles->False,\nAutoSpacing->False],
SchwarzschildPerturbations`l,\nEditable->False]}], \"+\", \"1\"}], \")\"}], \" \", 
RowBox[{\"(\", 
RowBox[{
RowBox[{\"2\", \" \", InterpretationBox[
StyleBox[SubscriptBox[\"l\", \"1\"],\nShowAutoStyles->False,\nAutoSpacing->False],
SchwarzschildPerturbations`l1,\nEditable->False]}], \"+\", \"1\"}], \")\"}], \" \", 
RowBox[{\"(\", 
RowBox[{
RowBox[{\"2\", \" \", InterpretationBox[
StyleBox[SubscriptBox[\"l\", \"2\"],\nShowAutoStyles->False,\nAutoSpacing->False],
SchwarzschildPerturbations`l2,\nEditable->False]}], \"+\", \"1\"}], \")\"}]}], 
RowBox[{\"4\", \" \", \"\[Pi]\"}]]]\) ThreeJSymbol[{SchwarzschildPerturbations`l,s},{SchwarzschildPerturbations`l1,-s1},{SchwarzschildPerturbations`l2,-s2}] ThreeJSymbol[{SchwarzschildPerturbations`l,-SchwarzschildPerturbations`m},{SchwarzschildPerturbations`l1,SchwarzschildPerturbations`m1},{SchwarzschildPerturbations`l2,SchwarzschildPerturbations`m2}]}";


mu::usage = "\[Mu]=\!\(\*SqrtBox[\(\((l + 1 - s)\) \((l + s)\)\)]\)";


lmReplacerule::usage = "lmReplaceRule[func_,ld_,md_,l1d_,m1d_,l2d_,m2d_]:=func/.l->ld/.m->md/.l1->l1d/.m1->m1d/.l2->l2d/.m2->m2d/.CIntRule/.mutolrule. 

Replaces l, m, \!\(\*SubscriptBox[\(l\), \(1\)]\), \!\(\*SubscriptBox[\(m\), \(1\)]\), \!\(\*SubscriptBox[\(l\), \(2\)]\), \!\(\*SubscriptBox[\(m\), \(2\)]\) with ld, md, l1d, m1d, l2d, m2d respectivly. Note, CIntRule is applied before mutolrule to avoid spurious Indeterminate quantities";


\[Sigma]::usage = "\[Sigma] = (-1)^(\[ScriptQ]+\[ScriptL]+\[ScriptP])";
\[Sigma]m::usage = "\[Sigma]-1";
\[Sigma]p::usage = "\[Sigma]+1";


(* ::Subsubsection:: *)
(*Coordinates*)


BL::usage ="Boyer-Lindquist coordinates.";
NP::usage ="Newman-Penrose tetrad basis.";


r::usage = "Boyer\[Dash]Lindquist radial coordinate.";
t::usage = "Boyer\[Dash]Lindquist time coordinate.";
f::usage = "Schwarzschild's function, f(r)=1-2M/r.";
M::usage = "Mass.";


(* ::Subsubsection:: *)
(*Mode numbers*)


l::usage ="Angular number l.";
l1::usage ="Angular number l1.";
l2::usage ="Angular number l2.";


m::usage ="Magnetic number m.";
m1::usage ="Magnetic number m1.";
m2::usage ="Magnetic number m2.";


\[Omega]::usage ="Frequency \[Omega].";
\[Omega]1::usage ="Frequency \[Omega]1.";
\[Omega]2::usage ="Frequency \[Omega]2.";


(* ::Subsubsection:: *)
(*Metric perturbations*)


h::usage ="Metric perturbation.";


hBS::usage ="Metric perturbation in the Barack-Sago basis.";


hK::usage ="Metric perturbation in the Kinnersley basis.";


hab::usage ="Metric perturbation in t-r basis.";
haA::usage ="Metric perturbation in t-r and angular basis.";
hAB::usage ="Metric perturbation in angular basis.";


hablm::usage ="Modes of metric perturbation in t-r basis.";
htrab::usage ="Trace of metric perturbation in t-r basis.";
hap::usage ="Metric perturbation in t-r basis plus angular part.";
ham::usage ="Metric perturbation in t-r basis minus angular part.";
htrAB::usage ="Trace of metric perturbation in angular basis";
hp::usage ="Metric perturbation plus angular part.";
hm::usage ="Metric perturbation minus angular part.";


htt::usage ="tt component of the metric perturbation.";
htr::usage ="tr component of the metric perturbation.";
hrr::usage ="rr component of the metric perturbation.";
htp::usage ="t component of the plus angular part of the metric perturbation.";
hrp::usage ="r component of the plus angular part of the metric perturbation.";
htm::usage ="t component of the minus angular part of the metric perturbation.";
hrm::usage ="r component of the minus angular part of the metric perturbation.";


(* ::Subsection:: *)
(*Master Teukolsky Variables*)


psiMasterplus2::usage ="spin +2 Master Teukolsky variable";
psiMasterminus2::usage ="spin -2 Master Teukolsky variable";


(* ::Subsection:: *)
(*Functions*)


SchwarzschildQuadraticOperator::usage = "SchwarzschildQuadraticOperator[source, gauge, output, input] generates decomposed, Boyer\[Dash]Lindquist coordinate form, quadratic operators for second-order sources in Schwarzschild spacetime.";


SchwarzschildLinearOperator::usage = "SchwarzschildLinearOperator[source, gauge, output, input] generates decomposed, Boyer\[Dash]Lindquist coordinate form, linear operators in Schwarzschild spacetime.";


SchwarzschildQuadraticCovariantSource::usage = "SchwarzschildQuadraticCovariantSource[source, gauge] generates d2G, a second-order source in Schwarzschild spacetime, in the t-r covariant manifold.";


(* ::Subsection:: *)
(*Error Messages*)


SchwarzschildSource::argserror = "Argument `1` is unknown";
SchwarzschildSource::argserror2 = "Cannot put `1` in output basis `2`";
SchwarzschildCovariantSource::argserror = "Argument `1` is unknown";


(* ::Section:: *)
(*Definitions*)


DefManifold[S2,2,{A,B,F,G,H,J,P,Q}];
DefMetric[1,\[CapitalOmega][-A,-B],CDS2,SymbolOfCovD->{"|","D "}];

DefManifold[R2,2,{a,b,c,d,i,j,k,p}];
DefMetric[-1,q[-a,-b],cd,SymbolOfCovD->{"|","\[Delta] "}];

DefManifold[M4,{R2,S2},{\[Alpha],\[Beta],\[Gamma],\[Mu],\[Nu],\[Rho],\[Upsilon],\[Iota],\[Chi]}];
DefProductMetric[g[-\[Alpha],-\[Beta]],{{TangentR2,1},{TangentS2,r[]}},CD,SymbolOfCovD->{";","\[Del]"}];

DefConstantSymbol[M];
DefScalarFunction[f];
(*DefTensor[r[],M4];*)

DefChart[BL,R2,{0,1},{t[],r[]}];

MetricInBasis[q,-BL,{{-f[r[]],0},{0,1/f[r[]]}}];

$Assumptions=f[r[]]>0;

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

ComponentValue[ComponentArray[n[{-a,BL}]],{0,1}];
ComponentValue[ComponentArray[dt[{-a,BL}]],{1,0}];
ComponentValue[ComponentArray[n[{a,BL}]],{0,f[r[]]}];

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


DefTensor[CInt[LI[l],LI[m],LI[s],-LI[l1],-LI[m1],-LI[s1],-LI[l2],-LI[m2],-LI[s2]],M4,PrintAs->"C"];
DefTensor[mu[LI[l],-LI[s]],M4,PrintAs->"\[Mu]"];


Unprotect[PD];
PD[{0,-BL}][PD[{1,-BL}][a_]]:=PD[{1,-BL}][PD[{0,-BL}][a]];
PD[{0,-BL}][rs[]]=0;
PD[{0,-BL}][mu[__]]=0;
PD[{1,-BL}][mu[__]]=0;
PD[{0,-BL}][r[]]=0;
PD[{1,-BL}][r[]]=1;
PD[{0,-BL}][F]=0;
PD[{0,-BL}][f[r[]]]=0;
Protect[PD];

DefConstantSymbol[\[Omega]]
DefConstantSymbol[\[Omega]1,PrintAs->"\!\(\*SubscriptBox[\(\[Omega]\), \(1\)]\)"]
DefConstantSymbol[\[Omega]2,PrintAs->"\!\(\*SubscriptBox[\(\[Omega]\), \(2\)]\)"]


DefTensor[psiMasterplus2[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*
StyleBox[AdjustmentBox[\"2\",\nBoxBaselineShift->0.6276432914790547,\nBoxMargins->{{0., 0.}, {-0.6276432914790547, 0.6276432914790547}}],\nFontSize->9]\)\[Psi]"];
DefTensor[psiMasterminus2[LI[l],LI[\[ScriptM]]],R2,PrintAs->"\!\(\*
StyleBox[AdjustmentBox[\"-2\",\nBoxBaselineShift->0.6276432914790547,\nBoxMargins->{{0., 0.}, {-0.6276432914790547, 0.6276432914790547}}],\nFontSize->9]\)\[Psi]"];


(* ::Section:: *)
(*Begin Private part of package*)


Begin["`Private`"]


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


(* ::Subsection::Closed:: *)
(*dG*)


Get["xAct`PerturbationEquations`dGCarter`"];


Get["xAct`PerturbationEquations`dGLorenzCarter`"];


(* ::Subsection::Closed:: *)
(*dR*)


Get["xAct`PerturbationEquations`dRCarter`"];


Get["xAct`PerturbationEquations`dRLorenzCarter`"];


(* ::Subsection::Closed:: *)
(*d2R*)


Get["xAct`PerturbationEquations`d2RCarter`"];


Get["xAct`PerturbationEquations`d2RLorenzCarter`"];


(* ::Subsection::Closed:: *)
(*d2G*)


Get["xAct`PerturbationEquations`d2GCarter`"];


Get["xAct`PerturbationEquations`d2GLorenzCarter`"];


(* ::Subsection::Closed:: *)
(*d2G Vector Harmonics*)


Get["xAct`PerturbationEquations`d2GVectorHarmonics`"];


(* ::Subsection::Closed:: *)
(*Sd2G*)


Get["xAct`PerturbationEquations`S0d2GCarter`"];


Get["xAct`PerturbationEquations`S0d2GLorenzCarter`"];


Get["xAct`PerturbationEquations`S4d2GCarter`"];


Get["xAct`PerturbationEquations`S4d2GLorenzCarter`"];


(* ::Subsection:: *)
(*Master Teukolsky*)


MasterTeukolskyplus2=(psiMasterplus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]] (4 I xAct`PerturbationEquations`M xAct`PerturbationEquations`\[Omega]-xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] (-6+xAct`PerturbationEquations`l+xAct`PerturbationEquations`l^2-8 I xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[])+xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[] (-4 I+xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[])))/xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]]+6 (xAct`PerturbationEquations`M+xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] xAct`PerturbationEquations`r[]) xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][psiMasterplus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]]]+xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] xAct`PerturbationEquations`r[]^2 xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][psiMasterplus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]]]];

MasterTeukolskyminus2=(psiMasterminus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]] (-4 I xAct`PerturbationEquations`M xAct`PerturbationEquations`\[Omega]-xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] (-2+xAct`PerturbationEquations`l+xAct`PerturbationEquations`l^2+8 I xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[])+xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[] (4 I+xAct`PerturbationEquations`\[Omega] xAct`PerturbationEquations`r[])))/xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]]-2 (xAct`PerturbationEquations`M+xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] xAct`PerturbationEquations`r[]) xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][psiMasterminus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]]]+xAct`PerturbationEquations`f[xAct`PerturbationEquations`r[]] xAct`PerturbationEquations`r[]^2 xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][xAct`xTensor`PD[{1, -xAct`PerturbationEquations`BL}][psiMasterminus2[xAct`xTensor`LI[xAct`PerturbationEquations`l], xAct`xTensor`LI[xAct`PerturbationEquations`\[ScriptM]]]]];


(* ::Section::Closed:: *)
(*Regge-Wheeler gauge condition rule*)


RWGaugeConditionNPform={h[{3,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{4,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>0,h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>h[{1,-NP},{3,-NP},LI[-s],LI[l],LI[m]],h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>h[{2,-NP},{3,-NP},LI[-s],LI[l],LI[m]]}

RWGaugeConditionVectorHarmonicdecompform={(hap|htp|hrp|hp|hm)[___]->0};


(* ::Section::Closed:: *)
(*Radiation gauge condition rule*)


IngoingRadiationGauge={h[{1,-NP}, _, LI[s_], LI[l_], LI[m_]] -> 0, h[_, {1,-NP}, LI[s_], LI[l_], LI[m_]] -> 0};
OutgoingRadiationGauge={h[{2,-NP}, _, LI[s_], LI[l_], LI[m_]] -> 0, h[_, {2,-NP}, LI[s_], LI[l_], LI[m_]] -> 0};

TraceFreeGauge={h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]] -> 0, h[{3,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]] -> 0};


(* ::Section::Closed:: *)
(*From Carter to Barack--Lousto--Sago rule and function*)


(*Note hBS[LI[i],LI[l],LI[m]] denote the coefficients hilm in h = sum_{ilm} (a/r)hilm Yilm, not the coefficients hbar_{ilm} in the analogous expansion of hbar.*)


F=f[r[]]
CarterToBLShRule={h[ {1, -NP}, {1, -NP},LI[0],LI[l_],LI[m_]]:>1/(2r[]f[r[]])(hBS[LI[1],LI[l],LI[m]]+hBS[LI[2],LI[l],LI[m]]),

h[ {2, -NP}, {2, -NP},LI[0],LI[l_],LI[m_]]:>1/(2r[]f[r[]])(hBS[LI[1],LI[l],LI[m]]-hBS[LI[2],LI[l],LI[m]]),

h[ {1, -NP}, {2, -NP},LI[0],LI[l_],LI[m_]]:>1/(2r[])hBS[LI[3],LI[l],LI[m]],

h[ {3, -NP}, {4, -NP},LI[0],LI[l_],LI[m_]]:>1/(2r[])hBS[LI[6],LI[l],LI[m]],

h[ {1, -NP}, {3, -NP},LI[1],LI[l_],LI[m_]]:>-1/(4r[]Sqrt[f[r[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]+hBS[LI[5],LI[l],LI[m]]-I(hBS[LI[8],LI[l],LI[m]]+hBS[LI[9],LI[l],LI[m]])),

h[ {1, -NP}, {4, -NP},LI[-1],LI[l_],LI[m_]]:>1/(4r[]Sqrt[f[r[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]+hBS[LI[5],LI[l],LI[m]]+I(hBS[LI[8],LI[l],LI[m]]+hBS[LI[9],LI[l],LI[m]])),

h[ {2, -NP}, {3, -NP},LI[1],LI[l_],LI[m_]]:>-1/(4r[]Sqrt[f[r[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]-hBS[LI[5],LI[l],LI[m]]-I(hBS[LI[8],LI[l],LI[m]]-hBS[LI[9],LI[l],LI[m]])),

h[ {2, -NP}, {4, -NP},LI[-1],LI[l_],LI[m_]]:>1/(4r[]Sqrt[f[r[]]]mu[LI[l],-LI[1]])(hBS[LI[4],LI[l],LI[m]]-hBS[LI[5],LI[l],LI[m]]+I(hBS[LI[8],LI[l],LI[m]]-hBS[LI[9],LI[l],LI[m]])),

h[ {3, -NP}, {3, -NP},LI[2],LI[l_],LI[m_]]:>1/(2r[]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])(hBS[LI[7],LI[l],LI[m]]-I hBS[LI[10],LI[l],LI[m]]),

h[ {4, -NP}, {4, -NP},LI[-2],LI[l_],LI[m_]]:>1/(2r[]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])(hBS[LI[7],LI[l],LI[m]]+I hBS[LI[10],LI[l],LI[m]])};

ail={1/Sqrt[2],1/Sqrt[2],1/Sqrt[2],1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/Sqrt[2],1/(Sqrt[2]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]),1/(Sqrt[2]mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])}

CarterToBLS[x_]:={ail[[1]]/r[](f[r[]] x[[1]] r[]+f[r[]] x[[5]] r[]),

ail[[2]]/r[](f[r[]] x[[1]] r[]-f[r[]] x[[5]] r[]),

ail[[3]]/r[](2 x[[2]] r[]),

ail[[4]]/r[](-Sqrt[f[r[]]] x[[3]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[4]] mu[LI[l], -LI[1]] r[]-Sqrt[f[r[]]] x[[6]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[7]] mu[LI[l], -LI[1]] r[]),

ail[[5]]/r[](-Sqrt[f[r[]]] x[[3]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[4]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[6]] mu[LI[l], -LI[1]] r[]-Sqrt[f[r[]]] x[[7]] mu[LI[l], -LI[1]] r[]),

ail[[6]]/r[](2 x[[9]] r[]),

ail[[7]]/r[](x[[8]] mu[LI[l], -LI[1]] mu[LI[l], -LI[2]] r[]+x[[10]] mu[LI[l], -LI[1]] mu[LI[l], -LI[2]] r[]),

ail[[8]]/r[](-I (Sqrt[f[r[]]] x[[3]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[4]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[6]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[7]] mu[LI[l], -LI[1]] r[])),

ail[[9]]/r[](-I (Sqrt[f[r[]]] x[[3]] mu[LI[l], -LI[1]] r[]+Sqrt[f[r[]]] x[[4]] mu[LI[l], -LI[1]] r[]-Sqrt[f[r[]]] x[[6]] mu[LI[l], -LI[1]] r[]-Sqrt[f[r[]]] x[[7]] mu[LI[l], -LI[1]] r[])),

ail[[10]]/r[](I (x[[8]] mu[LI[l], -LI[1]] mu[LI[l], -LI[2]] r[]-x[[10]] mu[LI[l], -LI[1]] mu[LI[l], -LI[2]] r[]))};
(*Input order for CarterToBLS: "ll","ln","lm","lmbar","nn","nm","nmbar","mm","mmbar","mbarmbar"*)


(* ::Section::Closed:: *)
(*BLS to t-r basis rule and function*)


BLStotrhRule={hBS[LI[1],LI[l_],LI[m_]]:>r[](f[r[]]^2 htt[LI[l],LI[m]]+hrr[LI[l],LI[m]]),
hBS[LI[2],LI[l_],LI[m_]]:>2r[]f[r[]] htr[LI[l],LI[m]],
hBS[LI[3],LI[l_],LI[m_]]:>r[]/f[r[]](f[r[]]^2 htt[LI[l],LI[m]]-hrr[LI[l],LI[m]]),
hBS[LI[4],LI[l_],LI[m_]]:>2 mu[LI[l],-LI[1]]^2 htp[LI[l],LI[m]],
hBS[LI[5],LI[l_],LI[m_]]:>2 mu[LI[l],-LI[1]]^2 f[r[]] hrp[LI[l],LI[m]],
hBS[LI[8],LI[l_],LI[m_]]:>-2 mu[LI[l],-LI[1]]^2 htm[LI[l],LI[m]],
hBS[LI[9],LI[l_],LI[m_]]:>-2 mu[LI[l],-LI[1]]^2 f[r[]] hrm[LI[l],LI[m]],
hBS[LI[6],LI[l_],LI[m_]]:>2/r[] htrAB[LI[l],LI[m]],
hBS[LI[7],LI[l_],LI[m_]]:>mu[LI[l],-LI[1]]^2 mu[LI[l],-LI[2]]^2/r[] hp[LI[l],LI[m]],
hBS[LI[10],LI[l_],LI[m_]]:>-mu[LI[l],-LI[1]]^2 mu[LI[l],-LI[2]]^2/r[] hm[LI[l],LI[m]]};


BLStotr[x_]:={1/Sqrt[2](f[r[]]x[[3]]+x[[1]]),1/(Sqrt[2]f[r[]])x[[2]],1/(Sqrt[2]f[r[]]^2)(-f[r[]]x[[3]]+x[[1]]),r[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[4]],r[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[5]],r[]^2/(Sqrt[2])x[[6]],Sqrt[2]r[]^2/(mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])x[[7]],-r[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[8]],-r[]/(Sqrt[2]mu[LI[l],-LI[1]])x[[9]],-Sqrt[2]r[]^2/(mu[LI[l],-LI[1]]mu[LI[l],-LI[2]])x[[10]]};


(* ::Section::Closed:: *)
(*Carter tetrad to t-r basis*)


CarterTotrhRule={h[{1, -NP}, {1, -NP}, LI[0], LI[l_], LI[m_]] :> hrr[LI[l], LI[m]]f[r[]]/2 + htr[LI[l], LI[m]] + htt[LI[l], LI[m]]/(2f[r[]]), 
h[{2, -NP}, {2, -NP}, LI[0], LI[l_], LI[m_]] :> hrr[LI[l], LI[m]]f[r[]]/2 - htr[LI[l], LI[m]] + htt[LI[l], LI[m]]/(2f[r[]]), 
h[{1, -NP}, {2, -NP}, LI[0], LI[l_], LI[m_]] :> -hrr[LI[l], LI[m]]f[r[]]/2 + htt[LI[l], LI[m]]/(2f[r[]]), 
h[{3, -NP}, {4, -NP}, LI[0], LI[l_], LI[m_]] :> htrAB[LI[l], LI[m]]/r[]^2,
h[{1, -NP}, {3, -NP}, LI[1], LI[l_], LI[m_]] :> -((f[r[]]*(I*hrm[LI[l], LI[m]] + hrp[LI[l], LI[m]]) + I*htm[LI[l], LI[m]] + htp[LI[l], LI[m]])*mu[LI[l], -LI[1]])/(2*Sqrt[f[r[]]]*r[]), 
h[{1, -NP}, {4, -NP}, LI[-1], LI[l_], LI[m_]] :> ((f[r[]]*((-I)*hrm[LI[l], LI[m]] + hrp[LI[l], LI[m]]) - I*htm[LI[l], LI[m]] + htp[LI[l], LI[m]])*mu[LI[l], -LI[1]])/(2*Sqrt[f[r[]]]*r[]),
h[{2, -NP}, {3, -NP}, LI[1], LI[l_], LI[m_]] :> -((f[r[]]*(-I*hrm[LI[l], LI[m]] - hrp[LI[l], LI[m]]) + I*htm[LI[l], LI[m]] + htp[LI[l], LI[m]])*mu[LI[l], -LI[1]])/(2*Sqrt[f[r[]]]*r[]), 
h[{2, -NP}, {4, -NP}, LI[-1], LI[l_], LI[m_]] :> ((f[r[]]*(I*hrm[LI[l], LI[m]] - hrp[LI[l], LI[m]]) - I*htm[LI[l], LI[m]] + htp[LI[l], LI[m]])*mu[LI[l], -LI[1]])/(2*Sqrt[f[r[]]]*r[]),
h[{3, -NP}, {3, -NP}, LI[2], LI[l_], LI[m_]] :> ((I*hm[LI[l], LI[m]] + hp[LI[l], LI[m]])*mu[LI[l], -LI[1]]*mu[LI[l], -LI[2]])/(2*r[]^2), 
h[{4, -NP}, {4, -NP}, LI[-2], LI[l_], LI[m_]] :> (((-I)*hm[LI[l], LI[m]] + hp[LI[l], LI[m]])*mu[LI[l], -LI[1]]*mu[LI[l], -LI[2]])/(2*r[]^2)}


CarterTotr[x_]:={(f[r[]]*(x[[1]] + 2*x[[2]] + x[[5]]))/2,
(x[[1]] - x[[5]])/2,
(x[[1]] - 2*x[[2]] + x[[5]])/(2*f[r[]]),
(Sqrt[f[r[]]]*(-x[[3]] + x[[4]] - x[[6]] + x[[7]])*r[])/(2*mu[LI[l], -LI[1]]),
(1/Sqrt[f[r[]]]*(-x[[3]] + x[[4]] + x[[6]] - x[[7]])*r[])/(2*mu[LI[l], -LI[1]]),
x[[9]]*r[]^2,
((x[[8]] + x[[10]])*r[]^2)/(mu[LI[l], -LI[1]]*mu[LI[l], -LI[2]]),
((I/2)*Sqrt[f[r[]]]*(x[[3]] + x[[4]] + x[[6]] + x[[7]])*r[])/mu[LI[l], -LI[1]],
((I/2)/Sqrt[f[r[]]]*(x[[3]] + x[[4]] - x[[6]] - x[[7]])*r[])/mu[LI[l], -LI[1]],
((-I)*(x[[8]] - x[[10]])*r[]^2)/(mu[LI[l], -LI[1]]*mu[LI[l], -LI[2]])}
(*Output order: "tt","tr","rr","t+","r+","\[EmptyCircle]","+","t-","r-","-"}. 
Input order: "ll","ln","lm","lmbar","nn","nm","nmbar","mm","mmbar","mbarmbar"*)


(* ::Section:: *)
(*From Carter to Kinnersley tetrad rule and function*)


CarterToKinnersleyhRule={h[{1,-NP},{1,-NP},LI[s_],LI[l_],LI[m_]]:>f[r[]]/2hK[{1,-NP},{1,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{1,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[f[r[]]/2]hK[{1,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[f[r[]]/2]hK[{1,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>2/f[r[]]hK[{2,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[2/f[r[]]]hK[{2,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>Sqrt[2/f[r[]]]hK[{2,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{4,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{4,-NP},{4,-NP},LI[s],LI[l],LI[m]]};

CarterToKinnersley[x_]:={2/f[r[]]x[[1]],x[[2]],Sqrt[2/f[r[]]]x[[3]],Sqrt[2/f[r[]]]x[[4]],f[r[]]/2  x[[5]],Sqrt[f[r[]]/2]x[[6]],Sqrt[f[r[]]/2]x[[7]],x[[8]],x[[9]],x[[10]]}


(* ::Section:: *)
(*From Carter to Hartle--Hawking tetrad rule and function*)


CarterToHartleHawkinghRule={h[{1,-NP},{1,-NP},LI[s_],LI[l_],LI[m_]]:>(f[r[]]/2)^(-1)hK[{1,-NP},{1,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{1,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>(Sqrt[f[r[]]/2])^(-1)hK[{1,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{1,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>(Sqrt[f[r[]]/2])^(-1)hK[{1,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{2,-NP},LI[s_],LI[l_],LI[m_]]:>(2/f[r[]])^(-1)hK[{2,-NP},{2,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>(Sqrt[2/f[r[]]])^(-1)hK[{2,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{2,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>(Sqrt[2/f[r[]]])^(-1)hK[{2,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{3,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{3,-NP},LI[s],LI[l],LI[m]],
h[{3,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{3,-NP},{4,-NP},LI[s],LI[l],LI[m]],
h[{4,-NP},{4,-NP},LI[s_],LI[l_],LI[m_]]:>hK[{4,-NP},{4,-NP},LI[s],LI[l],LI[m]]};

CarterToHartleHawking[x_]:={(2/f[r[]])^(-1)x[[1]],x[[2]],(Sqrt[2/f[r[]]])^(-1)x[[3]],(Sqrt[2/f[r[]]])^(-1)x[[4]],(f[r[]]/2 )^(-1) x[[5]],(Sqrt[f[r[]]/2])^(-1)x[[6]],(Sqrt[f[r[]]/2])^(-1)x[[7]],x[[8]],x[[9]],x[[10]]}


(* ::Section:: *)
(*Rules for f[r], mu[l,s], etc.*)


ftoMrule=f->Function[{r},1-2M/r];

Derivative[n_][f] := Evaluate[Function[{r}, Evaluate[D[1-2M/r,{r,n}]]]];

lambdatolrule={\[Lambda]1[l_]->Sqrt[(l)(l+1)],\[Lambda]2[l_]->Sqrt[(l-1)(l+2)]};
mutolrule=mu[LI[l_],-LI[s_]]:>Sqrt[(l+1-s)(l+s)];
sigmarule={\[Sigma]->(-1)^(\[ScriptQ]+\[ScriptL]+\[ScriptP]),\[Sigma]p->\[Sigma]+1,\[Sigma]m->\[Sigma]-1};


CIntrule={CInt[xAct`xTensor`LI[l_],xAct`xTensor`LI[m_],xAct`xTensor`LI[s_],-xAct`xTensor`LI[l1_],-xAct`xTensor`LI[m1_],-xAct`xTensor`LI[s1_],-xAct`xTensor`LI[l2_],-xAct`xTensor`LI[m2_],-xAct`xTensor`LI[s2_]]:>(-1)^(m+s)Sqrt[(2l+1)(2l1+1)(2l2+1)/(4 Pi)]ThreeJSymbol[{l,s},{l1,-s1},{l2,-s2}]ThreeJSymbol[{l,-m},{l1,m1},{l2,m2}]};


(* ::Text:: *)
(*Automatically set coupling terms that violate the 3j-symbol selection rules to 0*)


CInt[LI[l_], LI[m_], LI[s_], -LI[l1_], -LI[m1_], -LI[s1_], -LI[l2_], -LI[m2_], -LI[s2_]] := 0 /; s!=s1+s2
CInt[LI[l_], LI[m_], LI[s_], -LI[l1_], -LI[m1_], -LI[s1_], -LI[l2_], -LI[m2_], -LI[s2_]] := 0 /; m!=m1+m2
CInt[LI[l_], LI[m_], LI[s_], -LI[l1_], -LI[m1_], -LI[s1_], -LI[l2_], -LI[m2_], -LI[s2_]] := 0 /; m>Abs[l] || m<-Abs[l] || m1>Abs[l1] || m1<-Abs[l1] || m2>Abs[l2] || m2<-Abs[l2]
CInt[LI[l_], LI[m_], LI[s_], -LI[l1_], -LI[m1_], -LI[s1_], -LI[l2_], -LI[m2_], -LI[s2_]] := 0 /; -s>Abs[l] || -s<-Abs[l] || -s1>Abs[l1] || -s1<-Abs[l1] || -s2>Abs[l2] || -s2<-Abs[l2]
CInt[LI[l_], LI[m_], LI[s_], -LI[l1_], -LI[m1_], -LI[s1_], -LI[l2_], -LI[m2_], -LI[s2_]] := 0 /; Abs[l1-l2]>l || l>l1+l2


lmReplacerule[func_,ld_,md_,l1d_,m1d_,l2d_,m2d_]:=func/.l->ld/.m->md/.l1->l1d/.m1->m1d/.l2->l2d/.m2->m2d/.CIntrule/.mutolrule;


(* ::Section:: *)
(*SchwarzschildSource Function*)


SchwarzschildQuadraticOperator[Source_,Gauge_:"Generic",OutputBasis_:"Carter",InputBasis_:"Carter"]:=Module[{func},



If[!MemberQ[{"d2G","d2R","S4d2G","S0d2G"},Source],Message[SchwarzschildSource::argserror,Source]];
If[!MemberQ[{"Generic","Lorenz","ReggeWheeler","OutgoingRadiationGauge","TraceFreeOutgoingRadiationGauge","IngoingRadiationGauge","TraceFreeIngoingRadiationGauge"},Gauge],Message[SchwarzschildSource::argserror,Gauge]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking","Master"},OutputBasis],Message[SchwarzschildSource::argserror,OutputBasis]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking"},InputBasis],Message[SchwarzschildSource::argserror,InputBasis]];

If[MemberQ[{"S4d2G","S0d2G"},Source]&&MemberQ[{"BLS","trTensor"},OutputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];
If[MemberQ[{"d2G","d2R"},Source]&&MemberQ[{"Master"},OutputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];

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
func = Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"} -> func];

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


If[OutputBasis=="BLS"&&MemberQ[{"d2G","d2R"},Source],func=Association@@Thread[Range[1,10]->CarterToBLS[Values[func]]]];
If[OutputBasis=="trTensor"&&MemberQ[{"d2G","d2R"},Source],func=Association@@Thread[{"tt","tr","rr","t+","r+","\[EmptyCircle]","+","t-","r-","-"}->CarterTotr[Values[func]]]];
If[OutputBasis=="Kinnersley"&&MemberQ[{"d2G","d2R"},Source],func=Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"}->CarterToKinnersley[Values[func]]]];
If[OutputBasis=="Kinnersley"&&Source=="S0d2G",func=func*2/f[r[]]];
If[OutputBasis=="Kinnersley"&&Source=="S4d2G",func=func*f[r[]]/2];

If[OutputBasis=="HartleHawking"&&MemberQ[{"d2G","d2R"},Source],func=Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"}->CarterToHartleHawking[Values[func]]]];
If[OutputBasis=="HartleHawking"&&Source=="S0d2G",func=func*(2/f[r[]])^(-1)];
If[OutputBasis=="HartleHawking"&&Source=="S4d2G",func=func*(f[r[]]/2)^(-1)];

If[OutputBasis=="Master"&&Source=="S0d2G",func=func*-4*r^2/f[r[]]];
If[OutputBasis=="Master"&&Source=="S4d2G",func=func*-r^6*f[r[]]];

If[InputBasis=="BLS",func=func/.CarterToBLShRule];
If[InputBasis=="trTensor",func=func/.CarterTotrhRule];
If[InputBasis=="Kinnersley",func=func/.CarterToKinnersleyhRule];
If[InputBasis=="HartleHawking",func=func/.CarterToHartleHawkinghRule];

Return[func]];




SchwarzschildLinearOperator[Source_,Gauge_:"Generic",OutputBasis_:"Carter",InputBasis_:"Carter"]:=Module[{func},



If[!MemberQ[{"dG","dR","Teukolsky"},Source],Message[SchwarzschildSource::argserror,Source]];
If[!MemberQ[{"Generic","Lorenz","ReggeWheeler","OutgoingRadiationGauge","TraceFreeOutgoingRadiationGauge","IngoingRadiationGauge","TraceFreeIngoingRadiationGauge"},Gauge],Message[SchwarzschildSource::argserror,Gauge]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking","+2","-2"},OutputBasis],Message[SchwarzschildSource::argserror,OutputBasis]];
If[!MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking","Master"},InputBasis],Message[SchwarzschildSource::argserror,InputBasis]];

If[MemberQ[{"dG","dR"},Source]&&MemberQ[{"+2","-2"},OutputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];
If[MemberQ[{"dG","dR"},Source]&&MemberQ[{"Master"},InputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];
If[MemberQ[{"Teukolsky"},Source]&&MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking"},OutputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];
If[MemberQ[{"Teukolsky"},Source]&&MemberQ[{"Carter","BLS","trTensor","Kinnersley","HartleHawking"},InputBasis],Message[SchwarzschildSource::argserror2,Source,OutputBasis]&&Abort[]];

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
func = Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"} -> func];

If[OutputBasis=="BLS"&&MemberQ[{"dG","dR"},Source],func=Association@@Thread[Range[1,10]->CarterToBLS[Values[func]]]];
If[OutputBasis=="trTensor"&&MemberQ[{"dG","dR"},Source],func=Association@@Thread[{"tt","tr","rr","t+","r+","\[EmptyCircle]","+","t-","r-","-"}->CarterTotr[Values[func]]]];
If[OutputBasis=="Kinnersley"&&MemberQ[{"dG","dR"},Source],func=Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"}->CarterToKinnersley[Values[func]]]];
If[OutputBasis=="HartleHawking"&&MemberQ[{"dG","dR"},Source],func=Association@@Thread[{"ll","ln","lm","l\!\(\*OverscriptBox[\(m\), \(_\)]\)","nn","nm","n\!\(\*OverscriptBox[\(m\), \(_\)]\)","mm","m\!\(\*OverscriptBox[\(m\), \(_\)]\)","\!\(\*OverscriptBox[\(m\), \(_\)]\)\!\(\*OverscriptBox[\(m\), \(_\)]\)"}->CarterToHartleHawking[Values[func]]]]

If[InputBasis=="BLS",func=func/.CarterToBLShRule];
If[InputBasis=="trTensor",func=func/.CarterTotrhRule];
If[InputBasis=="Kinnersley",func=func/.CarterToKinnersleyhRule];
If[InputBasis=="HartleHawking",func=func/.CarterToHartleHawkinghRule];

If[InputBasis=="trTensor"&&OutputBasis=="trTensor",
Do[func[key]=func[key]/.(htm|hrm|hm)[__]->0,{key,{"tt","tr","rr","t+","r+","\[EmptyCircle]","+"}}];
Do[func[key]=func[key]/.(htp|hrp|hp)[__]->0,{key,{"t-","r-","-"}}]];

If[Gauge=="ReggeWheeler",func=func/.RWGaugeConditionVectorHarmonicdecompform];

If[Source=="Teukolsky"&&OutputBasis=="+2"&&InputBasis=="Master",func=MasterTeukolskyplus2];
If[Source=="Teukolsky"&&OutputBasis=="-2"&&InputBasis=="Master",func=MasterTeukolskyminus2];

func=func/.{l1->l,m1->m};

Return[func]];




SchwarzschildQuadraticCovariantSource[Source_,Gauge_:"Generic"]:=Module[{func},

If[!MemberQ[{"d2G"},Source],Message[SchwarzschildCovariantSource::argserror,Source]];
If[!MemberQ[{"Generic","ReggeWheeler"},Gauge],Message[SchwarzschildCovariantSource::argserror,Gauge]];

If[Source=="d2G"&&Gauge=="Generic",func=d2GVectorHarmonics];
If[Source=="d2G"&&Gauge=="ReggeWheeler",func=d2GVectorHarmonics/.RWGaugeConditionVectorHarmonicdecompform];

Return[func]];


(* ::Section::Closed:: *)
(*End Package*)


End[];
Protect /@ xAct`PerturbationEquations`Private`PerturbationEquationsSymbols;
EndPackage[];
