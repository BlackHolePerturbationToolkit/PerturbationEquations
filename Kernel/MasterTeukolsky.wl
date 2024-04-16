(* ::Package:: *)

(* ::Section:: *)
(*Master Teukolsky*)


MasterTeukolskyplus2=(psiMasterplus2[LI[l], LI[m]] (4 I m \[Omega]-f[r[]] (-6+l+l^2-8 I \[Omega] r[])+\[Omega] r[] (-4 I+\[Omega] r[])))/f[r[]]+6 (m+f[r[]] r[]) PD[{1, -BL}][psiMasterplus2[LI[l], LI[m]]]+f[r[]] r[]^2 PD[{1, -BL}][PD[{1, -BL}][psiMasterplus2[LI[l], LI[m]]]];


MasterTeukolskyminus2=(psiMasterminus2[LI[l], LI[m]] (-4 I m \[Omega]-f[r[]] (-2+l+l^2+8 I \[Omega] r[])+\[Omega] r[] (4 I+\[Omega] r[])))/f[r[]]-2 (m+f[r[]] r[]) PD[{1, -BL}][psiMasterminus2[LI[l], LI[m]]]+f[r[]] r[]^2 PD[{1, -BL}][PD[{1, -BL}][psiMasterminus2[LI[l], LI[m]]]];
