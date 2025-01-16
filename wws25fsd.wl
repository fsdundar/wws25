(* ::Package:: *)

(* ::Title:: *)
(*WWS25 Project Library*)


(* ::Author:: *)
(*Furkan Semih D\[UDoubleDot]ndar*)


(* ::Text:: *)
(*Date: 15 January 2025.*)


(*
  # LICENSE
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  Copyright 2025, Furkan Semih D\[CapitalUDoubleDot]NDAR
  Email: f.semih.dundar@yandex.com
*)


(* ::Section:: *)
(*Functions related to BSD variety*)


MaxM[l_]:=If[EvenQ[l],l/2-1,(l-1)/2];


StringNeighborhood[s_,i_,m_]:=With[
{s3=s<>s<>s,l=StringLength[s]},
StringTake[s3,{l+i-m,l+i+m}]
];


StringEqual[s1_,s2_]:=s1==s2||s1==StringReverse[s2];


StringIsomorphic[s1_,s2_]:=With[
{s2s=Table[StringRotateRight[s2,i],{i,StringLength[s2]}]},
AnyTrue[s2s,StringEqual[s1,#]&]
];


RelativeIndifference[s_,i_,j_]:=With[{maxm=MaxM[StringLength[s]]},
For[m=0,m<=maxm,m++,
If[m <= maxm \[And] Not[StringIsomorphic[StringNeighborhood[s,i,m],StringNeighborhood[s,j,m]]],
Return[m],
If[m==maxm,Return["Indiscernible"],Continue[]]
]
]
]


AbsoluteIndifference[s_,i_]:=With[{rilist=Table[If[j!=i,RelativeIndifference[s,i,j],Nothing],{j,StringLength[s]}]},
If[MemberQ[rilist,"Indiscernible"],
0,
Max[rilist]
]
]


Variety[s_]:=With[{ailist=Table[AbsoluteIndifference[s,i],{i,StringLength[s]}]},
If[MemberQ[ailist,0],
0,
Total[1/ailist]]
]


VarRule[mw_]:=#->Variety[#]&/@VertexList[mw]


PhysicalPathQ[path_,varRule_]:=MemberQ[VertexList[path]/.varRule,0]//Not


nonPhysicalPathQ[path_,varRule_]:=MemberQ[VertexList[path]/.varRule,0]


ActionOnPath[path_,varRule_]:=If[nonPhysicalPathQ[path,varRule],
"Non-Physical-Path",
Total[VertexList[path]/.varRule]
]


PathsBetween[mw_,v1_,v2_,depth_]:=PathGraph/@FindPath[mw,v1,v2,depth,All]


PhysicalPathsBetween[mw_,v1_,v2_,depth_,varRule_]:=Select[PathsBetween[mw,v1,v2,depth],PhysicalPathQ[#,varRule]&]


MaxVarPathsBetween[mw_,v1_,v2_,depth_,varRule_]:=With[{physPathsWithVars={ActionOnPath[#],#}&/@PhysicalPathsBetween[mw,v1,v2,depth,varRule]},
Last/@Select[
physPathsWithVars,First[#]&==Max[First/@physPathsWithVars]
]
]


PathsBetweenLayers[mw_,l1_,l2_,depth_]:=Flatten[
Table[
PathsBetween[mw,l1[[i]],l2[[j]],depth]
,{i,Length[l1]},{j,Length[l2]}]
]


PhysicalPathsBetweenLayers[mw_,l1_,l2_,depth_,varRule_]:=Select[PathsBetweenLayers[mw,l1,l2,depth],PhysicalPathQ[#,varRule]&]


PhysicalMultiwaySystem[mw_,vr_]:=VertexDelete[mw,Select[VertexList[mw],(#/.vr)==0&]]


(* ::Section:: *)
(*Functions related to string sums*)


joinStr2Data[n_,alphabet_]:=With[{words=StringJoin/@Tuples[alphabet,n]},
Table[{i,j,Variety[words[[i]]<>words[[j]]]},{i,Power[Length[alphabet],n]},{j,Power[Length[alphabet],n]}]
]


joinStr2PtPlot[n_,alphabet_,params___]:=With[{data=Flatten[joinStr2Data[n,alphabet],1]},
Show[
Graphics[{params,Point[Most[#]]}]&/@Select[data,Last[#]>0&]
]
]


(* ::Section:: *)
(*Functions related to path sums and s-matrix*)


pathToSentence[path_]:=VertexList[path]


PathWeight[path_,vr_,k_,"Unity"]:=1


SumOverPaths[paths_,vr_,k_,params___]:=PathWeight[#,vr,k,params] Exp[I ActionOnPath[#,vr]/k]&/@paths//Total


SumOverPathsList[pathsList_,vr_,k_,params___]:={Last[VertexList[Last[#]]],SumOverPaths[#,vr,k,params]}&/@pathsList


SMatrixElements[mw_,head_,lastLayer_,depth_,k_,vr_,params___]:=With[{physPaths=Table[PhysicalPathsBetween[mw,head,lastLayer[[n]],depth,vr],{n,Length[lastLayer]}]},
SumOverPathsList[DeleteCases[physPaths,{}],vr,k,params]
]
