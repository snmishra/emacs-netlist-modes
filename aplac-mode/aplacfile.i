*---------------------------------------------------------------
*-- Project       : aplac-mode
*-- Circuit name  : aplacfile.i
*---------------------------------------------------------------
*-- Designer(s)   : 
*-- Library       : 
*-- Purpose       : sample aplac netlist to test aplac-mode
*-- Inputs        : 
*-- Outputs       : 
*-- Supplies      : 
*-- References    : 
*---------------------------------------------------------------

#include "$HOME/applications/electronics/aplac/defaults.i"
#define nharm 8 
#library eee nnn
#library uuu

prepare tone 1 nharm

aplacvar Io 100m opt min 1m
aplacvar fo   8k opt 

oscvar sOsc 1 6 tone 1,Io

$ Transistor model with package parasitics
defmodel transistor 3 c b e
  model bjtrf1     is 0.89f  bf 105 ikf 200m nf 1.01  ise 54f   
  	+ ne 1.55  vaf 45    br 13  ikr 20m  nr 1.01  isc 50f
	+ nc 2.12  var 5     re 0.7 rc 2.2   rb 10    cjc 0.74p
	+ cje 2.1p vjc 0.60  tr 2n  vje 0.60 mjc 0.51 mje 0.36
	+ tf 23p
  ind Lc c c1 0.2n
  ind Lb b b1 1n
  ind Le e e1 0.9n
  cap Cbc c1 b1 0.5p
  cap Cce c1 e1 0.3p
  cap Cbe b1 e1 0.5p
  bjt q1 c1 b1 e1 model bjtrf1
endmodel

$ Common-emitter amplifier stage
res RB1 0 2 21k
res RB2 2 4 100k
res RC 1 4 1.4k
res RE 0 3 100
transistor Q1 1 2 3
cap CB1 2 5 100n
cap CB2 1 10 1u
cap CE 0 3 10u

$ Phase-shift network
cap C1 0 5 1u
cap C2 0 1 1u
ind L1 5 6 500u I bOsc

$ Load
res RL 0 10 1k

volt Vcc 4 0 DC 10 R 1
optimmethod minmax

sweep "HB Analysis"
+ hb fc=fo
+ window 0
+   x "f" "Hz" 0 2*fo multx=k
+   y "V_{out}" "V" 0 5 
+   big_screen
+   eps="colpits4.eps"
+ window 1
+   title="Waveform"
+   x "t" "s" 0 2/10k multx u
+   y "u" "V" -5 5 
+   y2 "i" "A" -260m 260m multy2 m
+   eps="colpits5.eps"
  show window 0 spectrum mag(spectrum(10))
  display window 1 
  + waveform "v_{out}" VWf(10)
  + waveform2 "i" IWf(bOsc) marker=1
endsweep

print s "Io =" real io bl bl s "f =" real fo bl bl
+     s "OscGoal =" real oscgoal(sOsc) 

print optvar

print s "Harmonic distortion" real distortion(10,0) S "%" lf lf
+     s "DC level     " real mag(SpectralLine(10,DC)) lf lf

print s "Complete spectrum: " lf
+     s "------------------ " lf
for i 1 nharm
  print int i s "*fo level " real mag(Spectrum(10)[i]) lf
endfor


