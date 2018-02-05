#!/usr/local/bin/bash

loc=$1;
startTime=$2;
endTime=$3;

declare -A fNms=(["Val"]='"data\/soil\/valencia_20102011.csv"'
		 ["Oul"]='"data\/soil\/oulu_20102011.csv"'
		 ["Hal"]='"data\/soil\/halle_20102011.csv"'
		 ["Nor"]='"data\/soil\/norwich_20102011.csv"')

declare -A fOutDirs=(["Val"]='out/fmLiteExpsVal'
		     ["Oul"]='out/fmLiteExpsOul'
		     ["Hal"]='out/fmLiteExpsHal'
		     ["Nor"]='out/fmLiteExpsNor')

locFName="${fNms[$loc]}"
outDir="${fOutDirs[$loc]}"

sed "s/ENV/$locFName/" Plant/EnvTempl.hs | sed "s/ST/$startTime/" > Plant/EnvT.hs
stack build
echo "compiled"
echo "stack exec -- fmLite $outDir $startTime $endTime"
stack exec -- fmLite "$outDir" "$startTime" "$endTime"
open "$outDir/plots/mass.png"

#./runPlant.sh Oul `echo "$((236*24))"` `echo "$((498*24))"`
