#!/usr/local/bin/bash

loc=$1;
startTime=$2;
endTime=$3;

declare -A fNms=(["Val"]='"data\/rad\/weatherValencia2yrsRad.csv"'
		 ["Oul"]='"data\/rad\/weatherOulu2yrsRad.csv"'
		 ["Hal"]='"data\/rad\/weatherHalle2yrsRad.csv"'
		 ["Nor"]='"data\/rad\/weatherNorwich2yrsRad.csv"')

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

