#!/bin/bash

echo ""
echo "Simple script for automate translation from High Level Chemical Language to ODEs System"
echo "Usage: $0 <HLCL-source-file> <output-file> (matlab|scipy) (g|c)"
echo "Where:"
echo -e "\t\"mablab\" and \"scipy\" are the selectable target language"
echo -e "\t\"-g\" means ENABLE GRAPHIC OUTPUT"
echo -e "\t\"-c\" means ENABLE CSV OUTPUT (supported only with scipy at the moment)"
echo "NOTE: parameters are mandatory!"

if [ $# -ne 4 ]; then
	echo "Wrong parameters.. RTFM!"
	exit -1
fi
echo -n "Generating.."
cat $1 | ./hlchem2chemeq.sh | ./chem2kinetic.py -l $3 -o $2 -$4
echo "Done!"

