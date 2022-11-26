{ pkgs, home-manager-options, json2nix }:
pkgs.writers.writeBash "nix-attrs-search-test-json2nix" ''
  ELEMENT_COUNT=$(${jq} 'keys | length' ${home-manager-options}/share/doc/home-manager/options.json)
  echo "Trying $(($ELEMENT_COUNT - 1)) elements"
  for i in $(seq 0 $(($ELEMENT_COUNT - 1))); do
  	if ! (($i % 100)); then
  		echo "$i..."
  	fi
  	DEFAULT_VAL=$(${jq} "[.[]][$i].default" ${home-manager-options}/share/doc/home-manager/options.json)
  	ERROR_MESSAGE=$( { echo $DEFAULT_VAL | ${json2nix} | ${nixfmt} } 2>&1 )
  	if [ $? != 0 ]; then
  		echo "[$i] ERROR:"
  		echo $DEFAULT_VAL
  		echo $ERROR_MESSAGE
  	fi
  done
''
