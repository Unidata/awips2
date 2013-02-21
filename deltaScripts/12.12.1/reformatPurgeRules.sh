#!/bin/sh

# NOTE: UPGRADE SCRIPT IS SIMPLE.  XML Purge Rule Cannot be condensed and have multiple tags on a line.  Expects pretty print xml that is easily readable/parsable

SITE_PATH="/awips2/edex/data/utility/common_static/site"

if [ ! -d $SITE_PATH ]; then
	echo "Cannot find site directory. $SITE_PATH does not exist"
	usage
	exit 1
fi

echo "This script should be run as user awips.  Will back up purge rule directories to .bak"
echo "Press any key to continue or ^C to quit"
read

for d in `find $SITE_PATH -type d -name "purge"`; do
	newDir=$d.new
	if [ -d "$newDir" ]; then
		rm -rf $newDir
	fi
	mkdir -p $newDir

	echo "Processing directory $d"

	for f in `ls $d/*PurgeRules.xml`; do
		fileName=${f##*/}
		NEW_FILE=$newDir/${fileName}
		if [ -f $f.modelPathKey ]; then
			f="$f.modelPathKey"
		elif [ -f $.radarPathKey ]; then
			f="$f.radarPathKey"
		fi
		echo "  Processing $f into $NEW_FILE"

		PIFS=$IFS
		IFS=$'\n'

		# prescan to determine all key combinations
		hasDefaultRule='0'
		keys=()
		keysIndex=0

		for key in `grep key $f `; do
			# strip tags
			key=${key#*<key>}
			key=${key%</key>*}

			if [ "$key" == "default" ]; then
				hasDefaultRule='1'
			else
				# split on semicolon and equal
				keyTokens=( $( echo "$key" | tr ';=' '\n' ) )
				size="${#keyTokens[@]}"
				tokenIndex=0

				while [ "$tokenIndex" -lt "$size" ]; do
					curToken="${keyTokens[$tokenIndex]}"
					addKey=1

					# verify key hasn't already been added
					if [ $keysIndex -gt 0 ]; then
						tempIndex=0
						while [ $tempIndex -lt $keysIndex ]; do
							if [ "${keys[$tempIndex]}" == "$curToken" ]; then
								addKey=0
								break
							fi
							let tempIndex+=1
						done
					fi

					if [ $addKey -eq 1 ]; then
						keys[$keysIndex]="$curToken"
						let keysIndex+=1
					fi

					let tokenIndex+=2
				done
			fi
		done

		keysSize=$keysIndex
		keysIndex=0
		state='0'
		rule=()
		ruleIndex=0
		ruleTag="rule"

		for line in `cat $f`; do
			case $state in
				# looking for <purgeRuleSet>
				0)	if [[ $line =~ "<purgeRuleSet .*>" ]]; then
						# drop name space
						echo "<purgeRuleSet>" >> $NEW_FILE
						keyIndex=0
						while [ $keyIndex -lt $keysSize ]; do
							echo "	<key>${keys[$keyIndex]}</key>" >> $NEW_FILE
							let keyIndex+=1
						done
						state='1'
					else
						# copy line to new file
						echo $line >> $NEW_FILE
					fi
					;;
				# looking for <rule>
				1)	if [[ $line =~ "<rule>" ]]; then
						state='2'
					else
						# copy line to new file
						echo $line >> $NEW_FILE
					fi
					;;
				# looking for <key>
				2)	if [[ $line =~ "<key>.*</key>" ]]; then
						state='3'

						# strip tags
						key=${line#*<key>}
						key=${key%</key>*}

						if [ "$key" == "default" ]; then
							# default rule, nothing to do besides set rule tag
							ruleTag="defaultRule"
						else
							# normal rule, split into tokens, and order by keys
							ruleTag="rule"

							# split on semicolon and equal
							keyTokens=( $( echo "$key" | tr ';=' '\n' ) )
							tokenSize="${#keyTokens[@]}"
							keyIndex=0

							while [ $keyIndex -lt $keysSize ]; do
								curKey="${keys[$keyIndex]}"
								tokenIndex=0

								while [ $tokenIndex -lt $tokenSize ]; do
									if [ "$curKey" == "${keyTokens[$tokenIndex]}" ]; then
										# found key, add value tag to rule
										let tokenIndex+=1
										rule[$ruleIndex]="		<keyValue>${keyTokens[$tokenIndex]}</keyValue>"
										let ruleIndex+=1
										break
									else
										# advance tokenIndex to next key
										let tokenIndex+=2
									fi
								done

								let keyIndex+=1
							done
						fi
					elif [[ ! $line =~ "</?id>" ]] && [[ ! $line =~ "<pluginName>" ]]; then
						# copy line to rule buffer, skipping <id> and <pluginName>
						rule[$ruleIndex]="$line"
						let ruleIndex+=1
					fi
					;;
				# looking for </rule>
				3)	if [[ $line =~ "</rule>" ]]; then
						state='1'
						ruleSize=$ruleIndex
						ruleIndex=0

						echo "	<$ruleTag>" >> $NEW_FILE
						while [ $ruleIndex -lt $ruleSize ]; do
							echo "${rule[$ruleIndex]}" >> $NEW_FILE
							let ruleIndex+=1
						done
						echo "	</$ruleTag>" >> $NEW_FILE

						ruleIndex=0
						rule=()
					elif [[ ! $line =~ "</?id>" ]] && [[ ! $line =~ "<pluginName>" ]]; then
						# copy line to rule buffer
						rule[$ruleIndex]="$line"
						let ruleIndex+=1
					fi
					;;
			esac
		done

		IFS=$PIFS
	done

	echo "  Moving $d to $d.bak"
	mv $d $d.bak
	echo "  Moving $newDir to $d"
	mv $newDir $d
done

