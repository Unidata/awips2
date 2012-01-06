## Must be placed in the same directory as the index.xml - under
# localization/menus/radar
#!/bin/sh

turn_to=$1

function change()
{
	echo "Changing radar menus to $1 style"
	pushd ../../../cave_static/configured > /dev/null
	if [ "$1" = "new" ]; then
		for site in *
		do
			if [ -d $site/menus/radar ]; then
				echo "Creating new menus for $site"
				pushd $site/menus/radar > /dev/null
			
				# create backup of the original file
				cp index.xml index.xml.bak
	    		sed -i 's/dualPol\/baseLocalRadarMenu.xml/baseLocalRadarMenu.xml/g' index.xml
		    	sed -i 's/dualPol\/baseTerminalLocalRadarMenu.xml/baseTerminalLocalRadarMenu.xml/g' index.xml
		    	sed -i 's/baseLocalRadarMenu.xml/dualPol\/baseLocalRadarMenu.xml/g' index.xml
	    		sed -i 's/baseTerminalLocalRadarMenu.xml/dualPol\/baseTerminalLocalRadarMenu.xml/g' index.xml
		    	mkdir -p ../../../../site/$site/menus/radar
		    	mv index.xml ../../../../site/$site/menus/radar/index.xml
	    	
	    		# copy backup back to original
			   	mv index.xml.bak index.xml
			   	
			   	# change permissions and ownership
	    	    chown awips.fxalpha index.xml ../../../../site/$site/menus/radar/index.xml

				# create backup of the original file	    	
				cp dialRadars.xml dialRadars.xml.bak
	    		sed -i 's/dualPol\/baseLocalRadarMenu.xml/baseLocalRadarMenu.xml/g' dialRadars.xml
	    		sed -i 's/dualPol\/baseTerminalLocalRadarMenu.xml/baseTerminalLocalRadarMenu.xml/g' dialRadars.xml
		    	sed -i 's/baseLocalRadarMenu.xml/dualPol\/baseLocalRadarMenu.xml/g' dialRadars.xml
		    	sed -i 's/baseTerminalLocalRadarMenu.xml/dualPol\/baseTerminalLocalRadarMenu.xml/g' dialRadars.xml
	    		mv dialRadars.xml ../../../../site/$site/menus/radar/dialRadars.xml
	    	
	    		# copy backup back to original
		    	mv dialRadars.xml.bak dialRadars.xml
		    	
			   	# change permissions and ownership
	    	    chown awips.fxalpha dialRadars.xml ../../../../site/$site/menus/radar/dialRadars.xml
	    	
		    	# move back to original directory
	    		popd > /dev/null
	    	else 
				echo "Menus not configured for $site"
			fi	    	
	    done 
	elif [ "$1" = "old" ]; then
	    for site in *
	    do
			if [ -d $site/menus/radar ]; then
		    	echo "Creating old menus for $site"
		    	pushd $site/menus/radar > /dev/null
	    		cp index.xml index.xml.bak
		    	sed -i 's/dualPol\/baseLocalRadarMenu.xml/baseLocalRadarMenu.xml/g' index.xml
		    	sed -i 's/dualPol\/baseTerminalLocalRadarMenu.xml/baseTerminalLocalRadarMenu.xml/g' index.xml
	    		mkdir -p ../../../../site/$site/menus/radar
		    	mv index.xml ../../../../site/$site/menus/radar/index.xml
		    	mv index.xml.bak index.xml
		    	
		    	chown awips.fxalpha index.xml ../../../../site/$site/menus/radar/index.xml
	
		    	cp dialRadars.xml dialRadars.xml.bak
		    	sed -i 's/dualPol\/baseLocalRadarMenu.xml/baseLocalRadarMenu.xml/g' dialRadars.xml
	    		sed -i 's/dualPol\/baseTerminalLocalRadarMenu.xml/baseTerminalLocalRadarMenu.xml/g' dialRadars.xml
		    	mv dialRadars.xml ../../../../site/$site/menus/radar/dialRadars.xml
		    	mv dialRadars.xml.bak dialRadars.xml
		    	chown awips.fxalpha dialRadars.xml ../../../../site/$site/menus/radar/dialRadars.xml
	    		popd > /dev/null
			else
				echo "Menus not configured for $site"
			fi
		done
	fi
	popd > /dev/null
}

if [ "$turn_to" = "--help" ]; then
	echo "Using 'new' argument turns the menus to dual polar, using 'old' command changes them back"
	echo "dualPolTurnKey.sh <new/old>"
	exit 1
fi

if [ "$turn_to" = "" ]; then
	echo "Usage : Argument is missing"
	exit 1
fi

if [ "$turn_to" = "new" ] || [ "$turn_to" = "old" ]; then
	change "$turn_to"	
else
	echo "Sorry, argument is incorrect, use --help"
	exit 1
fi

echo "Done converting menus"
