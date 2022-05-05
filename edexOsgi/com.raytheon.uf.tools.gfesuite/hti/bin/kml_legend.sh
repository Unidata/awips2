#!/bin/sh
#
# kml_legend.sh - creates legends for the KML files.
#
# requirements: imagemagick (tested on version 6.2.8); 
#               file logos.png must be in the same directory as this script
# 
# UPDATED: 26 June 2012 - JCM - changed legend text per DWS request
# UPDATED: 19 July 2012 - JCM - changed legend text to black from white
# UPDATED: 08 Sept 2014 - JCM - changed None and Low to new phraseology;
#                               removed marine legend; updated coastal
#                               and inland threats
# UPDATED: 11 Sept 2014 - JCM - completed updates above
# UPDATED: 20 Oct. 2014 - JCM - set up for 2015 season
# UPDATED: 18 Feb. 2015 - JCM - added full path for logos.png
# UPDATED: 09 Jun. 2017 - PS - Custom labels for each hazard. Removed original labels. 
#
########################################################################
#  CONFIGURATION SECTION BELOW
########################################################################
GFESUITE_HOME="/awips2/GFESuite"
HTI_HOME="${GFESUITE_HOME}/hti"
site=$(hostname|cut -c 5-)

if [ ! -f ${HTI_HOME}/etc/sitevars.${site} ]; then
   cp ${HTI_HOME}/etc/sitevars.ccc ${HTI_HOME}/etc/sitevars.${site}
fi

# SITES CAN CUSTOMIZE THE SITEVARS AS NEEDED
. ${HTI_HOME}/etc/sitevars.${site}

########################################################################
#  BEGIN MAIN SCRIPT
########################################################################

cd ${HTI_HOME}/data

for element in wind surge flood tornado
do

# create canvases
if [ $element = "wind" ]
then
convert -size 400x500 xc:black temp.png
elif [ $element = "surge" ]
then
convert -size 500x500 xc:black temp.png
elif [ $element = "flood" ]
then
convert -size 400x500 xc:black temp.png
else # tornado
convert -size 400x500 xc:black temp.png
fi


# make the image transparent
##convert temp.png -alpha transparent transparent.png
convert temp.png null: -matte -compose Clear -composite -compose Over transparent.png

# insert the logos at the bottom
composite -gravity south -geometry +0+0 ${HTI_HOME}/bin/logos.png transparent.png trans2.png

# write the date onto the image
DATE=`date +"Issued %F %H%MZ"`
# DATE= " "
convert trans2.png -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +0+400 "$DATE" trans2a.png

if [ $element = "wind" ]
then

convert trans2a.png -fill black -draw 'rectangle 5,340 25,360' \
                    -fill "#E5E5E5" -draw 'rectangle 6,341 24,359' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+355 "Wind < 39 mph" \
                    -fill black -draw 'rectangle 5,320 25,340' \
                    -fill "#FFFF00" -draw 'rectangle 6,321 24,339' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+335 "Potential for Wind 39-57 mph" \
                    -fill black -draw 'rectangle 5,300 25,320' \
                    -fill "#FFA70F" -draw 'rectangle 6,301 24,319' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+315 "Potential for Wind 58-73 mph" \
                    -fill black -draw 'rectangle 5,280 25,300' \
                    -fill "#FF0000" -draw 'rectangle 6,281 24,299' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+295 "Potential for Wind 74-110 mph" \
                    -fill black -draw 'rectangle 5,260 25,280' \
                    -fill "#CC00CC" -draw 'rectangle 6,261 24,279' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+275 "Potential for Wind > 110 mph" \
                    trans2b.png

# wind threat
convert trans2b.png -font Century-Schoolbook-Bold -pointsize 20 -fill black \
                    -annotate +5+250 "Wind Threat" windthreatlegend.png

elif [ $element = "surge" ]
then

convert trans2a.png -fill black -draw 'rectangle 5,340 25,360' \
                    -fill "#E5E5E5" -draw 'rectangle 6,341 24,359' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+355 "Little to no storm surge flooding" \
                    -fill black -draw 'rectangle 5,320 25,340' \
                    -fill "#FFFF00" -draw 'rectangle 6,321 24,339' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+335 "Potential for storm surge flooding > 1 ft above ground" \
                    -fill black -draw 'rectangle 5,300 25,320' \
                    -fill "#FFA70F" -draw 'rectangle 6,301 24,319' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+315 "Potential for storm surge flooding > 3 ft above ground" \
                    -fill black -draw 'rectangle 5,280 25,300' \
                    -fill "#FF0000" -draw 'rectangle 6,281 24,299' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+295 "Potential for storm surge flooding > 6 ft above ground" \
                    -fill black -draw 'rectangle 5,260 25,280' \
                    -fill "#CC00CC" -draw 'rectangle 6,261 24,279' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+275 "Potential for storm surge flooding > 9 ft above ground" \
                    trans2b.png

# coastal flood threat (Storm Surge Threat)
convert trans2b.png -font Century-Schoolbook-Bold -pointsize 20 -fill black \
                    -annotate +5+250 "Storm Surge Threat" stormsurgethreatlegend.png

elif [ $element = "flood" ]
then

convert trans2a.png -fill black -draw 'rectangle 5,340 25,360' \
                    -fill "#E5E5E5" -draw 'rectangle 6,341 24,359' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+355 "Little to no potential for flooding rain" \
                    -fill black -draw 'rectangle 5,320 25,340' \
                    -fill "#FFFF00" -draw 'rectangle 6,321 24,339' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+335 "Potential for localized flooding rain" \
                    -fill black -draw 'rectangle 5,300 25,320' \
                    -fill "#FFA70F" -draw 'rectangle 6,301 24,319' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+315 "Potential for moderate flooding rain" \
                    -fill black -draw 'rectangle 5,280 25,300' \
                    -fill "#FF0000" -draw 'rectangle 6,281 24,299' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+295 "Potential for major flooding rain" \
                    -fill black -draw 'rectangle 5,260 25,280' \
                    -fill "#CC00CC" -draw 'rectangle 6,261 24,279' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+275 "Potential for extreme flooding rain" \
                    trans2b.png

# inland flood threat (Flooding Rain Threat)
convert trans2b.png -font Century-Schoolbook-Bold -pointsize 20 -fill black \
                    -annotate +5+250 "Flooding Rain Threat" floodingrainthreatlegend.png

else

convert trans2a.png -fill black -draw 'rectangle 5,340 25,360' \
                    -fill "#E5E5E5" -draw 'rectangle 6,341 24,359' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+355 "Tornadoes not expected" \
                    -fill black -draw 'rectangle 5,320 25,340' \
                    -fill "#FFFF00" -draw 'rectangle 6,321 24,339' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+335 "Potential for a few tornadoes" \
                    -fill black -draw 'rectangle 5,300 25,320' \
                    -fill "#FFA70F" -draw 'rectangle 6,301 24,319' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+315 "Potential for several tornadoes" \
                    -fill black -draw 'rectangle 5,280 25,300' \
                    -fill "#FF0000" -draw 'rectangle 6,281 24,299' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+295 "Potential for many tornadoes" \
                    -fill black -draw 'rectangle 5,260 25,280' \
                    -fill "#CC00CC" -draw 'rectangle 6,261 24,279' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+275 "Potential for outbreak of tornadoes" \
                    trans2b.png

# tornado threat
convert trans2b.png -font Century-Schoolbook-Bold -pointsize 20 -fill black \
                    -annotate +5+250 "Tornado Threat" tornadothreatlegend.png

fi

# clean up scraps
rm temp.png transparent.png trans2.png trans2a.png trans2b.png

done

chmod 666 *legend.png
mv *legend.png ${PRODUCTdir}/

