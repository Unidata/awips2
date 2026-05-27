#!/bin/sh
#
# This script will take the AWIPS1 browserFieldMenu.txt file and
# convert it to the AWIPS2 fieldsMenus.xml file.  It will make
# use of the results of AWIPS1's testGridKeyServer in order to
# make sense of the code numbers in the browserFieldMenu.txt file.
#
# This script assumes that browserFieldMenu.txt exists in the same
# directory.  It also assumes that testGridKeyServer has been run
# with the "v" option, and that the results exist in
# testGridKeyServer_v.txt, also in the same directory.
#
# Usage: VBconversion.sh > outputfile
# Expected Inputs: browserFieldMenu.txt and testGridKeyServer_v.txt
#   (testGridKeyServer_v.txt contains the A1 output of
#    /awips/fxa/bin/testGridKeyServer v)
#
# KNOWN ISSUES:
# 1) This script is inefficient...it may take a minute to finish.
#
# 2) If $line has an asterisk in it, the asterisk gets treated as a
#    wildcard and it is replaced by the filenames of all files in the
#    current directory.
#
# 3) Little to no indentation will exist in the output, although
#    menu sections will be visually delimited in the output.
#
# 4) The Raytheon legal block at the top of the resulting file will
#    not be populated. 
#
# Author: Jim Calkins, NWS/OST/SEC
#

#
# set endtagflag so we know whether to put a closing menu tag before
# we put a new opening menu tag in the file
#
endtagflag=1
#
# Opening tags
echo "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
echo "<!-- insert Raytheon legalese here"
echo "-->"
echo "<menuTemplate xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"

cat browserFieldMenu.txt | while read line
do
   action=`echo $line | cut -f1 -d:`
   code=`echo $line | cut -f2 -d" "`
   label=`echo $line | cut -f2 -d\"`
#
# If it's a menu title...quick programming job unfortunately means
# "code" actually contains the label we want.
#
   if [ $action == "menu" ]
   then
      if [ $endtagflag == 2 ]
      then
         echo "</contribute>"
      fi
      echo "<!--"
      echo "$code menu item"
      echo "-->"

      echo "<contribute xsi:type=\"toolBarItem\" toolItemName=$code>"
#
# Set the flag so we start closing menu tags before we open a new one
      endtagflag=2
#
# If it's the start of a submenu
#
   elif [ $action == "submenu" ]
   then
#
# Redefine code since multi-word menu titles are truncated due to cut command.
# Also, convert < and > to &lt; and &gt; respectively.
      code=`echo $line | cut -f2 -d\" | sed 's/>/\&gt\;/'`
      code=`echo $line | cut -f2 -d\" | sed 's/</\&lt\;/'`
      echo "<contribute xsi:type=\"toolbarSubMenu\" menuText=\"$code\">"
#
# If it's the end of a submenu
#
   elif [ $action == "endSubmenu" ]
   then
      echo "</contribute>"
#
# If it's a title (inline header)
#
   elif [ $action == "title" ]
   then
      code=`echo $line | cut -f2 -d\"`
      echo "<contribute xsi:type=\"titleImgItem\" titleText=\"$code\" displayImage=\"true\" displayDashes=\"true\"/>"
#
# If it's a parameter
#
   elif [ $action == "button" ]
   then
      cat testGridKeyServer_v.txt | while read code_lookup fileID label2 styleinfo
      do
        if [ $code == $code_lookup ]
        then
           fileID=`echo $fileID | cut -f1 -d:`
           label=`echo $label | cut -f2 -d\" | sed 's/>/\&gt\;/'`
           label=`echo $label | cut -f2 -d\" | sed 's/</\&lt\;/'`
           echo "     <contribute xsi:type=\"menuItem\" menuText=\"$label\" key=\"$fileID\" indentText=\"false\" />"
        fi 
      done
   fi
done
#
# Close the last menu tags
echo "</contribute>"
echo "</menuTemplate>"
#
# Outta here...
exit
