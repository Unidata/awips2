****
**** NOTE: This delta script must be run AFTER the delta script for DR 5211
****
This delta script will add the new "GFE Focal Point" role to the site's userRoles.xml file.
This role allows a user to edit GFE and GFE Server site level configuration files. 

It will remove the following permissions from all users and will grant the new "GFE Focal Point" 
role to any other users who previously had either of these roles.
 
      com.raytheon.localization.site/cave_static/gfe
      com.raytheon.localization.site/common_static/gfe
 