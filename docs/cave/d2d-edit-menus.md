
Any of the data menus can be customized in the Localization Perspective.  

# Changing the models displayed with D2D bundles

1. In the Localization Perspective you can navigate to **D2D** > **Volume Browser** > **VbSources** and click on **volume.xml** to expand. Then right-click on **BASE** and select **Copy To...** **User** (awips).

2. You can then right-click again on the **VbSources** folder and select **Refresh** to see your new localization file called **USER (awips)**.  

3. Open the file and add or edit the model names: 

        <menuContributionFile>
        	<include installTo="menu:models" 
                     fileName="menus/volume/allFamilies.xml">
                     <substitute key="modelName" value="CMC" />
                     <substitute key="menuName" value="CMC" />
                     <substitute key="frameCount" value="41" />
                     <substitute key="TP" value="TP"/>
            </include>
        	<include installTo="menu:models" 
                     fileName="menus/volume/allFamilies.xml">
                     <substitute key="modelName" value="DGEX" />
                     <substitute key="menuName" value="DGEX" />
                     <substitute key="frameCount" value="41" />
                     <substitute key="TP" value="TP"/>
            </include>
            ...

Notice that you specify both the `modelName` (what it's named inside EDEX, such as NAM12), and `menuName`, if you prefer a more detailed menu entry, such as "NAM 12km". When you are finished, save the file and restart CAVE for the changes to take effect.

Your new localization file exists on the EDEX server (assuming username awips) as
`/awips2/edex/data/utility/cave_static/users/awips/volumebrowser/VbSources/index.html`

and will exist on the local CAVE machine under `~/caveData/etc/user/awips/volumebrowser/VbSources/index.html`


# Removing menus from the menubar

This example covers how to remove a menu (in this case **MRMS**) from D2D:

1. switch to the Localization Perspective
2. find the **mrms** folder under CAVE > Menus
3. double-click to expand **index.html**
4. right-click **BASE** and select **Copy To...**, then select **USER** level
5. right-click refresh the **mrms** entry
6. double click **USER** to open the editor and change
    
        <menuContributionFile>
            <include installTo="menu:mrms?after=MRMS_MENU_START"
            fileName="menus/mrms/mrms.xml">
            </include>
        </menuContributionFile>

    to 
    
        <menuContributionFile>
        </menuContributionFile>

With this you can restart CAVE and will not see the MRMS menu anymore.  Repeat this example for other product menus, such as **local**, **hydro**, **scan**, etc., to further customize D2D data menus for any level of localization.