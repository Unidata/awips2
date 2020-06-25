# Editing Menus

Any of the menus in the menubar can be customized in the **Localization Perspective**.  

---

## Modifying Menus

Once in the **Localization Perspective**, menus can be modified by going to the **D2D** > **Menus** directory in the File Browser.  Here there are submenus for different data types and menu structures.  Usually the **index.xml** file found in these submenus is the *master* file which the actual menu is based off of.  This file can reference other xml files and you may have to modify these *child* xml files to get the results you are looking for.  

In order to modify any file, you must right click on it and 
select **Copy To > USER (my-username)**.  Then you may open this copy and begin to modify it.  Once this process has been completed and a change has been made and saved, CAVE will need to be restarted and opened in the D2D perspective to see the change.

This example covers how to add a new menu entry to an existing menu.

1. Switch to the **Localization Perspective**
2. Find the **grid** folder under **D2D** > **Menus**
3. Double-click to expand **index.xml**
4. Right-click to **BASE (common_static)** and select **Copy To...**, then select **USER** level
5. Double-click **USER** to open the editor and copy an existing include tag, and update the *modelName* (this must match an existing product found in the Product Browser) and the *menuName* (this can be anything)
    
        <include installTo="menu:models" fileName="menus/grid/allFamilies.xml">
            <substitute key="modelName" value="GEFS" />
            <substitute key="menuName" value="GEFS" />
            <substitute key="frameCount" value="41" />
            <substitute key="TP" value="TP"/>
        </include>
      
6. Once this is completed, save the file and restart CAVE
7. Navigate to the **Models** menu and you should see a new entry with **GEFS**

---

## Removing Menus

This example covers how to remove a menu (in this case **MRMS**) from D2D:

1. Switch to the **Localization Perspective**
2. Find the **mrms** folder under **D2D** > **Menus**
3. Double-click to expand **index.xml**
4. Right-click **BASE** and select **Copy To...**, then select **USER** level
5. Right-click refresh the **mrms** entry
6. Double click **USER** to open the editor and change
    
        <menuContributionFile>
            <include installTo="menu:mrms?after=MRMS_MENU_START" fileName="menus/mrms/mrms.xml"/>
        </menuContributionFile>

    to 
    
        <menuContributionFile>
        </menuContributionFile>

With this completed, you can now restart CAVE and will not see the MRMS menu anymore.  Repeat this example for other product menus, such as **radar**, **upperair**, **tools**, etc., to further customize D2D data menus for any level of localization.
