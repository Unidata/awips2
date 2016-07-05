/**
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.gfe.ui.zoneselector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.zoneselector.AbstractZoneSelector;
import com.raytheon.uf.viz.zoneselector.ZoneSelectorResource;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.IZoneCombiner;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Zone Selector
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            randerso    Initial creation
 * Aug 31, 2015  4749      njensen     Overrode dispose()
 * Feb 05, 2016 5316       randerso    Moved AbstractZoneSelector to separate project
 *                                     Code cleanup
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ZoneSelector extends AbstractZoneSelector {
    private static final int NO_GROUP = -1;

    IInputHandler theMouseListener = new InputAdapter() {

        /**
         * Primary mouse button
         */
        private static final int MB1 = 1;

        /**
         * Middle (Wheel) mouse button
         */
        private static final int MB2 = 2;

        @Override
        public boolean handleDoubleClick(int x, int y, int button) {
            return false;
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            switch (mouseButton) {
            case MB1:
                button1Press(x, y);
                break;

            case MB2:
                button2Press(x, y);
                break;

            default:
                // do nothing
            }

            return true;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            switch (mouseButton) {
            case MB1:
                button1Drag(x, y);
            default:
                // do nothing
            }
            return true;
        }

        @Override
        public boolean handleMouseHover(int x, int y) {
            return false;
        }

        @Override
        public boolean handleMouseMove(int x, int y) {
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            switch (mouseButton) {
            case MB1:
                button1Release(x, y);
            default:
                // do nothing
            }
            return true;
        }

        @Override
        public boolean handleMouseWheel(Event event, int x, int y) {
            return true;
        }

    };

    protected static class ZoneInfo {
        public int group;

        public RGB color;

        ZoneInfo(int group, RGB color) {
            this.group = group;
            this.color = color;
        }
    }

    private IZoneCombiner lclFmtrDialog;

    // map of zone name to group number
    private Map<String, Integer> comboDict;

    // current color map
    private List<RGB> colors;

    // state flag, whether to label the zone groups
    private boolean labelZoneGroups;

    // state flag, include all of the zones or not
    private boolean includeAllZones;

    // state flag, limits combinations to 1
    private boolean limitToOneGroup;

    private String pickUpZone;

    private Point pressLocation;

    private String pressZone;

    /**
     * @param parent
     * @param gloc
     * @param lclFmtrDialog
     */
    public ZoneSelector(Composite parent, GridLocation gloc,
            IZoneCombiner lclFmtrDialog) {
        this(parent, gloc, lclFmtrDialog, null);
    }

    /**
     * @param parent
     * @param gloc
     * @param lclFmtrDialog
     * @param selectCB
     */
    public ZoneSelector(Composite parent, GridLocation gloc,
            IZoneCombiner lclFmtrDialog, IZoneSelectionListener selectCB) {
        super(parent, gloc, selectCB);

        this.lclFmtrDialog = lclFmtrDialog;

        comboDict = new HashMap<String, Integer>();
        colors = new ArrayList<RGB>();

        // get basic editor zone colors
        noZoneColor = RGBColors.getRGBColor(config("ZoneCombiner_noZoneColor",
                "black"));
        backColor = RGBColors.getRGBColor(config(
                "ZoneCombiner_backgroundColor", "gray40"));
    }

    @Override
    public void dispose() {
        lclFmtrDialog = null;
        super.dispose();
    }

    @Override
    protected void registerHandlers(IDisplayPane pane) {
        super.registerHandlers(pane);

        registerMouseHandler(theMouseListener);
    }

    /**
     * Command to set the state of include all zones.
     * 
     * If it has changed, redo the combo dictionary and redraw.
     * 
     * @param includeAllZones
     */
    public void setIncludeAllZones(boolean includeAllZones) {
        if (this.includeAllZones != includeAllZones) {
            this.includeAllZones = includeAllZones;

            this.includeUnmentionedZones(this.includeAllZones);
            updateCombos(this.comboDict);
        }
    }

    /**
     * Command to limit combos to one group
     * 
     * @param limitOneGroup
     */
    public void setLimitToOneGroup(boolean limitOneGroup) {
        if (this.limitToOneGroup != limitOneGroup) {
            this.limitToOneGroup = limitOneGroup;

            if (this.limitToOneGroup) {
                this.comboDict = this.setOneGroup(this.comboDict);
                updateCombos(this.comboDict);
            }

        }
    }

    /**
     * @param mapNames
     * @param comboDict
     * @param colors
     */
    public void setMap(List<String> mapNames, Map<String, Integer> comboDict,
            List<RGB> colors) {
        super.setMap(mapNames, false);
        setMapInternal(mapRscList, comboDict, colors);
    }

    protected void setMapInternal(List<ZoneSelectorResource> mapRscList,
            Map<String, Integer> comboDict, List<RGB> colors) {
        super.setMapInternal(mapRscList);
        // set up the comboDict locally, may need to modify it
        if (comboDict == null) {
            this.comboDict.clear();
        } else {
            this.comboDict = new HashMap<String, Integer>(comboDict);
            this.includeUnmentionedZones(this.includeAllZones);
            if (this.limitToOneGroup) {
                this.comboDict = this.setOneGroup(this.comboDict);
            }
        }
        this.colors = colors;

        for (Entry<String, Integer> entry : this.comboDict.entrySet()) {
            String zoneName = entry.getKey();
            int group = NO_GROUP;
            if (entry.getValue() != null) {
                group = entry.getValue();
            }
            this.assignCombo(zoneName, group);
        }

        for (ZoneSelectorResource mapRsc : mapRscList) {
            mapRsc.setLabelZoneGroups(this.labelZoneGroups);
        }

        cleanUpCombosOfUnknownZones();
    }

    /**
     * @return current list of colors
     */
    public List<RGB> getColors() {
        return this.colors;
    }

    /**
     * @return current combinations
     */
    public Map<String, Integer> getCombos() {
        return this.comboDict;
    }

    /**
     * update colors command
     * 
     * @param colors
     */
    public void updateColors(List<RGB> colors) {
        this.colors = colors;
        this.updateCombos(this.comboDict);
    }

    /**
     * command to label the zone groups
     * 
     * @param labelZoneGroups
     */
    public void setLabelZoneGroups(boolean labelZoneGroups) {
        if (labelZoneGroups == this.labelZoneGroups) {
            return;
        }
        this.labelZoneGroups = labelZoneGroups;
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setLabelZoneGroups(labelZoneGroups);
        }
    }

    /**
     * update combo dictionary with different combinations
     * 
     * @param comboDict
     */
    public void updateCombos(Map<String, Integer> comboDict) {
        if (this.limitToOneGroup) {
            comboDict = this.setOneGroup(comboDict);
        }

        this.comboDict = new HashMap<String, Integer>(comboDict);
        this.cleanUpCombosOfUnknownZones();

        // existing keys
        for (String c : comboDict.keySet()) {
            this.assignCombo(c, comboDict.get(c));
        }

        // other keys not in comboDict
        if (this.includeAllZones) {
            for (String z : this.zoneNames) {
                if (!comboDict.containsKey(z)) {
                    this.assignCombo(z, this.getNewGroup());
                }
            }
        } else {
            for (String z : this.zoneNames) {
                if (!comboDict.containsKey(z)) {
                    this.assignCombo(z, NO_GROUP);
                }
            }
        }
    }

    private void button1Drag(int x, int y) {
        // how much have we moved from the press point?
        int diff = Math.abs(x - this.pressLocation.x)
                + Math.abs(y - this.pressLocation.y);

        // drag
        if ((diff >= 10) && (this.pressZone != null)) {
            // get the zones that were selected
            List<String> zones = this.selectedZones(x, y);

            if (!zones.isEmpty()) {
                int group = this.getZoneInfo(this.pressZone).group;
                if (group == NO_GROUP) { // new group
                    group = this.getNewGroup();

                    // reassign group
                    this.comboDict.put(this.pressZone, group);

                    // force color to be assigned
                    this.getGroupColor(group);

                    this.assignCombo(this.pressZone, group, true);
                }

                // extend selection using group and color (might be multiple z)
                for (String zone : zones) {
                    // reassign group
                    this.comboDict.put(zone, group);

                    // set pickup value to this value too
                    this.pickUpZone = zone;

                    this.assignCombo(zone, group, true);
                }
            }
        }
    }

    // callback when the button 1 is released, check for "click" event
    private void button1Release(int x, int y) {
        int diff = Math.abs(x - this.pressLocation.x)
                + Math.abs(y - this.pressLocation.y);

        // click
        if (diff < 10) {
            List<String> zones = this.selectedZones(x, y);
            if (!zones.isEmpty()) {
                for (String zone : zones) {
                    // get the current and pickupzone information
                    ZoneInfo info = this.getZoneInfo(zone); // existing
                    if (this.pickUpZone != null) {
                        ZoneInfo puInfo = this.getZoneInfo(this.pickUpZone);
                        // complicated logic, toggles between pickup value
                        // and not included value
                        if (info.group != puInfo.group) {
                            this.assignCombo(zone, puInfo.group, true);
                        } else if (this.includeAllZones) {
                            this.assignCombo(zone, puInfo.group, true);
                        } else {
                            this.assignCombo(zone, NO_GROUP, true);
                        }
                    } else {
                        this.assignCombo(zone, NO_GROUP, true);
                    }
                }
            }
        }
        // cleanup
        this.pressZone = null;
        this.pressLocation = null;
    }

    // callback when the button1 is pressed.
    // Save the current location and zone.
    private void button1Press(int x, int y) {
        this.pressLocation = new Point(x, y);
        List<String> zones = this.selectedZones(x, y);
        if (zones.isEmpty()) {
            this.pressZone = null;
        } else {
            this.pressZone = zones.get(0);
        }
    }

    // button 2 callback. pickup current zone action.
    private void button2Press(int x, int y) {
        List<String> zones = this.selectedZones(x, y);
        if (zones.isEmpty()) {
            this.pickUpZone = null;
        } else {
            this.pickUpZone = zones.get(0);
        }
    }

    // Looks up the edit area name in the combo dictionary, returns
    // the group Number (record#) and the color to use.
    // None for group indicates not part of the definition.
    private ZoneInfo getZoneInfo(String editAreaName) {
        if (this.comboDict.containsKey(editAreaName)) {
            int group = this.comboDict.get(editAreaName);
            RGB color = this.getGroupColor(group);
            return new ZoneInfo(group, color);
        } else {
            return new ZoneInfo(NO_GROUP, this.noZoneColor);
        }
    }

    // Looks up the color based on the group number. Returns the color
    // if not enough colors, make random ones.
    private RGB getGroupColor(int groupNumber) {
        if (groupNumber == NO_GROUP) {
            return this.noZoneColor;
        } else if (this.colors.size() >= groupNumber) {
            return this.colors.get(groupNumber - 1);
        } else {
            // make some extra colors
            Random random = new Random();
            while ((groupNumber - this.colors.size()) > 0) {
                RGB rgb = new RGB(random.nextInt((210 - 30) + 1) + 30,
                        random.nextInt((210 - 30) + 1) + 30,
                        +random.nextInt((210 - 30) + 1) + 30);
                this.colors.add(rgb);
            }
            return this.getGroupColor(groupNumber);
        }
    }

    // Get a new group number and return it
    private int getNewGroup() {
        // if limited to one group, always return group #1
        if (this.limitToOneGroup) {
            return 1;
        }
        // determine the groups in use
        ArrayList<Integer> groupsInUse = new ArrayList<Integer>();
        for (String k : this.comboDict.keySet()) {
            if (!groupsInUse.contains(this.comboDict.get(k))) {
                groupsInUse.add(this.comboDict.get(k));
            }
        }
        int newGroup = 1;
        while (true) {
            if (!groupsInUse.contains(newGroup)) {
                return newGroup;
            } else {
                newGroup += 1;
            }
        }
    }

    private void assignCombo(String zone, int group) {
        assignCombo(zone, group, false);
    }

    // Set a combination entry, given the zone and group number
    private void assignCombo(String zone, int group, boolean doCallback) {
        RGB color = this.getGroupColor(group);
        this.setZone(zone, color);
        if (group == NO_GROUP) {
            if (this.comboDict.containsKey(zone)) {
                this.comboDict.remove(zone);
            }
        } else {
            this.comboDict.put(zone, group); // reassign group
        }
        // deal with the group label
        this.setZoneGroupLabel(zone, group);

        if (doCallback && (this.selectCB != null)) {
            this.selectCB.zoneSelected(zone);
        }
    }

    // Clean up combos of unknown zones
    // if there are zones identified in the input combo file that
    // don't appear as edit area names, then eliminate them.
    private void cleanUpCombosOfUnknownZones() {
        Set<String> keys = new HashSet<String>(comboDict.keySet());
        List<String> deletedEans = new ArrayList<String>();
        for (String k : keys) {
            if (!this.zoneNames.contains(k)) {
                deletedEans.add(k);
                this.comboDict.remove(k);
            }
        }

        if ((deletedEans.size() > 0) && (this.lclFmtrDialog != null)) {
            this.lclFmtrDialog.setStatusText("S",
                    "Removed Unknown Edit Areas: " + deletedEans);
        }
    }

    // sets the zone label to the given group name, does not affect combinations
    private void setZoneGroupLabel(String zone, int group) {
        String text;
        if (group == NO_GROUP) {
            text = "";
        } else {
            text = Integer.toString(group);
        }

        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setZoneGroupLabel(zone, text);
        }

    }

    // sets the zone to the given color, does not affect the combinations
    private void setZone(String zoneName, RGB color) {
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setZone(color, zoneName);
        }

        // this.update_idletasks();

        // enable Apply button
        if (this.lclFmtrDialog != null) {
            this.lclFmtrDialog.applyButtonState(true);
        }
    }

    // adds in unmentioned zones to the combo dictionary if include flag is set.
    private void includeUnmentionedZones(boolean includeAllZones) {
        if (!includeAllZones) {
            return;
        }
        List<String> added = new ArrayList<String>();
        Set<String> keys = this.comboDict.keySet();
        for (String z : this.zoneNames) {
            if (!keys.contains(z)) {
                this.comboDict.put(z, this.getNewGroup());
                if (this.lclFmtrDialog != null) {
                    this.lclFmtrDialog.applyButtonState(true);
                }
                added.add(z);
            }
        }
        if (added.size() > 0) {
            if (this.lclFmtrDialog != null) {
                this.lclFmtrDialog.setStatusText("R",
                        "Missing zones have been added " + added.toString());
            }
        }
    }

    // converts the incoming comboDict to only have 1 group.
    private Map<String, Integer> setOneGroup(Map<String, Integer> comboDict) {
        for (String k : comboDict.keySet()) {
            comboDict.put(k, 1); // everything gets the 1st group.
        }
        return comboDict;
    }

    // gets Config() information.
    private String config(String configName, String defaultVal) {
        // If the configName is set in AFPS.AFPSConfig(), then return
        // its value. Otherwise, return the defaultVal
        String retVal = defaultVal;
        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        if (prefs.contains(configName)) {
            retVal = prefs.getString(configName);
        }

        return retVal;
    }

    /**
     * @return the zone groupings
     */
    public List<List<String>> getZoneGroupings() {
        Map<String, Integer> comboDict = getCombos();

        Map<Integer, List<String>> reverseDict = new HashMap<Integer, List<String>>();
        for (Entry<String, Integer> entry : comboDict.entrySet()) {
            List<String> list = reverseDict.get(entry.getValue());
            if (list == null) {
                list = new ArrayList<String>();
                reverseDict.put(entry.getValue(), list);
            }
            list.add(entry.getKey());
        }

        List<Integer> keys = new ArrayList<Integer>(reverseDict.keySet());
        Collections.sort(keys);

        List<List<String>> groupings = new ArrayList<List<String>>(
                reverseDict.size());
        for (Integer key : keys) {
            groupings.add(reverseDict.get(key));
        }
        return groupings;
    }

    /**
     * @return the no zone color
     */
    public RGB getNoZoneColor() {
        return this.noZoneColor;
    }

}
