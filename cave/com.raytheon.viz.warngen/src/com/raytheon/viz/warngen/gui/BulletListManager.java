package com.raytheon.viz.warngen.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.lang3.ArrayUtils;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.Bullet;
import com.raytheon.uf.common.dataplugin.warning.config.PresetInfoBullet;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;

/**
 *
 * BulletListManager.java
 *
 * Manages the selection of the individual bullets that might be a part of a
 * multi-selection or a single-selection group.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Oct 05, 2011           jsanchez    Initial creation
 * Jan 26, 2012  14466    D.Friedman  Fix parseString processing.
 * Jan 26, 2012  14469    D.Friedman  Fix followup bullet processing
 * Feb 13, 2013  1606     jsanchez    Did not set default bullets for CORs.
 * May 29, 2015  4443     randerso    Fix parseString/showString for mixed case
 * Aug 29, 2017  6328     randerso    Convert to use PresetInfoBullet
 * Jan 12, 2017  7191     Robert.Blum Fix parseString so that if it contained a comma or ellipsis, 
 *                                    it would also match against the opposite type.
 *
 * </pre>
 *
 * @author jsanchez
 */
public class BulletListManager {

    private static final String TITLE = "title";

    private static final String DAM = "dam";

    private static final String SCENARIO = "scenario";

    private Bullet[] bullets;

    private Set<Integer> selectedIndices;

    private ArrayList<Integer> titleGroup;

    private Map<String, List<Integer>> bulletGroups;

    private Map<String, List<Integer>> damScenarios;

    private ArrayList<String> lockedGroups;

    private ArrayList<String> mapsToLoad;

    private int selectedPresetIndex;

    /**
     * Constructor
     */
    public BulletListManager() {
        selectedIndices = new TreeSet<>();
        titleGroup = new ArrayList<>();
        bulletGroups = new TreeMap<>();
        damScenarios = new TreeMap<>();
        mapsToLoad = new ArrayList<>();
        lockedGroups = new ArrayList<>();
        clear();
    }

    /**
     * Separates all the bullets in it's corresponding group. Identifies all the
     * bullets that should be selected by default.
     *
     * @param bullets
     * @param presetInfoBullets
     */
    public void recreateBullets(Bullet[] bullets,
            PresetInfoBullet[] presetInfoBullets) {
        recreateBullets(bullets, presetInfoBullets, null);
    }

    /**
     * Separates all the bullets in it's corresponding group. Identifies all the
     * bullets that should be selected by default unless the action is a COR.
     *
     * @param bullets
     * @param presetInfoBullets
     * @param action
     */
    public void recreateBullets(Bullet[] bullets,
            PresetInfoBullet[] presetInfoBullets, WarningAction action) {
        loadBullets(bullets, presetInfoBullets);
        clear();
        String damName = null;
        Set<Integer> defaultIndices = new TreeSet<>();
        for (int i = 0; i < this.bullets.length; i++) {
            Bullet b = this.bullets[i];
            if (TITLE.equalsIgnoreCase(b.getBulletType())) {
                titleGroup.add(i);
            } else if (b.getBulletGroup() != null) {
                List<Integer> indices = bulletGroups.get(b.getBulletGroup());
                if (indices == null) {
                    indices = new ArrayList<>();
                }
                indices.add(i);
                bulletGroups.put(b.getBulletGroup(), indices);

                /* Stores all the scenario indices for a dam group */
                if (DAM.equalsIgnoreCase(b.getBulletGroup())) {
                    damName = b.getBulletName();
                } else if (damName != null) {
                    if (b.getBulletGroup().equalsIgnoreCase(SCENARIO)) {
                        List<Integer> scenarioIndices = damScenarios
                                .get(damName);
                        if (scenarioIndices == null) {
                            scenarioIndices = new ArrayList<>();
                        }
                        scenarioIndices.add(i);
                        damScenarios.put(damName, scenarioIndices);
                    } else {
                        damName = null;
                    }
                }
            }

            if (b.isBulletDefault()) {
                defaultIndices.add(i);
            }

            if (b.getLoadMap() != null) {
                mapsToLoad.add(b.getLoadMap());
            }
        }

        if (action != WarningAction.COR) {
            for (Integer index : defaultIndices) {
                updateSelectedIndices(index, false);
            }
        }
    }

    /**
     * Updates the bullets that should be selected based on the warning text and
     * the parseStrings of each bullet.
     *
     * @param configuration
     * @param action
     * @param record
     */
    public void recreateBulletsFromFollowup(WarngenConfiguration configuration,
            WarningAction action, AbstractWarningRecord record) {
        /* Not a valid follow up */
        if (record == null) {
            return;
        }

        String warningText = record.getRawmessage().replaceAll("\\s+", " ");

        /* Test 'showString' to determine if the bullet is to be hidden */
        ArrayList<Bullet> displayedBullets = null;
        ArrayList<Bullet> displayedPresetInfoBullets = null;

        for (int pass = 0; pass < 2; ++pass) {
            Bullet[] sourceList = pass == 0 ? configuration.getBullets()
                    : configuration.getPresetInfoBullets();
            ArrayList<Bullet> resultList = new ArrayList<>();
            if (sourceList != null) {
                for (Bullet b : sourceList) {
                    if ((b != null) && ((b.getShowString() == null)
                            || selectBulletFromFollowup(b.getShowString(),
                                    warningText))) {
                        resultList.add(b);
                    }
                }
            }
            if (pass == 0) {
                displayedBullets = resultList;
            } else {
                displayedPresetInfoBullets = resultList;
            }
        }

        /* Sets up the appropriate bullet groups */
        recreateBullets(
                displayedBullets.toArray(new Bullet[displayedBullets.size()]),
                displayedPresetInfoBullets
                        .toArray(new PresetInfoBullet[displayedPresetInfoBullets
                                .size()]),
                action);

        if (configuration.getLockedGroupsOnFollowup() != null) {
            for (String lockedGroup : configuration.getLockedGroupsOnFollowup()
                    .split(",")) {
                if ((lockedGroup != null) && (lockedGroup.length() != 0)) {
                    lockedGroups.add(lockedGroup.toLowerCase());
                }
            }
        }

        /* Updates the selection based on the 'parseString' test */
        for (int i = 0; i < bullets.length; i++) {
            Bullet bullet = bullets[i];
            if (selectBulletFromFollowup(bullet.getParseString(),
                    warningText)) {
                updateSelectedIndices(i, false, true);
            }

            if ((bullet.getFloodSeverity() != null) && bullet.getFloodSeverity()
                    .equals(record.getFloodSeverity())) {
                updateSelectedIndices(i, false, true);
            }
        }
    }

    /**
     * Returns the maps to load with the bullet list.
     *
     * @return list of maps to load
     */
    public List<String> getMapsToLoad() {
        return mapsToLoad;
    }

    /**
     * Updates the list of selected of indices by including or removing indices
     * depending on if the bullet is already selected or is a part of a group of
     * bullets.
     *
     * @param selectionIndex
     * @param isFollowup
     */
    public void updateSelectedIndices(int selectionIndex, boolean isFollowup) {
        updateSelectedIndices(selectionIndex, isFollowup, false);
    }

    /**
     * Updates the list of selected of indices by including or removing indices
     * depending on if the bullet is already selected or is a part of a group of
     * bullets. If selectUnconditionally is true, sets (instead of toggles)
     * bullets.
     *
     * @param selectionIndex
     * @param isFollowup
     * @param selectUnconditionally
     */
    public void updateSelectedIndices(int selectionIndex, boolean isFollowup,
            boolean selectUnconditionally) {
        if ((selectionIndex < 0) || (selectionIndex >= bullets.length)
                || titleGroup.contains(selectionIndex)) {
            return;
        }

        Bullet bullet = bullets[selectionIndex];
        String group = bullet.getBulletGroup();

        if (group == null) {
            if (selectUnconditionally) {
                if (!selectedIndices.contains(selectionIndex)) {
                    selectedIndices.add(selectionIndex);
                }
            } else {
                // toggle
                if (selectedIndices.contains(selectionIndex)) {
                    selectedIndices.remove(selectionIndex);
                } else {
                    selectedIndices.add(selectionIndex);
                }
            }
            return;
        }

        /*
         * Can't change selection when a part of a locked group on a follow up
         */
        if (isFollowup && lockedGroups.contains(group.toLowerCase())) {
            return;
        }

        if (bullet instanceof PresetInfoBullet
                && ((PresetInfoBullet) bullet).getCoords() != null) {
            /* deselect the scenarios */
            if ((selectedPresetIndex != -1)
                    && (selectionIndex != selectedPresetIndex)) {
                clearScenarios(selectedPresetIndex);
            }
            selectedPresetIndex = selectionIndex;
        }

        /*
         * a scenario can only be selected if a preset has already been selected
         * and if it falls under the corresponding preset group
         */
        if (SCENARIO.equalsIgnoreCase(group)) {
            if (selectedPresetIndex != -1) {
                List<Integer> scenarioIndices = damScenarios
                        .get(bullets[selectedPresetIndex].getBulletName());
                if ((scenarioIndices != null)
                        && scenarioIndices.contains(selectionIndex)) {
                    if (selectedIndices.contains(selectionIndex)) {
                        selectedIndices.remove(selectionIndex);
                    } else {
                        selectedIndices.add(selectionIndex);
                    }
                }
            }
            return;
        }

        List<Integer> groupIndices = bulletGroups.get(group);
        for (Integer index : groupIndices) {
            /*
             * Unselect items in a group except for the latest selection in the
             * group
             */
            if (selectUnconditionally) {
                if (index.equals(selectionIndex)) {
                    if (!selectedIndices.contains(selectionIndex)) {
                        selectedIndices.add(index);
                    }
                } else {
                    selectedIndices.remove(index);
                    clearScenarios(index);
                }
            } else {
                // toggles off if selected
                if (!selectedIndices.contains(selectionIndex)
                        && index.equals(selectionIndex)) {
                    selectedIndices.add(index);
                } else {
                    selectedIndices.remove(index);
                    clearScenarios(index);
                }
            }
        }
    }

    /**
     * The array of bullet names that are currently selected.
     *
     * @return the selected bullet names
     */
    public String[] getSelectedBulletNames() {
        String[] selectedBulletNames = new String[selectedIndices.size()];
        Iterator<Integer> iterator = selectedIndices.iterator();

        int counter = 0;
        while (iterator.hasNext()) {
            selectedBulletNames[counter] = bullets[iterator.next()]
                    .getBulletName();
            counter++;
        }

        return selectedBulletNames;
    }

    /**
     * An array of all the bullet texts that are being managed.
     *
     * @return text of all bullets
     */
    public String[] getAllBulletTexts() {
        String[] selectedBulletTexts = new String[bullets.length];
        for (int i = 0; i < bullets.length; i++) {
            selectedBulletTexts[i] = bullets[i].getBulletText();
        }

        return selectedBulletTexts;
    }

    /**
     * An array of all the indices that are currently selected.
     *
     * @return the selected indices
     */
    public int[] getSelectedIndices() {
        int counter = 0;
        int[] indices = new int[selectedIndices.size()];
        Iterator<Integer> iterator = selectedIndices.iterator();

        while (iterator.hasNext()) {
            indices[counter] = iterator.next().intValue();
            counter++;
        }

        return indices;
    }

    /**
     *
     * @return true if a preset name is selected
     */
    public boolean isPresetNameSeletcted() {
        return selectedPresetIndex != -1;
    }

    /**
     * Returns the PresetInfoBullet object of the selected preset bullet.
     * Returns null otherwise.
     *
     * @return the selected preset info bullet
     */
    public PresetInfoBullet getSelectedPresetInfoBullet() {
        PresetInfoBullet bullet = null;
        if (selectedPresetIndex != -1) {
            bullet = (PresetInfoBullet) bullets[selectedPresetIndex];
        }
        return bullet;
    }

    private void clear() {
        selectedIndices.clear();
        titleGroup.clear();
        lockedGroups.clear();
        bulletGroups.clear();
        damScenarios.clear();
        mapsToLoad.clear();
        selectedPresetIndex = -1;
    }

    private void loadBullets(Bullet[] bullets,
            PresetInfoBullet[] presetInfoBullets) {
        this.bullets = bullets;
        if (presetInfoBullets != null) {
            this.bullets = ArrayUtils.addAll(bullets, presetInfoBullets);
        }
    }

    private boolean selectBulletFromFollowup(String parseString,
            String warningText) {
        warningText = warningText.toUpperCase();
        warningText = warningText.replace("...", ",");

        if ((parseString == null) || (parseString.length() == 0)) {
            return false;
        }
        parseString = parseString.toUpperCase();
        parseString = parseString.replace("...", ",");
        parseString = parseString.replaceAll("\\s+", " ");
        boolean selectBullet = true;
        for (String p : parseString.split("\",")) {
            p = p.replace("\"", "");
            if ((p.startsWith("-") && warningText.contains(p.substring(1)))
                    || (!p.startsWith("-") && !warningText.contains(p))) {
                selectBullet = false;
                break;
            }
        }

        return selectBullet;
    }

    private void clearScenarios(int presetIndex) {
        if (presetIndex == selectedPresetIndex) {
            selectedPresetIndex = -1;
        }
        List<Integer> scenarioIndices = damScenarios
                .get(bullets[presetIndex].getBulletName());
        if ((scenarioIndices != null) && (!scenarioIndices.isEmpty())) {
            selectedIndices.removeAll(scenarioIndices);
        }
    }
}
