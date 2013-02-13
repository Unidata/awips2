package com.raytheon.viz.warngen.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.lang.ArrayUtils;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.Bullet;
import com.raytheon.uf.common.dataplugin.warning.config.DamInfoBullet;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;

/**
 * 
 * BulletLIstManager.java
 * 
 * Manages the selection of the individual bullets that might be a part of a
 * multi-selection or a single-selection group.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2011             jsanchez    Initial creation
 * 
 * 01/26/2012   14466      D.Friedman  Fix parseString processing.
 * 01/26/2012   14469      D.Friedman  Fix followup bullet processing
 * Feb 13, 2013 1606       jsanchez    Did not set default bullets for CORs.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class BulletListManager {

    private static final String TITLE = "title";

    private static final String TRUE = "true";

    private static final String DAM = "dam";

    private static final String SCENARIO = "scenario";

    private static final String IC = "ic";

    private Bullet[] bullets;

    private Set<Integer> selectedIndices;

    private ArrayList<Integer> titleGroup;

    private Map<String, List<Integer>> bulletGroups;

    private Map<String, List<Integer>> damGroups;

    private ArrayList<String> lockedGroups;

    private ArrayList<String> mapsToLoad;

    private boolean isDamCauseSelected = false;

    private int selectedDamIndex;

    public BulletListManager() {
        selectedIndices = new TreeSet<Integer>();
        titleGroup = new ArrayList<Integer>();
        bulletGroups = new TreeMap<String, List<Integer>>();
        damGroups = new TreeMap<String, List<Integer>>();
        mapsToLoad = new ArrayList<String>();
        lockedGroups = new ArrayList<String>();
        clear();
    }

    /**
     * Separates all the bullets in it's corresponding group. Identifies all the
     * bullets that should be selected by default.
     * 
     * @param bullets
     * @param damInfoBullets
     */
    public void recreateBullets(Bullet[] bullets, DamInfoBullet[] damInfoBullets) {
        recreateBullets(bullets, damInfoBullets, null);
    }

    /**
     * Separates all the bullets in it's corresponding group. Identifies all the
     * bullets that should be selected by default unless the action is a COR.
     * 
     * @param bullets
     * @param damInfoBullets
     * @param action
     */
    public void recreateBullets(Bullet[] bullets,
            DamInfoBullet[] damInfoBullets, WarningAction action) {
        loadBullets(bullets, damInfoBullets);
        clear();
        String damName = null;
        Set<Integer> defaultIndices = new TreeSet<Integer>();
        for (int i = 0; i < this.bullets.length; i++) {
            Bullet b = this.bullets[i];
            if (b.getBulletType() != null
                    && b.getBulletType().equalsIgnoreCase(TITLE)) {
                titleGroup.add(i);
            } else if (b.getBulletGroup() != null) {
                List<Integer> indices = bulletGroups.get(b.getBulletGroup());
                if (indices == null) {
                    indices = new ArrayList<Integer>();
                }
                indices.add(i);
                bulletGroups.put(b.getBulletGroup(), indices);

                /* Stores all the screnario indices for a dam group */
                if (b.getBulletGroup().equalsIgnoreCase("DAM")) {
                    damName = b.getBulletName();
                } else if (damName != null
                        && b.getBulletGroup().equalsIgnoreCase(SCENARIO)) {
                    List<Integer> scenarioIndices = damGroups.get(damName);
                    if (scenarioIndices == null) {
                        scenarioIndices = new ArrayList<Integer>();
                    }
                    scenarioIndices.add(i);
                    damGroups.put(damName, scenarioIndices);
                }
            }

            if (b.getBulletDefault() != null
                    && b.getBulletDefault().equalsIgnoreCase(TRUE)) {
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
        ArrayList<Bullet> displayedDamInfoBullets = null;

        for (int pass = 0; pass < 2; ++pass) {
            Bullet[] sourceList = pass == 0 ? configuration.getBullets()
                    : configuration.getDamInfoBullets();
            ArrayList<Bullet> resultList = new ArrayList<Bullet>();
            if (sourceList != null) {
                for (Bullet b : sourceList) {
                    if (b != null
                            && (b.getShowString() == null || selectBulletFromFollowup(
                                    b.getShowString(), warningText))) {
                        resultList.add(b);
                    }
                }
            }
            if (pass == 0)
                displayedBullets = resultList;
            else
                displayedDamInfoBullets = resultList;
        }

        /* Sets up the appropriate bullet groups */
        recreateBullets(displayedBullets.toArray(new Bullet[displayedBullets
                .size()]),
                displayedDamInfoBullets
                        .toArray(new DamInfoBullet[displayedDamInfoBullets
                                .size()]), action);

        if (configuration.getLockedGroupsOnFollowup() != null) {
            for (String lockedGroup : configuration.getLockedGroupsOnFollowup()
                    .split(",")) {
                if (lockedGroup != null && lockedGroup.length() != 0) {
                    lockedGroups.add(lockedGroup.toLowerCase());
                }
            }
        }

        /* Updates the selection based on the 'parseString' test */
        for (int i = 0; i < bullets.length; i++) {
            Bullet bullet = bullets[i];
            if (selectBulletFromFollowup(bullet.getParseString(), warningText)) {
                updateSelectedIndices(i, false, true);
            }

            if (bullet.getFloodSeverity() != null
                    && bullet.getFloodSeverity().equals(
                            record.getFloodSeverity())) {
                updateSelectedIndices(i, false, true);
            }
        }
    }

    /**
     * Returns the maps to load with the bullet list.
     * 
     * @return
     */
    public ArrayList<String> getMapsToLoad() {
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
        if (selectionIndex < 0 || selectionIndex >= bullets.length
                || titleGroup.contains(selectionIndex)) {
            return;
        }

        Bullet bullet = bullets[selectionIndex];
        String group = bullet.getBulletGroup();

        if (group == null) {
            if (selectUnconditionally) {
                if (!selectedIndices.contains(selectionIndex))
                    selectedIndices.add(selectionIndex);
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

        /* Can't change selection when a part of a locked group on a follow up */
        if (isFollowup && lockedGroups.contains(group.toLowerCase())) {
            return;
        }

        if (!isDamCauseSelected
                && bullet.getBulletName() != null
                && (bullet.getBulletName().equalsIgnoreCase("siteimminent") || bullet
                        .getBulletName().equalsIgnoreCase("sitefailed"))) {
            isDamCauseSelected = true;
        } else if (group.equalsIgnoreCase(DAM)) {
            /* Unselect the scenarios */
            if (selectedDamIndex != -1 && selectionIndex != selectedDamIndex) {
                clearScenarios(selectedDamIndex);
            }
            selectedDamIndex = selectionIndex;
        }

        /*
         * a scenario can only be selected if a dam has already been selected
         * and if it falls under the corresponding dam group
         */
        if (group.equalsIgnoreCase(SCENARIO)) {
            if (selectedDamIndex != -1) {
                List<Integer> scenarioIndices = damGroups
                        .get(bullets[selectedDamIndex].getBulletName());
                if (scenarioIndices != null
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
                    if (!selectedIndices.contains(selectionIndex))
                        selectedIndices.add(index);
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
     * @return
     */
    public String[] getSelectedBulletNames() {
        String[] selectedBulletNames = new String[selectedIndices.size()];
        Iterator<Integer> iterator = selectedIndices.iterator();

        int counter = 0;
        while (iterator.hasNext()) {
            selectedBulletNames[counter++] = bullets[iterator.next()]
                    .getBulletName();
        }

        return selectedBulletNames;
    }

    /**
     * An array of all the bullet texts that are being managed.
     * 
     * @return
     */
    public String[] getAllBulletTexts() {
        String[] selectedBulletTexts = new String[bullets.length];
        for (int i = 0; i < bullets.length; i++) {
            selectedBulletTexts[i] = bullets[i].getBulletText();
        }

        return selectedBulletTexts;
    }

    /**
     * An array of all the inidices that are currently selected.
     * 
     * @return
     */
    public int[] getSelectedIndices() {
        int counter = 0;
        int[] indices = new int[selectedIndices.size()];
        Iterator<Integer> iterator = selectedIndices.iterator();

        while (iterator.hasNext()) {
            indices[counter++] = iterator.next().intValue();
        }

        return indices;
    }

    public boolean isDamCauseSelected() {
        return isDamCauseSelected;
    }

    public boolean isDamNameSeletcted() {
        return selectedDamIndex != -1;
    }

    /**
     * Returns the DamInfoBullet object of the selected dam bullet. Returns null
     * otherwise.
     * 
     * @return
     */
    public DamInfoBullet getSelectedDamInfoBullet() {
        DamInfoBullet bullet = null;
        if (selectedDamIndex != -1) {
            bullet = (DamInfoBullet) bullets[selectedDamIndex];
        }
        return bullet;
    }

    private void clear() {
        selectedIndices.clear();
        titleGroup.clear();
        lockedGroups.clear();
        bulletGroups.clear();
        mapsToLoad.clear();
        isDamCauseSelected = false;
        selectedDamIndex = -1;
    }

    private void loadBullets(Bullet[] bullets, DamInfoBullet[] damInfoBullets) {
        this.bullets = bullets;
        if (damInfoBullets != null) {
            this.bullets = (Bullet[]) ArrayUtils
                    .addAll(bullets, damInfoBullets);
        }
    }

    private boolean selectBulletFromFollowup(String parseString,
            String warningText) {
        if (parseString == null || parseString.length() == 0) {
            return false;
        }

        boolean selectBullet = true;
        for (String p : parseString.toUpperCase().replaceAll("\\s+", " ")
                .split("\",")) {
            p = p.replace("\"", "");
            if ((p.startsWith("-") && warningText.contains(p.substring(1)))
                    || (p.startsWith("-") == false && warningText.contains(p) == false)) {
                selectBullet = false;
                break;
            }
        }

        return selectBullet;
    }

    private void clearScenarios(int damIndex) {
        if (damIndex == selectedDamIndex) {
            selectedDamIndex = -1;
        }
        List<Integer> scenarioIndices = damGroups.get(bullets[damIndex]
                .getBulletName());
        if (scenarioIndices != null && scenarioIndices.size() > 0) {
            selectedIndices.removeAll(scenarioIndices);
        }
    }
}
