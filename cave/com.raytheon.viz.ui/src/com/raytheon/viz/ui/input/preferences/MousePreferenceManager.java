/**
 * This software was developed and / or modified by Raytheon Company,
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
package com.raytheon.viz.ui.input.preferences;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.ui.UiPlugin;

/**
 * 
 * The MousePreferenceManager handles the preferences for mouse events. The
 * primary task of the manager is to map mouse events(clicks and drags and such)
 * to actions(such as panning and zooming). Every action should register an id
 * which allows it to be set in the MousePreferencePage. The manager allows the
 * preference page to set preferences and provides these preferences to
 * IInputHandlers. An input handler should use the handleEvent or other handle
 * functions to determine if the user has set a preference for the specific
 * action to occur for an event. The manager determines which perspective is
 * active, which profile to use for that perspective, and what preferences the
 * user has for that profile.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MousePreferenceManager {

    public static final String EXTENSION_POINT = "com.raytheon.uf.viz.ui.mouse.action";

    private static final String DEFAULT_PROFILE_ID = "default";

    private static final String P_PROFILE_IDS = "mouse.profile";

    private static final String T_PROFILE = "mouse.profiles.%s";

    private static final String T_PROFILE_NAME = T_PROFILE + ".name";

    private static final String T_PERSPECTIVE_PROFILE = "mouse.%s.profile";

    private static final String T_PREFERENCE_ID = T_PROFILE + ".%s";

    private static MousePreferenceManager mousePreferenceManager;

    public static MousePreferenceManager getInstance() {
        if (mousePreferenceManager == null) {
            mousePreferenceManager = new MousePreferenceManager();
        }
        return mousePreferenceManager;
    }

    private Map<String, String> profileNameToId = new HashMap<String, String>();

    private Map<String, String> profileIdToName = new HashMap<String, String>();

    private Map<String, String> perspectiveLabelToId = new HashMap<String, String>();

    private Map<String, String> perspectiveLabelToProfileId = new HashMap<String, String>();

    private IPerspectiveDescriptor activePerspective = null;

    private String activeProfile = null;

    private Map<String, MouseEvent[]> idToPreferences = new HashMap<String, MouseEvent[]>();

    private HierarchicalPreferenceStore prefStore;

    private MousePreferenceManager() {
        prefStore = (HierarchicalPreferenceStore) UiPlugin.getDefault()
                .getPreferenceStore();
        init();
    }

    private void init() {
        profileIdToName.clear();
        profileNameToId.clear();
        perspectiveLabelToId.clear();
        perspectiveLabelToProfileId.clear();
        // Initialize profiles
        String[] ids = prefStore.getStringArray(P_PROFILE_IDS);
        for (String id : ids) {
            String name = prefStore
                    .getString(String.format(T_PROFILE_NAME, id));
            profileIdToName.put(id, name);
            profileNameToId.put(name, id);
        }
        // Initialize perspectives
        if (PlatformUI.isWorkbenchRunning()) {
            IWorkbench workbench = PlatformUI.getWorkbench();
            for (IPerspectiveDescriptor perspective : workbench
                    .getPerspectiveRegistry().getPerspectives()) {
                String label = perspective.getLabel();
                String id = getPerspectiveId(perspective);
                String profile = getProfile(id);
                perspectiveLabelToId.put(label, id);
                perspectiveLabelToProfileId.put(label, profile);
            }
        }
    }

    /**
     * Determine if the action specified by id should handle the given mouse
     * event
     * 
     * @param id
     *            the id of the action
     * @param event
     *            the Specific MousePreference to check for
     * @return
     */
    public boolean handleEvent(String id, MouseEvent event) {
        MouseEvent[] events = getEvents(id);
        for (MouseEvent e : events) {
            if (e == event) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get all the events the user has selected for this id
     * 
     * @param id
     *            the id of an action
     * @return an array of all events the user has selected
     */
    public MouseEvent[] getEvents(String id) {
        IPerspectiveDescriptor perspective = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getPerspective();
        if (activePerspective != perspective) {
            activePerspective = perspective;
            activeProfile = perspectiveLabelToProfileId.get(perspective
                    .getLabel());
            idToPreferences.clear();
        }
        MouseEvent[] preferences = null;
        if (!idToPreferences.containsKey(id)) {
            preferences = getEventsInternal(activeProfile, id);
            idToPreferences.put(id, preferences);
        } else {
            preferences = idToPreferences.get(id);
        }
        return preferences;
    }

    /**
     * Determine if the action specified by id should handle a mouse click
     * 
     * @param id
     *            the preference id
     * @param button
     *            1=left, 2=middle, 3=right
     * @return
     */
    public boolean handleClick(String id, int button) {
        if (button == 1 && handleEvent(id, MouseEvent.LEFT_CLICK))
            return true;
        if (button == 2 && handleEvent(id, MouseEvent.MIDDLE_CLICK))
            return true;
        if (button == 3 && handleEvent(id, MouseEvent.RIGHT_CLICK))
            return true;
        return false;
    }

    /**
     * Determine if the action specified by id should handle a long mouse click
     * 
     * @param id
     *            the preference id
     * @param button
     *            1=left, 2=middle, 3=right
     * @return
     */
    public boolean handleLongClick(String id, int button) {
        if (button == 1 && handleEvent(id, MouseEvent.LONG_LEFT_CLICK))
            return true;
        if (button == 2 && handleEvent(id, MouseEvent.LONG_MIDDLE_CLICK))
            return true;
        if (button == 3 && handleEvent(id, MouseEvent.LONG_RIGHT_CLICK))
            return true;
        return false;
    }

    /**
     * Determine if the action specified by id should handle a mouse drag
     * 
     * @param id
     *            the preference id
     * @param button
     *            1=left, 2=middle, 3=right
     * @return
     */
    public boolean handleDrag(String id, int button) {
        if (button == 1 && handleEvent(id, MouseEvent.LEFT_DRAG))
            return true;
        if (button == 2 && handleEvent(id, MouseEvent.MIDDLE_DRAG))
            return true;
        if (button == 3 && handleEvent(id, MouseEvent.RIGHT_DRAG))
            return true;
        return false;
    }

    public String[] getProfileNames() {
        Set<String> names = profileNameToId.keySet();
        return names.toArray(new String[names.size()]);
    }

    public String getProfileName(String perspectiveLabel) {
        String profileId = perspectiveLabelToProfileId.get(perspectiveLabel);
        return profileIdToName.get(profileId);
    }

    public void setProfile(String perspectiveLabel, String profileName) {
        activePerspective = null;
        activeProfile = null;
        String perspectiveId = perspectiveLabelToId.get(perspectiveLabel);
        String prefId = String.format(T_PERSPECTIVE_PROFILE, perspectiveId);
        String profileId = profileNameToId.get(profileName);
        prefStore.setValue(prefId, profileId);
        perspectiveLabelToProfileId.put(perspectiveLabel, profileId);
    }

    public void setEvents(String profileName, String id, MouseEvent[] prefs) {
        // More checking is too much work, it will get repopulated soon enough
        idToPreferences.remove(id);
        String profileId = profileNameToId.get(profileName);
        String prefId = String.format(T_PREFERENCE_ID, profileId, id);
        prefStore.setValue(prefId, MouseEvent.toStringArray(prefs));
    }

    public MouseEvent[] getEvents(String profileName, String id) {
        String profileId = profileNameToId.get(profileName);
        String prefId = String.format(T_PREFERENCE_ID, profileId, id);
        String[] prefs = prefStore.getStringArray(prefId);
        if (prefs == null || prefs.length == 0) {
            return getDefaultEventsFromExtension(id);
        }
        return MouseEvent.fromStringArray(prefs);
    }

    public void restoreDefaults() {
        prefStore.clearUserOverrides("mouse");
        init();
    }

    public void save() throws IOException {
        prefStore.save();
    }

    public void duplicateProfile(String origProfile, String newProfile) {
        // If we already have a profile by that name we will override it.
        if (!profileNameToId.containsKey(newProfile)) {
            generateNewProfileId(newProfile);
        }
        IConfigurationElement[] config = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(EXTENSION_POINT);
        for (IConfigurationElement e : config) {
            String id = e.getAttribute("id");
            setEvents(newProfile, id, getEvents(origProfile, id));
        }
    }

    public void exportProfile(String file, String profileName)
            throws ConfigurationException {
        String profileId = profileNameToId.get(profileName);
        XMLConfiguration export = new XMLConfiguration();
        IConfigurationElement[] config = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(EXTENSION_POINT);
        for (IConfigurationElement e : config) {
            String id = e.getAttribute("id");
            String prefId = String.format(T_PREFERENCE_ID, profileId, id);
            export.setProperty(id, prefStore.getStringArray(prefId));
        }
        export.setProperty("name", profileName);
        export.save(file);
    }

    public String importProfile(String file) throws ConfigurationException {
        XMLConfiguration impor = new XMLConfiguration(file);
        String profileName = impor.getString("name");
        String profileId = null;
        // Dont create a new profile if we have a matching name
        if (profileNameToId.containsKey(profileName)) {
            profileId = profileNameToId.get(profileName);
        } else {
            profileId = generateNewProfileId(profileName);
        }
        IConfigurationElement[] config = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(EXTENSION_POINT);
        for (IConfigurationElement e : config) {
            String id = e.getAttribute("id");
            String prefId = String.format(T_PREFERENCE_ID, profileId, id);
            String[] pref = impor.getStringArray(id);
            prefStore.clearUserOverrides(prefId);
            prefStore.setValue(prefId, pref);
        }
        return profileName;
    }

    private MouseEvent[] getDefaultEventsFromExtension(String id) {
        IConfigurationElement[] config = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(EXTENSION_POINT);
        for (IConfigurationElement e : config) {
            if (id.equals(e.getAttribute("id"))) {
                String defaults = e.getAttribute("default");
                if (defaults != null) {
                    return MouseEvent.fromListString(defaults);
                }
            }
        }
        return new MouseEvent[] { MouseEvent.DISABLED };
    }

    private String generateNewProfileId(String profileName) {
        String profileId = profileName.toLowerCase()
                .replaceAll("[^a-z0-9]", "");
        if (profileIdToName.containsKey(profileId)) {
            String newProfileId = profileId;
            for (int i = 1; profileIdToName.containsKey(newProfileId); i++) {
                newProfileId = profileId + String.valueOf(i);
            }
            profileId = newProfileId;
        }
        profileIdToName.put(profileId, profileName);
        profileNameToId.put(profileName, profileId);
        prefStore.setValue(String.format(T_PROFILE_NAME, profileId),
                profileName);
        Set<String> profiles = profileIdToName.keySet();
        prefStore.setValue(P_PROFILE_IDS,
                profiles.toArray(new String[profiles.size()]));
        return profileId;
    }

    /**
     * Internally use Ids rather than names, they are faster
     * 
     * @param profileId
     * @param id
     * @return
     */
    private MouseEvent[] getEventsInternal(String profileId, String id) {
        String prefId = String.format(T_PREFERENCE_ID, profileId, id);
        String[] prefs = prefStore.getStringArray(prefId);
        if (prefs == null || prefs.length == 0) {
            return getDefaultEventsFromExtension(id);
        }
        return MouseEvent.fromStringArray(prefs);
    }

    private String getPerspectiveId(IPerspectiveDescriptor perspective) {
        String perspId = perspective.getId();
        int lastDot = perspId.lastIndexOf(".");
        return perspId.substring(lastDot + 1);
    }

    private String getProfile(String perspectiveId) {
        String profile = prefStore.getString(String.format(
                T_PERSPECTIVE_PROFILE, perspectiveId));
        if (profile == null || profile.isEmpty()) {
            profile = DEFAULT_PROFILE_ID;
        }
        return profile;
    }

}
