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
package com.raytheon.viz.gfe.core.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.weatherelement.WEGroup;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IWEGroupManager;

/**
 * The WEGroupManager is a class which manages WEGroups.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 5, 2008				chammack	Initial creation
 * Mar 06,2013  15717       jzeng       Change CAVE_STATIC to COMMON_STATIC 
 *                                      for GFE localization files 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class WEGroupManager implements IWEGroupManager,
        ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WEGroupManager.class);

    private String WEGROUP_DIR = "gfe/weGroups";

    private static final String SUFFIX = ".xml";

    private DataManager dataManager;

    private Map<LocalizationLevel, Set<String>> inventory;

    private final Comparator<String> comparator;

    private List<String> sortOrder;

    private LocalizationFile weGroupDir;

    public WEGroupManager(final DataManager dataManager) {
        this.dataManager = dataManager;

        loadGroups();

        IPathManager pathManager = PathManagerFactory.getPathManager();
        weGroupDir = pathManager.getLocalizationFile(pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                WEGROUP_DIR);
        weGroupDir.addFileUpdatedObserver(this);

        this.sortOrder = Arrays.asList(Activator.getDefault()
                .getPreferenceStore().getStringArray("WEList"));
        if (sortOrder == null) {
            sortOrder = new ArrayList<String>();
        }

        this.comparator = new Comparator<String>() {

            @Override
            public int compare(String o1, String o2) {
                int idx1 = sortOrder.indexOf(o1);
                int idx2 = sortOrder.indexOf(o2);

                if (idx1 == -1) {
                    idx1 = sortOrder.size();
                }
                if (idx2 == -1) {
                    idx2 = sortOrder.size();
                }
                if (idx1 == idx2) {
                    return o1.compareTo(o2);
                } else {
                    return idx1 - idx2;
                }
            }

        };
    }

    private void loadGroups() {
        IPathManager pathManager = PathManagerFactory.getPathManager();

        LocalizationContext[] contexts = pathManager
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        LocalizationFile[] files = pathManager.listFiles(contexts, WEGROUP_DIR,
                new String[] { ".xml" }, false, true);

        this.inventory = new LinkedHashMap<LocalizationLevel, Set<String>>();
        for (LocalizationFile lf : files) {
            try {
                File file = lf.getFile(false);
                String fn = file.getName();
                LocalizationLevel levelType = lf.getContext()
                        .getLocalizationLevel();

                Set<String> inventoryList = this.inventory.get(levelType);
                if (inventoryList == null) {
                    inventoryList = new HashSet<String>();
                    this.inventory.put(levelType, inventoryList);
                }
                String s = construct(fn);
                if (s != null) {
                    inventoryList.add(s);
                }
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving WE Group inventory", e);
            }
        }
    }

    private String construct(String fileName) {
        // return the display name given the file name
        String s = fileName.replace(SUFFIX, "");
        if (!FileUtil.isValidFilename(s)) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Weather Element Group name \""
                                    + s
                                    + SUFFIX
                                    + "\" is invalid. Only the following characters may be used: "
                                    + FileUtil.VALID_FILENAME_CHARS);
            return null;
        }
        return s;
    }

    private String getName(String displayName) {
        return displayName + SUFFIX;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IWEGroupManager#save(java.lang.String,
     * com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID[],
     * com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID[])
     */
    @Override
    public void save(String name, ParmID[] parmIDs, ParmID[] availableParmIDs) {
        WEGroup group = new WEGroup(name, parmIDs, availableParmIDs);
        this.save(name, group);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.IWEGroupManager#remove(java.lang.String
     * )
     */
    public boolean remove(String name) {
        return this.delete(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.IWEGroupManager#getParmIDs(java.lang
     * .String, com.raytheon.edex.plugin.gfe.db.objects.ParmID[])
     */
    public ParmID[] getParmIDs(String name, ParmID[] availableParmIDs) {
        LocalizationFile file = this.getLocalizationFile(name);
        if (file == null || !file.exists()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading weather element group \"" + name
                            + "\". File does not exist");
            return new ParmID[0];
        }

        WEGroup weGroup = null;
        try {
            weGroup = (WEGroup) SerializationUtil.jaxbUnmarshalFromXmlFile(file
                    .getFile().getPath());
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting weather element group", e);
        }

        return getParmIDs(weGroup, availableParmIDs);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.IWEGroupManager#getParmIDs(com.raytheon
     * .edex.plugin.gfe.weatherElement.WEGroup,
     * com.raytheon.edex.plugin.gfe.db.objects.ParmID[])
     */
    public ParmID[] getParmIDs(final WEGroup bundle,
            final ParmID[] availableParmIDs) {

        bundle.rectifyForSite(this.dataManager.getSiteID(), availableParmIDs);
        // Append the ParmIDs to the returning argument
        List<ParmID> parmIDs = new ArrayList<ParmID>();
        for (int i = 0; i < bundle.getWeItems().length; i++) {
            ParmID parmID = bundle.getWeItems()[i].getParmID();
            if (parmID != null) {
                parmIDs.add(parmID);
            }
        }

        CAVEMode mode = CAVEMode.getMode();
        if (mode.equals(CAVEMode.TEST) || mode.equals(CAVEMode.PRACTICE)) {

            DatabaseID practiceDB = this.dataManager.getParmManager()
                    .getMutableDatabase();
            DatabaseID origMutableDB = this.dataManager.getParmManager()
                    .getOrigMutableDatabase();
            for (int i = 0; i < parmIDs.size(); i++) {
                if (parmIDs.get(i).getDbId().equals(origMutableDB)) {
                    parmIDs.set(i, new ParmID(parmIDs.get(i).getParmName(),
                            practiceDB, parmIDs.get(i).getParmLevel()));
                }
            }
        }
        return parmIDs.toArray(new ParmID[parmIDs.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.AbstractFileBasedManager#getInventory
     * ()
     */
    @Override
    public List<String> getInventory() {
        List<String> completeList = new ArrayList<String>();

        for (LocalizationLevel lt : this.inventory.keySet()) {

            Set<String> subList = this.inventory.get(lt);

            if (subList != null) {
                for (String item : subList) {
                    if (!completeList.contains(item)) {
                        completeList.add(item);
                    }
                }
            }
        }

        Collections.sort(completeList, comparator);

        return Collections.unmodifiableList(completeList);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IWEGroupManager#getDefaultGroup()
     */
    @Override
    public String getDefaultGroup() {
        String defaultGroup = Activator.getDefault().getPreferenceStore()
                .getString("DefaultGroup");
        if (defaultGroup == null || defaultGroup.isEmpty()) {
            defaultGroup = "Public";
        }
        return defaultGroup;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IWEGroupManager#isProtected(java.lang.String )
     */
    @Override
    public boolean isProtected(String name) {
        LocalizationFile file = getLocalizationFile(name);
        if (file != null) {
            return file.isProtected();
        } else {
            return false;
        }
    }

    private boolean delete(String object) {
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
                        FileUtil.join(WEGROUP_DIR, getName(object)));

        LocalizationContext context = file.getContext();
        if (context.getLocalizationLevel() != LocalizationLevel.USER) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to delete Weather Element Group, because it is not owned by you.");
            return false;
        }

        try {
            file.delete();
            inventory.get(context.getLocalizationLevel()).remove(object);
            return true;
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting from localization server", e);
        }

        return false;

    }

    private void save(String key, WEGroup objectToSave) {

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);

        LocalizationFile file = pm.getLocalizationFile(lc,
                FileUtil.join(WEGROUP_DIR, getName(key)));
        try {
            SerializationUtil.jaxbMarshalToXmlFile(objectToSave, file.getFile()
                    .getPath());
            file.save();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving to localization server", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error serializing to file",
                    e);
        }

        Set<String> list = this.inventory.get(LocalizationLevel.USER);
        if (list == null) {
            list = new HashSet<String>();
            this.inventory.put(LocalizationLevel.USER, list);
        }
        list.add(key);
    }

    private LocalizationFile getLocalizationFile(String displayName) {
        // return localization file given display name
        LocalizationFile lf = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(
                        FileUtil.join(WEGROUP_DIR, getName(displayName)));

        return lf;
    }

    /**
     * Returns the inventory of user context only content
     * 
     * @return user inventory
     */
    public List<String> getUserInventory() {
        List<String> completeList = new ArrayList<String>();
        Set<String> userInv = this.inventory.get(LocalizationLevel.USER);
        if (userInv != null) {
            completeList.addAll(userInv);
        }
        return Collections.unmodifiableList(completeList);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        loadGroups();
    }

}
