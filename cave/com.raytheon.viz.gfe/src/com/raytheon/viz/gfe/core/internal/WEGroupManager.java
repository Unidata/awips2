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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.weatherelement.WEGroup;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.protectedfiles.ProtectedFileLookup;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IWEGroupManager;

/**
 * The WEGroupManager is a class which manages WEGroups.
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 05, 2008           chammack  Initial creation
 * Sep 30, 2013  2361     njensen   Use JAXBManager for XML
 * Aug 13, 2015  4749     njensen   Implemented dispose()
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Feb 05, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Aug 07, 2017  6379     njensen   Use ProtectedFileLookup
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */

public class WEGroupManager
        implements IWEGroupManager, ILocalizationFileObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WEGroupManager.class);

    private String WEGROUP_DIR = "gfe/weGroups";

    private static final String SUFFIX = ".xml";

    private DataManager dataManager;

    private Map<LocalizationLevel, Set<String>> inventory;

    private final Comparator<String> comparator;

    private List<String> sortOrder;

    private LocalizationFile weGroupDir;

    private SingleTypeJAXBManager<WEGroup> jaxb;

    /**
     * Constructor
     *
     * @param dataManager
     */
    public WEGroupManager(final DataManager dataManager) {
        this.dataManager = dataManager;

        try {
            this.jaxb = new SingleTypeJAXBManager<>(WEGroup.class);
        } catch (JAXBException e) {
            statusHandler.error(
                    "Error initializing WEGroup JAXBManager, Weather Element Groups will not work",
                    e);
        }

        loadGroups();

        IPathManager pathManager = PathManagerFactory.getPathManager();
        weGroupDir = pathManager.getLocalizationFile(
                pathManager.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                WEGROUP_DIR);
        weGroupDir.addFileUpdatedObserver(this);

        this.sortOrder = Arrays.asList(GFEPreference.getStringArray("WEList"));
        if (sortOrder == null) {
            sortOrder = new ArrayList<>();
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
                .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
        ILocalizationFile[] files = pathManager.listFiles(contexts, WEGROUP_DIR,
                new String[] { ".xml" }, false, true);

        inventory = new LinkedHashMap<>();
        for (ILocalizationFile lf : files) {
            String fn = LocalizationUtil.extractName(lf.getPath());
            LocalizationLevel levelType = lf.getContext()
                    .getLocalizationLevel();

            Set<String> inventoryList = inventory.get(levelType);
            if (inventoryList == null) {
                inventoryList = new HashSet<>();
                inventory.put(levelType, inventoryList);
            }
            String s = construct(fn);
            if (s != null) {
                inventoryList.add(s);
            }
        }
    }

    private String construct(String fileName) {
        // return the display name given the file name
        String s = fileName.replace(SUFFIX, "");
        if (!FileUtil.isValidFilename(s)) {
            statusHandler.handle(Priority.ERROR,
                    "Weather Element Group name \"" + s + SUFFIX
                            + "\" is invalid. Only the following characters may be used: "
                            + FileUtil.VALID_FILENAME_CHARS);
            return null;
        }
        return s;
    }

    private String getName(String displayName) {
        return displayName + SUFFIX;
    }

    @Override
    public void save(String name, ParmID[] parmIDs, ParmID[] availableParmIDs) {
        WEGroup group = new WEGroup(name, parmIDs, availableParmIDs);
        this.save(name, group);
    }

    @Override
    public boolean remove(String name) {
        return this.delete(name);
    }

    @Override
    public ParmID[] getParmIDs(String name, ParmID[] availableParmIDs) {
        ILocalizationFile file = getLocalizationFile(name);
        if ((file == null) || !file.exists()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading weather element group \"" + name
                            + "\". File does not exist");
            return new ParmID[0];
        }

        WEGroup weGroup = null;
        try (InputStream inStream = file.openInputStream()) {
            weGroup = jaxb.unmarshalFromInputStream(inStream);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting weather element group", e);
            return new ParmID[0];
        }

        return getParmIDs(weGroup, availableParmIDs);
    }

    @Override
    public ParmID[] getParmIDs(final WEGroup bundle,
            final ParmID[] availableParmIDs) {

        bundle.rectifyForSite(this.dataManager.getSiteID(), availableParmIDs);
        // Append the ParmIDs to the returning argument
        List<ParmID> parmIDs = new ArrayList<>();
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

    @Override
    public List<String> getInventory() {
        List<String> completeList = new ArrayList<>();

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

    @Override
    public String getDefaultGroup() {
        String defaultGroup = GFEPreference.getString("DefaultGroup", "Public");
        return defaultGroup;
    }

    @Override
    public boolean isProtected(String name) {
        LocalizationFile file = getLocalizationFile(name);
        if (file != null) {
            return ProtectedFileLookup.isProtected(file);
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
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to delete Weather Element Group, because it is not owned by you.");
            return false;
        }

        try {
            file.delete();
            inventory.get(context.getLocalizationLevel()).remove(object);
            return true;
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting from localization server", e);
        }

        return false;

    }

    private void save(String key, WEGroup objectToSave) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        ILocalizationFile file = pm.getLocalizationFile(lc,
                FileUtil.join(WEGROUP_DIR, getName(key)));
        try (SaveableOutputStream out = file.openOutputStream()) {
            jaxb.marshalToStream(objectToSave, out);
            out.save();
        } catch (IOException e) {
            String msg = String.format("Error writing to WEGroup file %s: %s",
                    file, e.getLocalizedMessage());
            statusHandler.error(msg, e);
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving to localization server", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error serializing to file",
                    e);
        }

        Set<String> list = inventory.get(LocalizationLevel.USER);
        if (list == null) {
            list = new HashSet<>();
            inventory.put(LocalizationLevel.USER, list);
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
    @Override
    public List<String> getUserInventory() {
        List<String> completeList = new ArrayList<>();
        Set<String> userInv = this.inventory.get(LocalizationLevel.USER);
        if (userInv != null) {
            completeList.addAll(userInv);
        }
        return Collections.unmodifiableList(completeList);
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        loadGroups();
    }

    @Override
    public void dispose() {
        weGroupDir.removeFileUpdatedObserver(this);
    }

}
