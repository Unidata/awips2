package gov.noaa.nws.ncep.viz.localization;

//import gov.noaa.nws.ncep.viz.localization.adapter.NcepCAVELocalizationAdapter;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationConstants;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.application.ProgramArguments;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends Plugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.localization";

    // The shared instance
    private static Activator plugin;

    private String currentDesk, currentSite, currentUser, baseDirPath,
            caveDataDirPath;

    /*
     * the following variables for testing file synchronization only
     */
    private LocalizationContext caveNcepBaseLevelLocalizationContext;

    private LocalizationContext caveNcepUserLevelLocalizationContext;

    private LocalizationContext caveNcepDeskLevelLocalizationContext;

    private LocalizationContext caveNcepSiteLevelLocalizationContext;

    public String getCurrentDesk() {
        return currentDesk;
    }

    public String getCurrentSite() {
        return currentSite;
    }

    public String getCurrentUser() {
        return currentUser;
    }

    public String getBaseDirPath() {
        return baseDirPath;
    }

    public String getCaveDataDirPath() {
        return caveDataDirPath;
    }

    /*
     * New added getters
     */
    public LocalizationContext getCaveNcepBaseLevelLocalizationContext() {
        return caveNcepBaseLevelLocalizationContext;
    }

    public LocalizationContext getCaveNcepUserLevelLocalizationContext() {
        return caveNcepUserLevelLocalizationContext;
    }

    public LocalizationContext getCaveNcepDeskLevelLocalizationContext() {
        return caveNcepDeskLevelLocalizationContext;
    }

    public LocalizationContext getCaveNcepSiteLevelLocalizationContext() {
        return caveNcepSiteLevelLocalizationContext;
    }

    /**
     * The constructor
     */
    public Activator() {
        initialization();
    }

    private void initialization() {
        LocalizationManager localizationManager = LocalizationManager
                .getInstance();

        currentDesk = initializeCurrentDesk(localizationManager);
        currentSite = initializeCurrentSite(localizationManager);
        currentUser = initializeCurrentUser(localizationManager);
        baseDirPath = initializeBase(localizationManager);
        caveDataDirPath = initializeCaveDataDirPath(localizationManager);

        /*
         * new added logic to initialize new added variables
         */
        // caveNcepBaseLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // LocalizationContext.LocalizationLevel.BASE);
        //
        // caveNcepUserLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // LocalizationContext.LocalizationLevel.USER, getCurrentUser());
        //
        // // caveNcepDeskLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // // LocalizationContext.LocalizationLevel.DESK,
        // constructContextNameusingCurrentSiteAndDesk(getCurrentSite(),
        // // getCurrentDesk()));
        // caveNcepDeskLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // LocalizationContext.LocalizationLevel.SITE,
        // constructContextNameusingCurrentSiteAndDesk(getCurrentSite(),
        // getCurrentDesk()));
        //
        // caveNcepSiteLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // LocalizationContext.LocalizationLevel.SITE, getCurrentSite());
        caveNcepBaseLevelLocalizationContext = new LocalizationContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        caveNcepUserLevelLocalizationContext = new LocalizationContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.USER,
                constructContextNameUsingCurrentUser(getCurrentUser()));

        // caveNcepDeskLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_NCEP,
        // LocalizationContext.LocalizationLevel.DESK,
        // constructContextNameusingCurrentSiteAndDesk(getCurrentSite(),
        // getCurrentDesk()));
        caveNcepDeskLevelLocalizationContext = new LocalizationContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.SITE,
                constructContextNameUsingCurrentSiteAndDesk(getCurrentSite(),
                        getCurrentDesk()));

        caveNcepSiteLevelLocalizationContext = new LocalizationContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.SITE,
                constructContextNameUsingCurrentSite(getCurrentSite()));

        String[] topResourcesDirArray = ResourceBundleUtil
                .getNcepResourceDirInfo(",");
        // IPathManager pathManager = PathManagerFactory.getPathManager(new
        // NcepCAVELocalizationAdapter());
        for (String eachTopResourcesDirName : topResourcesDirArray) {
            // PathManagerFactory.getPathManager().getFile(caveNcepBaseLevelLocalizationContext,
            // eachTopResourcesDirName.trim());
            PathManagerFactory.getPathManager().getFile(
                    caveNcepUserLevelLocalizationContext,
                    eachTopResourcesDirName.trim());
            PathManagerFactory.getPathManager().getFile(
                    caveNcepSiteLevelLocalizationContext,
                    eachTopResourcesDirName.trim());
            PathManagerFactory.getPathManager().getFile(
                    caveNcepDeskLevelLocalizationContext,
                    eachTopResourcesDirName.trim());
        }
    }

    private String initializeCurrentDesk(LocalizationManager localizationManager) {
        /*
         * Now the priority is: 1. command line argument, 2. from UI preference
         * setting 3. system variable
         */
        String desk = ProgramArguments.getInstance().getString(
                LocalizationConstants.ACTIVE_DESK);
        if (!isValidDesk(desk))
            // desk = localizationManager.getCurrentDesk();
            if (!isValidDesk(desk))
                desk = System.getenv("ACTIVE_DESK");
        if (!isValidDesk(desk))
            desk = "none";
        else {
            /*
             * set the valid current desk value back to localization store
             * through LocalizationManager
             */
            // localizationManager.setCurrentDesk(desk);
        }
        // System.out.println("===================, the active_desk value is:" +
        // desk);
        return desk;
    }

    private boolean isValidDesk(String deskName) {
        boolean isValidDesk = false;
        if (!StringUtil.isStringEmpty(deskName)
                && !deskName.equalsIgnoreCase("none"))
            isValidDesk = true;
        return isValidDesk;
    }

    private String constructContextNameUsingCurrentSite(String currentSite) {
        // String contextName = LocalizationConstants.NCEP_DIR + File.separator
        // +
        // LocalizationConstants.SITE_DIR + File.separator + currentSite;
        // String contextName = LocalizationConstants.SITE_DIR + File.separator
        // + currentSite;
        String contextName = currentSite;
        return contextName;
    }

    private String constructContextNameUsingCurrentUser(String currentUser) {
        // String contextName = LocalizationConstants.NCEP_DIR + File.separator
        // +
        // LocalizationConstants.USER_DIR + File.separator + currentUser;
        // String contextName = LocalizationConstants.USER_DIR + File.separator
        // + currentUser;
        String contextName = currentUser;
        return contextName;
    }

    private String constructContextNameUsingCurrentSiteAndDesk(
            String currentSite, String currentDesk) {
        String contextName = currentSite + File.separator
                + LocalizationConstants.DESK_DIR + File.separator + currentDesk;
        // String contextName = LocalizationConstants.NCEP_DIR + File.separator
        // + currentSite + File.separator +
        // LocalizationConstants.DESK_DIR + File.separator + currentDesk;
        return contextName;
    }

    private String initializeCurrentSite(LocalizationManager localizationManager) {
        /*
         * Now the priority is: 1. command line argument, 2. from UI preference
         * setting 3. system variable
         */
        String site = ProgramArguments.getInstance().getString(
                LocalizationConstants.ACTIVE_SITE);
        if (!isValidSite(site))
            site = localizationManager.getCurrentSite();
        if (!isValidSite(site))
            site = System.getenv("ACTIVE_SITE");
        if (!isValidSite(site))
            site = "none";
        else {
            /*
             * set the valid current site value back to localization store
             * through LocalizationManager
             */
            localizationManager.setCurrentSite(site);
        }
        // System.out.println("===================, the active_site value is:" +
        // site);
        return site;
    }

    private boolean isValidSite(String siteName) {
        boolean isValidSite = false;
        if (!StringUtil.isStringEmpty(siteName)
                && !siteName.equalsIgnoreCase("none"))
            isValidSite = true;
        return isValidSite;
    }

    private String initializeCurrentUser(LocalizationManager localizationManager) {
        String user = localizationManager.getCurrentUser();
        if (StringUtil.isStringEmpty(user))
            user = System.getenv("ACTIVE_USER");
        if (StringUtil.isStringEmpty(user))
            user = "none";
        return user;
    }

    private String initializeBase(LocalizationManager localizationManager) {
        String baseDir = LocalizationManager.getBaseDir() + "etc"
                + File.separator;
        // System.out.println("=============, now, LocalizationManager.getUserDir() is="+LocalizationManager.getUserDir());
        if (StringUtil.isStringEmpty(baseDir))
            baseDir = System.getenv("ACTIVE_BASE");
        // System.out.println("=============, now, the base DIR is="+base);
        return baseDir;
    }

    private String initializeCaveDataDirPath(
            LocalizationManager localizationManager) {
        String caveDataDir = LocalizationManager.getUserDir();
        // System.out.println("=============, now, LocalizationManager.getBaseDir() is="+LocalizationManager.getBaseDir());
        if (StringUtil.isStringEmpty(caveDataDir))
            caveDataDir = System.getenv("ACTIVE_CAVE_DATA");
        return caveDataDir;
    }

    private void locationTest() {
        String configurationLocation = Platform.getConfigurationLocation()
                .getURL().getPath();
        System.out
                .println("Platform.getConfigurationLocation().getURL().getPath()="
                        + configurationLocation);

        String installLocation = Platform.getInstallLocation().getURL()
                .getPath();
        System.out.println("Platform.getInstallLocation().getURL().getPath()="
                + installLocation);

        String instanceLocation = Platform.getInstanceLocation().getURL()
                .getPath();
        System.out.println("Platform.getInstanceLocation().getURL().getPath()="
                + instanceLocation);

        String locationOSString = Platform.getLocation().toOSString();
        System.out.println("Platform.getLocation().toOSString()="
                + locationOSString);

        String locationToString = Platform.getLocation().toString();
        System.out.println("Platform.getLocation().toString()="
                + locationToString);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    private void displayFileInfo(File file) {
        if (file == null)
            System.out.println("=== The file is NULL!!!!!!");
        System.out.println("=====file.getAbsolutePath(): "
                + file.getAbsolutePath());
        System.out.println("=====file.getName(): " + file.getName());
        System.out.println("=====file.getParent(): " + file.getParent());
        System.out.println("=====file.getName(): " + file.getName());
        try {
            System.out.println("=====file.getCanonicalPath(): "
                    + file.getCanonicalPath());
        } catch (IOException ioe) {
            System.out
                    .println("###=== catch ioexception when calling file.getCanonicalPath(), error="
                            + ioe.getMessage());
        }
    }

}
