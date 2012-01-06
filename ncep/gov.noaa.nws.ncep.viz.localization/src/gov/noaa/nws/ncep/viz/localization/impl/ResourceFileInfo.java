package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.viz.localization.Activator;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.exception.ParameterValidationException;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Map;

public class ResourceFileInfo implements ILoadedFileResourceInfo<File> {
    private NcepLogger logger = NcepLoggerManager
            .getNcepLogger(this.getClass());

    private String localPath, basePath, sitePath, deskPath, userPath,
            userSitePath, userSiteDeskPath;

    private FilenameFilter filenameFilter;

    private String individualDirPortion;

    private String individualFilename;

    private String resourceNamePattern;

    private int resourceNamePatternMatchStyle;

    private int localizationLevel;

    public ResourceFileInfo() {
        this(null, null);
    }

    // public ResourceFileInfo(String rootDirPortion) {
    public ResourceFileInfo(String baseRootDirPortion,
            String siteOrUserRootDirPortion) {
        // String defaultRootDirPrefix =
        // Platform.getUserLocation().getURL().getPath();
        // String defaultRootDirPrefix = "";
        // if(StringUtil.isStringEmpty(rootDirPortion))
        // rootDirPortion = defaultRootDirPrefix +
        // LocalizationConstants.DEFAULT_ROOT_DIR_PORTION;
        // else
        // rootDirPortion = defaultRootDirPrefix + rootDirPortion;
        if (StringUtil.isStringEmpty(baseRootDirPortion))
            baseRootDirPortion = LocalizationConstants.DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_BASE;
        if (StringUtil.isStringEmpty(siteOrUserRootDirPortion))
            siteOrUserRootDirPortion = LocalizationConstants.DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_SITE_OR_USER;
        /*
         * The following code is for getting the CAVE installation DIR
         */
        // Platform.getInstallLocation().getURL().getPath();

        localPath = LocalizationConstants.LOCAL_DIR;

        basePath = Activator.getDefault().getBaseDirPath() + baseRootDirPortion
                + File.separator;

        // sitePath = Activator.getDefault().getBaseDirPath()
        // + rootDirPortion + File.separator
        // + LocalizationConstants.SITE_DIR
        // + File.separator + Activator.getDefault().getCurrentSite();
        sitePath = Activator.getDefault().getCaveDataDirPath()
                + siteOrUserRootDirPortion + File.separator
                + LocalizationConstants.SITE_DIR + File.separator
                + Activator.getDefault().getCurrentSite();

        /*
         * Now the file structure is DESK under Site
         */
        // deskPath = Activator.getDefault().getBaseDirPath()
        // + rootDirPortion + File.separator
        // + LocalizationConstants.DESK_DIR
        // + File.separator + Activator.getDefault().getCurrentDesk();

        deskPath = sitePath + File.separator + LocalizationConstants.DESK_DIR
                + File.separator + Activator.getDefault().getCurrentDesk();

        // userPath = Activator.getDefault().getBaseDirPath()
        // + rootDirPortion + File.separator
        // + LocalizationConstants.USER_DIR
        // + File.separator + Activator.getDefault().getCurrentUser();
        userPath = Activator.getDefault().getCaveDataDirPath()
                + siteOrUserRootDirPortion + File.separator
                + LocalizationConstants.USER_DIR + File.separator
                + Activator.getDefault().getCurrentUser();

        userSitePath = Activator.getDefault().getCaveDataDirPath()
                + siteOrUserRootDirPortion + File.separator
                + LocalizationConstants.USER_DIR + File.separator
                + Activator.getDefault().getCurrentUser() + File.separator
                + LocalizationConstants.SITE_DIR + File.separator
                + Activator.getDefault().getCurrentSite();

        userSiteDeskPath = userSitePath + File.separator
                + LocalizationConstants.DESK_DIR + File.separator
                + Activator.getDefault().getCurrentDesk();
        ;

        /*
         * debug
         */
        // displayPathInfo(localPath, "localPath");
        // displayPathInfo(basePath, "basePath");
        // displayPathInfo(sitePath, "sitePath");
        // displayPathInfo(deskPath, "deskPath");
        // displayPathInfo(userPath, "userPath");
        // displayPathInfo(userSitePath, "userSitePath");
        // displayPathInfo(userSiteDeskPath, "userSiteDeskPath");
    }

    private void displayPathInfo(String path, String msg) {
        logger.warn("=====================" + msg + " : =" + path);
    }

    public ResourceFileInfo(String currentUser, String currentSite,
            String currentDesk, String absoluteRootDir) {
        validateInitParameters(currentUser, currentSite, currentDesk,
                absoluteRootDir);

        localPath = LocalizationConstants.LOCAL_DIR;

        basePath = absoluteRootDir + File.separator
                + LocalizationConstants.BASE_DIR;

        sitePath = absoluteRootDir + File.separator
                + LocalizationConstants.SITE_DIR + File.separator + currentSite;

        /*
         * Now the DESK is under the associated site
         */
        // deskPath = absoluteRootDir + File.separator
        // + LocalizationConstants.DESK_DIR
        // + File.separator + currentDesk;
        deskPath = sitePath + File.separator + LocalizationConstants.DESK_DIR
                + File.separator + currentDesk;

        userPath = absoluteRootDir + File.separator
                + LocalizationConstants.USER_DIR + File.separator + currentUser;

        userSitePath = userPath + File.separator
                + LocalizationConstants.SITE_DIR + File.separator + currentSite;

        userSiteDeskPath = userSitePath + File.separator
                + LocalizationConstants.DESK_DIR + currentDesk;
    }

    private void validateInitParameters(String currentUser, String currentSite,
            String currentDesk, String absoluteRootDir) {
        if (StringUtil.isStringEmpty(currentUser))
            throw new ParameterValidationException("Invalid currentUser found");
        if (StringUtil.isStringEmpty(currentSite))
            throw new ParameterValidationException("Invalid currentSite found");
        if (StringUtil.isStringEmpty(currentDesk))
            throw new ParameterValidationException("Invalid currentDesk found");
        if (StringUtil.isStringEmpty(absoluteRootDir))
            throw new ParameterValidationException(
                    "Invalid absoluteRootDir found");
    }

    @Override
    public String getLocalPath() {
        return localPath;
    }

    @Override
    public String getBasePath() {
        return basePath;
    }

    @Override
    public String getDeskPath() {
        return deskPath;
    }

    @Override
    public String getSitePath() {
        return sitePath;
    }

    @Override
    public String getUserPath() {
        return userPath;
    }

    @Override
    public String getUserSiteDeskPath() {
        return userSiteDeskPath;
    }

    @Override
    public String getUserSitePath() {
        return userSitePath;
    }

    @Override
    public String getIndividualDirPortion() {
        return individualDirPortion;
    }

    public void setIndividualDirPortion(String individualDirPortion) {
        this.individualDirPortion = individualDirPortion;
    }

    @Override
    public FilenameFilter getFilenameFilter() {
        return filenameFilter;
    }

    public void setFilenameFilter(FilenameFilter filenameFilter) {
        this.filenameFilter = filenameFilter;
    }

    @Override
    public int getLocalizationLevel() {
        return localizationLevel;
    }

    @Override
    public void setLocalizationLevel(int localizationLevel) {
        this.localizationLevel = localizationLevel;
    }

    @Override
    public String getIndividualFilename() {
        return this.individualFilename;
    }

    public void setIndividualFilename(String _individualFilename) {
        this.individualFilename = _individualFilename;
    }

    @Override
    public String getResourceNamePattern() {
        return this.resourceNamePattern;
    }

    public void setResourceNamePattern(String _resourceNamePattern) {
        this.resourceNamePattern = _resourceNamePattern;
    }

    @Override
    public int getResourceNamePatternMatchStyle() {
        return this.resourceNamePatternMatchStyle;
    }

    public void setResourceNamePatternMatchStyle(
            int resourceNamePatternMatchStyle) {
        this.resourceNamePatternMatchStyle = resourceNamePatternMatchStyle;
    }

    @Override
    public File doFileInfoLoading(
            ISingleResourceFileInfoLoader<File> fileInfoLoader) {
        return fileInfoLoader.loadSingleFielInfo(this);
    }

    @Override
    public Map<String, File> doFileInfoLoading(
            IMultiResourceFileInfoLoader<File> fileInfoLoader) {
        return fileInfoLoader.loadMultiFileInfo(this);
    }

}
