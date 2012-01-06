package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.viz.localization.Activator;
import gov.noaa.nws.ncep.viz.localization.LocalizationUtil;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.exception.FailedToCreatingFileException;
import gov.noaa.nws.ncep.viz.localization.exception.ParameterValidationException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

public class LocalizationManager extends AbstractLocalizationManager<File> {
    private NcepLogger logger = NcepLoggerManager
            .getNcepLogger(this.getClass());

    private static LocalizationManager localizationManager;

    private static final ISingleResourceFileInfoLoader<File> singleResourceLoader;

    private Map<String, File> localizationDataMap;

    // private String ncRootDir;

    static {
        localizationManager = new LocalizationManager();
        singleResourceLoader = new SingleFileLoaderImpl();
    }

    private LocalizationManager() {
        localizationDataMap = new HashMap<String, File>();
    }

    public static LocalizationManager getInstance() {
        return localizationManager;
    }

    @Override
    public File getLocalizationFile(String resourceKey) {
        doLoading();
        return localizationDataMap.get(resourceKey);
    }

    public String getFilename(String resourceKey) {
        doLoading();
        String resourceName = "Not_Found";
        File file = localizationDataMap.get(resourceKey);
        if (file != null)
            // resourceName = file.getCanonicalPath();
            resourceName = file.getAbsolutePath();

        /*
         * test code
         */
        // String prefixDir =
        // "resources"+File.separator+"obs_and_analysis_rscs"+
        // File.separator+"MISC"+File.separator+"FFA";
        // String testPrmName = prefixDir + File.separator + "default.prm";
        // String testFfaXmlName = prefixDir + File.separator + "FFA.xml";
        // localizationSynchronizationTest(testPrmName);
        // localizationSynchronizationTest(testFfaXmlName);

        return resourceName;
    }

    private void localizationSynchronizationTest(String resourceName) {
        String fileName = resourceName;
        // if(!fileName.equalsIgnoreCase("Not_Found"))
        // fileName = getFileName(fileName, File.separator);
        /*
         * Test: LocalizationType: CAVE_NCEP LocalizationLevel: USER
         * ContextName: mgao
         */
        // LocalizationContext caveConfigUserLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_CONFIG,
        // LocalizationContext.LocalizationLevel.USER);
        // caveConfigUserLevelLocalizationContext.setContextName("mgao");
        File caveConfigUserLevelLocalFile = PathManagerFactory.getPathManager()
                .getFile(
                        Activator.getDefault()
                                .getCaveNcepUserLevelLocalizationContext(),
                        fileName);
        displayFileInfo(caveConfigUserLevelLocalFile);

        displaySeperationLine();
        /*
         * Test: LocalizationType: CAVE_NCEP LocalizationLevel: SITE
         * ContextName: OAX
         */
        // LocalizationContext caveConfigSiteLevelLocalizationContext = new
        // LocalizationContext(LocalizationContext.LocalizationType.CAVE_CONFIG,
        // LocalizationContext.LocalizationLevel.SITE);
        // caveConfigSiteLevelLocalizationContext.setContextName("OAX");
        File caveConfigSiteLevelLocalFile = PathManagerFactory.getPathManager()
                .getFile(
                        Activator.getDefault()
                                .getCaveNcepSiteLevelLocalizationContext(),
                        fileName);
        displayFileInfo(caveConfigSiteLevelLocalFile);

    }

    private String getFileName(String fileFullPath, String fileSeparator) {
        String[] fileSegamentArray = fileFullPath.split(fileSeparator);
        int arrayLength = fileSegamentArray.length;
        String lastSegamentOfFile = fileSegamentArray[arrayLength - 1];
        return lastSegamentOfFile;
    }

    private void displayFileInfo(File file) {
        if (file == null)
            System.out.println("=== The retrieved file is NULL!!!");
        System.out.println("====file.getAbsolutePath()="
                + file.getAbsolutePath());
        try {
            System.out.println("====file.getCanonicalPath()="
                    + file.getCanonicalPath());
        } catch (IOException ioe) {
            System.out
                    .println("file.getCanonicalPath() causes IOException, error="
                            + ioe.getMessage());
        }
        System.out.println("====file.getParent()=" + file.getParent());
        System.out.println("====file.getPath()=" + file.getPath());
        System.out.println("====file.getName()=" + file.getName());

    }

    private void displaySeperationLine() {
        System.out
                .println("=================================================================");
        System.out
                .println("          ############################################");
        System.out
                .println("=================================================================");
    }

    public File reloadingResourceInfo(String resourceName) {
        return reLoadingResourceInfo(null, null, null, resourceName, true);
    }

    public File reloadingResourceInfo(String resourceLocation,
            String resourceName) {
        return reLoadingResourceInfo(null, resourceLocation, null,
                resourceName, true);
    }

    public String reloadingResourceInfoName(String resourceName) {
        String rscName = "Not_Found";
        File file = reloadingResourceInfo(resourceName);
        if (file != null)
            rscName = file.getAbsolutePath();
        return rscName;
    }

    public File reLoadingResourceInfo(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName, boolean reloading) {
        if (!reloading)
            return getLocalizationFile(individualResourceName);

        String resourceName = null, resourcePath = null;
        File loadedFile = null;
        if (StringUtil.isStringEmpty(individualResourceName))
            return loadedFile;

        String[] parsedStringArray = LocalizationUtil
                .getParsedNameAndPathValues(individualResourceName);
        resourceName = parsedStringArray[0];
        if (!LocalizationUtil.isResourceNameOnly(parsedStringArray.length))
            resourcePath = LocalizationUtil.getResourcePath(resourceLocation,
                    parsedStringArray[1]);
        else
            resourcePath = resourceLocation;

        ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(
                resourceRootValue, resourcePath,
                resourceLocalizationLevelStringValue, resourceName);
        loadedFile = loadedFileInfoObject
                .doFileInfoLoading(singleResourceLoader);
        localizationDataMap.put(resourceName, loadedFile);
        return loadedFile;
    }

    public String reLoadingResourceInfoName(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName, boolean reloading) {
        String rscName = "Not_Found";
        File file = reLoadingResourceInfo(resourceRootValue, resourceLocation,
                resourceLocalizationLevelStringValue, individualResourceName,
                reloading);
        if (file != null)
            rscName = file.getAbsolutePath();
        return rscName;
    }

    protected void doLoading() {
        if (localizationDataMap.isEmpty()) {
            readExtensionPoint();
        }
    }

    @Override
    protected void readExtensionPoint() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint extensionPoint = registry
                .getExtensionPoint(EET_POINT_ID);
        IExtension[] extensions = null;
        /*
         * make sure extensionPoint is not NULL
         */
        if (extensionPoint != null) {
            extensions = extensionPoint.getExtensions();
        } else {
            extensions = new IExtension[0];
        }

        for (IExtension eachExtension : extensions) {
            IConfigurationElement[] configs = eachExtension
                    .getConfigurationElements();
            for (IConfigurationElement eachConfig : configs) {
                final String resourceName = eachConfig.getAttribute("fileName");
                if (!isSingleResourceLoadingExtension(resourceName))
                    continue;
                final String resourceKey = eachConfig.getAttribute("name");
                final String resourceRootValue = eachConfig
                        .getAttribute("rootValue");
                final String resourceLocation = eachConfig
                        .getAttribute("location");
                final String resourceLocalizationLevelStringValue = eachConfig
                        .getAttribute("localizationLevel");

                ISafeRunnable runnable = new ISafeRunnable() {
                    @Override
                    public void handleException(Throwable exception) {
                        System.out
                                .println("Exception in singleResourceLoading, error="
                                        + exception.getMessage());
                    }

                    @Override
                    public void run() throws Exception {
                        final ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(
                                resourceRootValue, resourceLocation,
                                resourceLocalizationLevelStringValue,
                                resourceName);
                        File loadedFile = loadedFileInfoObject
                                .doFileInfoLoading(singleResourceLoader);
                        localizationDataMap.put(resourceKey, loadedFile);
                    }
                };
                SafeRunner.run(runnable);
            }
        }
    }

    /*
     * This method is used to differentiate this extension from other extensions
     */
    private boolean isSingleResourceLoadingExtension(String resourceName) {
        boolean checkResult = true;
        if (StringUtil.isStringEmpty(resourceName))
            checkResult = false;
        return checkResult;
    }

    private ILoadedFileResourceInfo<File> getLoadedFileInfoObject(
            String resourceRootValue, String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName) {
        // ResourceFileInfo resourceFileInfoObject = new
        // ResourceFileInfo(resourceRootValue);
        ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(
                resourceRootValue, null); // it is a quick fix. Michael Gao's
                                          // comment
        resourceFileInfoObject.setIndividualDirPortion(resourceLocation);
        resourceFileInfoObject
                .setLocalizationLevel(LocalizationUtil
                        .translateLocalizationLevel(resourceLocalizationLevelStringValue));
        resourceFileInfoObject.setIndividualFilename(individualResourceName);

        return resourceFileInfoObject;
    }

    @Override
    public File getLocalizationFileDirectly(String individualResourceName) {
        return getLocalizationFileDirectly(null, individualResourceName);
    }

    public String getLocalizationFileNameDirectly(String individualResourceName) {
        return getLocalizationFileNameDirectly(null, individualResourceName);
    }

    @Override
    public File getLocalizationFileDirectly(String resourceLocation,
            String individualResourceName) {
        return getLocalizationFileDirectly(null, resourceLocation,
                individualResourceName);
    }

    public String getLocalizationFileNameDirectly(String resourceLocation,
            String individualResourceName) {
        return getLocalizationFileNameDirectly(null, resourceLocation,
                individualResourceName);
    }

    @Override
    public File getLocalizationFileDirectly(String resourceRootValue,
            String resourceLocation, String individualResourceName) {
        return getLocalizationFileDirectly(resourceRootValue, resourceLocation,
                null, individualResourceName);
    }

    public String getLocalizationFileNameDirectly(String resourceRootValue,
            String resourceLocation, String individualResourceName) {
        return getLocalizationFileNameDirectly(resourceRootValue,
                resourceLocation, null, individualResourceName);
    }

    @Override
    public File getLocalizationFileDirectly(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName) {
        if (StringUtil.isStringEmpty(individualResourceName))
            throw new ParameterValidationException(
                    "A resource name can not be empty or null");

        File loadedFile = doDirectlyLoadinggetLocalizationFile(
                resourceRootValue, resourceLocation,
                resourceLocalizationLevelStringValue, individualResourceName);

        return loadedFile;
    }

    public String getLocalizationFileNameDirectly(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName) {
        File loadedFile = getLocalizationFileDirectly(resourceRootValue,
                resourceLocation, resourceLocalizationLevelStringValue,
                individualResourceName);
        String fileName = getFilePathFromFile(loadedFile);
        return fileName;
    }

    // @Override
    public File getLocalizationFileDirectory(String resourceLocation) {
        return getLocalizationFileDirectory(null, resourceLocation, null);
    }

    public String getLocalizationFileDirectoryName(String resourceLocation) {
        return getLocalizationFileDirectoryName(null, resourceLocation, null);
    }

    public File getLocalizationFileDirectory(String resourceRootValue,
            String resourceLocation, String resourceLocalizationLevelStringValue) {
        validationResourceLocationValue(resourceLocation);
        File loadedFile = doDirectlyLoadinggetLocalizationFile(
                resourceRootValue, resourceLocation,
                resourceLocalizationLevelStringValue, null);
        return loadedFile;
    }

    public String getLocalizationFileDirectoryName(String resourceRootValue,
            String resourceLocation, String resourceLocalizationLevelStringValue) {
        File loadedFile = getLocalizationFileDirectory(resourceRootValue,
                resourceLocation, resourceLocalizationLevelStringValue);
        String directoryName = getFilePathFromFile(loadedFile);
        return directoryName;
    }

    public void jaxbMarshalToXmlFileUnderUserLocalizationLevel(
            Object serializableObject, String filePathPostfix,
            String destinationFileName) {
        File destinationFile = getFileForSavingUnderUserLocalizationLevel(
                filePathPostfix, destinationFileName);
        if (destinationFile == null) {
            String filePathPostfixString = filePathPostfix + File.separator
                    + destinationFileName;
            logger.error("Failed to create file: " + filePathPostfixString
                    + " Generated file is NULL");
            throw new FailedToCreatingFileException("Failed to create file: "
                    + filePathPostfixString + " Generated file is NULL");
        }
        try {
            SerializationUtil.jaxbMarshalToXmlFile(serializableObject,
                    destinationFile.getAbsolutePath());
        } catch (SerializationException e) {
            logger.error("Failed on marshalling file "
                    + destinationFile.getAbsolutePath() + " Error: "
                    + e.getMessage());
        }

    }

    public void savePlainTextToFileUnderUserLocalizationLevel(
            String stringText, String filePathPostfix,
            String destinationFileName) {
        File destinationFile = getFileForSavingUnderUserLocalizationLevel(
                filePathPostfix, destinationFileName);
        if (destinationFile == null) {
            String filePathPostfixString = filePathPostfix + File.separator
                    + destinationFileName;
            logger.error("Failed to create file: " + filePathPostfixString
                    + " Generated file is NULL");
            throw new FailedToCreatingFileException("Failed to create file: "
                    + filePathPostfixString + " Generated file is NULL");
        }
        try {
            // Create file
            FileWriter fWriter = new FileWriter(destinationFile);
            fWriter.write(stringText);
            fWriter.close();
            // BufferedWriter out = new BufferedWriter(fWriter);
            // out.write("Hello Java");
            // Close the output stream
            // out.close();
        } catch (IOException ioe) {// Catch exception if any
            logger.error("Failed on writing file "
                    + destinationFile.getAbsolutePath() + " Error: "
                    + ioe.getMessage());
        }
    }

    public String getFullFilePathNameStringUnderUserLocalizationLevel(
            String filePathPostfix, String destinationFileName) {
        File file = getFileForSavingUnderUserLocalizationLevel(filePathPostfix,
                destinationFileName);
        String fullFilePathString = file.getAbsolutePath();
        return fullFilePathString;
    }

    private File getFileForSavingUnderUserLocalizationLevel(
            String filePathPostfix, String destinationFileName) {
        ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(null,
                null); // a quick and a little dirty fix, M. Gao's comment
        File parentPathFile = createParentDirFile(
                resourceFileInfoObject.getUserPath(), filePathPostfix);

        File file = new File(parentPathFile, destinationFileName);

        return file;
    }

    private File createParentDirFile(String parentRootPathString,
            String filePathToBeCreated) {
        String[] filePathPortionArray = filePathToBeCreated
                .split(File.separator);
        File parentDirFile = new File(parentRootPathString);
        for (String eachFileDirString : filePathPortionArray) {
            parentDirFile = new File(parentDirFile, eachFileDirString);
            if (!parentDirFile.exists()) {
                parentDirFile.mkdir();
            }
        }
        return parentDirFile;
    }

    // private void validationResourceLocationAndLocalizationLevelValues(String
    // resourceLocation, String resourceLocalizationLevelStringValue) {
    // if(StringUtil.isStringEmpty(resourceLocation))
    // throw new
    // ParameterValidationException("A resourceLocation can not be empty or null");
    // if(StringUtil.isStringEmpty(resourceLocalizationLevelStringValue))
    // throw new
    // ParameterValidationException("A resource Localization Level can not be empty or null");
    //
    // }

    private void validationResourceLocationValue(String resourceLocation) {
        if (StringUtil.isStringEmpty(resourceLocation))
            throw new ParameterValidationException(
                    "A resourceLocation can not be empty or null");
    }

    private File doDirectlyLoadinggetLocalizationFile(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName) {

        File loadedFile = null;
        String resourceKey = getResourceKey(resourceRootValue,
                resourceLocation, resourceLocalizationLevelStringValue,
                individualResourceName);
        loadedFile = localizationDataMap.get(resourceKey);
        if (loadedFile != null)
            return loadedFile;

        ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(
                resourceRootValue, resourceLocation,
                resourceLocalizationLevelStringValue, individualResourceName);
        loadedFile = loadedFileInfoObject
                .doFileInfoLoading(singleResourceLoader);
        if (loadedFile != null) {
            localizationDataMap.put(resourceKey, loadedFile);
            /*
             * display debug information
             */
            // System.out.println("=============================================================");
            // System.out.println("                      loaded resource path ="+
            // loadedFile.getAbsolutePath());
            // System.out.println("=============================================================");
        }

        return loadedFile;
    }

    private String getResourceKey(String resourceRootValue,
            String resourceLocation,
            String resourceLocalizationLevelStringValue,
            String individualResourceName) {
        StringBuilder builder = new StringBuilder(15);

        /*
         * the resource key is constructed in the sequence of resourceRootValue,
         * resourceLocation, resourceLocalizationLevelStringValue,
         * individualResourceName
         */
        // Step 1: append resourceRootValue
        if (!StringUtil.isStringEmpty(resourceRootValue))
            builder.append(resourceRootValue);
        else
            // builder.append(LocalizationConstants.DEFAULT_ROOT_DIR_PORTION);
            builder.append(LocalizationConstants.DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_BASE);

        // Step 2: append resourceLocation
        if (!StringUtil.isStringEmpty(resourceLocation)) {
            resourceLocation = resourceLocation.replace(File.separator, "_");
            builder.append("_").append(resourceLocation);
        }

        // Step 3: append resourceLocalizationLevelStringValue
        if (!StringUtil.isStringEmpty(resourceLocalizationLevelStringValue)) {
            builder.append("_").append(resourceLocalizationLevelStringValue);
        }

        // Step 4: append individualResourceName
        if (!StringUtil.isStringEmpty(individualResourceName)) {
            builder.append("_").append(individualResourceName);
        }

        /*
         * display debug information
         */
        String constructedResourceKey = builder.toString();
        // System.out.println("=============================================================");
        // System.out.println("                      constructed resource key="+constructedResourceKey);
        // System.out.println("=============================================================");

        return constructedResourceKey;
    }

    private String getFilePathFromFile(File file) {
        String resourceName = "Not_Found";
        if (file != null)
            resourceName = file.getAbsolutePath();

        return resourceName;

    }

    // Added for outside code to get the root of the ncep directory for files
    // that are
    // NOT under localization control. Internally, the Localization code is
    // still referencing
    // Activator.getBaseDir().
    // public String getNcepRootDir() {
    // if( ncRootDir == null ) {
    // ncRootDir = com.raytheon.uf.viz.core.localization.LocalizationManager.
    // getBaseDir() + LocalizationConstants.DEFAULT_ROOT_DIR_PORTION;
    // }
    // return ncRootDir;
    // }
}
