package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.LocalizationUtil;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.filter.DefaultFilenameFilter;
import gov.noaa.nws.ncep.viz.localization.impl.AbstractLocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;
import gov.noaa.nws.ncep.viz.localization.impl.IMultiResourceFileInfoLoader;
import gov.noaa.nws.ncep.viz.localization.impl.ResourceFileInfo;

import java.io.File;
import java.io.FilenameFilter;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;

//public class DirectoryLocalizationManager extends AbstractLocalizationManager<File[]> {
public class DirectoryLocalizationManager  {

	private static DirectoryLocalizationManager directoryLocalizationManager; 
	
	private Map<String, Map<String, File>> resourceInfoMap; 
	private static final IMultiResourceFileInfoLoader<File> multiResourceLoader; 
	
	static {
		directoryLocalizationManager = new DirectoryLocalizationManager(); 
		multiResourceLoader = new DirectoryLoaderImpl();
	}
	
	private DirectoryLocalizationManager() {}; 
	
	public static DirectoryLocalizationManager getInstance() {
		return directoryLocalizationManager; 
	}
	
	public File[] getLocalizationDirectoryArray(String resourceKey) {
		if(resourceInfoMap == null) {
			resourceInfoMap = new HashMap<String, Map<String, File>>(); 
			readExtensionPoint();
		}
		Map<String, File> dirResourceInfoMap = resourceInfoMap.get(resourceKey); 
		File[] loadedFileArray = LocalizationUtil.getAvailableMultiFileInfoInArray(dirResourceInfoMap);
		return loadedFileArray;
	}

	public File getLocalizationDirectory(String resourceKey, String resourceName) {
		if(resourceInfoMap == null) {
			resourceInfoMap = new HashMap<String, Map<String, File>>(); 
			readExtensionPoint();
		}
		File loadedFile = null; 
		Map<String, File> dirResourceInfoMap = resourceInfoMap.get(resourceKey); 
		if(dirResourceInfoMap != null)
			loadedFile = dirResourceInfoMap.get(resourceName); 
		return loadedFile;
	}

	public String getLocalizationDirectoryName(String resourceKey, String resourceName) {
		File loadedDirectory = getLocalizationDirectory(resourceKey, resourceName); 
		String directoryName = null; 
		if(loadedDirectory != null)
			directoryName = loadedDirectory.getAbsolutePath(); 
		return directoryName;
	}

	public File[] reloadingDirectoryResourceInfo(String directoryPath) {
		return reLoadingDirectoryResourceInfo(null, directoryPath, null); 
	}
	
	public File[] reLoadingDirectoryResourceInfo(String resourceRootValue, 
			String directoryPath,  
			String resourceLocalizationLevelStringValue) {
		if(StringUtil.isStringEmpty(directoryPath))
			return new File[1]; //Reluctant to return a NULL
		
		FilenameFilter filenameFilter = getFilenameFilter(); 
		
		ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(resourceRootValue, 
				directoryPath, null, null, 
				resourceLocalizationLevelStringValue, filenameFilter); 
		Map<String, File> loadedFileInfoMap = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
		if(resourceInfoMap == null) 
			resourceInfoMap = new HashMap<String, Map<String, File>>(); 
		addLoadedFileInfo(resourceInfoMap, directoryPath, loadedFileInfoMap); 			
		Map<String, File> consolidatedFileInfoMap = resourceInfoMap.get(directoryPath); 
		
		File[] loadedFileArray = getAvailableMultiFileInfoInArray(consolidatedFileInfoMap);  
		
		return loadedFileArray; 
	}
	
	private FilenameFilter getFilenameFilter() {
		FilenameFilter filter = new DefaultFilenameFilter(); 
		return filter; 
	}
	
	protected void readExtensionPoint() {
		IExtensionRegistry registry = Platform.getExtensionRegistry(); 
		IExtensionPoint extensionPoint = registry.getExtensionPoint(AbstractLocalizationManager.EET_POINT_ID); 
		IExtension[] extensions = extensionPoint.getExtensions(); 

		for(IExtension eachExtension : extensions) {
			IConfigurationElement[] configs = eachExtension.getConfigurationElements(); 
			for (IConfigurationElement eachConfig : configs) {
				final String directoryPath = eachConfig.getAttribute("directoryPath"); 
				if(!isDirectoryResourceLoadingExtension(directoryPath))
					continue; 
				final String resourceKey = eachConfig.getAttribute("name"); 
				final String resourceRootValue = eachConfig.getAttribute("rootValue"); 
				final String resourceLocalizationLevelStringValue = eachConfig.getAttribute("localizationLevel"); 

//				final Object fileFilterClassObject = null; 
//				try {
////					final Object fileFilterClassObject = eachConfig.createExecutableExtension("fileFilterClass"); 
//					fileFilterClassObject = eachConfig.createExecutableExtension("fileFilterClass"); 
//					if(fileFilterClassObject instanceof FilenameFilter) {
//						ISafeRunnable runnable = new ISafeRunnable() {
//							@Override
//							public void handleException(Throwable exception) {
//								System.out.println("Exception in multiResourceLoading, error="+exception.getMessage());
//							}
//
//							@Override
//							public void run() throws Exception {
//								final ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(resourceRootValue, 
//										resourceLocation, resourceNamePattern, resourceNamePatternMatchStyleStringValue, 
//										resourceLocalizationLevelStringValue, (FilenameFilter)fileFilterClassObject); 
//								Map<String, File> loadedFileInfoMap = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
//								File[] loadedFileArray = getAvailableMultiFileInfoInArray(loadedFileInfoMap);  
//								resourceInfoMap.put(resourceKey, loadedFileArray); 
//							}
//						};
//						SafeRunner.run(runnable);
//					}
//				} catch(CoreException ce) {
//					String error = ce.getMessage(); 
//					System.out.println("Error is: "+ error); 
//					continue; 
//				}

				final FilenameFilter fileFilterClassObject = getFilterClassObject(eachConfig, "fileFilterClass", getFilenameFilter()); 
				ISafeRunnable runnable = new ISafeRunnable() {
					@Override
					public void handleException(Throwable exception) {
						System.out.println("Exception in directoryResourceLoading, error="+exception.getMessage());
					}

					@Override
					public void run() throws Exception {
						final ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(resourceRootValue, 
								directoryPath, null, null, 
								resourceLocalizationLevelStringValue, fileFilterClassObject); 
						Map<String, File> loadedFileInfoMap = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
						addLoadedFileInfo(resourceInfoMap, resourceKey, loadedFileInfoMap); 
					}
				};
				SafeRunner.run(runnable);

			} 
		} 
	}

	private void addLoadedFileInfo(Map<String, Map<String, File>> resourceInfoMap, String resourceKey, Map<String, File> loadedFileInfoMap) {
		if(StringUtil.isStringEmpty(resourceKey) || loadedFileInfoMap == null)
			return; 
		Map<String, File> retrievedFileInfoMap = resourceInfoMap.get(resourceKey); 
		if(retrievedFileInfoMap == null)
			resourceInfoMap.put(resourceKey, loadedFileInfoMap); 
		else {
			consolidateExistingResourceInfoMap(retrievedFileInfoMap, loadedFileInfoMap); 
			resourceInfoMap.put(resourceKey, retrievedFileInfoMap); 
		}
	}
	
	private void consolidateExistingResourceInfoMap(Map<String, File> existingFileInfoMap, Map<String, File> newLoadedFileInfoMap) {
		if(existingFileInfoMap != null && newLoadedFileInfoMap != null) {
			Set<String> newMapKeySet = newLoadedFileInfoMap.keySet(); 
			for(String eachMapKey : newMapKeySet) {
				File retrievedFileFromExistingMap = existingFileInfoMap.get(eachMapKey); 
				if(retrievedFileFromExistingMap == null) {
					existingFileInfoMap.put(eachMapKey, newLoadedFileInfoMap.get(eachMapKey)); 
				}
			}
		}
	}
	
	private FilenameFilter getFilterClassObject(IConfigurationElement eachConfig, String extensionAttrName, FilenameFilter defaultFilenameFilter) {
		FilenameFilter fileFilterClassObject = null; 
		try {
			Object classObject = eachConfig.createExecutableExtension(extensionAttrName);
			if(classObject != null && classObject instanceof FilenameFilter) {
				fileFilterClassObject = (FilenameFilter)classObject; 
			} else {
				fileFilterClassObject = defaultFilenameFilter; 
			}
		} catch(CoreException ce) {
			String error = ce.getMessage(); 
			System.out.println("Error is: "+ error + " Now use the default FilenameFilter"); 
			fileFilterClassObject = defaultFilenameFilter; 
		}
		return fileFilterClassObject; 
	}
	
	/*
	 * This method is used to differentiate this extension from other extensions
	 */
	private boolean isDirectoryResourceLoadingExtension(String directoryPath) {
		boolean checkResult = true; 
		if(StringUtil.isStringEmpty(directoryPath))
			checkResult = false;
		return checkResult; 
	}
	
	private ILoadedFileResourceInfo<File> getLoadedFileInfoObject(String resourceRootValue, 
			String resourceLocation, String resourceNamePattern, String resourceNamePatternMatchStyleStringValue, 
			String resourceLocalizationLevelStringValue, FilenameFilter fileFilterClass) {
//		ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(resourceRootValue); 
		ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(resourceRootValue, null); //it is a quick fix. Michael Gao's comment
		resourceFileInfoObject.setIndividualDirPortion(resourceLocation); 
		resourceFileInfoObject.setLocalizationLevel(LocalizationUtil.translateLocalizationLevel(resourceLocalizationLevelStringValue)); 
		
		resourceFileInfoObject.setFilenameFilter(fileFilterClass); 
		
		return resourceFileInfoObject; 
	}

	private File[] getAvailableMultiFileInfoInArray(Map<String, File> fileInfoMap) {
		Collection<File> collection = fileInfoMap.values(); 
		File[] genericArray = convertToArray(collection); 
		if(genericArray != null)
			Arrays.sort(genericArray);
		return genericArray; 
	}
	
	private File[] convertToArray(Collection<File> collection) {
		if(collection == null)
			return null; 
		File object = getGenericObject(collection); 
		if(object == null)
			return null; 
		
		File[] array = (File[])Array.newInstance(object.getClass(), collection.size()); 
		populateArray(array, collection); 
		return array; 
	}
	
	private File getGenericObject(Collection<File> collection) {
		Iterator<File> itr = collection.iterator(); 
		File object = null; 
		if(itr.hasNext())
			object = itr.next(); 
		return object; 
	}
	
	private void populateArray(File[] array, Collection<File> collection) {
		if(collection == null)
			return; 
		int arrayIndex = 0; 
		for(File currentT : collection) {
			array[arrayIndex++] = currentT; 
		}
	}

}
