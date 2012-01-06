package gov.noaa.nws.ncep.viz.localization.impl;

import gov.noaa.nws.ncep.viz.localization.LocalizationUtil;
import gov.noaa.nws.ncep.viz.localization.StringUtil;
import gov.noaa.nws.ncep.viz.localization.filter.DefaultFilenameFilter;
import gov.noaa.nws.ncep.viz.localization.impl.AbstractLocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.ILoadedFileResourceInfo;
import gov.noaa.nws.ncep.viz.localization.impl.IMultiResourceFileInfoLoader;
import gov.noaa.nws.ncep.viz.localization.impl.MultiFileLoaderImpl;
import gov.noaa.nws.ncep.viz.localization.impl.MultiValueLocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.ResourceFileInfo;

import java.io.File;
import java.io.FilenameFilter;
//import java.lang.reflect.Array;
//import java.util.Arrays;
//import java.util.Collection;
import java.util.HashMap;
//import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;

public class MultiValueLocalizationManager extends AbstractLocalizationManager<File[]> {

	private static MultiValueLocalizationManager multiValueLocalizationManager; 
	
	private Map<String, File[]> resourceInfoMap; 
	private static final IMultiResourceFileInfoLoader<File> multiResourceLoader; 
	
	private String resourceName; 
	
	static {
		multiValueLocalizationManager = new MultiValueLocalizationManager(); 
		multiResourceLoader = new MultiFileLoaderImpl();
	}
	
	private MultiValueLocalizationManager() {}; 
	
	public static MultiValueLocalizationManager getInstance() {
		return multiValueLocalizationManager; 
	}
	
	public File[] getLocalizationFile(String resourceKey) {
		if(resourceInfoMap == null) {
			resourceInfoMap = new HashMap<String, File[]>(); 
			readExtensionPoint();
		}
		return resourceInfoMap.get(resourceKey);
	}

	public File[] reloadingResourceInfo(String resourceName) {
		return reLoadingResourceInfo(null, null, LocalizationConstants.EXACT_MATCH_STRING_VALUE, null, resourceName, true); 
	}
	
	public File[] reloadingResourceInfo(String resourceName, String matchStyleValue) {
		return reLoadingResourceInfo(null, null, matchStyleValue, null, resourceName, true); 
	}
	
	public File[] reLoadingResourceInfo(String resourceRootValue, 
			String resourceLocation, String resourceNamePatternMatchStyle, 
			String resourceLocalizationLevelStringValue, 
			String individualResourceName, boolean reloading) {
		if(!reloading) 
			return getLocalizationFile(individualResourceName); 

		String resourcePath = null; 
		File[] loadedFileArray = null; 
		if(StringUtil.isStringEmpty(individualResourceName))
			return loadedFileArray; 
		
		String[] parsedStringArray = LocalizationUtil.getParsedNameAndPathValues(individualResourceName); 
		resourceName = parsedStringArray[0]; 
		if(!LocalizationUtil.isResourceNameOnly(parsedStringArray.length))
			resourcePath = LocalizationUtil.getResourcePath(resourceLocation, parsedStringArray[1]); 
		
		FilenameFilter filenameFilter = getFilenameFilter(); 
		
		ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(resourceRootValue, 
				resourcePath, resourceName, resourceNamePatternMatchStyle, resourceLocalizationLevelStringValue, 
				filenameFilter); 
		Map<String, File> loadedFileInfoMap = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
		loadedFileArray = LocalizationUtil.getAvailableMultiFileInfoInArray(loadedFileInfoMap);  
//		loadedFileArray = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
		if(resourceInfoMap == null) 
			resourceInfoMap = new HashMap<String, File[]>(); 
		if(loadedFileArray != null)
			resourceInfoMap.put(individualResourceName, loadedFileArray); 
		
		return loadedFileArray; 
	}
	
	private FilenameFilter getFilenameFilter() {
		FilenameFilter filter = new DefaultFilenameFilter(); 
		return filter; 
	}
	
	protected void readExtensionPoint() {
		IExtensionRegistry registry = Platform.getExtensionRegistry(); 
		IExtensionPoint extensionPoint = registry.getExtensionPoint(EET_POINT_ID); 
		IExtension[] extensions = extensionPoint.getExtensions(); 

		for(IExtension eachExtension : extensions) {
			IConfigurationElement[] configs = eachExtension.getConfigurationElements(); 
			for (IConfigurationElement eachConfig : configs) {
				final String resourceNamePattern = eachConfig.getAttribute("fileNamePattern"); 
				final String resourceNamePatternMatchStyleStringValue = eachConfig.getAttribute("patternMatchStyle"); 
				if(!isMultiResourceLoadingExtension(resourceNamePattern, resourceNamePatternMatchStyleStringValue))
					continue; 
				final String resourceKey = eachConfig.getAttribute("name"); 
				final String resourceRootValue = eachConfig.getAttribute("rootValue"); 
				final String resourceLocation = eachConfig.getAttribute("location"); 
				final String resourceLocalizationLevelStringValue = eachConfig.getAttribute("localizationLevel"); 


				try {
					final Object fileFilterClassObject = eachConfig.createExecutableExtension("fileFilterClass"); 
					if(fileFilterClassObject instanceof FilenameFilter) {
						ISafeRunnable runnable = new ISafeRunnable() {
							@Override
							public void handleException(Throwable exception) {
								System.out.println("Exception in multiResourceLoading, error="+exception.getMessage());
							}

							@Override
							public void run() throws Exception {
								final ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject(resourceRootValue, 
										resourceLocation, resourceNamePattern, resourceNamePatternMatchStyleStringValue, 
										resourceLocalizationLevelStringValue, (FilenameFilter)fileFilterClassObject); 
								Map<String, File> loadedFileInfoMap = loadedFileInfoObject.doFileInfoLoading(multiResourceLoader); 
								File[] loadedFileArray = LocalizationUtil.getAvailableMultiFileInfoInArray(loadedFileInfoMap);  
								resourceInfoMap.put(resourceKey, loadedFileArray); 
							}
						};
						SafeRunner.run(runnable);
					}
				} catch(CoreException ce) {
					String error = ce.getMessage(); 
					System.out.println("Error is: "+ error); 
					continue; 
				}

			} 
		} 
	}

	/*
	 * This method is used to differentiate this extension from other extensions
	 */
	private boolean isMultiResourceLoadingExtension(String resourceNamePattern, String resourceNamePatternMatchStyleStringValue) {
		boolean checkResult = true; 
		if(StringUtil.isStringEmpty(resourceNamePattern) || StringUtil.isStringEmpty(resourceNamePatternMatchStyleStringValue))
			checkResult = false;
		return checkResult; 
	}
	
	private ILoadedFileResourceInfo<File> getLoadedFileInfoObject(String resourceRootValue, 
			String resourceLocation, String resourceNamePattern, String resourceNamePatternMatchStyleStringValue, 
			String resourceLocalizationLevelStringValue, FilenameFilter fileFilterClass) {
//		ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(resourceRootValue); 
		ResourceFileInfo resourceFileInfoObject = new ResourceFileInfo(resourceRootValue, null); //It is a quick fix. Michael Gao's comment. 
		resourceFileInfoObject.setIndividualDirPortion(resourceLocation); 
		resourceFileInfoObject.setResourceNamePattern(resourceNamePattern); 
		resourceFileInfoObject.setResourceNamePatternMatchStyle(LocalizationUtil.translateNamePatternMatchStyle(resourceNamePatternMatchStyleStringValue)); 
		resourceFileInfoObject.setLocalizationLevel(LocalizationUtil.translateLocalizationLevel(resourceLocalizationLevelStringValue)); 
		
		resourceFileInfoObject.setFilenameFilter(fileFilterClass); 
		
		return resourceFileInfoObject; 
	}

//	private File[] getAvailableMultiFileInfoInArray(Map<String, File> fileInfoMap) {
//		Collection<File> collection = fileInfoMap.values(); 
//		File[] genericArray = convertToArray(collection); 
//		if(genericArray != null)
//			Arrays.sort(genericArray);
//		return genericArray; 
//	}
//	
//	private File[] convertToArray(Collection<File> collection) {
//		if(collection == null)
//			return null; 
//		File object = getGenericObject(collection); 
//		if(object == null)
//			return null; 
//		
//		File[] array = (File[])Array.newInstance(object.getClass(), collection.size()); 
//		populateArray(array, collection); 
//		return array; 
//	}
//	
//	private File getGenericObject(Collection<File> collection) {
//		Iterator<File> itr = collection.iterator(); 
//		File object = null; 
//		if(itr.hasNext())
//			object = itr.next(); 
//		return object; 
//	}
//	
//	private void populateArray(File[] array, Collection<File> collection) {
//		if(collection == null)
//			return; 
//		int arrayIndex = 0; 
//		for(File currentT : collection) {
//			array[arrayIndex++] = currentT; 
//		}
//	}

	@Override
	public File[] getLocalizationFileDirectly(String resourceRootValue,
			String resourceLocation,
			String resourceLocalizationLevelStringValue,
			String individualResourceName) {
		// TBD
		return null;
	}

	@Override
	public File[] getLocalizationFileDirectly(String resourceRootValue,
			String resourceLocation, String individualResourceName) {
		// TBD
		return null;
	}

	@Override
	public File[] getLocalizationFileDirectly(String resourceLocation,
			String individualResourceName) {
		// TBD
		return null;
	}

	@Override
	public File[] getLocalizationFileDirectly(String individualResourceName) {
		// TBD
		return null;
	}

//	@Override
//	public File[] getLocalizationFileDirectory(String resourceRootValue,
//			String resourceLocation, String resourceLocalizationLevelStringValue) {
//		// TBD
//		return null;
//	}

//	@Override
//	public File[] getLocalizationFileDirectory(String resourceLocation,
//			String resourceLocalizationLevelStringValue) {
//		// TBD
//		return null;
//	}
	
}
