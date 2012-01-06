package gov.noaa.nws.ncep.viz.localization.impl;

public abstract class AbstractLocalizationManager<T> {
	protected static final String EET_POINT_ID = "gov.noaa.nws.ncep.viz.localization.localizationData";

	/*
	 * This method retrieves a resource using eclipse extension
	 */
	abstract public T getLocalizationFile(String resourceKey); 

	/*
	 * This method retrieves a resource without any dependency on
	 * eclipse extension
	 */
	abstract public T getLocalizationFileDirectly(String resourceRootValue, 
			String resourceLocation, String resourceLocalizationLevelStringValue, 
			String individualResourceName); 

	/*
	 * This method retrieves a resource without any dependency on
	 * eclipse extension
	 */
	abstract public T getLocalizationFileDirectly(String resourceRootValue, 
			String resourceLocation, String individualResourceName); 

	/*
	 * This method retrieves a resource without any dependency on
	 * eclipse extension
	 */
	abstract public T getLocalizationFileDirectly(String resourceLocation, String individualResourceName); 

	/*
	 * This method retrieves a resource without any dependency on
	 * eclipse extension
	 */
	abstract public T getLocalizationFileDirectly(String individualResourceName); 

	/*
	 * This method retrieves a resource file directory without any 
	 * dependency on eclipse extension
	 */
//	abstract public T getLocalizationFileDirectory(String resourceRootValue, 
//			String resourceLocation, String resourceLocalizationLevelStringValue); 

	/*
	 * This method retrieves a resource file directory without any 
	 * dependency on eclipse extension
	 */
//	abstract public T getLocalizationFileDirectory(String resourceLocation, 
//			String resourceLocalizationLevelStringValue); 

	
	abstract protected void readExtensionPoint(); 
	
}
