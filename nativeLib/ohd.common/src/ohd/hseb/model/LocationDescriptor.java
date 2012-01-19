/*
 * Created on Oct 8, 2004
 *
 * 
 */
package ohd.hseb.model;

/**
 * @author GobsC
 *
 * 
 */
public class LocationDescriptor
{
    private String _id;
    private String _locationName;
    private String _streamName;
    private String _basinId;
    private String _hsa;
    private String _modelPreference;
  // ------------------------------------------------------------------------------------------------  
    /**
     * @param id The id to set.
     */
    public void setId(String id)
    {
        _id = id;
    }
    /**
     * @return Returns the id.
     */
    public String getId()
    {
        return _id;
    }
//  ------------------------------------------------------------------------------------------------  
    
    /**
     * @param name The name to set.
     */
    public void setLocationName(String locationName)
    {
        _locationName = locationName;
    }
    /**
     * @return Returns the name.
     */
    public String getLocationName()
    {
        return _locationName;
    }
//  ------------------------------------------------------------------------------------------------  
    
    /**
     * @param streamName The streamName to set.
     */
    public void setStreamName(String streamName)
    {
        _streamName = streamName;
    }
    /**
     * @return Returns the streamName.
     */
    public String getStreamName()
    {
        return _streamName;
    }
//  ------------------------------------------------------------------------------------------------  
    
    /**
     * @param basinName The basinName to set.
     */
    public void setBasinId(String basinId)
    {
        _basinId = basinId;
    }
    /**
     * @return Returns the basinName.
     */
    public String getBasinId()
    {
        return _basinId;
    }
//  ------------------------------------------------------------------------------------------------  
    public void setHsa(String hsa)
    {
        _hsa = hsa;
    }
    
    public String getHsa()
    {
        return _hsa;
    }
//  ------------------------------------------------------------------------------------------------  
    public void setModelPreference(String modelPreference)
    {
        _modelPreference = modelPreference;
    }
    public String getModelPreference()
    {
        return _modelPreference;
    }
   
}
