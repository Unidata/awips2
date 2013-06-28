package ohd.hseb.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

/**
 * @author GobsC
 * This class describes a particular unit hydrograph, by location, basin, model, and duration.
 *
 */
public class UnitHydrographDescriptor
{
    private String _locationId = null;
    private String _basinId = null;
    private String _model = null;
    private int _duration = 0;
 // ------------------------------------------------------------------------------   
    public UnitHydrographDescriptor(String locationId, String basinId,
            String model, int duration)
    {
        _locationId = locationId;
        _basinId = basinId;
        _model = model;
        _duration = duration;
        return;
    }
    

    // ------------------------------------------------------------------------------   
    public static List createUnitHydrographDescriptorList(
                                                          List unitHydrographEntryList)
    {
        String header = "UnitHydrographDescriptor.createUnitHydrographDescriptorList(): ";
        List descriptorList = new ArrayList();
        UnitHydrographEntry entry = null;
        UnitHydrographDescriptor descriptor = null;
        
        HashSet descriptorSet = new HashSet();

        boolean addToDescriptorList = false;

        for (int i = 0; i < unitHydrographEntryList.size(); i++)
        {
            entry = (UnitHydrographEntry) unitHydrographEntryList.get(i);

            descriptor = UnitHydrographDescriptor.createUnitHydrographDescriptorFromEntry(entry);
            
            
            if (! descriptorSet.contains(descriptor))
            {
                descriptorSet.add(descriptor);
            }
        }

        
      
        //convert the Set to a List
        descriptorList = new ArrayList(descriptorSet);
          
        //print it all out
        
/*
        System.out.println(header + " UHG Descriptor List");
        for (int i = 0; i < descriptorList.size(); i++)
        {
            UnitHydrographDescriptor desc = (UnitHydrographDescriptor) descriptorList.get(i);
            System.out.println(desc);
        }
*/

        return descriptorList;

    }
    //------------------------------------------------------------------------------
    public static UnitHydrographDescriptor createUnitHydrographDescriptorFromEntry(UnitHydrographEntry entry)
    {
        UnitHydrographDescriptor descriptor =
            new UnitHydrographDescriptor(entry.getLocationId(),
                						 entry.getAreaId(),
                						 entry.getModel(),
                						 entry.getDur());
        
        
        
        return descriptor;
    }
    //------------------------------------------------------------------------------
    public boolean isMatchingEntry(UnitHydrographEntry entry)
    {
        boolean result = false;
        
        if ((_locationId.equals(entry.getLocationId()))
                && (_basinId.equals(entry.getAreaId()))
                && (_model.equals(entry.getModel()))
                && (_duration == entry.getDur()))
        {
            result = true;
        }
        
        return result;
    }	
    //------------------------------------------------------------------------------  
   
    /**
     * @return Returns the locationId.
     */
    public String getLocationId()
    {
        return _locationId;
    }
    //------------------------------------------------------------------------------  
  
    /**
     * @return Returns the basinId.
     */
    public String getBasinId()
    {
        return _basinId;
    }
    
    //------------------------------------------------------------------------------  
    
    
    /**
     * @return Returns the model.
     */
    public String getModel()
    {
        return _model;
    }
    //------------------------------------------------------------------------------  
    
  
    /**
     * @return Returns the duration.
     */
    public int getDuration()
    {
        return _duration;
    }
    //------------------------------------------------------------------------------  
    public boolean equals(Object object)
    {
        boolean result = false;
        
        if (object instanceof UnitHydrographDescriptor)
        {
            result = equals((UnitHydrographDescriptor) object);
        }
        else
        {
            result = false;
        }
      
        return result;
    }
   
    //------------------------------------------------------------------------------  
    public int hashCode()
    {

        String hashString = _locationId + "|" + _basinId + "|" + _model + "|" +_duration;
        
        int hashValue = hashString.hashCode();
    
        return hashValue;
    }
    
    //------------------------------------------------------------------------------  

    public boolean equals(UnitHydrographDescriptor desc)
    {
         
        boolean result = false;
        
        if (  (_locationId.equals(desc.getLocationId())) &&
              (_basinId.equals(desc.getBasinId())) &&
              (_model.equals(desc.getModel())) &&
              (_duration == desc.getDuration())        
           )     
        {
            result = true;  
        }
        
        return result;
    }
  //------------------------------------------------------------------------------  
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("location = " + _locationId + " " +
                	  "basin = " + _basinId + " " +
                	  "model = " + _model + " " +
                	  "duration = " + _duration);
        
        return buffer.toString();
    }
    //------------------------------------------------------------------------------  

}
