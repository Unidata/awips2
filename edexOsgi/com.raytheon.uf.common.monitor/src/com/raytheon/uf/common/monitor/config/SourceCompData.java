package com.raytheon.uf.common.monitor.config;

import java.util.ArrayList;


public class SourceCompData
{
    private String sourceName;
    private String areaFFGValue;
    
    private ArrayList<ValueNameIdData> countyBasinData = new ArrayList<ValueNameIdData>();
    
    public SourceCompData()
    {        
    }

    public String getSourceName()
    {
        return sourceName;
    }

    public void setSourceName(String sourceName)
    {
        this.sourceName = sourceName;
    }

    public String getAreaFFGValue()
    {
        return areaFFGValue;
    }

    public void setAreaFFGValue(String areaFFGValue)
    {
        this.areaFFGValue = areaFFGValue;
    }

    public ArrayList<ValueNameIdData> getCountyBasinData()
    {
        return countyBasinData;
    }

    public void setCountyBasinData(ArrayList<ValueNameIdData> countyBasinData)
    {
        this.countyBasinData = countyBasinData;
    }
    
    public void mergeCountyBasinData(ArrayList<ValueNameIdData> data)
    {
        boolean matchFound = false;
        ArrayList<ValueNameIdData> dataToMerge = new ArrayList<ValueNameIdData>();
        
        /*
         * Loop over the data to be merged.
         */
        for (ValueNameIdData dataVnid : data)
        {
            matchFound = false;
            
            /*
             * Loop over the existing data and see if there is data that
             * matches the new data element.
             */
            for (ValueNameIdData cbdVnid : countyBasinData)
            {                
                if (dataVnid.getUniqueId().compareTo(cbdVnid.getUniqueId()) == 0)
                {
                    matchFound = true;
                    break;
                }
            }
            
            // Add the data because there was not match found.
            if (matchFound == false)
            {
                dataToMerge.add(dataVnid);
            }
        }
        
        // Add all of the new data to the county/basin array.
        countyBasinData.addAll(dataToMerge);
    }
    
    public void mergeOverwriteCountyBasinData(ArrayList<ValueNameIdData> data)
    {
        boolean matchFound = false;
        ArrayList<ValueNameIdData> dataToMerge = new ArrayList<ValueNameIdData>();
        ValueNameIdData cbdVnid;
        
        /*
         * Loop over the data to be merged/overwritten.
         */
        for (ValueNameIdData dataVnid : data)
        {
            matchFound = false;
            
            /*
             * Loop over the existing data and see if there is data that
             * matches the new data element.
             */                
            for (int i = 0; i < countyBasinData.size(); i++)
            {             
                cbdVnid = countyBasinData.get(i);
                
                // If a match is found then overwrite the entry in the 
                // county/basin array.
                if (dataVnid.getUniqueId().compareTo(cbdVnid.getUniqueId()) == 0)
                {
                    matchFound = true;
                    countyBasinData.set(i, dataVnid);
                    break;
                }
            }
            
            // Add the data because there was not match found. 
            if (matchFound == false)
            {
                dataToMerge.add(dataVnid);
            }
        }
        
        // Add all of the new data to the county/basin array.
        countyBasinData.addAll(dataToMerge);
    }
    
    public void mergeAreaFFG(String areaFfg) {
        if ((this.getAreaFFGValue() == null) || (this.getAreaFFGValue().length() == 0)) {
            this.setAreaFFGValue(areaFfg);
        }
    }
    
    public void printData()
    {
        System.out.println("*******************************************");
        
        System.out.println("areaFFGValue = " + this.areaFFGValue);
        System.out.println("sourceName   = " + this.sourceName);
        
        if (countyBasinData == null)
        {
            System.out.println("Null data for County & Basin data...");
        }
       
        for (ValueNameIdData data : countyBasinData)  
        {
            if (data != null)
            {
                System.out.println("--- type = " + data.getType());
                System.out.println("--- name = " + data.getName());
                System.out.println("--- id   = " + data.getId());
                System.out.println("--- val  = " + data.getValue());
            }
        }
    }
}
