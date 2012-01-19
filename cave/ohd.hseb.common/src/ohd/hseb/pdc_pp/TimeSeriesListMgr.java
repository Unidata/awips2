package ohd.hseb.pdc_pp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//import ohd.hseb.util.CodeTimer;

public class TimeSeriesListMgr
{
    
    final static String INITIAL_MATCH= "NO MATCH";
    final static String REGULAR_MATCH= "Fresh and Cached";
    final static String REGULAR_MATCH_PE_SWITCH= "Fresh and Cached WITH PE swap";
    final static String CACHE_ONLY =  "Cache Only";
    final static String FRESH_ONLY =  "Fresh Only";
    
    
    /**The purpose of this class is to group all the methods for handling the List of RegularObsTimeSeries
     * It is intended to be called by the PDCPreprocessorDataManager */
//  -------------------------------------------------------------------------    
   
    public static void setNewStartTime(List timeSeriesList, long newStartTime)
    {
        
        RegularObsTimeSeries timeSeries = null;
        RegularObsTimeSeriesDescriptor descriptor = null;
        
        if (timeSeriesList == null)
        {
            return;    
        }
        
        for (int i = 0; i < timeSeriesList.size(); i++)
        {
            timeSeries = (RegularObsTimeSeries) timeSeriesList.get(i);
            
            descriptor = timeSeries.getDescriptor();    
            descriptor.setStartTime(newStartTime);
      
            //consider combining this with the setting of the descriptor's starttime, since if they are
            // not atomic, bugs could be introduced
            timeSeries.removeTimeValuePairsBefore(newStartTime);                  
        }
    }
  
//  -------------------------------------------------------------------------    

    public static List mergeTimeSeriesLists(List freshTimeSeriesList, 
                                      List cachedTimeSeriesList,
                                      long newStartTime,
                                      long endTime)
                                   
    {
        // CodeTimer addTimer = new CodeTimer();
         
         String header = "TimeSeriesListMgr.mergeTimeSeriesLists() :";
         List mergedTimeSeriesList = new ArrayList();
         
         RegularObsTimeSeries freshTs = null;
         RegularObsTimeSeries cachedTs = null;
         RegularObsTimeSeries mergedTs = null;  
         
         RegularObsTimeSeriesDescriptor freshTsDesc = null;
   
      //   System.out.println(header + "newStartTime = " + newStartTime);
       
         if ((freshTimeSeriesList != null) && (cachedTimeSeriesList != null))
         {         
            //build a map to be used for searching for the matching time series
            Map cachedTimeSeriesMap = createTimeSeriesMap(cachedTimeSeriesList);
            List unmatchedCachedTimeSeriesDescriptorList = createUnmatchedTimeSeriesDescriptorList(cachedTimeSeriesList);
                          
             // for each time series in list 1, find the matching time series
            for (int i = 0; i < freshTimeSeriesList.size(); i++)
            {            
               
                freshTs = (RegularObsTimeSeries) freshTimeSeriesList.get(i);
                freshTsDesc = freshTs.getDescriptor();
                
                // add in the cachedTs data
               // mergedTs = copyTimeSeriesWithAdjustedStartTime(freshTs, newStartTime);
                mergedTs = freshTs;
                mergedTs.setStartTime(newStartTime);
                mergedTs.setEndTime(endTime);
                
                mergedTs.setMergerString(INITIAL_MATCH);
                
                cachedTs = findMatchingTimeSeries(freshTs, cachedTimeSeriesMap);
                 
                //removed this section, since I won't merge different PE's or TS's
                /*
                //make a second attempt at retrieving the cached data since the
                // pe might have changed from PC to PP or vice versa.
                if (cachedTs == null)
                {
                   
                    String origPe = freshTs.getDescriptor().getPe();
                    String newPe = null;
                    if (origPe.equals("PC"))
                    {
                        newPe = "PP";                           
                    }
                    else if (origPe.equals("PP"))
                    {
                        newPe = "PC";   
                    }
                   
                    if (newPe != null) //if performed a swap
                    {
                      //  System.out.println(header
                      //          + "Swapping PEs for cache search for \nfreshTs Descriptor = "
                      //          + " from " + origPe + " to " + newPe); 
                        freshTs.getDescriptor().setPe(newPe);  
                        cachedTs = findMatchingTimeSeries(freshTs, cachedTimeSeriesMap);
                    }
                }
                */
                
                // if found matching cached data
                if (cachedTs != null) //foundMatching TS
                {
                    mergedTs.setMergerString(REGULAR_MATCH);
                    
                    //This cachedTs has been matched with a freshTs, so remove it from the unmatched list
                    unmatchedCachedTimeSeriesDescriptorList.remove(cachedTs.getDescriptor());
                     
                    mergedTs.replaceNonMatchingTimeValuePairs(cachedTs);  
                }
                else //cachedTs == null //did not find a match
                {
                    
                    mergedTs.setMergerString(FRESH_ONLY);
                    
                    
                    System.out.println(header
                            + "ERROR - Could not find a match for the \nfreshTs Descriptor = "
                            + freshTsDesc);
              
                }  
                
                // add the merged time series to the list
                mergedTimeSeriesList.add(mergedTs);
        
                
            } //end for each freshTimeSeriesList
            
            
            // For each time series that was in the cachedTimeSeriesList, but not in the fresh data time series List,
            // trim the times neatly and then let that be the merged list, since there
            // really is no merger possible
            
            RegularObsTimeSeries unmatchedCachedTs = null;
            RegularObsTimeSeriesDescriptor unmatchedCachedTsDesc = null;
                       
           // System.out.println(header + "before unmatched ts loop");
   
            int matchedIndex = -1;
            int size = unmatchedCachedTimeSeriesDescriptorList.size();
          //  System.out.println(header + " preparing to loop " + size + " times. ");
            
            for (int i = 0; i < size ; i++)
            {                    
                unmatchedCachedTsDesc = (RegularObsTimeSeriesDescriptor) 
                                        unmatchedCachedTimeSeriesDescriptorList.get(i);
                unmatchedCachedTs = (RegularObsTimeSeries) cachedTimeSeriesMap.get(unmatchedCachedTsDesc);
     
                
                if (unmatchedCachedTs != null)
                {               
                    unmatchedCachedTs.setStartTime(newStartTime);
                    unmatchedCachedTs.setEndTime(endTime);
                    unmatchedCachedTs.setMergerString(CACHE_ONLY);
                                     
                    matchedIndex = addToOrderedMergedTimeSeriesList(unmatchedCachedTs,
                                                                    mergedTimeSeriesList,
                                                                    matchedIndex+1);          
                }
                else
                {  
                    System.err.println(header + "ERROR unexpectedly null time series");
                }
                 
            } // end for each unmatchedTimeSeries
                    
        } //end if
         
        else //missing either time series
        {
            // there is no cached list to merge, so the freshTimeSeries List is expected to contain 
            // maxHours worth of data . Normally, it will contain less data. 
            //System.out.println(header + "");
            mergedTimeSeriesList = freshTimeSeriesList;
            
        }
             
        return mergedTimeSeriesList;
    }
    
//  -------------------------------------------------------------------------    
    private static int addToOrderedMergedTimeSeriesList(RegularObsTimeSeries timeSeries,
                                                        List mergedTimeSeriesList,
                                                        int startingIndex)
    {
        String header = "TimeSeriesListMgr.addToOrderedMergedTimeSeriesList(): ";
        int matchingIndex = -1;
        RegularObsTimeSeries curTs = null;
        RegularObsTimeSeriesDescriptor curTsDesc = null;
  
        RegularObsTimeSeriesDescriptor tsDesc = timeSeries.getDescriptor();
        
        boolean inserted = false;
        
        for (int i = startingIndex; i < mergedTimeSeriesList.size(); i++)
        {
            curTs = (RegularObsTimeSeries) mergedTimeSeriesList.get(i);         
            curTsDesc = curTs.getDescriptor();
               
            int compareValue = tsDesc.toString().compareTo(curTsDesc.toString());
             
            if (compareValue < 0)
            {
                 mergedTimeSeriesList.add(i, timeSeries);
                 matchingIndex = i;
                 inserted = true;
                 break;
            }   
            else if (compareValue == 0)
            {
                 System.out.println(header + "ERROR, the compare value equals 0.  Not expected. ");
                 break;
            }
        }
        
        if (! inserted) //either only item in list, or multiple items and it is last
        {
            mergedTimeSeriesList.add(timeSeries); 
            matchingIndex = mergedTimeSeriesList.size() - 1;
        }
        
        return matchingIndex;
        
    }
    
//  -------------------------------------------------------------------------    
 
    
    private static List createUnmatchedTimeSeriesDescriptorList(List regularObsTimeSeriesList)
    {
        List newList = new ArrayList();
        
        for (int i = 0; (regularObsTimeSeriesList != null ) && i < regularObsTimeSeriesList.size(); i++)
        {
            RegularObsTimeSeries ts = (RegularObsTimeSeries)  regularObsTimeSeriesList.get(i);
            RegularObsTimeSeriesDescriptor desc = ts.getDescriptor();
            newList.add(desc);
        }
         
        return newList;
    }
//  -------------------------------------------------------------------------    
    
    private static Map createTimeSeriesMap(List regularObsTimeSeriesList)
    {
        Map newMap = new HashMap();
        
        for (int i = 0; (regularObsTimeSeriesList != null ) && i < regularObsTimeSeriesList.size(); i++)
        {
            RegularObsTimeSeries ts = (RegularObsTimeSeries)  regularObsTimeSeriesList.get(i);
            newMap.put(ts.getDescriptor(), ts);
        }
         
        return newMap;
    }
//  -------------------------------------------------------------------------    
    private static RegularObsTimeSeries findMatchingTimeSeries(RegularObsTimeSeries tsToFindMatchFor, Map timeSeriesMap)
    {
         RegularObsTimeSeries foundTs = null;
         RegularObsTimeSeriesDescriptor descriptor = null;
         RegularObsTimeSeriesDescriptor alteredDescriptorCopy = null;
         boolean changedDuration = false;
         short previousDuration = -1;
         
         if (timeSeriesMap != null)
         {     
             descriptor = tsToFindMatchFor.getDescriptor();
             
             Object key = null;
             
             if ( descriptor.getPe().equals("PC")) //need to change the duration to 0
             {
                 previousDuration = descriptor.getDur();
                 changedDuration = true;
                 descriptor.setDur( (short) 0);
                 key = descriptor;
                 
                // alteredDescriptorCopy = new RegularObsTimeSeriesDescriptor(descriptor);
                // alteredDescriptorCopy.setDur((short)0);
                // key = alteredDescriptorCopy;
             }
             else //everything is normal, no changes needed
             {
                 key = descriptor;
             }
             
             foundTs = (RegularObsTimeSeries) timeSeriesMap.get(key);
             
             if (changedDuration)
             {
                 descriptor.setDur(previousDuration);
             }
         }
        
         return foundTs;
    }
    
//  -------------------------------------------------------------------------    
    
    private static RegularObsTimeSeries copyTimeSeriesWithAdjustedStartTime(RegularObsTimeSeries origTs,
                                                                     long newStartTime)
    {
        String header = "PDCPreprocessorDataMgr.copyTimeSeriesWithAdjustedStartTime(): ";
        RegularObsTimeSeries newTs = null;
        RegularObsTimeSeriesDescriptor newTsDesc = null;
        RegularObsTimeSeriesDescriptor origTsDesc = null;
        
        origTsDesc = origTs.getDescriptor();
       
        
        newTs = new RegularObsTimeSeries(origTs);
         
 /*       
        if (origTsDesc.getLid().equals("KERO2")
                && origTsDesc.getPe().equals("PC"))
        {
            System.out.println("***************************************");
            System.out.println(header + "after copying \n"
                    + "\n origTimeSeries = " + origTs + "\n newTimeSeries  = "
                    + newTs);
        }
 */       
        // adjust the start time of the mergedTs so that it goes back as far as
        // we want it to
        long endTime = origTsDesc.getEndTime();
        
    
        newTsDesc  = newTs.getDescriptor();      
        newTsDesc.setStartTime(newStartTime);
        newTsDesc.setEndTime(endTime);
        
 /*       
        if (origTsDesc.getLid().equals("KERO2")
                && origTsDesc.getPe().equals("PC"))
        {
            System.out.println(header + "after changing start and end times \n"
                    + "\n origTimeSeries = " + origTs + "\n newTimeSeries  = "
                    + newTs);
         
            System.out.println(header + "\nold startTime = "  +
                    DbTimeHelper.getDateTimeStringFromLongTime(origTsDesc.getStartTime()) +
                    "\nold EndTime = " +
                    DbTimeHelper.getDateTimeStringFromLongTime(origTsDesc.getEndTime()) +
                    "\nnew startTime = " + 
                    DbTimeHelper.getDateTimeStringFromLongTime(newStartTime) +
                    "\nnew endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
            System.out.println("***************************************");
                    
        }
*/
        
        return newTs;
    }

//  -------------------------------------------------------------------------    

}
