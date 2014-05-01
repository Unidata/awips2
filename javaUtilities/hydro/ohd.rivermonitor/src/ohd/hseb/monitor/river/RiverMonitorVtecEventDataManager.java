package ohd.hseb.monitor.river;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.VTECeventRecord;

public class RiverMonitorVtecEventDataManager 
{
 
    private List _vtecAllEventList = null;
    private List _mostRecentEventList = null;
    private List _mostRecentActiveEventList = null;
    private VTECeventRecord _vtecEventRecordForEndTime = null;
    private VTECeventRecord _vtecEventRecordForExpireTime = null;
    
    public RiverMonitorVtecEventDataManager(List vtecAllEventList)
    {
        _vtecAllEventList = vtecAllEventList;
    }
    
    public List getMostRecentEventList()
    {
        if(_mostRecentEventList == null)
            _mostRecentEventList = calculateMostRecentEventList();
        return _mostRecentEventList;
    }
    
    public List getMostRecentActiveEventList()
    {
        if(_mostRecentActiveEventList == null)
            _mostRecentActiveEventList = calculateMostRecentActiveEventList();
        return _mostRecentActiveEventList;
    }
    
    public VTECeventRecord getVtecEventRecordForEndTime()
    {
        return _vtecEventRecordForEndTime;    
    }
    
    public VTECeventRecord getVtecEventRecordForExpireTime()
    {
      return _vtecEventRecordForExpireTime;   
    }
    
    //Creates the list that contains the recent event info for each signif
    public List calculateMostRecentEventList()
    {
        Map mostRecentEventMap = new HashMap();
        VTECeventRecord mostRecentEventRecord = null;

        if(_vtecAllEventList != null)
        {
            for(int i=0; i < _vtecAllEventList.size(); i++)
            {
                VTECeventRecord record = (VTECeventRecord) _vtecAllEventList.get(i);
                String key = record.getSignif();

                mostRecentEventRecord = (VTECeventRecord) mostRecentEventMap.get(key);

                //the first time, add the current record
                if (mostRecentEventRecord == null)
                {
                    mostRecentEventMap.put(key, record);
                    mostRecentEventRecord = record;
                }

                else // this is not the first active record for this geoId|signif
                {
                    //this one is more recent, so replace the old record.
                    if (record.getProducttime() > mostRecentEventRecord.getProducttime())
                    {
                        mostRecentEventMap.put(key, record);
                    }
                }
            }
        }
        List mostRecentEventList = new ArrayList (mostRecentEventMap.values());
        
        return mostRecentEventList;
    }
 
    public List calculateMostRecentActiveEventList()
    {
        List mostRecentActiveEventList = null;
        
        getMostRecentEventList();
        
        if(_mostRecentEventList != null)
        {
            for(int i=0;i < _mostRecentEventList.size(); i++) 
            {
                VTECeventRecord record = (VTECeventRecord) _mostRecentEventList.get(i); 
                if(RiverMonitorVtecEventDataManager.checkIfEventActive(record))
                {
                    if(mostRecentActiveEventList == null)
                    {
                        mostRecentActiveEventList = new ArrayList();       
                    }
                
                    mostRecentActiveEventList.add(record);
                }
            }
        }
        return mostRecentActiveEventList;
    }
    
    public static boolean checkIfEventActive(VTECeventRecord record)
    {
        boolean           active = false;
        String action = record.getAction();
        long endTime  = record.getEndtime();
        long curTime  = System.currentTimeMillis();
        /* if the event is explicity expired or cancelled,
         then the event tracking is not active.
         also, all ROU events are considered inactive.
         otherwise, see if the event end time has not passed yet */
        
        if (action.equals("CAN") || action.equals("EXP") ||
                action.equals("ROU") || action.length() == 0)
        {
            active = false;
        }
        else
        {
            /* if the endtime is not specified, assume the event is still active.
             unspecified can occur if the endtime has a missing value indicator
             or is set to 0.  A 0 value can occur because the time may be NULL in
             the database, which is converted to a 0 value by the time conversion
             functions. */
            
            if (endTime == (long) DbTable.getNullLong())
                active = true;
            else  //end time is past the current time, so the event is still active.
            {
                if (endTime > curTime)
                    active = true;
                else
                    active = false;
            }
        }
        return(active);
    }
    

    public boolean thereAreAnyActiveEventsForThisLocation()
    {
        boolean result = false;
        
        getMostRecentActiveEventList();
        
        if(_mostRecentActiveEventList != null)//there are active events 
        {
            result = true;
        }
        return result;
    }

    public String getVtecSummary()
    {
        String summary = "";
        StringBuffer summaryStringBuffer = new StringBuffer("");
        
        getMostRecentActiveEventList();

        if(thereAreAnyActiveEventsForThisLocation())
        {
            for(int i=0;i < _mostRecentActiveEventList.size(); i++)
            {
                VTECeventRecord record = (VTECeventRecord) _mostRecentActiveEventList.get(i);
                summaryStringBuffer = summaryStringBuffer.append(record.getSignif());
            }
        }
        summary = summaryStringBuffer.toString();
        return summary;
    }

    public long getEventEndTime(long earliestAcceptableTimeForVtecEventProductTimeLookBack)
    {
        VTECeventRecord record = null;
        long endTime = DbTable.getNullLong();

        getMostRecentActiveEventList();
        
        //if there are any active events among the recent event for various signif
        //use the one that has the endtime close to current time (which is the earliest endtime in the most recent event list)
        if(thereAreAnyActiveEventsForThisLocation())
        {
            // loop thru the mostRecentActiveEventsList
            // This list will have size 0 (min) size 3 (max- accouting for W A Y)
            // Find the earliest endtime
            // Ignore the record with missing endtime

            long tempEndTime = Long.MAX_VALUE;
            for (int i = 0; i < _mostRecentActiveEventList.size(); i++)
            {
                record = (VTECeventRecord) _mostRecentActiveEventList.get(i);  

                if(record.getEndtime() != DbTable.getNullLong())
                {
                    if(record.getEndtime() < tempEndTime)
                    {
                        tempEndTime = record.getEndtime();
                        endTime = tempEndTime;
                        _vtecEventRecordForEndTime = record;
                    }
                }
            }

            if(endTime == DbTable.getNullLong()) //ie., The recent active list has 1 or more events and all of their endtime is null
            {
                _vtecEventRecordForEndTime = (VTECeventRecord)_mostRecentActiveEventList.get(0);
            }
        }
        else if(_mostRecentEventList != null)//No active events going on, so consider the latest/most recent endtime(max of all endtime)
        {
            for (int i = 0; i < _mostRecentEventList.size(); i++)
            {
                record = (VTECeventRecord) _mostRecentEventList.get(i);  

                if(i==0)
                {
                    endTime = record.getEndtime();
                    _vtecEventRecordForEndTime = record;
                }
                else
                {
                    if(record.getEndtime() >  endTime)
                    {
                        endTime = record.getEndtime();
                        _vtecEventRecordForEndTime = record;
                    }
                }
            }
            
            //check if the record's product time is within the window
            if( _vtecEventRecordForEndTime != null &&
                    (_vtecEventRecordForEndTime.getProducttime() < earliestAcceptableTimeForVtecEventProductTimeLookBack))
            {
                _vtecEventRecordForEndTime = null;
                endTime = DbTable.getNullLong();
            }
        }

        return endTime;
    }

    public long getUGCExpireTime(long earliestAcceptableTimeForVtecEventProductTimeLookBack)
    {
        VTECeventRecord record = null;
        long expireTime = DbTable.getNullLong();

        getMostRecentActiveEventList();
        
        //if there are any active events among the recent event for various signif
        //use the one that has the expiretime close to current time (which is the earliest endtime in the most recent event list)
        if(thereAreAnyActiveEventsForThisLocation())
        {
            // loop thru the mostRecentActiveEventsList
            // This list will have size 0 (min) size 3 (max- accouting for W A Y)
            // Find the earliest expiretime
            // Ignore the record with missing expiretime

            long tempExpireTime = Long.MAX_VALUE;
            for (int i = 0; i < _mostRecentActiveEventList.size(); i++)
            {
                record = (VTECeventRecord) _mostRecentActiveEventList.get(i);  

                if(record.getExpiretime() != DbTable.getNullLong())
                {
                    if(record.getExpiretime() < tempExpireTime)
                    {
                        tempExpireTime = record.getExpiretime();
                        _vtecEventRecordForExpireTime = record;
                        expireTime = tempExpireTime;
                    }
                }
            }

            if(expireTime == DbTable.getNullLong()) //ie., The recent active list has 1 or more events and all of their expiretime is null 
                _vtecEventRecordForExpireTime = (VTECeventRecord) _mostRecentActiveEventList.get(0);// expiretime will be TBD

        }
        else if(_mostRecentEventList != null)//No active events going on, so consider the latest/most recent expiretime(max of all expiretime)
        {
            for (int i = 0; i < _mostRecentEventList.size(); i++)
            {
                record = (VTECeventRecord) _mostRecentEventList.get(i);  

                if(i==0)
                {
                    expireTime = record.getExpiretime();
                    _vtecEventRecordForExpireTime = record;
                }
                else
                {
                    if(record.getExpiretime() >  expireTime)
                    {
                        expireTime = record.getExpiretime();
                        _vtecEventRecordForExpireTime = record;
                    }
                }
            }
            
            //check if the record's product time is within the window
            if(_vtecEventRecordForExpireTime != null &&
                    (_vtecEventRecordForExpireTime.getProducttime() < earliestAcceptableTimeForVtecEventProductTimeLookBack))
            {
                _vtecEventRecordForExpireTime = null;
                expireTime = DbTable.getNullLong();
            }
        }

        return expireTime;
    }

   
//  -------------------------------------------------------------------------------------

}
