/*
 * Created on Apr 19, 2004
 *
 * 
 */
package ohd.hseb.sshp;

import ohd.hseb.measurement.*;

import java.util.Date;
import java.util.TimeZone;
import java.text.*;

/**
 * @author GobsC
 *
 * 
 */
public class FFHDescriptor
{
    private String _locationId = null;
    
    private AbsTimeMeasurement _ffhMeasurement = null;
    private int _ihfsDurationCode = 0;
    
    private boolean _isGridded = false;
    private boolean _isDefaultValue = false;
    
    private String _description = null;
 
//  ---------------------------------------------------------------
    
    public FFHDescriptor(String locationId, 
                         AbsTimeMeasurement measurement,
                         int ihfsDurationCode,
                         boolean isGridded)
    {
        _locationId = locationId;
        _ffhMeasurement = measurement;
        _ihfsDurationCode = ihfsDurationCode;
        _isGridded = isGridded;

        return;
    }

// ---------------------------------------------------------------
 

    
    public void setFfhMeasurement(AbsTimeMeasurement ffhMeasurement)
    {
        _ffhMeasurement = ffhMeasurement;
    }
//  ---------------------------------------------------------------

    public Measurement getFfhMeasurement()
    {
        return _ffhMeasurement;
    }
//  ---------------------------------------------------------------
    public double getValueInInches()
    {
        return _ffhMeasurement.getValue(MeasuringUnit.inches);
    }
//  ---------------------------------------------------------------
    public long getTime()
    {
        return _ffhMeasurement.getTime();
    }
    
//  ---------------------------------------------------------------

    /**
     * @param ihfsDurationCode The ihfsDurationCode to set.
     */
    public void setIhfsDurationCode(int ihfsDurationCode)
    {
        _ihfsDurationCode = ihfsDurationCode;
    }

//  ---------------------------------------------------------------

    /**
     * @return Returns the ihfsDurationCode.
     */
    public int getIhfsDurationCode()
    {
        return _ihfsDurationCode;
    }


//  ---------------------------------------------------------------

    public void setDescription(String description)
    {
        _description = description;
    }
//  ---------------------------------------------------------------

    public String getDescription()
    {
        return _description;
    }
//  ---------------------------------------------------------------

    public void setGridded(boolean isGridded)
    {
        _isGridded = isGridded;
    }
//  ---------------------------------------------------------------

    public boolean isGridded()
    {
        return _isGridded;
    }

 //---------------------------------------------------------------------- 
    private String getDateTimeStringToMinutes(long time)
    {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        String timeString = sdf.format(new Date(time)) + "Z";

        return timeString;
    }

   
//  ---------------------------------------------------------------
    public boolean equals(FFHDescriptor descriptor)
    {
        boolean result = false;
        
        if (
        
            (this.getTime() == descriptor.getTime()) &&
            (this._isGridded == descriptor._isGridded)&&
            (this._ffhMeasurement.equals(descriptor._ffhMeasurement) )
          
            
           )
        {
            result = true;    
        }
            
        
        return result;
        
    }

//  ---------------------------------------------------------------
    public void setIsDefaultValue(boolean isDefaultValue)
    {
        _isDefaultValue = isDefaultValue;
    }
//  ---------------------------------------------------------------

    public boolean isDefaultValue()
    {
        return _isDefaultValue;
    }
//  ---------------------------------------------------------------
    
    public String toString()
    {
        String outString = null;
            
        String timeString = getDateTimeStringToMinutes(this.getTime());
            
        double value = this.getValueInInches();
            
        String ffhTypeString= null;
            
        if (this.isGridded())
        {
             ffhTypeString = "Gridded";
        }
        else if (! isDefaultValue() )
        {
             ffhTypeString = "Areal";
        }  
        else
        {
             ffhTypeString = "BOGUS DEFAULT";    
        }

        NumberFormat format = new DecimalFormat("0.00");
        
        String formattedString = format.format(value);
            
        int duration = _ihfsDurationCode - 1000;

        outString = _locationId + " " + timeString + " " + 
                                  duration + " hr " +
                                  ffhTypeString + " FFH=" + 
                                  formattedString;

        return outString;
    } //end toString

   

}
