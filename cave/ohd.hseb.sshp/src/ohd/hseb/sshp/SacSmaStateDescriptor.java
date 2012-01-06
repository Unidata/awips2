/*
 * Created on Apr 22, 2004
 *
 * 
 */
package ohd.hseb.sshp;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import ohd.hseb.model.sacsma.SacSmaState;

/**
 * @author GobsC
 *
 * This class is used to summarize the states available for use by
 * SSHP.  Instances of this class may be used as items in a ComboBox.
 */
public class SacSmaStateDescriptor
{

   private SacSmaState _state = null;
   private boolean _isDefaultValue = false;

// -----------------------------------------------------------------

   public SacSmaStateDescriptor(SacSmaState state)
   {
       _state = state;
       
   }
// -----------------------------------------------------------------
   
   public boolean equals(SacSmaStateDescriptor descriptor)
   {
    
       boolean result = false;
       
       if (this.getState().equals(descriptor.getState()))
       {
           result = true;    
       } 
       
       return result; 
       
   }
// -----------------------------------------------------------------
   public long getValidTime()
   {
        long time = 0;
        
        if (_state != null)
        {
            time = _state.getValidTime();        
        }   
        
        return time; 
   }
   
// -----------------------------------------------------------------

   public void setIsDefaultValue(boolean isDefaultValue)
   {
         _isDefaultValue = isDefaultValue;
   }
//   ---------------------------------------------------------------

   public boolean isDefaultValue()
   {
         return _isDefaultValue;
   }
//   ---------------------------------------------------------------

// -----------------------------------------------------------------

   public void setState(SacSmaState state)
   {
       _state = state;
   }
//-----------------------------------------------------------------

   public SacSmaState getState()
   {
       return _state;
   } 
// -----------------------------------------------------------------
   private String getDateTimeStringToMinutes(long time)
   {

          SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm");
          sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
          String timeString = sdf.format(new Date(time)) + " Z";

          return timeString;
   }
// -----------------------------------------------------------------
    
   public String toString()
   {
       String outString = null;
       
       if (!isDefaultValue())
       {
            outString = _state.getSource()+ " State " + "at "  + 
            getDateTimeStringToMinutes(_state.getValidTime());
       }
       else  //is bogus default value
       {
            outString = "BOGUS DEFAULT" + " State " + "at "  + 
                       getDateTimeStringToMinutes(_state.getValidTime());
       }
       return outString;    
   }
// -----------------------------------------------------------------

}
