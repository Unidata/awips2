/*
 * Created on Apr 22, 2004
 *
 * 
 */
package ohd.hseb.sshp;

import java.util.Date;
import java.text.SimpleDateFormat;
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
   private int _hashCode = -1;

// -----------------------------------------------------------------

   public SacSmaStateDescriptor(SacSmaState state)
   {
       _state = state;
       
   }
// -----------------------------------------------------------------
   public int hashCode()
   {
       if (_hashCode == -1)
       {
           _hashCode = _state.toString().hashCode();
       }
       return _hashCode;
   }
   
// -----------------------------------------------------------------
   public boolean equals(Object otherObject)
   {
    
      return equals((SacSmaStateDescriptor) otherObject);
       
   }
   
   public boolean equals(SacSmaStateDescriptor descriptor)
   {
    
       boolean result = false;
        
       if (  ( descriptor != null) && 
             (this.getState().equals(descriptor.getState())) 
          )
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
            outString = String.format("%10s State at       %15s ", 
                    _state.getSource(), getDateTimeStringToMinutes(_state.getValidTime()) ); 
            
           // String sourceString = _state.getSource();
           // outString =  sourceString + " State " + "at "  + 
          //  getDateTimeStringToMinutes(_state.getValidTime());
       }
       else  //is bogus default value
       {
           outString = String.format("%10s State at       %15s ", 
                   "BOGUS DFLT", getDateTimeStringToMinutes(_state.getValidTime()) ); 
           // outString = "BOGUS DEFAULT" + " State " + "at "  + 
           //            getDateTimeStringToMinutes(_state.getValidTime());
       }
       return outString;    
   }
// -----------------------------------------------------------------

}
