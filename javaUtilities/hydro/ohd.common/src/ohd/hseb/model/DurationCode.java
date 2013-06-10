/*
 * Created on Aug 20, 2004
 *
 * 
 */
package ohd.hseb.model;

/**
 * @author GobsC
 *
 *
 */
public class DurationCode
{
    public static int getHoursFromCode(int durationCode)
    {
        int hours = 0;
        if ((durationCode >= 1000) && (durationCode < 2000))
        {
            hours = durationCode - 1000;
        }
        else if (durationCode >= 2000 && (durationCode < 3000))
        {
            int days = durationCode - 2000;
            hours = days * 24;
        }
            
        
        return hours;
    }
    
    
    public static int getCodeFromHours(int hours)
    {
        int durationCode = 0;
        
        if (hours <= 999)
        {
            durationCode = 1000 + hours;
        }
     
        return durationCode;
    }
    
    public static int getCodeFromHoursString(String hoursString)
    {
       int hours = Integer.parseInt(hoursString);
       int durationCode = getCodeFromHours(hours);
       
       return durationCode;
       
    }
}
