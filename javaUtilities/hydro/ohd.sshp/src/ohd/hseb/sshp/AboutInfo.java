/*
 * Created on Aug 6, 2004
 *
 *
 */
package ohd.hseb.sshp;

/**
 * @author GobsC
 *
 * 
 */
public class AboutInfo
{
    private static final String _versionString = "OB 9.0";
    private static final String _aboutText =  "    Application : Site Specific Hydrologic Predictor\n" +
                                              "          Version :  " + _versionString + "\n" + 
                                              "               Date :  9/3/2008 " + "\n" + 
                                              " Developed by :\n" +
                                              "                National Weather Service\n" + 
                                              "                Office of Hydrologic Development\n" +
                                              "                Hydrology Laboratory"; 

    private AboutInfo()
    {
        
        
    }
    
    public static String getText()
    {
        return _aboutText;
    }
    
}
