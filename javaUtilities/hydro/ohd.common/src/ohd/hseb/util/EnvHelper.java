
package ohd.hseb.util;

import java.util.*;
import java.io.*;

public class EnvHelper
{
    private Properties _props = null;
    
    //--------------------------------------------------------------
    // Note, this version of EnvHelper uses the returned System.getenv() method.
    // It was unavailable for a few java versions (1.3.x to 1.4.x ?)
    // Code timings on a Linux box reveal an improvement with the old code taking
    // 54 milliseconds and the new code taking only 5 millis in SSHP.
    //--------------------------------------------------------------
  
    public Properties getProperties()
    {
          
        if (_props == null)
        {
           _props = new Properties();
                                       
            _props.putAll(System.getenv());
            
        } //end if
              
        return _props;
    }
    
    //--------------------------------------------------------------
    
    private Properties getProperties_old()
    {
         
        if (_props == null)
        {
           _props = new Properties();
           
            Process process = null;
         
            String commandString = this.getEnvCommandString();          
            Runtime runtime = Runtime.getRuntime();
            
            try
            {
                 
                process = runtime.exec(commandString);
         
                InputStream inStream = process.getInputStream();
            
            
                BufferedReader br = new BufferedReader
                             ( new InputStreamReader( inStream)  );
                String line;
                while( (line = br.readLine()) != null )
	            {
	                int index = line.indexOf( '=' );
	                if (index < 0)
	                {
	                	//something is wrong with the line,
	                	// ignore it and read the next line
	                    continue;	
	                }
	                
	                String key = line.substring( 0, index );
	                String value = line.substring( index+1 );
	                
	                // works in java >= 1.2
	                //_props.setProperty( key, value );
	                
	                // works in java < 1.2, and later, but is not prefered.
	                _props.put(key, value);
	                
	               // System.out.println( key + " = " + value );
	            }
            }
			catch (java.io.IOException e)
			{
			    e.printStackTrace();
				return _props;
			}
        

        } //end if
      
        return _props;
    }
    
//  --------------------------------------------------------------
    private String getEnvCommandString()
    {
        String OS = System.getProperty("os.name").toLowerCase();
        String commandString = "";
        
        if (OS.indexOf("windows 9") > -1)
        {
           commandString = "command.com /c set " ;
        }
        else if ( (OS.indexOf("nt") > -1)
               || (OS.indexOf("windows 2000") > -1)
               || (OS.indexOf("windows xp") > -1) )
        {
            // thanks to JuanFran for the xp fix!
            commandString = "cmd.exe /c set " ;
        }
        else //assume unix
        {
            commandString = "env";
        }
        
        return commandString;
    }
    
    //--------------------------------------------------------------
    public String getProperty(String key) 
    {
         Properties props = this.getProperties();
          
         return props.getProperty(key);
    }
    //--------------------------------------------------------------
    public static void main(String[] args)
    {
        EnvHelper envHelper = new EnvHelper();

      
            Properties env = envHelper.getProperties();
            
            if (env == null) 
            {
                System.out.println("dude, env is null");
            }
            else
            {
                System.out.println("------------------");
                env.list(System.out);
            }
            
            String tempVar = envHelper.getProperty("TEMP");

            System.out.println("TEMP = " + tempVar);
     
        
        return;
    }

}