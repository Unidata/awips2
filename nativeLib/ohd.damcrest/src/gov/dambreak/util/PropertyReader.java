package gov.dambreak.util;

/**
 * This class is used to get properties from the
 * Dambreak Analysis 'resources' file.
 */
import java.util.*;
import java.io.*;

import ohd.hseb.util.*;

public class PropertyReader 
{
    
    private static Properties env = null;
    
    /**
     * This method returns the value of the property contained
     * in the Dambreak Analysis 'resources' file or null if the
     * property is not found.
     * Creation date: (8/4/2003 11:02:25 AM)
     */
    public static String getProperty(String strName) 
    {
        if (env == null) 
        {
            String fileName;
            String osName = System.getProperty("os.name");
            String lowerCaseName = osName.toLowerCase();
            
            if ((fileName = System.getProperty("damcrest.home")) == null)
            {
                return null;
            }
            if(lowerCaseName.indexOf("windows") > -1)
            {
                
                if (!fileName.endsWith(System.getProperty("file.separator")))
                {
                    fileName = fileName.concat(System.getProperty("file.separator"));
                }
                
                fileName = fileName.concat("resources.txt");
                
                // Create an InputStream from the file
                InputStream stream = null;
                env = new Properties();
                try 
                {
                    stream = new FileInputStream(new File(fileName));
                    if (stream != null) 
                    {
                        // Read the file into the env Properties
                        env.load(stream);
                        stream.close();
                    }
                    
                } 
                catch(Exception e) 
                {
                    System.err.println("Could not open resource file \"" + fileName + "\"");
                    return null;
                }
            }
            else
            {
                if(lowerCaseName.indexOf("linux") > -1)
                {
                    if (!fileName.endsWith(System.getProperty("file.separator")))
                    {
                        fileName = fileName.concat(System.getProperty("file.separator"));
                    }
                    
                    String resourcesFileName = fileName.concat("resources.txt");
                    File fileresources = new File(resourcesFileName);
                    
                    if (!fileresources.exists())
                    {
                        ohd.hseb.util.AppsDefaults appsDef = new ohd.hseb.util.AppsDefaults();
                        ohd.hseb.util.EnvHelper envHelper = new EnvHelper();
                        env = new Properties();
                        
                        String DBConnectionName = "CONNECTION_STRING";
                        String DBConnectionString = envHelper.getProperty(DBConnectionName);
                        if (DBConnectionString != null)
                        {
                            DBConnectionString = DBConnectionString.trim();
                            env.setProperty(DBConnectionName, DBConnectionString);
                        }
                        else
                        {
                            System.out.println("DBConnectionString is not set\n");
                        }
                        String DBDriverName = "DBDRIVER";
                        String DBDriver = envHelper.getProperty(DBDriverName);
                        
                        if (DBDriver != null)
                        {
                            DBDriver = DBDriver.trim();
                            env.setProperty(DBDriverName, DBDriver);
                        }
                        else
                        {
                            System.out.println("DBDriver is not set\n");
                        }
                        // TEMPPATH variable is renamed to DAMCREST_DATA_DIR for
                        // better readability
                        String tempPathName = "DAMCREST_DATA_DIR";
                        String tempPath = envHelper.getProperty(tempPathName);
                        if (tempPath != null)
                        {
                            tempPath = tempPath.trim();
                            env.setProperty(tempPathName, tempPath);
                        }
                        else
                        {
                            System.out.println("DAMCREST_DATA_DIR is not set\n");
                        }
                        String tokenEnabled = "damcrest.db_enabled";
                        String strEnabled = appsDef.getToken(tokenEnabled);
                        if(strEnabled != null)
                        {
                            strEnabled = strEnabled.trim();
                            env.setProperty(tokenEnabled, strEnabled);
                        }
                        else
                        {
                            System.out.println("damcrest.db_enabled is not set\n");
                        }
                        String tokenHasListAllDams = "damcrest.hasListAllDams";
                        String strHasListAllDams = appsDef.getToken(tokenHasListAllDams);
                        if(tokenHasListAllDams != null)
                        {
                            tokenHasListAllDams = tokenHasListAllDams.trim();
                            env.setProperty(tokenHasListAllDams, strHasListAllDams);
                        }
                        else
                        {
                            System.out.println("damcrest.hasListAllDams is not set\n");
                        }
                        // There is stored gvim resource file .vimrc
                        String damcrestAppDirPath = "DAMCREST_RES_DIR";
                        String damcrestAppDir = envHelper.getProperty(damcrestAppDirPath);
                        if (damcrestAppDir != null)
                        {
                            damcrestAppDir = damcrestAppDir.trim();
                            env.setProperty(damcrestAppDirPath, damcrestAppDir);
                        }
                        else
                        {
                            System.out.println("DAMCREST_RES_DIR is not set\n");
                        }
                        String tokenEditor = "damcrest.editor";
                        String strEditor = appsDef.getToken(tokenEditor);
                        if(strEditor != null)
                        {
                            strEditor = strEditor.trim();
                            env.setProperty(tokenEditor, strEditor);
                        }
                        else
                        {
                            System.out.println("damcrest.editor is not set\n");
                        }
                    }
                    else
                    {
                        System.out.println("File \"resources.txt\" exists.\n");
                        // Create an InputStream from the file
                        InputStream stream = null;
                        env = new Properties();
                        try 
                        {
                            stream = new FileInputStream(fileresources);
                            if (stream != null) 
                            {
                                // Read the file into the env Properties
                                env.load(stream);
                                stream.close();
                            }  
                        } 
                        catch(Exception e) 
                        {
                            System.err.println("Could not open resource file \"" + fileresources + "\"");
                            return null;
                        }
                    }
                }
            }
        }
        return env.getProperty(strName);
    }
}
