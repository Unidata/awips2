/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.uengine;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Reads the .js files on the server side and returns them.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 5, 2007                      njensen             Initial Creation
 *
 * </PRE>
 *
 */
public class ScriptLibraryManager
{
    
    private static final transient Log logger = LogFactory.getLog(ScriptLibraryManager.class);
    
    private static final String LIBRARY_KEY = "JSLIBRARY";
    
    private static String libraryPath = null;
    
    private static HashMap<String, String> libraries = new HashMap<String, String>();
    
    private static final String DEVMODE_KEY = "DEVMODE";
    
    private static String devMode = null;

    /**
     * Returns the JavaScript library with the specified name, e.g. util.js
     * @param aLibraryName the name of the library to return
     * @return the contents of the library
     */
    public static String getLibrary(String aLibraryName)
    {
        boolean devMode = getDevMode();
        String code = libraries.get(aLibraryName);
        if(code == null || devMode)
        {            
            try
            {
                code = loadLibrary(aLibraryName);
            }
            catch (IOException e)
            {
                logger.warn("Unable to load JavaScript library " + aLibraryName, e);
            }
        }
        
        return code;
    }
    
    /**
     * Reads in the specified library and stores it in the HashMap.
     * @param aLibraryName the name of the library to read in
     * @return the contents of the file
     * @throws IOException
     */
    private static String loadLibrary(String aLibraryName) throws IOException
    {
        logger.debug("Loading library: " + aLibraryName);
        String eol = Util.EOL;
        StringBuffer buffer = new StringBuffer();
        String path = getLibraryDirectory() + aLibraryName; 
        FileReader fr = new FileReader(path);        
        BufferedReader reader = new BufferedReader(fr);
        String line = "";
        while(line != null)
        {
            buffer.append(line).append(eol);
            line = reader.readLine();
        }
        
        reader.close();
        fr.close();
        
        String text = buffer.toString();
        libraries.put(aLibraryName, text);
        return text;                        
    }
    
    /**
     * Gets the directory for js libraries from the EnvProperties
     * @return the directory for server side js libraries
     */
    private static String getLibraryDirectory()
    {
        if(libraryPath == null)
        {
            EnvProperties props = PropertiesFactory.getInstance().getEnvProperties();
            libraryPath = props.getEnvValue(LIBRARY_KEY);
        }
        
        return libraryPath;
    }
    /**
     * 
     * @return
     */
    private static boolean getDevMode() {
        if (devMode == null) {
            EnvProperties props = PropertiesFactory.getInstance().getEnvProperties();
            String key = props.getEnvValue(DEVMODE_KEY);
            devMode = System.getProperty(key);
            
        }
        return devMode.equals("on");
    }
}
