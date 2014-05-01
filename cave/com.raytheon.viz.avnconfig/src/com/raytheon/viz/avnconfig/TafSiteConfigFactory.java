package com.raytheon.viz.avnconfig;

import java.io.FileNotFoundException;

import org.apache.commons.configuration.ConfigurationException;

/**
 * This is a dummy Factory class that allows the use of given class that
 * implements the ITafSiteConfig interface. Intent is to allow changing between
 * classes without having to impact all the other classes that depend on
 * configuration class. This assumes implementing ITafSiteConfig contains the
 * static methods <code>getInstance</code> and <code>clearInstance</code>. if
 * needed it is up to the implementing class to synchronize these methods.
 * 
 * <br>
 * <br>
 * This class is a bridge form using a single xml configuration file for
 * all sites to using the AWIPS I style configuration files. This makes porting
 * from AWIPS I easier for sites.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2010            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class TafSiteConfigFactory {
    /**
     * Get an instance of the desired ITafSiteConfig implementing class.
     * 
     * @return config
     * @throws ConfigurationException
     * @throws FileNotFoundException
     */
    public static ITafSiteConfig getInstance() throws ConfigurationException,
            FileNotFoundException {
        return TafSiteConfigIni.getInstance();
    }

    /**
     * This clears the current instance of the ITafSiteConfig implementing
     * class.
     */
    public static void clearInstance() {
        TafSiteConfigIni.clearInstance();
    }
}
