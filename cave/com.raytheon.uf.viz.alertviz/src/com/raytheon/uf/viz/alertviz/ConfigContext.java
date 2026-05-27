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
package com.raytheon.uf.viz.alertviz;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;

/**
 * Class for managing configuration context.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09 2015  4346       rferrel     Added {@link #ALERTVIZ_DIR} and {@link #XML_EXT}.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class ConfigContext implements Comparable<ConfigContext> {

    protected static final String ALERTVIZ_DIR = "alertViz";

    protected static final String XML_EXT = ".xml";

    protected static final String DEFAULT_NAME = "Default";

    protected static final String DEFAULT_SUBDIR = "configurations";

    private final String name;

    private final LocalizationLevel level;

    private String localizationFileName;

    public ConfigContext(String name, String subDirectory,
            LocalizationLevel level) {
        this.name = (name == null && level.equals(LocalizationLevel.BASE)) ? DEFAULT_NAME
                : name;
        this.level = level;
        String newName = name;
        if (newName.endsWith(XML_EXT) == false) {
            newName += XML_EXT;
        }
        this.localizationFileName = ALERTVIZ_DIR + IPathManager.SEPARATOR
                + subDirectory + IPathManager.SEPARATOR + newName;

    }

    public ConfigContext(String name, LocalizationLevel level) {
        this(name, DEFAULT_SUBDIR, level);
    }

    public ConfigContext(LocalizationContext context) {
        this(context.getContextName(), context);
    }

    public ConfigContext(String name, LocalizationContext context) {
        this(name, DEFAULT_SUBDIR, context.getLocalizationLevel());
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return name + " (" + getLevel() + ")";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((getLevel() == null) ? 0 : getLevel().hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ConfigContext other = (ConfigContext) obj;
        if (level != other.level)
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }

    @Override
    public int compareTo(ConfigContext o) {
        if (level.equals(o.level)) {
            return name.compareTo(o.name);
        }
        return level.compareTo(o.level);
    }

    /**
     * @return the level
     */
    public LocalizationLevel getLevel() {
        return level;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the localizationFileName
     */
    public String getLocalizationFileName() {
        return localizationFileName;
    }

    /**
     * @param localizationFileName
     *            the localizationFileName to set
     */
    public void setLocalizationFileName(String localizationFileName) {
        this.localizationFileName = localizationFileName;
    }

    public boolean isBaseOrConfiguredLevel() {
        return LocalizationLevel.BASE.equals(this.level)
                || LocalizationLevel.CONFIGURED.equals(this.level);
    }
}