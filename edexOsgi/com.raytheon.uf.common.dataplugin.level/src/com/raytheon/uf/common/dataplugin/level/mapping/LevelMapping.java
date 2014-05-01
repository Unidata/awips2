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

package com.raytheon.uf.common.dataplugin.level.mapping;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Class defines a mapping from a display level to one or more database levels.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/01/2007    #518      S.Manoj     Initial version
 * 11/16/2009    #3120     rjpeter     Modifed to better integrate with level framework.
 * 11/21/2009    #3576     rjpeter     Added group
 * 04/17/2013    #1913     randerso    Moved to common
 * 
 * &#064;author smanoj
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class LevelMapping implements ISerializableObject {

    @XmlAttribute
    private String displayName = null;

    @XmlAttribute
    private String key = null;

    @XmlAttribute
    private String group = null;

    @XmlElement(name = "DatabaseLevel")
    private List<DatabaseLevelMapping> databaseLevels = null;

    private transient List<Level> levelList = null;

    /**
     * Copies the contents of the given levelMapping into a new LevelMapping
     * 
     * @param levelMapping
     *            from which to create a copy
     */
    public LevelMapping(LevelMapping levelMapping) {
        displayName = levelMapping.displayName;
        key = levelMapping.key;
        databaseLevels = new ArrayList<DatabaseLevelMapping>(
                levelMapping.databaseLevels);
    }

    /**
     * Default constructor
     */
    public LevelMapping() {
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getGroup() {
        return group;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public List<DatabaseLevelMapping> getDatabaseLevels() {
        return databaseLevels;
    }

    public void setDatabaseLevels(List<DatabaseLevelMapping> databaseLevels) {
        this.databaseLevels = databaseLevels;
    }

    public List<Level> getLevels() throws CommunicationException {
        if (levelList == null) {
            List<Level> levelList = new ArrayList<Level>();
            for (DatabaseLevelMapping mapping : databaseLevels) {
                levelList.addAll(mapping.getLevels());
            }
            this.levelList = levelList;
        }

        return levelList;
    }
}
