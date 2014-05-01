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
package com.raytheon.uf.common.monitor.config;

import com.raytheon.uf.common.monitor.config.FFFGConfig.GuidSectType;

/**
 * 
 * This class contains the data used to populate the source components.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ValueNameIdData
{
    /**
     * Value.
     */
    private String value;
    
    /**
     * Name.
     */
    private String name;
    
    /**
     * ID.
     */
    private Long id;
    
    /**
     * Display string.
     */
    private String displayString;
    
    /**
     * Guidance section type.
     */
    private GuidSectType type;
    
    /**
     * Unique ID used to determine if the entry already exists.
     */
    private StringBuilder uniqueId;
    
    /**
     * Constructor.
     * @param value Value.
     * @param name Name.
     * @param id ID.
     * @param type Guidance section type.
     */
    public ValueNameIdData(String value, String name, Long id, GuidSectType type)
    {
        this.value = value;
        this.name = name;
        this.id = id;
        this.type = type;
        
        if (this.type == GuidSectType.COUNTY)
        {
            displayString = this.value + ";" + this.name;
        }
        else
        {
            displayString = this.value + ";" + this.id;
        }
        
        uniqueId = new StringBuilder();
        uniqueId.append(name);
        uniqueId.append(id);
    }

    /**
     * Get the value.
     * @return The value.
     */
    public String getValue()
    {
        return value;
    }

    /**
     * Get the name.
     * @return The name.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Get the ID.
     * @return The ID.
     */
    public Long getId()
    {
        return id;
    }

    /**
     * Get the guidance section type.
     * @return The guidance section type.
     */
    public GuidSectType getType()
    {
        return type;
    }

    /**
     * Get the display string.
     * @return The display string.
     */
    public String getDisplayString()
    {
        return displayString;
    }
    
    /**
     * Get the unique ID string.
     * @return The unique ID string.
     */
    public String getUniqueId()
    {
        return uniqueId.toString();
    }
}
