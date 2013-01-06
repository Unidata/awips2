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

import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * Exception indicating that multiple mappings are defined when the LevelMapper
 * is used to alias to a single level. Catching this exception gives a decoder
 * the ability to easily grab all the levels or an arbitrary level so it can
 * store the data while logging an exception.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MultipleLevelMappingException extends MultipleMappingException {

    private static final long serialVersionUID = -2542711470181578302L;

    private final Set<Level> levelMappings;

    public MultipleLevelMappingException(String name, String namespace,
            Set<String> mappings, Set<Level> levelMappings) {
        super(true, name, namespace, mappings);
        this.levelMappings = levelMappings;
    }

    /**
     * Get all the matching levels, same as if LevelMapper.lookupLevels was
     * called to begin with.
     * 
     * @return
     */
    public Set<Level> getLevelMappings() {
        return levelMappings;
    }

    /**
     * Get one level that was mapped by the alias, which level specifically is
     * not well defined.
     * 
     * @return
     */
    public Level getArbitraryLevelMapping() {
        return levelMappings.iterator().next();
    }

}
