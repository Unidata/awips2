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
package com.raytheon.uf.common.dataplugin.grid.util;

import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;


/**
 * Styling utility for grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridStyleUtil {

    /**
     * Create a style rule match criteria from grid metadata
     * 
     * @param record
     * @return
     */
    public static ParamLevelMatchCriteria getMatchCriteria(GridRecord record) {
        ParamLevelMatchCriteria matchCriteria = new ParamLevelMatchCriteria();
        matchCriteria.setParameterName(new ArrayList<String>());
        matchCriteria.setLevels(new ArrayList<Level>());
        matchCriteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = record.getParameter().getAbbreviation();
        SingleLevel level = GridLevelTranslator.constructMatching(record
                .getLevel());
        String creatingEntity = record.getDatasetId();
        if (!matchCriteria.getParameterNames().contains(parameter)) {
            matchCriteria.getParameterNames().add(parameter);
        }
        if (!matchCriteria.getLevels().contains(level)) {
            matchCriteria.getLevels().add(level);
        }
        if (!matchCriteria.getCreatingEntityNames().contains(creatingEntity)) {
            matchCriteria.getCreatingEntityNames().add(creatingEntity);
        }
        return matchCriteria;
    }

}
