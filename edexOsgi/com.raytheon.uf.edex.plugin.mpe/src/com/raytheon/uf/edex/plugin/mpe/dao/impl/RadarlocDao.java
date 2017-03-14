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
package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Radarloc} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2016 5576       bkowal      Initial creation
 * Jun 30, 2016 4625       bkowal      Added {@link #getRadarsInUse()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RadarlocDao extends AbstractIHFSDbDao<Radarloc, String> {

    public RadarlocDao() {
        super(Radarloc.class);
    }

    public List<Radarloc> getNumAndOfficeForRadarIds(final Set<String> radarIds) {
        List<?> returnObjects = this
                .findPartialRecordByNamedQueryAndNamedCollection(
                        Radarloc.SELECT_NUM_AND_OFFICE_BY_ID, "radids",
                        radarIds);
        if (returnObjects.isEmpty()) {
            return Collections.emptyList();
        }

        List<Radarloc> records = new ArrayList<>(returnObjects.size());
        for (Object returnObject : returnObjects) {
            Object[] objects = (Object[]) returnObject;
            // column return order: radid, radarNum, officeId
            Radarloc radarloc = new Radarloc();
            radarloc.setRadid((String) objects[0]);
            radarloc.setRadarNum((Short) objects[1]);
            radarloc.setOfficeId((String) objects[2]);
            records.add(radarloc);
        }
        return records;
    }

    public List<Radarloc> getRadarsInUse() {
        return findByNamedQuery(Radarloc.SELECT_WHERE_USE_RADAR);
    }
}