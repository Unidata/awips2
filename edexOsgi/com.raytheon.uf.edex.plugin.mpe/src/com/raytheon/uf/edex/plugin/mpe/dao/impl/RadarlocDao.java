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
import java.util.HashSet;
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
 * Jun 30, 2016 4625       bkowal      Added {@link #getInUseRadarlocs()}.
 * Jul 21, 2016 4622       jschmid     Added {@link #isRadarActive()}.
 * Nov 02, 2016 4622       skorolev    Added {@link #getRadarloc()}
 * Dec 15, 2016 4622       bkowal      Updated {@link #getRadarloc(String)} to only retrieve
 *                                     the immediate record.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RadarlocDao extends AbstractIHFSDbDao<Radarloc, String> {

    public RadarlocDao() {
        super(Radarloc.class);
    }

    /**
     * @param radarIds
     * @return
     */
    public List<Radarloc> getNumAndOfficeForRadarIds(
            final Set<String> radarIds) {
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

    /**
     * @return List of active radars
     */
    public List<Radarloc> getInUseRadarlocs() {
        return findByNamedQuery(Radarloc.SELECT_WHERE_USE_RADAR);
    }

    /**
     * Use product radarId to check if given radar-key has: use_radar set to
     * 'T'.
     * 
     * @param radarId
     *            RadarId primary-key to check boolean value for.
     * @return boolean of true if radarId had column use_radar = 'T'.
     */
    public boolean isRadarActive(String radarId) {

        Set<String> radarIdSet = new HashSet<String>();
        radarIdSet.add(radarId);
        List<?> returnObjs = findPartialRecordByNamedQueryAndNamedCollection(
                Radarloc.SELECT_USE_RADAR_ON_ID, "radids", radarIdSet);
        if ("T".equalsIgnoreCase(returnObjs.get(0).toString().trim())) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Gets all fields from Radarloc for specific radar id.
     * 
     * @param radarId
     * @return
     */
    public Radarloc getRadarloc(String radarId) {
        List<?> returnObjects = this.findByNamedQueryAndNamedParam(
                Radarloc.SELECT_REC_EXCLUDE_LINKS, "radid", radarId);
        /*
         * Selecting by id, so there should only be a maximum of one returned
         * record.
         */
        if (returnObjects.isEmpty() || returnObjects.size() != 1) {
            return null;
        }

        /*
         * Convert to a Radarloc record.
         */
        Object returnObject = returnObjects.iterator().next();
        Object[] objects = (Object[]) returnObject;
        Radarloc radarloc = new Radarloc();
        /*
         * All columns except linked objects are returned.
         */
        // radid
        radarloc.setRadid((String) objects[0]);
        // name
        radarloc.setName((String) objects[1]);
        // radidPrefix
        radarloc.setRadidPrefix((String) objects[2]);
        // radarNum
        radarloc.setRadarNum((Short) objects[3]);
        // state
        radarloc.setState((String) objects[4]);
        // lat
        radarloc.setLat((Double) objects[5]);
        // lon
        radarloc.setLon((Double) objects[6]);
        // elev
        radarloc.setElev((Double) objects[7]);
        // towerHt
        radarloc.setTowerHt((Double) objects[8]);
        // useRadar
        radarloc.setUseRadar((String) objects[9]);
        // officeId
        radarloc.setOfficeId((String) objects[10]);
        return radarloc;
    }
}