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

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Height;
import com.raytheon.uf.common.dataplugin.shef.tables.HeightId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSObservationDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Height} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2016 5699       bkowal      Initial creation
 * Jun 29, 2016 5699       bkowal      Implemented {@link #getIdForObservation(Observation)}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class HeightDao extends AbstractIHFSObservationDbDao<Height, HeightId> {

    public HeightDao() {
        super(Height.class, Height.SELECT_OBS, Height.SELECT_OBS_BY_LID,
                Height.SELECT_OBS_BY_PE, Height.SELECT_OBS_BY_LID_AND_PE);
    }

    @Override
    protected Observation convertRecordToObservation(Height record) {
        Observation observation = new Observation();
        observation.setLid(record.getId().getLid());
        observation.setPe(record.getId().getPe());
        observation.setDur(record.getId().getDur());
        observation.setTs(record.getId().getTs());
        observation.setExtremum(record.getId().getExtremum());
        observation.setObstime(record.getId().getObstime());
        observation.setValue(record.getValue());
        observation.setShefQualCode(record.getShefQualCode());
        observation.setQualityCode(record.getQualityCode());
        observation.setRevision(record.getRevision());
        observation.setProductId(record.getProductId());
        observation.setProducttime(record.getProducttime());
        return observation;
    }

    @Override
    protected HeightId getIdForObservation(Observation observation) {
        HeightId id = new HeightId();
        id.setLid(observation.getLid());
        id.setPe(observation.getPe());
        id.setDur((short) observation.getDur());
        id.setTs(observation.getTs());
        id.setExtremum(observation.getExtremum());
        id.setObstime(observation.getObstime());
        return id;
    }
}