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
package com.raytheon.uf.edex.plugin.mpe.conversion.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;

/**
 * POJO for JAXB serialization and deserialization of an intercepted
 * {@link Observation}. Keeps track of the primary key fields, entity
 * identification information, and updated quality value associated with an
 * observation that would have been updated in the database for later
 * comparison. TODO: the entire package that this class is a part of should be
 * removed when the decision has been made to use the converted mpe
 * applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "rocObservation")
public class RocInterceptedObservation extends Observation {
    /*
     * Entity identification field.
     */
    private String table;

    public RocInterceptedObservation() {
    }

    public RocInterceptedObservation(final Observation observation,
            final String table, final long updatedQualityCode) {
        this.table = table;
        setLid(observation.getLid());
        setPe(observation.getPe());
        setDur(observation.getDur());
        setTs(observation.getTs());
        setExtremum(observation.getExtremum());
        setObstime(observation.getObstime());
        setBasisTime(observation.getBasisTime());
        setValue(observation.getValue());
        setShefQualCode(observation.getShefQualCode());
        setQualityCode((int) updatedQualityCode);
        setRevision(observation.getRevision());
        setProductId(observation.getProductId());
        setProducttime(observation.getProducttime());
    }

    public String getTable() {
        return table;
    }

    public void setTable(String table) {
        this.table = table;
    }
}