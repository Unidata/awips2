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
package com.raytheon.edex.plugin.satellite.notify;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * A message representing a satellite record that was ingested, but only the
 * basic human-readable information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class SatelliteNotifyMessage {

    @DynamicSerializeElement
    private String source;

    @DynamicSerializeElement
    private String creatingEntity;

    @DynamicSerializeElement
    private String sectorId;

    @DynamicSerializeElement
    private String physicalElement;

    @DynamicSerializeElement
    private DataTime dataTime;

    @DynamicSerializeElement
    private Date insertTime;

    @DynamicSerializeElement
    private String dataURI;

    public SatelliteNotifyMessage() {
        // no-op -- only for serialization
    }

    /**
     * @param satRec
     */
    public SatelliteNotifyMessage(SatelliteRecord satRec) {
        this.source = satRec.getSource();
        this.creatingEntity = satRec.getCreatingEntity();
        this.sectorId = satRec.getSectorID();
        this.physicalElement = satRec.getPhysicalElement();
        this.dataTime = satRec.getDataTime();
        this.insertTime = satRec.getInsertTime().getTime();
        this.dataURI = satRec.getDataURI();
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getCreatingEntity() {
        return creatingEntity;
    }

    public void setCreatingEntity(String creatingEntity) {
        this.creatingEntity = creatingEntity;
    }

    public String getSectorId() {
        return sectorId;
    }

    public void setSectorId(String sectorId) {
        this.sectorId = sectorId;
    }

    public String getPhysicalElement() {
        return physicalElement;
    }

    public void setPhysicalElement(String physicalElement) {
        this.physicalElement = physicalElement;
    }

    public DataTime getDataTime() {
        return dataTime;
    }

    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    public String getDataURI() {
        return dataURI;
    }

    public void setDataURI(String dataURI) {
        this.dataURI = dataURI;
    }

}
