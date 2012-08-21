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
package com.raytheon.viz.satellite.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Satellite true color resource data. The field used to differentiate
 * red/green/blue bands should not be added to the resource data's metadata map.
 * The resource data itself will do that by adding an IN entry for the unique
 * field for each item in redGreenBlue object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SatTrueColorResourceData extends AbstractRequestableResourceData {

    @XmlElement
    private String uniqueField;

    @XmlElement
    private String[] redGreenBlue;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        List<SatelliteRecord> records = new ArrayList<SatelliteRecord>(
                objects.length);
        for (PluginDataObject pdo : objects) {
            records.add((SatelliteRecord) pdo);
        }
        return new SatTrueColorResource(this, loadProperties, records);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#getMetadataMap
     * ()
     */
    @Override
    public HashMap<String, RequestConstraint> getMetadataMap() {
        HashMap<String, RequestConstraint> mdm = new HashMap<String, RequestConstraint>(
                super.getMetadataMap());
        RequestConstraint constraint = new RequestConstraint("",
                ConstraintType.IN);
        constraint.setConstraintValueList(redGreenBlue);
        mdm.put(uniqueField, constraint);
        return mdm;
    }

    /**
     * @return the uniqueField
     */
    public String getUniqueField() {
        return uniqueField;
    }

    /**
     * @param uniqueField
     *            the uniqueField to set
     */
    public void setUniqueField(String uniqueField) {
        this.uniqueField = uniqueField;
    }

    /**
     * @return the redGreenBlue
     */
    public String[] getRedGreenBlue() {
        return redGreenBlue;
    }

    /**
     * @param redGreenBlue
     *            the redGreenBlue to set
     */
    public void setRedGreenBlue(String[] redGreenBlue) {
        this.redGreenBlue = redGreenBlue;
    }

}
