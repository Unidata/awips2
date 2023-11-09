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
package com.raytheon.uf.common.dataplugin.ldad;

import java.util.Date;

import javax.persistence.MappedSuperclass;
import javax.persistence.SequenceGenerator;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Abstract base class for LDAD records
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 06, 2018  6851     randerso  Initial creation
 * Apr 24, 2019  6140     tgurney   Remove Inheritance annotation
 *                                  (Hibernate 5.4 fix)
 *
 * </pre>
 *
 * @author randerso
 */

@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@DynamicSerialize
public abstract class LdadRecord extends PersistablePluginDataObject {
    private static final long serialVersionUID = 1L;

    /*
     * TODO: move common fields from MesonetLdadRecord and HydroLdadRecord up to
     * LdadRecord. Unfortunately this changes the dataURI so would require
     * additional changes.
     */

    /**
     * Default Constructor
     */
    public LdadRecord() {
        super();
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     *
     * @param uri
     *            A data uri applicable to this class.
     */
    public LdadRecord(String uri) {
        super(uri);
    }

    /**
     * @param dataProvider
     *            the dataProvider to set
     */
    public abstract void setDataProvider(String dataProvider);

    /**
     * @param stationType
     *            the stationType to set
     */
    public abstract void setStationType(String stationType);

    /**
     * @param reportTime
     *            the reportTime to set
     */
    public abstract void setReportTime(long reportTime);

    /**
     * @return the observationTime
     */
    public abstract Date getObservationTime();

    /**
     * @param observationTime
     *            the observationTime to set
     */
    public abstract void setObservationTime(Date observationTime);

    /**
     * @param location
     *            the location to set
     */
    public abstract void setLocation(SurfaceObsLocation location);

    /**
     * @param rawMessage
     *            the rawMessage to set
     */
    public abstract void setRawMessage(String rawMessage);

    /**
     * @return record as rawMessage
     */
    public abstract String toMessage();

}
