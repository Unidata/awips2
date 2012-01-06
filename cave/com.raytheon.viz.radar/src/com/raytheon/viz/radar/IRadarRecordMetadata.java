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
package com.raytheon.viz.radar;

import java.util.Calendar;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRadarRecordMetadata {

    public Calendar getInsertTime();

    public Float getLatitude();

    public Float getLongitude();

    public String getFormat();

    public Integer getGateResolution();

    public Integer getOperationalMode();

    public Integer getNumLevels();

    public Double getLayer();

    public Float getElevation();

    public Float getTrueElevationAngle();

    public Double getPrimaryElevationAngle();

    public Integer getNumBins();

    public Integer getNumRadials();

    public Integer getProductCode();

    public ProjectedCRS getCRS();

    public String getDataURI();

    public RadarRecord populateRecord();
}
