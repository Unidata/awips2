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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.Calendar;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * TODO This class currently is not even used. Resource Data implementation for
 * the {@link DisplayMeanArealPrecipResource}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2010 ?          snaples     Initial creation
 * Mar 01, 2017 6163       bkowal      Updates for {@link DisplayMeanArealPrecipResource}.
 * 
 * </pre>
 * 
 * @author snaples
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "displayMeanArealPrecipData")
public class DisplayMeanArealPrecipResourceData extends AbstractResourceData {

    private MPEDisplayManager dm;

    private String boundary;

    private DisplayFieldData fieldData;

    private int accumInterval;

    private String displayAs;

    private Calendar endDateTime;

    public DisplayMeanArealPrecipResourceData() {
    }

    public DisplayMeanArealPrecipResourceData(MPEDisplayManager displayManager,
            String boundary_type, DisplayFieldData fieldData, int accumInterval,
            final String displayAs, final Calendar endDateTime) {
        dm = displayManager;
        boundary = boundary_type;
        this.fieldData = fieldData;
        this.accumInterval = accumInterval;
        this.displayAs = displayAs;
        this.endDateTime = endDateTime;
    }

    @Override
    public DisplayMeanArealPrecipResource construct(
            LoadProperties loadProperties, IDescriptor descriptor)
                    throws VizException {
        return new DisplayMeanArealPrecipResource(dm, boundary, fieldData,
                accumInterval, displayAs, endDateTime);
    }

    @Override
    public void update(Object updateData) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dm == null) ? 0 : dm.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
    }
}