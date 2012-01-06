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

import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2010            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class TimeLapseResourceData extends AbstractRequestableResourceData {
    private DisplayFieldData displayFieldType = null;
    
    private List<Colorvalue> gageColorMap = null;
    
    /**
     * @param displayFieldType2
     * @param gageColorMap2
     */
    public TimeLapseResourceData(DisplayFieldData displayFieldType,
            List<Colorvalue> gageColorMap) {
        this.displayFieldType = displayFieldType;
        this.gageColorMap = gageColorMap;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#construct(com.raytheon.uf.viz.core.rsc.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new TimeLapseResource(MPEDisplayManager.getCurrent(), displayFieldType, gageColorMap);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties, com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new TimeLapseResource(MPEDisplayManager.getCurrent(), displayFieldType, gageColorMap);
    }

}
