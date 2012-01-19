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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;

import java.util.List;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 *
 * </pre>
 *
 * @author bsteffen
 * @version 1.0
 */
public class D2DNSharpDataObject extends PluginDataObject {

    private static final long serialVersionUID = -2882479369372348172L;

    private NsharpStationInfo stationInfo;

    private List<NcSoundingLayer> layers;

    public D2DNSharpDataObject() {
        super();

    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public NsharpStationInfo getStationInfo() {
        return stationInfo;
    }

    public void setStationInfo(NsharpStationInfo stationInfo) {
        this.stationInfo = stationInfo;
    }

    public List<NcSoundingLayer> getLayers() {
        return layers;
    }

    public void setLayers(List<NcSoundingLayer> layers) {
        this.layers = layers;
    }

}
