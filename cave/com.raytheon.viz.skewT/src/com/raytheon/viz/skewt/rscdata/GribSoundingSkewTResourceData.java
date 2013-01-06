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
package com.raytheon.viz.skewt.rscdata;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.sounding.adapters.GridSoundingAdapter;
import com.raytheon.uf.viz.sounding.adapters.IPointSounding;
import com.raytheon.viz.skewt.rsc.SkewTResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates soundings from Grib
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GribSoundingSkewTResourceData extends SkewTResourceData implements
        IPointSounding {

    @XmlAttribute
    private String point;

    @XmlElement
    private Coordinate coordinate;

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected SkewTResource constructResource(LoadProperties loadProperties,
            PluginDataObject[] objects) throws VizException {
        AbstractVerticalSoundingAdapter adapter = new GridSoundingAdapter(this);
        adapter.setObjects(objects);
        this.soundings = adapter.createSoundings();
        SkewTResource rsc = new SkewTResource(this, loadProperties);
        return rsc;
    }

    public String getPoint() {
        return point;
    }

    public void setPoint(String point) {
        this.point = point;
    }

    public Coordinate getCoordinate() {
        return coordinate;
    }

    public void setCoordinate(Coordinate coordinate) {
        this.coordinate = coordinate;
    }

    @Override
    public String getPointName() {
        return getPoint();
    }

}
