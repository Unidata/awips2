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
package com.raytheon.uf.viz.xy.crosssection.adapter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.measure.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.core.graphing.xy.XYData;

/**
 * Abstract class for data type-specific functionality that's needed to display
 * data as cross sections.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2009            mschenke    Initial creation
 * Dec 20, 2023 2036519    mapeters    Add dispose()
 * May 22, 2024 2037092    mapeters    Add setResource(), sync remove(time)
 * Jun 20, 2024 2037565    mapeters    Add getExtraNameText/getExtraLegendText,
 *                                     remove unused getParameterName
 * Jul 03, 2024 2037476    bines       Add getCreatingEntity()
 * Aug 09, 2024 2037698    bines       Add getParamterAbbrev() and
 *                                     useNearestNeighbor()
 * Aug 20, 2024 2037631    mapeters    Wrap float data in new class, remove
 *                                     getExtraLegendText()
 * </pre>
 *
 * @author mschenke
 */
public abstract class AbstractCrossSectionAdapter<T extends PluginDataObject>
        implements Serializable {

    private static final long serialVersionUID = 1L;

    protected CrossSectionResourceData resourceData;

    protected AbstractCrossSectionResource resource;

    protected CrossSectionDescriptor descriptor;

    protected final List<T> records = new ArrayList<>();

    /**
     * @param resourceData
     *            the resourceData to set
     */
    public void setResourceData(CrossSectionResourceData resourceData) {
        this.resourceData = resourceData;
    }

    /**
     * @param resource
     *            the resource to set
     */
    public void setResource(AbstractCrossSectionResource resource) {
        this.resource = resource;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public abstract CrossSectionFrameData loadData(DataTime currentTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException;

    public abstract Unit<?> getUnit();

    @SuppressWarnings("unchecked")
    public void addRecord(PluginDataObject pdo) {
        synchronized (records) {
            records.add((T) pdo);
        }
    }

    public void remove(DataTime time) {
        synchronized (records) {
            records.removeIf(record -> record.getDataTime().equals(time));
        }
    }

    public void sortData(List<XYData> data) {
        Collections.sort(data, (o1, o2) -> Float.compare((Float) o1.getY(),
                (Float) o2.getY()));
    }

    public CoordinateReferenceSystem getDataCoordinateReferenceSystem() {
        return MapUtil.LATLON_PROJECTION;
    }

    public void dispose() {
        records.clear();
    }

    /**
     * Get extra text to include in the resource's name.
     *
     * @return extra text string
     */
    public String getExtraNameText() {
        return "";
    }

    /**
     * Get creating entity/source
     *
     * @return String creating entity/source
     */
    public String getCreatingEntity() {
        return resourceData.getSource();
    }

    /**
     * Get parameter abbreviation
     *
     * @return String parameter
     */
    public String getParameterAbbrev() {
        return resourceData.getParameter();
    }

    /**
     * Indicates whether resource should use nearest neighbor OR bilinear
     * interpolation
     *
     * @return boolean
     */
    public boolean useNearestNeighbor() {
        return false;
    }
}
