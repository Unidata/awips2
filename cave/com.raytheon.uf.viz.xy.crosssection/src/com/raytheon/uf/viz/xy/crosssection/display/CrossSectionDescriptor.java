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
package com.raytheon.uf.viz.xy.crosssection.display;

import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IBaseLinesContainer;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.vividsolutions.jts.geom.LineString;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class CrossSectionDescriptor extends XyGraphDescriptor implements
        IBaseLinesContainer {

    public static final String LINE_PREFIX = "Line";

    @XmlElement
    private HeightScale heightScale;

    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @XmlElement(name = "line")
    private List<LineString> lines;

    @XmlAttribute
    private String lineID;

    public CrossSectionDescriptor() {
        super();
    }

    /**
     * @param pixelExtent
     * @param varHeightGraphFactory
     */
    public CrossSectionDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new CrossSectionGraph(this);
    }

    /**
     * @return the heightScale
     */
    public HeightScale getHeightScale() {
        return heightScale;
    }

    /**
     * @param heightScale
     *            the heightScale to set
     */
    public void setHeightScale(HeightScale heightScale) {
        if (heightScale != this.heightScale) {
            this.heightScale = heightScale;
            for (ResourcePair rp : this.resourceList) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc instanceof IGraphableResource<?, ?>) {
                    this.getGraph((IGraphableResource<?, ?>) rsc).reconstruct();
                    if (rsc instanceof AbstractCrossSectionResource) {
                        ((AbstractCrossSectionResource) rsc)
                                .setDescriptor(this);
                    }
                }
            }
            if (renderableDisplay != null) {
                ((CrossSectionRenderableDisplay) renderableDisplay)
                        .setTabTitle(getTitle());
            }
        }
    }

    public String getTitle() {
        if (lineID == null && heightScale == null) {
            return "Cross Section";
        } else if (lineID == null) {
            return String.format("Cross Section : %s", heightScale.getName());
        } else if (heightScale == null) {
            return String.format("Cross Section @ %s", lineID);
        } else {
            return String.format("Cross Section @ %s : %s", lineID,
                    heightScale.getName());
        }
    }

    /**
     * @return the lines
     */
    public List<LineString> getLines() {
        return lines;
    }

    /**
     * @param lines
     *            the lines to set
     */
    public void setLines(List<LineString> lines) {
        this.lines = lines;
    }

    /**
     * 
     * @return the line
     */
    public LineString getLine(DataTime time) {
        return lines.get(time.getLevelValue().intValue());
    }

    public LineString getCurrentLine() {
        FramesInfo info = getFramesInfo();
        int index = info.getFrameIndex();
        DataTime[] frames = info.getFrameTimes();
        if (frames == null || index < 0 || index >= frames.length) {
            return null;
        }
        return lines.get(frames[index].getLevelValue().intValue());
    }

    /**
     * @return the lineID
     */
    public String getLineID() {
        return lineID;
    }

    /**
     * @param lineID
     *            the lineID to set
     */
    public void setLineID(String lineID) {
        this.lineID = lineID;
        if (renderableDisplay != null) {
            ((CrossSectionRenderableDisplay) renderableDisplay)
                    .setTabTitle(getTitle());
        }
    }

    @Override
    public boolean isCompatible(IDescriptor other) {
        if (other instanceof CrossSectionDescriptor) {
            CrossSectionDescriptor csOther = (CrossSectionDescriptor) other;
            if (csOther.lines.equals(this.lines)) {
                return false;
            }
            return csOther.heightScale.equals(this.heightScale);
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IBaseLinesContainer#setBaseLine(java.lang
     * .String)
     */
    @Override
    public void setBaseLine(String baseLine) {
        setLineID(LINE_PREFIX + baseLine);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IBaseLinesContainer#setBaseLineString(com
     * .vividsolutions.jts.geom.LineString)
     */
    @Override
    public void setBaseLineString(LineString baseLineString) {
        setLines(Arrays.asList(baseLineString));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IBaseLinesContainer#getBaseLine()
     */
    @Override
    public String getBaseLine() {
        return getLineID();
    }

}
