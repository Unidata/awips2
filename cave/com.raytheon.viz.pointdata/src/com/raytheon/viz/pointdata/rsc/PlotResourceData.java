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
package com.raytheon.viz.pointdata.rsc;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.batik.util.ParsedURL;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.LocalizationParsedURLHandler;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractPlotInfoRetriever;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;

/**
 * Resource data for plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009            njensen     Initial creation
 * Jun 29, 2009     2538   jsanchez    Implemented Metars.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class PlotResourceData extends AbstractRequestableResourceData {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotResourceData.class);

    public static final String PLOT_DIR = "plotModels" + File.separator;

    @XmlAttribute
    int pixelSampleDistance = 32;

    @XmlAttribute
    protected int pixelSizeHint = 90;

    @XmlAttribute
    protected String spiFile = null;

    @XmlAttribute
    protected String plotModelFile = "stdObsDesign.svg";

    @XmlAttribute
    protected String plotSource = null;

    @XmlAttribute
    protected String levelKey = null;

    @XmlAttribute
    protected double lowerLimit = -9999.0;

    @XmlAttribute
    protected double upperLimit = 10000000.0;

    @XmlAttribute
    protected boolean plotMissingData = false;

    @XmlAttribute
    protected boolean isTopOfTheHour = false;

    @XmlElement
    protected AbstractPlotInfoRetriever plotInfoRetriever;

    @XmlElement
    protected BinOffset defaultPeriod;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    protected HashMap<String, RequestConstraint> timeQueryMetadataMap;

    private static Set<String> pluginNames = new HashSet<String>();

    static {
        pluginNames.add("goessounding");
        pluginNames.add("poessounding");
        pluginNames.add("obs");
        pluginNames.add("bufrssmi");
        pluginNames.add("bufrquikscat");
        pluginNames.add("bufrascat");
        pluginNames.add("radar");
        pluginNames.add("bufrhdw");
        pluginNames.add("bufrmthdw");
        pluginNames.add("bufrua");
        pluginNames.add("lsr");
        pluginNames.add("sfcobs");
        pluginNames.add("tcg");
        pluginNames.add("svrwx");
        pluginNames.add("ldadmesonet");
        pluginNames.add("ldadhydro");
        pluginNames.add("qc");
        pluginNames.add("profiler");
        pluginNames.add("fssobs");
        pluginNames.add("modelsounding");
        pluginNames.add("textPoints");
        pluginNames.add("bufrmosAVN");
        pluginNames.add("bufrmosETA");
        pluginNames.add("bufrmosGFS");
        pluginNames.add("bufrmosHPC");
        pluginNames.add("bufrmosLAMP");
        pluginNames.add("bufrmosMRF");
        pluginNames.add("bufrmosNGM");
    }

    static {
        ParsedURL.registerHandler(new LocalizationParsedURLHandler());
    }

    public PlotResourceData() {
        super();
        if (plotInfoRetriever == null) {
            plotInfoRetriever = new PointDataPlotInfoRetriever();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.comm.LoadProperties,
     * com.raytheon.edex.db.objects.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        if (pluginNames.contains(this.metadataMap.get("pluginName")
                .getConstraintValue())) {
            return new PlotResource2(this, loadProperties);
        }

        return new PlotResource(this, loadProperties);
    }

    /**
     * @return the pixelSizeHint
     */
    public int getPixelSizeHint() {
        return pixelSizeHint;
    }

    /**
     * @param pixelSizeHint
     *            the pixelSizeHint to set
     */
    public void setPixelSizeHint(int pixelSizeHint) {
        this.pixelSizeHint = pixelSizeHint;
    }

    /**
     * @return the spiFile
     */
    public String getSpiFile() {
        return spiFile;
    }

    /**
     * @param spiFile
     *            the spiFile to set
     */
    public void setSpiFile(String spiFile) {
        this.spiFile = spiFile;
    }

    /**
     * @return the plotModelFile
     */
    public String getPlotModelFile() {
        return plotModelFile;
    }

    /**
     * @param plotModelFile
     *            the plotModelFile to set
     */
    public void setPlotModelFile(String plotModelFile) {
        this.plotModelFile = plotModelFile;
    }

    /**
     * @return the plotSource
     */
    public String getPlotSource() {
        return plotSource;
    }

    /**
     * @param plotSource
     *            the plotSource to set
     */
    public void setPlotSource(String plotSource) {
        this.plotSource = plotSource;
    }

    /**
     * @return the lowerLimit
     */
    public double getLowerLimit() {
        return lowerLimit;
    }

    /**
     * @param lowerLimit
     *            the lowerLimit to set
     */
    public void setLowerLimit(double lowerLimit) {
        this.lowerLimit = lowerLimit;
    }

    /**
     * @return the upperLimit
     */
    public double getUpperLimit() {
        return upperLimit;
    }

    /**
     * @param upperLimit
     *            the upperLimit to set
     */
    public void setUpperLimit(double upperLimit) {
        this.upperLimit = upperLimit;
    }

    public boolean isPlotMissingData() {
        return plotMissingData;
    }

    public void setPlotMissingData(boolean plotMissingData) {
        this.plotMissingData = plotMissingData;
    }

    public AbstractPlotInfoRetriever getPlotInfoRetriever() {
        return plotInfoRetriever;
    }

    public void setPlotInfoRetriever(AbstractPlotInfoRetriever plotInfoRetriever) {
        this.plotInfoRetriever = plotInfoRetriever;
    }

    public String getLevelKey() {
        return levelKey;
    }

    public void setLevelKey(String levelKey) {
        this.levelKey = levelKey;
    }

    public boolean isTopOfTheHour() {
        return isTopOfTheHour;
    }

    public void setTopOfTheHour(boolean isTopOfTheHour) {
        this.isTopOfTheHour = isTopOfTheHour;
    }

    /**
     * @return the defaultPeriod
     */
    public BinOffset getDefaultPeriod() {
        return defaultPeriod;
    }

    /**
     * @param defaultPeriod
     *            the defaultPeriod to set
     */
    public void setDefaultPeriod(BinOffset defaultPeriod) {
        this.defaultPeriod = defaultPeriod;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * getAvailableTimes()
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        Map<String, RequestConstraint> map = null;
        if (timeQueryMetadataMap != null) {
            map = new HashMap<String, RequestConstraint>(timeQueryMetadataMap);
        } else {
            map = new HashMap<String, RequestConstraint>(this.metadataMap);
        }

        DataTime[] available = queryForTimes(map);
        if (isFrozen()) {
            available = filterTimes(available, frozenTime);
        }
        return available;
    }

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof PlotResourceData == false) {
            return false;
        }

        PlotResourceData other = (PlotResourceData) obj;

        if (this.plotSource != null && other.plotSource == null) {
            return false;
        } else if (this.plotSource == null && other.plotSource != null) {
            return false;
        } else if (this.plotSource != null
                && this.plotSource.equals(other.plotSource) == false) {
            return false;
        }

        if (this.plotModelFile != null && other.plotModelFile == null) {
            return false;
        } else if (this.plotModelFile == null && other.plotModelFile != null) {
            return false;
        } else if (this.plotModelFile != null
                && this.plotModelFile.equals(other.plotModelFile) == false) {
            return false;
        }

        if (this.spiFile != null && other.spiFile == null) {
            return false;
        } else if (this.spiFile == null && other.spiFile != null) {
            return false;
        } else if (this.spiFile != null
                && this.spiFile.equals(other.spiFile) == false) {
            return false;
        }

        if (this.levelKey != null && other.levelKey == null) {
            return false;
        } else if (this.levelKey == null && other.levelKey != null) {
            return false;
        } else if (this.levelKey != null
                && this.levelKey.equals(other.levelKey) == false) {
            return false;
        }

        if (this.plotInfoRetriever != null && other.plotInfoRetriever == null) {
            return false;
        } else if (this.plotInfoRetriever == null
                && other.plotInfoRetriever != null) {
            return false;
        } else if (this.plotInfoRetriever != null
                && this.plotInfoRetriever.equals(other.plotInfoRetriever) == false) {
            return false;
        }

        if (this.defaultPeriod != null && other.defaultPeriod == null) {
            return false;
        } else if (this.defaultPeriod == null && other.defaultPeriod != null) {
            return false;
        } else if (this.defaultPeriod != null
                && this.defaultPeriod.equals(other.defaultPeriod) == false) {
            return false;
        }

        return (this.pixelSizeHint == other.pixelSizeHint
                && this.lowerLimit == other.lowerLimit && this.upperLimit == other.upperLimit);
    }

    public int getPixelSampleDistance() {
        return this.pixelSampleDistance;
    }

    public void setPixelSampleDistance(int pixelSampleDistance) {
        this.pixelSampleDistance = pixelSampleDistance;
    }
}
