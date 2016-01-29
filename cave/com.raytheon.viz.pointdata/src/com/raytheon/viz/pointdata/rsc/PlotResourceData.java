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

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractPlotInfoRetriever;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;

/**
 * Resource data for plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 17, 2009  1960     njensen   Initial creation
 * Jun 29, 2009  2538     jsanchez  Implemented Metars.
 * May 14, 2013  1869     bsteffen  Get plots working without dataURI
 * Aug 09, 2013  2033     mschenke  Switched File.separator to
 *                                  IPathManager.SEPARATOR
 * Sep 05, 2013  2316     bsteffen  Unify pirep and ncpirep.
 * Jun 06, 2014  2061     bsteffen  Remove old PlotResource
 * Sep 16, 2014  2707     bclement  lsr no longer uses dataURI
 * Oct 27, 2015  4798     bsteffen  Move SVG localization url handler
 *                                  registration.
 * Nov 16, 2015  5119     bsteffen  Remove bufquikscat
 * Jan 19, 2016  5253     tgurney   Remove svrwx dependency on dataURI
 * Jan 28, 2016  5286     tgurney   Remove tcg dependency on dataURI
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class PlotResourceData extends AbstractRequestableResourceData {

    public static final String PLOT_DIR = "plotModels" + IPathManager.SEPARATOR;

    public static class PluginPlotProperties {

        /**
         * When this is true all plots will be correlated based on the
         * stationId, otherwise each dataURI is mapped to a specific set of
         * data.
         */
        public final boolean hasDistinctStationId;

        public PluginPlotProperties(boolean hasDistinctStationId) {
            this.hasDistinctStationId = hasDistinctStationId;
        }

        /**
         * This is the goal for all plugins, they should have distinct
         * stationIds.
         */
        public PluginPlotProperties() {
            this(true);
        }

    }

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

    private static final Map<String, PluginPlotProperties> pluginProps = new HashMap<String, PluginPlotProperties>();

    static {
        /*
         * These have a dependency on dataURI because they don't set stationId,
         * In the future if stationId can be set to anything that is even a
         * little unique we can get rid of this
         */
        pluginProps.put("radar", new PluginPlotProperties(false));
        pluginProps.put("ldadhydro", new PluginPlotProperties(false));
        pluginProps.put("textPoints", new PluginPlotProperties(false));

        /*
         * The good ones, these don't even need to be here because this is the
         * default behavior, but for now they are included so we have a
         * comprehensive list of which plugins use certain behaviors.
         */

        pluginProps.put("tcg", new PluginPlotProperties());
        pluginProps.put("svrwx", new PluginPlotProperties());
        pluginProps.put("obs", new PluginPlotProperties());
        pluginProps.put("goessounding", new PluginPlotProperties());
        pluginProps.put("poessounding", new PluginPlotProperties());
        pluginProps.put("bufrua", new PluginPlotProperties());
        pluginProps.put("sfcobs", new PluginPlotProperties());
        pluginProps.put("profiler", new PluginPlotProperties());
        pluginProps.put("fssobs", new PluginPlotProperties());
        pluginProps.put("modelsounding", new PluginPlotProperties());
        pluginProps.put("bufrmosAVN", new PluginPlotProperties());
        pluginProps.put("bufrmosETA", new PluginPlotProperties());
        pluginProps.put("bufrmosGFS", new PluginPlotProperties());
        pluginProps.put("bufrmosHPC", new PluginPlotProperties());
        pluginProps.put("bufrmosLAMP", new PluginPlotProperties());
        pluginProps.put("bufrmosMRF", new PluginPlotProperties());
        pluginProps.put("bufrmosNGM", new PluginPlotProperties());
        pluginProps.put("ldadmesonet", new PluginPlotProperties());
        pluginProps.put("qc", new PluginPlotProperties());
        pluginProps.put("bufrascat", new PluginPlotProperties());
        pluginProps.put("bufrhdw", new PluginPlotProperties());
        pluginProps.put("bufrmthdw", new PluginPlotProperties());
        pluginProps.put("bufrssmi", new PluginPlotProperties());
        pluginProps.put("madis", new PluginPlotProperties());
        pluginProps.put("pirep", new PluginPlotProperties());
        pluginProps.put("airep", new PluginPlotProperties());
        pluginProps.put("acars", new PluginPlotProperties());
        pluginProps.put("lsr", new PluginPlotProperties());
    }

    public PlotResourceData() {
        super();
        if (plotInfoRetriever == null) {
            plotInfoRetriever = new PointDataPlotInfoRetriever();
        }
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
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

    @Override
    public boolean isUpdatingOnMetadataOnly() {
        return false;
    }

    @Override
    public boolean isRetrieveData() {
        return false;
    }

    public PluginPlotProperties getPluginProperties() {
        return getPluginProperties(this.metadataMap);
    }

    public static PluginPlotProperties getPluginProperties(String pluginName) {
        PluginPlotProperties result = pluginProps.get(pluginName);
        if (result == null) {
            result = new PluginPlotProperties();
        }
        return result;
    }

    public static PluginPlotProperties getPluginProperties(
            Map<String, RequestConstraint> metadataMap) {
        RequestConstraint rc = metadataMap.get("pluginName");
        if (rc == null || rc.getConstraintType() != ConstraintType.EQUALS) {
            throw new IllegalArgumentException(
                    "Cannot find plugin properties because metadataMap does not specify a plugin.");
        }
        return getPluginProperties(rc.getConstraintValue());
    }

}
