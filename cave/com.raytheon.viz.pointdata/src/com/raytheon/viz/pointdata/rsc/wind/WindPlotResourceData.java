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
package com.raytheon.viz.pointdata.rsc.wind;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * Resource data for displaying wind barbs of point data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 13, 2015  4903     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class WindPlotResourceData extends AbstractRequestableResourceData {

    @XmlAttribute
    private String legend;

    @XmlAttribute
    private String windFile;

    @XmlAttribute
    private double baseDensity = 1.0;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    protected HashMap<String, RequestConstraint> dataMetadataMap;

    @XmlTransient
    protected WindPlotConfig config;

    public WindPlotResourceData() {
        /*
         * Resource data should never instigate a trip to the DB, the resource
         * is responsible for requesting its own stuff.
         */
        setRetrieveData(false);
        setUpdatingOnMetadataOnly(true);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        if (config == null) {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            String path = "plotWind" + IPathManager.SEPARATOR + windFile;
            LocalizationFile localizedFile = pathManager
                    .getStaticLocalizationFile(path);
            try (InputStream is = localizedFile.openInputStream()) {
                config = JAXB.unmarshal(is, WindPlotConfig.class);
            } catch (Exception e) {
                throw new VizException("Error loading " + path, e);
            }
        }
        return new WindPlotResource(this, loadProperties);
    }

    public String getLegend() {
        return legend;
    }

    public void setLegend(String legend) {
        this.legend = legend;
    }

    public String getWindFile() {
        return windFile;
    }

    public void setWindFile(String windFile) {
        this.windFile = windFile;
    }

    public double getBaseDensity() {
        return baseDensity;
    }

    public void setBaseDensity(double baseDensity) {
        this.baseDensity = baseDensity;
    }

    public HashMap<String, RequestConstraint> getDataMetadataMap() {
        return dataMetadataMap;
    }

    public void setDataMetadataMap(
            HashMap<String, RequestConstraint> dataMetadataMap) {
        this.dataMetadataMap = dataMetadataMap;
    }

    public Map<String, RequestConstraint> getFullDataMetadataMap() {
        Map<String, RequestConstraint> result = new HashMap<>(getMetadataMap());
        if (dataMetadataMap != null) {
            result.putAll(dataMetadataMap);
        }
        return result;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((dataMetadataMap == null) ? 0 : dataMetadataMap.hashCode());
        result = prime * result + ((legend == null) ? 0 : legend.hashCode());
        result = prime * result
                + ((windFile == null) ? 0 : windFile.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        WindPlotResourceData other = (WindPlotResourceData) obj;
        if (dataMetadataMap == null) {
            if (other.dataMetadataMap != null) {
                return false;
            }
        } else if (!dataMetadataMap.equals(other.dataMetadataMap)) {
            return false;
        }
        if (legend == null) {
            if (other.legend != null) {
                return false;
            }
        } else if (!legend.equals(other.legend)) {
            return false;
        }
        if (windFile == null) {
            if (other.windFile != null) {
                return false;
            }
        } else if (!windFile.equals(other.windFile)) {
            return false;
        }
        return true;
    }

    public WindPlotConfig getConfig() {
        return config;
    }

}
