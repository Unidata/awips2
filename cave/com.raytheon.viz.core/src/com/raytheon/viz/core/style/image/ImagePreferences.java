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

package com.raytheon.viz.core.style.image;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.style.AbstractStylePreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.LabelingPreferences;

/**
 * 
 * Contains the imagery preferences
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 27, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "imageStyle")
public class ImagePreferences extends AbstractStylePreferences {

    @XmlElement
    private String defaultColormap;

    @XmlElement(name = "range")
    private DataScale dataScale;

    @XmlElement(name = "displayLegend")
    private String legend;
    
    @XmlElement
    private SamplePreferences samplePrefs;

    @XmlElement(name = "dataMapping")
    private DataMappingPreferences dataMapping;

    @XmlElement(name = "colorbarLabeling")
    private LabelingPreferences colorbarLabeling;

    @XmlElement(name = "interpolate")
    private boolean interpolate = true;

    public boolean isInterpolate() {
        return interpolate;
    }

    public void setInterpolate(boolean interpolate) {
        this.interpolate = interpolate;
    }

    /**
     * @return the defaultColormap
     */
    public String getDefaultColormap() {
        return defaultColormap;
    }

    /**
     * @param defaultColormap
     *            the defaultColormap to set
     */
    public void setDefaultColormap(String defaultColormap) {
        this.defaultColormap = defaultColormap;
    }

    /**
     * @return the dataScale
     */
    public DataScale getDataScale() {
        return dataScale;
    }

    /**
     * @param dataScale
     *            the dataScale to set
     */
    public void setDataScale(DataScale dataScale) {
        this.dataScale = dataScale;
    }

    /**
     * @return the colorbarLabeling
     */
    public LabelingPreferences getColorbarLabeling() {
        return colorbarLabeling;
    }

    /**
     * @param colorbarLabeling
     *            the colorbarLabeling to set
     */
    public void setColorbarLabeling(LabelingPreferences colorbarLabeling) {
        this.colorbarLabeling = colorbarLabeling;
    }

    /**
     * @return the dataMapping
     */
    public DataMappingPreferences getDataMapping() {
        return dataMapping;
    }

    /**
     * @param dataMapping
     *            the dataMapping to set
     */
    public void setDataMapping(DataMappingPreferences dataMapping) {
        this.dataMapping = dataMapping;
    }

    public SamplePreferences getSamplePrefs() {
        return samplePrefs;
    }

    public void setSamplePrefs(SamplePreferences samplePrefs) {
        this.samplePrefs = samplePrefs;
    }
    
    /**
     *  @return the legend
     */
    public String getLegend() {
        return legend;
    }
    
}
