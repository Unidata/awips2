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
package com.raytheon.uf.edex.plugin.nos.description;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.edex.netcdf.description.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.date.DataTimeDescription;

/**
 * Contains a list of individual {@link NosProductDescription}s as well as
 * information that pertains to all listed products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2015 4696       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class NosProductDescriptions {

    @XmlElement(name = "description")
    private List<NosProductDescription> descriptions;

    @XmlElement(required = true)
    private LevelDescription level;

    @XmlElement(required = true)
    private DataTimeDescription dataTime;

    @XmlElement(required = true)
    private String datasetId;

    /**
     * Constructor.
     */
    public NosProductDescriptions() {
        super();
    }

    public List<NosProductDescription> getDescriptions() {
        return descriptions;
    }

    public void setDescriptions(List<NosProductDescription> descriptions) {
        this.descriptions = descriptions;
    }

    public void addDescription(NosProductDescription description) {
        if (this.descriptions == null) {
            this.descriptions = new ArrayList<>();
        }
        this.descriptions.add(description);
    }

    public void addDescriptions(NosProductDescriptions descriptions) {
        if (this.descriptions == null) {
            this.descriptions = new ArrayList<>();
        }
        this.descriptions.addAll(descriptions.getDescriptions());
    }

    /**
     * @return the level
     */
    public LevelDescription getLevel() {
        return level;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(LevelDescription level) {
        this.level = level;
    }

    /**
     * @return the dataTime
     */
    public DataTimeDescription getDataTime() {
        return dataTime;
    }

    /**
     * @param datatime
     *            the dataTime to set
     */
    public void setDataTime(DataTimeDescription datatime) {
        this.dataTime = datatime;
    }

    /**
     * @return the datasetId
     */
    public String getDatasetId() {
        return datasetId;
    }

    /**
     * @param datasetId
     *            the dataSetId to set
     */
    public void setDatasetId(String datasetId) {
        this.datasetId = datasetId;
    }
}
