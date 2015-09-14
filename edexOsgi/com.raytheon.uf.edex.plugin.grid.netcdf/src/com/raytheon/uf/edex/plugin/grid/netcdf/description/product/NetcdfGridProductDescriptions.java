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
package com.raytheon.uf.edex.plugin.grid.netcdf.description.product;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.edex.netcdf.description.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.date.DataTimeDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage.GridCoverageDescription;

/**
 * Contains a list of individual {@link NetcdfGridProductDescription}s as well
 * as information that pertains to all listed products.
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
public class NetcdfGridProductDescriptions {

    @XmlElement(name = "description")
    private List<NetcdfGridProductDescription> descriptions;

    @XmlElement(required = true)
    private LevelDescription level;

    @XmlElement(required = true)
    private DataTimeDescription dataTime;

    @XmlElement(required = true)
    private GridCoverageDescription coverage;

    @XmlElement(required = true)
    private String datasetId;

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(PatternTypeAdapter.class)
    private Pattern pattern;

    /**
     * Constructor.
     */
    public NetcdfGridProductDescriptions() {
        super();
    }

    public List<NetcdfGridProductDescription> getDescriptions() {
        return descriptions;
    }

    public void setDescriptions(List<NetcdfGridProductDescription> descriptions) {
        this.descriptions = descriptions;
    }

    public void addDescription(NetcdfGridProductDescription description) {
        if (this.descriptions == null) {
            this.descriptions = new ArrayList<>();
        }
        this.descriptions.add(description);
    }

    public void addDescriptions(NetcdfGridProductDescriptions descriptions) {
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
     * @return the coverage
     * @throws InvalidDescriptionException
     */
    public GridCoverageDescription getCoverage()
            throws InvalidDescriptionException {
        if (coverage == null) {
            throw new InvalidDescriptionException("Coverage not specified.");
        }
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(GridCoverageDescription coverage) {
        this.coverage = coverage;
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


    public Pattern getPattern() {
        return this.pattern;
    }

    /**
     * @param patternString
     *            the pattern to set
     */
    public void setPattern(Pattern pattern) {
        this.pattern = pattern;
    }

    /**
     * Convenience method to determine if the pattern matches a given String.
     *
     * @param input
     *            The String to test for matching.
     * @return True if the String matches the pattern, false otherwise.
     */
    public boolean matches(String input) {
        if (this.pattern == null) {
            return false;
        }

        Matcher m = this.pattern.matcher(input);
        return m.matches();
    }

    private static class PatternTypeAdapter extends XmlAdapter<String, Pattern> {

        @Override
        public Pattern unmarshal(String patternStr) throws Exception {
            if (patternStr == null) {
                return null;
            }

            return Pattern.compile(patternStr);
        }

        @Override
        public String marshal(Pattern pattern) throws Exception {
            if (pattern == null) {
                return null;
            }

            return pattern.pattern();
        }

    }
}
