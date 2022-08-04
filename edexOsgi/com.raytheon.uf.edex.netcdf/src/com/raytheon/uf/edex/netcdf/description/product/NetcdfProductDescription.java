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
package com.raytheon.uf.edex.netcdf.description.product;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.slf4j.Logger;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.date.DataTimeDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;
import com.raytheon.uf.edex.netcdf.description.field.level.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.field.parameter.ParameterDescription;
import com.raytheon.uf.edex.netcdf.description.match.MatcherDescription;

/**
 * A description of a Netcdf Product.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 09, 2016 5584       nabowle     Initial creation
 * Jul 07, 2016 5584       nabowle     Use mask.isPresent
 * Jul 21, 2016 5584       nabowle     Use data.isPresent
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlSeeAlso({ DelegateFieldDescription.class, MatcherDescription.class })
@XmlAccessorType(XmlAccessType.NONE)
public class NetcdfProductDescription {
    protected static final String OVERWRITE_ALLOWED_KEY = "overwriteAllowed";

    /**
     * This is intended to be true only when configuring new products because it
     * will print out information about why this description was not used.
     */
    @XmlAttribute
    private boolean debug;

    /**
     * If true, records matching this description will be allowed to overwrite
     * previous records.
     */
    @XmlAttribute
    private Boolean allowRecordOverwrite;

    /**
     *
     */
    @XmlElement
    private MatcherDescription matches;

    @XmlElement
    private DataTimeDescription dataTime;

    @XmlElement
    private LevelDescription level;

    @XmlElement
    private ParameterDescription parameter;

    private DataDescription data;

    /**
     * Constructor.
     */
    public NetcdfProductDescription() {
        super();
    }

    /**
     * Validate this description and the fields it contains.
     */
    public void validate() throws InvalidDescriptionException {
        if (this.matches != null) {
            this.matches.validate();
        }

        if (this.level != null) {
            this.level.validate();
        }

        if (this.dataTime != null) {
            this.dataTime.validate();
        }

        if (this.parameter != null) {
            this.parameter.validate();
        }

        if (this.data != null) {
            this.data.validate();
        }
    }

    public boolean matches(NetcdfFile file, Logger logger)
            throws InvalidDescriptionException {
        if (!isAllPresent(file, logger)) {
            return false;
        }

        if (this.matches == null) {
            // Nothing configured. Match everything.
            return true;
        }

        return this.matches.matches(file);
    }

    /**
     * Returns true only if every configured part of this description is present
     * in the file.
     *
     * @param file
     *            The netcdf file to check.
     * @param logger
     *            The logger to use to log a debug message to, if {@link #debug}
     *            is true.
     * @return True if every configured part of this description is present.
     *         False otherwise.
     * @throws InvalidDescriptionException
     *             if a description is invalid.
     */
    public boolean isAllPresent(NetcdfFile file, Logger logger) throws InvalidDescriptionException {
        if (this.level != null) {
            if (!this.level.getMasterLevel().isPresent(file)) {
                logDebugMessage(logger, "masterLevel");
                return false;
            }
            if (!this.level.getLevelOneValue().isPresent(file)) {
                logDebugMessage(logger, "levelOneValue");
                return false;
            }
            if (this.level.getLevelTwoValue() != null
                    && !this.level.getLevelTwoValue().isPresent(file)) {
                logDebugMessage(logger, "levelTwoValue");
                return false;
            }
        }

        if (this.dataTime != null) {
            if (this.dataTime.getRefTime() != null
                    && !this.dataTime.getRefTime().isPresent(file)) {
                logDebugMessage(logger, "refTime");
                return false;
            }
            if (this.dataTime.getForecast() != null
                    && !this.dataTime.getForecast().isPresent(file)) {
                logDebugMessage(logger, "forecast");
                return false;
            }
            if (this.dataTime.getValidTime() != null
                    && !this.dataTime.getValidTime().isPresent(file)) {
                logDebugMessage(logger, "validTime");
                return false;
            }
        }

        if (this.parameter != null) {
            if (this.parameter.getName() != null
                    && !this.parameter.getName().isPresent(file)) {
                logDebugMessage(logger, "parameter name");
                return false;
            }
            if (this.parameter.getAbbreviation() != null
                    && !this.parameter.getAbbreviation().isPresent(file)) {
                logDebugMessage(logger, "parameter abbreviation");
                return false;
            }
            if (this.parameter.getUnits() != null
                    && !this.parameter.getUnits().isPresent(file)) {
                logDebugMessage(logger, "parameter units");
                return false;
            }
        }

        if (this.data != null) {
            if (!this.data.isPresent(file)) {
                logDebugMessage(logger, "the data description");
                return false;
            }
        }

        return true;
    }

    /**
     * @param logger
     * @param field
     */
    protected void logDebugMessage(Logger logger, String fieldName) {
        if (this.debug && logger != null && logger.isDebugEnabled()) {
            logger.debug("description skipped because " + fieldName
                    + " is not present in the file.");
        }
    }

    /**
     * @return the debug
     */
    public boolean isDebug() {
        return debug;
    }

    /**
     * @param debug
     *            the debug to set
     */
    public void setDebug(boolean debug) {
        this.debug = debug;
    }

    /**
     * @return the allowRecordOverwrite
     */
    public Boolean getAllowRecordOverwrite() {
        return allowRecordOverwrite;
    }

    /**
     * @param allowRecordOverwrite
     *            the allowRecordOverwrite to set
     */
    public void setAllowRecordOverwrite(Boolean allowRecordOverwrite) {
        this.allowRecordOverwrite = allowRecordOverwrite;
    }

    /**
     * @return the matches
     */
    public MatcherDescription getMatches() {
        return matches;
    }

    /**
     * @param matches
     *            the matches to set
     */
    public void setMatches(MatcherDescription matches) {
        this.matches = matches;
    }

    /**
     * @return the dataTime
     */
    public DataTimeDescription getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTimeDescription dataTime) {
        this.dataTime = dataTime;
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
     * @return the parameter
     */
    public ParameterDescription getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(ParameterDescription parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the data
     */
    public DataDescription getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(DataDescription data) {
        this.data = data;
    }

    /**
     * For each of the configured fields in this description, the value of that
     * field is either added to the beanmap now, or the description is deferred
     * until later. Data, DataTime, and Level descriptions are always deferred.
     * All other fields are decoded now if they are scalar and deferred if not.
     *
     * Fields that are decoded now will not overwrite previously decoded fields.
     *
     * @param netcdfFile
     *            The netcdf file.
     * @param records
     *            The list of records.
     */
    public void updateFields(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> records) throws InvalidDescriptionException {
        if (this.parameter != null) {
            Parameter param = this.parameter.getParameter(netcdfFile);
            if (param != null) {
                for (NetcdfRecordInfo record : records) {
                    if (record.getBeanMap().containsKey(
                            ParameterDescription.PARAMETER_KEY)) {
                        record.addField(
                                ParameterDescription.PARAMETER_KEY, param);
                    }
                }
            }
        }

        if (this.getAllowRecordOverwrite() != null) {
            for (NetcdfRecordInfo record : records) {
                record.addField(OVERWRITE_ALLOWED_KEY,
                            this.getAllowRecordOverwrite().booleanValue());
            }
        }

        // Defer data, level, and datatime decoding until later processing
        if (this.getData() != null) {
            if (!this.getData().isPresent(netcdfFile)) {
                throw new InvalidDescriptionException(
                        "The described data is not present.");
            }

            for (NetcdfRecordInfo record : records) {
                if (record.getDeferredDescription(DataDescription.DATA_KEY) == null) {
                    record.addDeferredDescription(DataDescription.DATA_KEY,
                            this.getData());
                }
            }
        }

        if (this.getLevel() != null) {
            for (NetcdfRecordInfo record : records) {
                if (record.getDeferredDescription(LevelDescription.LEVEL_KEY) == null) {
                    record.addDeferredDescription(LevelDescription.LEVEL_KEY,
                            this.getLevel());
                }
            }
        }

        if (this.getDataTime() != null) {
            for (NetcdfRecordInfo record : records) {
                if (record
                        .getDeferredDescription(DataTimeDescription.DATATIME_KEY) == null) {
                    record.addDeferredDescription(
                            DataTimeDescription.DATATIME_KEY,
                            this.getDataTime());
                }
            }
        }
    }
}
