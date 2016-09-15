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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.slf4j.Logger;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Collection of {@link NetcdfProductDescription}s.
 *
 * Descriptions contain an optional matcher, and fields that should be decoded
 * for a record. Only descriptions that contain a data tag will produce a new
 * Record.
 *
 * The default description's matcher is used to determine if all the contained
 * descriptions matches the the file. Fields configured within the default
 * description are only applied when a record info is created from a non-default
 * description that contains a DataDescription within this set of Descriptions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2016  5584      nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlRootElement(name = "productDescriptions")
@XmlSeeAlso(NetcdfProductDescription.class)
@XmlAccessorType(XmlAccessType.NONE)
public abstract class NetcdfProductDescriptions {

    /**
     * A special description that does not generate any records and is used to
     * specify common fields for the entire Descriptions configuration, and
     * provide Descriptions-level matching.
     */
    protected NetcdfProductDescription defaultDescription;

    /**
     * A list of descriptions used to generate records.
     */
    protected List<NetcdfProductDescription> descriptions;

    /**
     * Constructor.
     */
    public NetcdfProductDescriptions() {
        super();
    }

    public void validate() throws InvalidDescriptionException {
        if (this.defaultDescription != null) {
            this.defaultDescription.validate();
        }

        if (this.descriptions == null || this.descriptions.isEmpty()) {
            throw new InvalidDescriptionException(
                    "descriptions are not configured.");
        }

        for (NetcdfProductDescription description : this.descriptions) {
            description.validate();
        }
    }


    /**
     * Indicates if the default description, if configured, matches the file.
     *
     * @param netcdfFile
     *            The file to match.
     * @return true if the defaultDescription isn't configured, otherwise the
     *         result of defaultDescription.matches(file) is returned.
     * @throws InvalidDescriptionException
     *             if some part of the description is invalid.
     */
    public boolean matches(NetcdfFile netcdfFile, Logger logger)
            throws InvalidDescriptionException {
        if (this.defaultDescription == null) {
            // nothing configured. Match everything.
            return true;
        }

        return this.defaultDescription.matches(netcdfFile, logger);
    }

    /*
     * The following methods are abstract to force subclasses to override them
     * and add the proper @XmlElement(type = DataTypeSub.class) tag.
     */

    public abstract NetcdfProductDescription getDefaultDescription();

    public abstract void setDefaultDescription(NetcdfProductDescription defaultDescription);

    public abstract List<NetcdfProductDescription> getDescriptions();

    public abstract void setDescriptions(List<NetcdfProductDescription> descriptions);

    /**
     * Merge the default description into the other descriptions.
     */
    public void mergeDefaultIntoDescriptions() {
        if (this.defaultDescription == null || this.descriptions == null) {
            return;
        }

        for (NetcdfProductDescription description : this.descriptions) {
            if (description.getLevel() == null
                    && this.defaultDescription.getLevel() != null) {
                description.setLevel(this.defaultDescription.getLevel());
            }

            if (description.getDataTime() == null
                    && this.defaultDescription.getDataTime() != null) {
                description.setDataTime(this.defaultDescription.getDataTime());
            }

            if (description.getParameter() == null
                    && this.defaultDescription.getParameter() != null) {
                description
                        .setParameter(this.defaultDescription.getParameter());
            }
        }
    }

}
