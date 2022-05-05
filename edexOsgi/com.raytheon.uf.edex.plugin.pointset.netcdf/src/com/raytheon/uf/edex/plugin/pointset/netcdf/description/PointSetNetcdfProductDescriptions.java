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
package com.raytheon.uf.edex.plugin.pointset.netcdf.description;

import java.util.List;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;

/**
 * A collection of {@link PointSetNetcdfProductDescription}s whose primary role
 * is to serve as a container for {@link JAXB} serialization of multiple
 * descriptions into a single file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2015 4709       bsteffen    Initial creation
 * May 17, 2016 5584       nabowle     Refactor and rename for consolidation.
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlRootElement(name = "pointSetProductDescriptions")
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ PointSetNetcdfProductDescription.class })
public class PointSetNetcdfProductDescriptions extends
        NetcdfProductDescriptions {

    /**
     *
     */
    public PointSetNetcdfProductDescriptions() {
        super();
    }

    @Override
    public void mergeDefaultIntoDescriptions() {
        super.mergeDefaultIntoDescriptions();

        if (this.defaultDescription == null
                || !(this.defaultDescription instanceof PointSetNetcdfProductDescription)) {
            return;
        }
        PointSetNetcdfProductDescription pDefault = (PointSetNetcdfProductDescription) this.defaultDescription;

        PointSetNetcdfProductDescription pDesc;
        for (NetcdfProductDescription desc : this.descriptions) {
            if (desc instanceof PointSetNetcdfProductDescription) {
                pDesc = (PointSetNetcdfProductDescription) desc;

                if (pDesc.getTriangulation() == null
                        && pDefault.getTriangulation() != null) {
                    pDesc.setTriangulation(pDefault.getTriangulation());
                }

                if (pDesc.getDatasetId() == null
                        && pDefault.getDatasetId() != null) {
                    pDesc.setDatasetId(pDefault.getDatasetId());
                }

                if (pDesc.getLatitude() == null
                        && pDefault.getLatitude() != null) {
                    pDesc.setLatitude(pDefault.getLatitude());
                }

                if (pDesc.getLongitude() == null
                        && pDefault.getLongitude() != null) {
                    pDesc.setLongitude(pDefault.getLongitude());
                }
            }
        }
    }

    /*
     * The following methods are overridden to be able to specify to JAXB which
     * subclass of NetcdfProductDescription to unmarshal into.
     */

    @Override
    @XmlElement(name = "default", type = PointSetNetcdfProductDescription.class)
    public NetcdfProductDescription getDefaultDescription() {
        return this.defaultDescription;
    }

    @Override
    public void setDefaultDescription(
            NetcdfProductDescription defaultDescription) {
        this.defaultDescription = defaultDescription;
    }

    @Override
    @XmlElement(name = "description", type = PointSetNetcdfProductDescription.class)
    public List<NetcdfProductDescription> getDescriptions() {
        return this.descriptions;
    }

    @Override
    public void setDescriptions(List<NetcdfProductDescription> descriptions) {
        this.descriptions = descriptions;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        for (NetcdfProductDescription description : this.descriptions) {
            if (!(description instanceof PointSetNetcdfProductDescription)) {
                throw new InvalidDescriptionException(
                        "invalid descriptions class configured. expected "
                                + PointSetNetcdfProductDescription.class
                                        .getSimpleName());
            }
        }
        super.validate();
    }
}
