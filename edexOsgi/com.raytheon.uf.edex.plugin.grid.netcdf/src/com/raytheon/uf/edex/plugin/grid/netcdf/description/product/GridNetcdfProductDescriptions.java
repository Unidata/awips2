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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;

/**
 * Contains a list of individual {@link GridNetcdfProductDescription}s as well
 * as information that pertains to all listed products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2015 4696       nabowle     Initial creation
 * Apr 07, 2016 5446       skorolev    Added latDimensionName and lonDimensionName
 * May 17, 2016 5584       nabowle     Refactor and rename for consolidation.
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlRootElement(name = "gridProductDescriptions")
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ GridNetcdfProductDescription.class })
public class GridNetcdfProductDescriptions extends
        NetcdfProductDescriptions {

    /**
     *
     */
    public GridNetcdfProductDescriptions() {
        super();
    }

    @Override
    public void mergeDefaultIntoDescriptions() {
        super.mergeDefaultIntoDescriptions();

        if (this.defaultDescription == null
                || !(this.defaultDescription instanceof GridNetcdfProductDescription)) {
            return;
        }
        GridNetcdfProductDescription gDefault = (GridNetcdfProductDescription) this.defaultDescription;

        GridNetcdfProductDescription gDesc;
        for (NetcdfProductDescription desc : this.descriptions) {
            if (desc instanceof GridNetcdfProductDescription) {
                gDesc = (GridNetcdfProductDescription) desc;

                if (gDesc.getCoverage() == null
                        && gDefault.getCoverage() != null) {
                    gDesc.setCoverage(gDefault.getCoverage());
                }

                if (gDesc.getDatasetId() == null
                        && gDefault.getDatasetId() != null) {
                    gDesc.setDatasetId(gDefault.getDatasetId());
                }

                if (gDesc.getSecondaryId() == null
                        && gDefault.getSecondaryId() != null) {
                    gDesc.setSecondaryId(gDefault.getSecondaryId());
                }

                if (gDesc.getEnsembleId() == null
                        && gDefault.getEnsembleId() != null) {
                    gDesc.setEnsembleId(gDefault.getEnsembleId());
                }
            }
        }
    }

    /*
     * The following methods are overridden to be able to specify to JAXB which
     * subclass of NetcdfProductDescription to unmarshal into.
     */

    @Override
    @XmlElement(name = "default", type = GridNetcdfProductDescription.class)
    public NetcdfProductDescription getDefaultDescription() {
        return this.defaultDescription;
    }

    @Override
    public void setDefaultDescription(
            NetcdfProductDescription defaultDescription) {
        this.defaultDescription = defaultDescription;
    }

    @Override
    @XmlElement(name = "description", type = GridNetcdfProductDescription.class)
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
            if (!(description instanceof GridNetcdfProductDescription)) {
                throw new InvalidDescriptionException(
                        "invalid descriptions class configured. expected "
                                + GridNetcdfProductDescription.class
                                        .getSimpleName());
            }
        }
        super.validate();
    }
}
