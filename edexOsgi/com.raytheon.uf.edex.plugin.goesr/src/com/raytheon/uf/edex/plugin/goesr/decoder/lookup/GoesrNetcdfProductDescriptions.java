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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

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
 * A collection of {@link GoesrNetcdfProductDescription}s whose primary role is
 * to serve as a container for {@link JAXB} serialization of multiple
 * descriptions into a single file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2016 5584       nabowle     Initial creation
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlRootElement(name = "goesrProductDescriptions")
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ GoesrNetcdfProductDescription.class })
public class GoesrNetcdfProductDescriptions extends
        NetcdfProductDescriptions {

    /**
     *
     */
    public GoesrNetcdfProductDescriptions() {
        super();
    }

    @Override
    public void mergeDefaultIntoDescriptions() {
        super.mergeDefaultIntoDescriptions();

        if (this.defaultDescription == null
                || !(this.defaultDescription instanceof GoesrNetcdfProductDescription)) {
            return;
        }
        GoesrNetcdfProductDescription gDefault = (GoesrNetcdfProductDescription) this.defaultDescription;

        GoesrNetcdfProductDescription gDesc;
        for (NetcdfProductDescription desc : this.descriptions) {
            if (desc instanceof GoesrNetcdfProductDescription) {
                gDesc = (GoesrNetcdfProductDescription) desc;

                if (gDesc.getPhysicalElement() == null
                        && gDefault.getPhysicalElement() != null) {
                    gDesc.setPhysicalElement(gDefault.getPhysicalElement());
                }

                if (gDesc.getUnits() == null && gDefault.getUnits() != null) {
                    gDesc.setUnits(gDefault.getUnits());
                }

                if (gDesc.getCreatingEntity() == null
                        && gDefault.getCreatingEntity() != null) {
                    gDesc.setCreatingEntity(gDefault.getCreatingEntity());
                }

                if (gDesc.getSource() == null && gDefault.getSource() != null) {
                    gDesc.setSource(gDefault.getSource());
                }

                if (gDesc.getSectorID() == null
                        && gDefault.getSectorID() != null) {
                    gDesc.setSectorID(gDefault.getSectorID());
                }

                if (gDesc.getSatHeight() == null
                        && gDefault.getSatHeight() != null) {
                    gDesc.setSatHeight(gDefault.getSatHeight());
                }
            }
        }
    }

    /*
     * The following methods are overridden to be able to specify to JAXB which
     * subclass of NetcdfProductDescription to unmarshal into.
     */

    @Override
    @XmlElement(name = "default", type = GoesrNetcdfProductDescription.class)
    public NetcdfProductDescription getDefaultDescription() {
        return this.defaultDescription;
    }

    @Override
    public void setDefaultDescription(
            NetcdfProductDescription defaultDescription) {
        this.defaultDescription = defaultDescription;
    }

    @Override
    @XmlElement(name = "description", type = GoesrNetcdfProductDescription.class)
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
            if (!(description instanceof GoesrNetcdfProductDescription)) {
                throw new InvalidDescriptionException(
                        "invalid descriptions class configured. expected "
                                + GoesrNetcdfProductDescription.class
                                        .getSimpleName());
            }
        }
        super.validate();
    }
}
