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
package com.raytheon.uf.edex.netcdf.description.data;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.data.mask.AbstractDataMaskDescription;
import com.raytheon.uf.edex.netcdf.description.data.mask.DiscreteDataMaskDescription;
import com.raytheon.uf.edex.netcdf.description.data.mask.RangeDataMaskDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;

/**
 * A description of the data contained with a {@link NetcdfFile} that can be
 * used to extract the message data for a record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * Jun 09, 2016  5584     nabowle     Refactor from goes-r for consolidation.
 * Jul 07, 2016  5584     nabowle     Add range data mask.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataDescription {

    public static String DATA_KEY = "messageData";

    @XmlElement
    protected VariableDescription variable;

    /**
     * A list of datamasks to run against the processed data after applying any
     * scaling and/or offsets.
     */
    @XmlElements({
            @XmlElement(name = "discreteMask", type = DiscreteDataMaskDescription.class),
            @XmlElement(name = "rangeMask", type = RangeDataMaskDescription.class) })
    protected List<AbstractDataMaskDescription> masks;

    public VariableDescription getVariable() {
        return variable;
    }

    public void setVariable(VariableDescription variable) {
        this.variable = variable;
    }

    public List<AbstractDataMaskDescription> getMasks() {
        return masks;
    }

    public void setMasks(List<AbstractDataMaskDescription> masks) {
        this.masks = masks;
    }

    public void validate() throws InvalidDescriptionException {
        if (this.variable == null) {
            throw new InvalidDescriptionException(
                    "the data variable is not configured.");
        }
        this.variable.validate();

        if (this.masks != null) {
            for (AbstractDataMaskDescription mask : this.masks) {
                mask.validate();
            }
        }
    }

    /**
     * Convenience method for getVariable().isPresent(file) that throws an
     * InvalidDescriptionException if the variable description is not
     * configured.
     *
     * @param file
     *            The netcdf file.
     * @return true if the described variable is present in the netcdf file.
     * @throws InvalidDescriptionException
     *             if the variable description is not configured, or if the
     *             variable description is invalid.
     */
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.variable == null) {
            throw new InvalidDescriptionException(
                    "the data variable is not configured.");
        }
        if (!this.variable.isPresent(file)) {
            return false;
        }

        if (this.masks != null) {
            for (AbstractDataMaskDescription mask : this.masks) {
                if (!mask.isPresent(file)) {
                    return false;
                }
            }
        }

        return true;
    }

    public String getVariableName() {
        if (this.variable == null) {
            return null;
        }
        return this.variable.getName();
    }
}
