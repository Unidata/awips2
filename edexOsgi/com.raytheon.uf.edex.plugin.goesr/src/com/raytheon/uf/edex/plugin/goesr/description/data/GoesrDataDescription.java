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
package com.raytheon.uf.edex.plugin.goesr.description.data;

import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.data.mask.AbstractDataMaskDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;

/**
 * A description of the data contained with a {@link NetcdfFile} that can be
 * used to extract the message data for use in a {@link SatelliteRecord}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2015 4336       bsteffen    Initial creation
 * May 26, 2016 5584       nabowle     Rename and refactor for consolidation.
 * Jul 21, 2016 5584       nabowle     Check data masks within isPresent()
 *
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GoesrDataDescription extends DataDescription {

    public static final int BITSET_FILL = 0xFF;

    @XmlAttribute
    private List<String> bitset;

    @XmlElement
    private DelegateFieldDescription units;

    public GoesrDataDescription() {
        super();
    }

    /**
     * @return the bitset
     */
    public List<String> getBitset() {
        return bitset;
    }

    /**
     * @param bitset
     *            the bitset to set
     */
    public void setBitset(List<String> bitset) {
        this.bitset = bitset;
    }

    /**
     * @return the units
     */
    public DelegateFieldDescription getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(DelegateFieldDescription units) {
        this.units = units;
    }

    /**
     * @return the bitsetFill
     */
    public static int getBitsetFill() {
        return BITSET_FILL;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.variable == null
                && (this.bitset == null || this.bitset.isEmpty())) {
            throw new InvalidDescriptionException(
                    "the data variable or bitset variables must be configured.");
        }

        if (this.variable != null && this.bitset != null
                && !this.bitset.isEmpty()) {
            throw new InvalidDescriptionException(
                    "the data variable and bitset variables cannot both be configured.");
        }

        if (this.variable != null) {
            this.variable.validate();
        }

        if (this.masks != null) {
            for (AbstractDataMaskDescription mask : this.masks) {
                mask.validate();
            }
        }
    }

    /**
     * If any data masks are configured, true will only be returned if all data
     * masks are present. If the variable is configured,
     * this.variable.isPresent(file) is returned. If the bitset is configured,
     * true will be returned only if every variable in the bitset is present. If
     * neither is configured, an InvalidDescriptionException will be thrown.
     *
     * @param file
     *            The netcdf file.
     * @return true if the described variable is present in the netcdf file, or
     *         if all the bitset variables are present, and all configured data
     *         masks are present.
     * @throws InvalidDescriptionException
     *             if the variable description and bitset variables are not
     *             configured, or if the variable description is invalid.
     */
    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        boolean dataVarConfigured = false;
        if (this.variable != null) {
            if (!this.variable.isPresent(file)) {
                return false;
            }
            dataVarConfigured = true;
        }

        if (this.bitset != null && !this.bitset.isEmpty()) {
            VariableDescription var = new VariableDescription();
            for (String varName : this.bitset) {
                var.setName(varName);
                if (!var.isPresent(file)) {
                    return false;
                }
            }
            dataVarConfigured = true;
        }

        if (!dataVarConfigured) {
            throw new InvalidDescriptionException(
                    "the data variable or bitset variables must be configured.");
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

    @Override
    public String getVariableName() {
        if (this.variable != null) {
            return this.variable.getName();
        } else if (this.bitset != null && !this.bitset.isEmpty()) {
            return Arrays.toString(this.bitset.toArray());
        }
        return null;
    }

}
