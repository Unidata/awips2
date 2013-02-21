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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A Gridded DataSet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2012 1022       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class GriddedDataSet extends DataSet {

    @XmlElement
    @DynamicSerializeElement
    protected Set<Integer> cycles = new HashSet<Integer>();

    @XmlElement
    @DynamicSerializeElement
    protected Set<Integer> forecastHours = new HashSet<Integer>();

    @DynamicSerializeElement
    @XmlElement
    private Ensemble ensemble;

    /**
     * @return the cycles
     */
    public Set<Integer> getCycles() {
        return cycles;
    }

    /**
     * @param cycles
     *            the cycles to set
     */
    public void setCycles(Set<Integer> cycles) {
        this.cycles = cycles;
    }

    /**
     * @return the forecastHours
     */
    public Set<Integer> getForecastHours() {
        return forecastHours;
    }

    /**
     * @param forecastHours
     *            the forecastHours to set
     */
    public void setForecastHours(Set<Integer> forecastHours) {
        this.forecastHours = forecastHours;
    }

    public Ensemble getEnsemble() {
        return ensemble;
    }

    public void setEnsemble(Ensemble ensemble) {
        this.ensemble = ensemble;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void combine(DataSet toCombine) {
        super.combine(toCombine);

        if (toCombine instanceof GriddedDataSet) {
            GriddedDataSet other = (GriddedDataSet) toCombine;
            this.getCycles().addAll(other.getCycles());
            this.getForecastHours().addAll(other.getForecastHours());
            if (this.getEnsemble() != null) {
                List<String> mine = this.getEnsemble().getSelectedMembers();
                List<String> theirs = other.getEnsemble().getSelectedMembers();
                if (mine == null) {
                    ensemble.setSelectedMembers(theirs);
                } else if (theirs != null) {
                    mine.addAll(theirs);
                }
            }
        }
    }
}
