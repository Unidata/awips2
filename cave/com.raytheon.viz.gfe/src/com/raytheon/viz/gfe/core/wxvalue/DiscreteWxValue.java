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
package com.raytheon.viz.gfe.core.wxvalue;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * A WxValue encapsulates a value at a single gridpoint of scalar type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation of skeleton.
 * 03/11/2008   879        rbell       Cleanup.
 * 06/10/2009   2159       rjpeter     Added equals.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DiscreteWxValue extends WxValue {

    public static DiscreteWxValue defaultValue(Parm parm) {
        if (!parm.getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            throw new IllegalArgumentException("parm must be type DISCRETE");
        }

        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();
        String defaultVal = DiscreteKey.discreteDefinition(siteId)
                .symbols(parmId.getCompositeName()).get(0);
        String key = parm.getParmID().compositeNameUI() + "_defaultValue";
        if (Activator.getDefault() != null
                && Activator.getDefault().getPreferenceStore().contains(key)) {
            defaultVal = Activator.getDefault().getPreferenceStore()
                    .getString(key);
        }

        return new DiscreteWxValue(new DiscreteKey(siteId, defaultVal, parmId),
                parm);
    }

    protected final DiscreteKey discreteKey;

    /**
     * Construct a discrete wx value
     * 
     * @param key
     * @param aParm
     */
    public DiscreteWxValue(final DiscreteKey key, final Parm aParm) {
        super(aParm);
        this.discreteKey = key;
    }

    /**
     * @return the weatherKey
     */
    public DiscreteKey getDiscreteKey() {
        return this.discreteKey;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.discreteKey.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((discreteKey == null) ? 0 : discreteKey.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DiscreteWxValue other = (DiscreteWxValue) obj;
        if (discreteKey == null) {
            if (other.discreteKey != null) {
                return false;
            }
        } else if (!discreteKey.equals(other.discreteKey)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.wxvalue.WxValue#isValid()
     */
    @Override
    public boolean isValid() {
        return getDiscreteKey().isValid();
    }
}
