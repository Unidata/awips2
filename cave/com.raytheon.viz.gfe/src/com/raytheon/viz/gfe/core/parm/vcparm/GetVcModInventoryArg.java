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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import jep.Jep;
import jep.JepException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GetVcModInventoryArg implements IVcModuleArgument {

    public List<long[]> trs;

    public GetVcModInventoryArg(List<long[]> trs) {
        this.trs = new ArrayList<long[]>(trs);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.vcparm.IVcModuleArgument#evaluateArgument
     * (jep.Jep, java.lang.String)
     */
    @Override
    public Collection<String> evaluateArgument(final Jep instance,
            String argName) throws JepException {
        StringBuilder sb = new StringBuilder(512);

        sb.append(argName + " = [");
        for (int i = 0; i < trs.size(); i++) {
            long[] tuple = trs.get(i);
            sb.append('(');
            sb.append(tuple[0]);
            sb.append("L, ");
            sb.append(tuple[1]);
            sb.append("L)");
            if (i < trs.size() - 1) {
                sb.append(',');
            }
        }
        sb.append(']');
        instance.eval(sb.toString());

        return Collections.emptyList();
    }

}
