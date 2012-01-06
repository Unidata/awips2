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
package com.raytheon.viz.core.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Combined resource data objects contain a primary and a secondary resource.
 * The secondary resource acts as a modifier to the primary resource. The
 * primary resource is modified according to the combine operation.
 * <p>
 * When implementing this interface make that when constructing and updating the
 * primary resource to also do the same to the secondary resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2010            jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public interface ICombinedResourceData {

    public enum CombineOperation {
        /**
         * The NONE operation does not modify or display the data. The NONE
         * operation is used to mark secondary resources.
         */
        NONE,
        /**
         * Subtract the secondary resource data from the primary resource and
         * display the difference
         */
        DIFFERENCE
    }

    public class CombineUtil {

        public static String getName(String primary, String secondary,
                CombineOperation operation) {
            StringBuilder name = new StringBuilder();

            List<String> firstResourceName = Arrays.asList(primary.split(" +"));
            List<String> secondResourceName = Arrays.asList(secondary
                    .split(" +"));
            List<String> firstDifferenceString = new ArrayList<String>();
            List<String> secondDifferenceString = new ArrayList<String>();

            for (String s : firstResourceName) {
                if (!secondResourceName.contains(s)) {
                    firstDifferenceString.add(s);
                }
            }

            for (String s : secondResourceName) {
                if (!firstResourceName.contains(s)) {
                    secondDifferenceString.add(s);
                }
            }

            boolean addedDifferenceString = false;

            for (String s : firstResourceName) {
                if (!firstDifferenceString.contains(s)
                        && !secondDifferenceString.contains(s)) {
                    if (name.length() != 0) {
                        name.append(" ");
                    }
                    name.append(s);
                } else if (!addedDifferenceString) {
                    name.append(" ");
                    name.append("[");
                    for (String d1 : firstDifferenceString) {
                        name.append(d1);
                        name.append(" ");
                    }
                    if (operation == CombineOperation.DIFFERENCE) {
                        name.append("-");
                    }
                    for (String d2 : secondDifferenceString) {
                        name.append(" ");
                        name.append(d2);
                    }
                    name.append("]");
                    addedDifferenceString = true;
                }
            }

            return name.toString();

        }

    }

    public void setSecondaryData(AbstractResourceData data);

    public AbstractResourceData getSecondaryData();

    public void setCombineOperation(CombineOperation operation);

    public CombineOperation getCombineOperation();

    /**
     * 
     * @return the viz resource created when a construct is performed on the
     *         secondary resource data. The secondary resource will be
     *         initialized when the primary resource is constructed.
     */
    public AbstractVizResource<?, ?> getSecondaryResource();
}
