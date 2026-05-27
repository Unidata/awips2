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
package com.raytheon.uf.common.monitor.scan.xml;

import java.util.List;
import java.util.Optional;

/**
 * Abstract class for scan config XML files
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2018  6673       tgurney     Add combine()
 * </pre>
 *
 * @author unknown
 */
public abstract class SCANAbstractXML<T extends SCANAbstractXML<T>> {

    /**
     * Incremental override. Copy all non-null fields from other to this (except
     * attributesData which is handled differently)
     */
    public abstract void combine(T other);

    protected static void combineAttributesData(
            List<SCANAttributesXML> thisData,
            List<SCANAttributesXML> otherData) {
        if (thisData != null && !thisData.isEmpty() && otherData != null
                && !otherData.isEmpty()) {
            /*
             * Only combine attributes from other that are in this. ignore
             * attributes that aren't in this
             */
            for (SCANAttributesXML otherAttributesXML : otherData) {
                Optional<SCANAttributesXML> match = thisData.stream()
                        .filter(x -> x.getAttrName()
                                .equals(otherAttributesXML.getAttrName()))
                        .findFirst();
                if (match.isPresent()) {
                    match.get().combine(otherAttributesXML);
                }
            }
        }
    }
}
