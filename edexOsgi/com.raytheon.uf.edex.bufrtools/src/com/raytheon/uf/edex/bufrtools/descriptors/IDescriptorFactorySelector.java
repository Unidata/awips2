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
package com.raytheon.uf.edex.bufrtools.descriptors;

/**
 * Indicates that a class supports the getSelector behavior.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IDescriptorFactorySelector {

    /**
     * Get a selector value from an object. The nature of the selector will
     * vary between implementations but provides a means for the implementor
     * to pass some sort of "key" value by inspection.
     * @return A selector value.
     */
    String getSelector();
}
