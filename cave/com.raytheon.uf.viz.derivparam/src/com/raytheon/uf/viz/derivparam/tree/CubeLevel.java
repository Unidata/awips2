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
package com.raytheon.uf.viz.derivparam.tree;

/**
 * A pair set of items for a cube.  Always represented as a parameter and a pressure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class CubeLevel<K, V> {
    private K pressure;

    private V param;

    public CubeLevel() {
    }

    public CubeLevel(K pressure, V param) {
        this.param = param;
        this.pressure = pressure;
    }

    public K getPressure() {
        return pressure;
    }

    public void setPressure(K pressure) {
        this.pressure = pressure;
    }

    public V getParam() {
        return param;
    }

    public void setParam(V param) {
        this.param = param;
    }

}
