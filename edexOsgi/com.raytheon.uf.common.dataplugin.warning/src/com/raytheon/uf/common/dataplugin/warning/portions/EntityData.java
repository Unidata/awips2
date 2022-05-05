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
package com.raytheon.uf.common.dataplugin.warning.portions;

/**
 * Simple port of an A1 struct created by GridUtil and used by PortionsUtil.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2013  2177      jsanchez     Initial creation
 * Dec 4, 2013  2604      jsanchez     Moved out of viz.warngen.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class EntityData {

    private int meanMask = 0;

    private int coverageMask = 0;

    private int octants = 0;

    public EntityData(int meanMask, int coverageMask, int octants) {
        this.meanMask = meanMask;
        this.coverageMask = coverageMask;
        this.octants = octants;
    }

    public int getMeanMask() {
        return meanMask;
    }

    public int getCoverageMask() {
        return coverageMask;
    }

    public int getOctants() {
        return octants;
    }

}
