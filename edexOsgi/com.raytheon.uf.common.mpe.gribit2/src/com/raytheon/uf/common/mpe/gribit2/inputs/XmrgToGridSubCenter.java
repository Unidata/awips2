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
package com.raytheon.uf.common.mpe.gribit2.inputs;

/**
 * POJO to store the xmrg to grib sub center inputs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2016 4619       bkowal      Initial creation
 * Sep 09, 2016 4628       bkowal      Added {@link #subCenter0}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgToGridSubCenter {

    private final int code;

    private final String office;

    private final boolean subCenter0;

    public XmrgToGridSubCenter(final int code, final boolean subCenter0) {
        this(code, null, subCenter0);
    }

    public XmrgToGridSubCenter(final int code, final String office,
            final boolean subCenter0) {
        this.code = code;
        this.office = office;
        this.subCenter0 = subCenter0;
    }

    public int getCode() {
        return code;
    }

    public String getOffice() {
        return office;
    }

    public boolean isSubCenter0() {
        return subCenter0;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("XmrgToGridSubCenter [");
        sb.append("code=").append(code);
        if (office != null) {
            sb.append(", office=").append(office);
        }
        sb.append(", subCenter0=").append(subCenter0);
        sb.append("]");
        return sb.toString();
    }
}