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
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgToGridSubCenter {

    private final int code;

    private final String office;

    public XmrgToGridSubCenter(final int code) {
        this(code, null);
    }

    public XmrgToGridSubCenter(final int code, final String office) {
        this.code = code;
        this.office = office;
    }

    public int getCode() {
        return code;
    }

    public String getOffice() {
        return office;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("XmrgToGridSubCenter [");
        sb.append("code=").append(code);
        if (office != null) {
            sb.append(", office=").append(office);
        }
        sb.append("]");
        return sb.toString();
    }
}