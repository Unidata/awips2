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
package com.raytheon.uf.viz.backupsvc.constants;

/**
 * This class defines the filter data contents for each filter table item for
 * Backup Service Jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * Jun 14, 2021 84476      Gang Chen    Initial creation
 *
 * </pre>
 *
 * @author Gang Chen
 */

public class BSJFilterData {
    private String condition;

    private String fieldName;

    private String operand;

    private String values;

    public BSJFilterData(String condition, String fieldName, String operand,
            String values) {
        this.condition = condition;
        this.fieldName = fieldName;
        this.operand = operand;
        this.values = values;
    }

    public String getCondition() {
        return this.condition;
    }

    public String getFieldName() {
        return this.fieldName;
    }

    public String getOperand() {
        return this.operand;
    }

    public String getValues() {
        return this.values;
    }
}
