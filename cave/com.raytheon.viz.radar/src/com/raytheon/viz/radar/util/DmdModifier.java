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
package com.raytheon.viz.radar.util;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing the XML elements located in dmdModifier.xml for
 * personalizing style of DMD sampling.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 06, 2011           jdortiz     Initial creation
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jdortiz
 * @version 1.0
 */

@XmlRootElement(name = "dmdModifier")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DmdModifier {

    @DynamicSerializeElement
    @XmlElement(name = "attribute", nillable = false)
    private DmdAttribute[] attributes;

    @DynamicSerializeElement
    @XmlElement(name = "tablePosition", nillable = false)
    private TablePosition tablePosition;

    @DynamicSerializeElement
    @XmlElement(name = "showTable", nillable = false)
    private boolean showTable;

    public DmdAttribute[] getAttributes() {
        return attributes;
    }

    public void setAttributes(DmdAttribute[] attribute) {
        this.attributes = attribute;
    }

    public TablePosition getTablePosition() {
        return tablePosition;
    }

    public void setTablePosition(TablePosition tablePosition) {
        this.tablePosition = tablePosition;
    }

    public boolean isShowTable() {
        return showTable;
    }

    public void setShowTable(boolean showTable) {
        this.showTable = showTable;
    }

    @XmlRootElement(name = "tablePosition")
    @XmlAccessorType(XmlAccessType.NONE)
    @DynamicSerialize
    public enum TablePosition {
        UPPER_CENTER(0, 20), LOWER_CENTER(0, 500), CENTER(0, 250), UPPER_RIGHT(
                170, 20), LOWER_RIGHT(170, 500), CENTER_RIGHT(170, 250), UPPER_LEFT(
                -170, 20), CENTER_LEFT(-170, 250), LOWER_LEFT(-170, 500);

        private int x;

        private int y;

        private TablePosition(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public int getX() {
            return x;
        }

        public void setX(int x) {
            this.x = x;
        }

        public int getY() {
            return y;
        }

        public void setY(int y) {
            this.y = y;
        }
    }
}
