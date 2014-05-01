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
package com.raytheon.uf.common.dataplugin.shef.tables;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public interface IHourlyTS {

    public String getLid();

    public String getTs();

    public String getMinuteOffset();

    public String getHourlyQc();

    public Short getHour1();

    public Short getHour2();

    public Short getHour3();

    public Short getHour4();

    public Short getHour5();

    public Short getHour6();

    public Short getHour7();

    public Short getHour8();

    public Short getHour9();

    public Short getHour10();

    public Short getHour11();

    public Short getHour12();

    public Short getHour13();

    public Short getHour14();

    public Short getHour15();

    public Short getHour16();

    public Short getHour17();

    public Short getHour18();

    public Short getHour19();

    public Short getHour20();

    public Short getHour21();

    public Short getHour22();

    public Short getHour23();

    public Short getHour24();

    public void setMinuteOffset(String offset);

    public void setHourlyQc(String qc);

    public void setHour1(Short val);

    public void setHour2(Short val);

    public void setHour3(Short val);

    public void setHour4(Short val);

    public void setHour5(Short val);

    public void setHour6(Short val);

    public void setHour7(Short val);

    public void setHour8(Short val);

    public void setHour9(Short val);

    public void setHour10(Short val);

    public void setHour11(Short val);

    public void setHour12(Short val);

    public void setHour13(Short val);

    public void setHour14(Short val);

    public void setHour15(Short val);

    public void setHour16(Short val);

    public void setHour17(Short val);

    public void setHour18(Short val);

    public void setHour19(Short val);

    public void setHour20(Short val);

    public void setHour21(Short val);

    public void setHour22(Short val);

    public void setHour23(Short val);

    public void setHour24(Short val);
}
