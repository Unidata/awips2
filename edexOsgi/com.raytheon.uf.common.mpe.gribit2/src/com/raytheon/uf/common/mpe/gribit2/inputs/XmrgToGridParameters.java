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
 * POJO to store, isolate, and keep track of the many parameters required for
 * xmrg to grid conversion. These are primarily parameters retrieved from lookup
 * tables and from the xmrg file that is being processed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2016 4619       bkowal      Initial creation
 * Jul 20, 2016 4619       bkowal      Added {@link #toString()}.
 * Aug 01, 2016 4619       bkowal      Added {@link #originatingCenterId}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgToGridParameters {

    private final XmrgToGridSubCenter xmrgToGridSubCenter;

    private final int originatingCenterId;

    private final int modelId;

    private final int ngrid;

    private final int tunit;

    private final int trang;

    private final int accumulationHours;

    private final int nturef;

    private final int ntufc;

    private final String wmo;

    private final String gribParam;

    private final Integer gribParamNumberFoundTable;

    private final Integer gribParamNumber;

    private final int packageFlag;

    private final int referenceValue;

    private final int binf;

    private final int decimalPositions;

    private final int width;

    private final int dataType;

    public XmrgToGridParameters(final XmrgToGridSubCenter xmrgToGridSubCenter,
            final int originatingCenterId, final int modelId, final int ngrid,
            final int tunit, final int trang, final int accumulationHours,
            final int nturef, final int ntufc, final String wmo,
            final String gribParam, final Integer gribParamNumberFoundTable,
            final Integer gribParamNumber, final int packageFlag,
            final int referenceValue, final int binf,
            final int decimalPositions, final int width, final int dataType) {
        this.xmrgToGridSubCenter = xmrgToGridSubCenter;
        this.originatingCenterId = originatingCenterId;
        this.modelId = modelId;
        this.ngrid = ngrid;
        this.tunit = tunit;
        this.trang = trang;
        this.accumulationHours = accumulationHours;
        this.nturef = nturef;
        this.ntufc = ntufc;
        this.wmo = wmo;
        this.gribParam = gribParam;
        this.gribParamNumberFoundTable = gribParamNumberFoundTable;
        this.gribParamNumber = gribParamNumber;
        this.packageFlag = packageFlag;
        this.referenceValue = referenceValue;
        this.binf = binf;
        this.decimalPositions = decimalPositions;
        this.width = width;
        this.dataType = dataType;
    }

    public XmrgToGridSubCenter getXmrgToGridSubCenter() {
        return xmrgToGridSubCenter;
    }

    public int getOriginatingCenterId() {
        return originatingCenterId;
    }

    public int getModelId() {
        return modelId;
    }

    public int getNgrid() {
        return ngrid;
    }

    public int getTunit() {
        return tunit;
    }

    public int getTrang() {
        return trang;
    }

    public int getAccumulationHours() {
        return accumulationHours;
    }

    public int getNturef() {
        return nturef;
    }

    public int getNtufc() {
        return ntufc;
    }

    public String getWmo() {
        return wmo;
    }

    public String getGribParam() {
        return gribParam;
    }

    public Integer getGribParamNumberFoundTable() {
        return gribParamNumberFoundTable;
    }

    public Integer getGribParamNumber() {
        return gribParamNumber;
    }

    public int getPackageFlag() {
        return packageFlag;
    }

    public int getReferenceValue() {
        return referenceValue;
    }

    public int getBinf() {
        return binf;
    }

    public int getDecimalPositions() {
        return decimalPositions;
    }

    public int getWidth() {
        return width;
    }

    public int getDataType() {
        return dataType;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("XmrgToGridParameters [");
        sb.append("xmrgToGridSubCenter=")
                .append(xmrgToGridSubCenter.toString());
        sb.append(", modelId=").append(modelId);
        sb.append(", ngrid=").append(ngrid);
        sb.append(", tunit=").append(tunit);
        sb.append(", trang=").append(trang);
        sb.append(", accumulationHours=").append(accumulationHours);
        sb.append(", nturef=").append(nturef);
        sb.append(", ntufc=").append(ntufc);
        sb.append(", wmo=").append(wmo);
        sb.append(", gribParam=").append(gribParam);
        sb.append(", gribParamNumberFoundTable=").append(
                gribParamNumberFoundTable);
        sb.append(", gribParamNumber=").append(gribParamNumber);
        sb.append(", packageFlag=").append(packageFlag);
        sb.append(", referenceValue=").append(referenceValue);
        sb.append(", binf=").append(binf);
        sb.append(", decimalPositions=").append(decimalPositions);
        sb.append(", width=").append(width);
        sb.append(", dataType=").append(dataType);
        sb.append("]");
        return sb.toString();
    }
}