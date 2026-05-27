package com.raytheon.uf.viz.monitor.scan.data;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;

public class UnwarnedCell {
    
    public String cellId;
    
    public WARN_TYPE warnType;
    
    public UnwarnedCell(String cellId, WARN_TYPE warnType) {
        this.cellId = cellId;
        this.warnType = warnType;
    }
    
    public String getCellId() {
        return cellId;
    }
    
    public void setCellId(String cellId) {
        this.cellId = cellId;
    }
    
    public WARN_TYPE getWarnType() {
        return warnType;
    }
    
    public void setWarnType(WARN_TYPE warnType) {
        this.warnType = warnType;
    }   
}
