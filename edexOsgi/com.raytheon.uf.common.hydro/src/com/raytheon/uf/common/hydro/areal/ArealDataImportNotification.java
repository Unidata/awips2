package com.raytheon.uf.common.hydro.areal;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
/**
 * Areal Data Import Notification
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 *
 * </pre>
 *
 * @author mgamazaychikov
 *
 */
@DynamicSerialize
public class ArealDataImportNotification {

    public ArealDataImportNotification() {
    }

    public ArealDataImportNotification(IMPORTSTATUS status, ArealTypeSelection type) {
        this.status = status;
        this.type = type;
    }

    public enum IMPORTSTATUS {
        SUCCESS, FAILURE
    }

    @DynamicSerializeElement
    protected IMPORTSTATUS status;

    @DynamicSerializeElement
    protected ArealTypeSelection type;

    public IMPORTSTATUS getStatus() {
        return status;
    }

    public void setStatus(IMPORTSTATUS status) {
        this.status = status;
    }

    public ArealTypeSelection getType() {
        return type;
    }

    public void setType(ArealTypeSelection type) {
        this.type = type;
    }

    public boolean isSuccess() {
        return this.status.equals(IMPORTSTATUS.SUCCESS);
    }

    public boolean isFailure() {
        return this.status.equals(IMPORTSTATUS.FAILURE);
    }
}
