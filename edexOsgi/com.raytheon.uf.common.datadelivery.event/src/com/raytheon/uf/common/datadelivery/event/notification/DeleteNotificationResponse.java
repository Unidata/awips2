package com.raytheon.uf.common.datadelivery.event.notification;

import java.util.ArrayList;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Response after notification records are deleted from the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public class DeleteNotificationResponse implements ISerializableObject {

    @DynamicSerializeElement
    private Integer rowsDeleted;

    @DynamicSerializeElement
    private ArrayList<Integer> ids;

    public ArrayList<Integer> getIds() {
        return ids;
    }

    public void setIds(ArrayList<Integer> ids) {
        this.ids = ids;
    }

    public Integer getRowsDeleted() {
        return rowsDeleted;
    }

    public void setRowsDeleted(Integer rowsDeleted) {
        this.rowsDeleted = rowsDeleted;
    }

}
