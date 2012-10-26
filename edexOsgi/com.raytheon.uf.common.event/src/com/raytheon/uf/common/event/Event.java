package com.raytheon.uf.common.event;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import javax.persistence.Column;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

public abstract class Event implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Column
    @DynamicSerializeElement
    protected Calendar date;

    @Column
    @DynamicSerializeElement
    protected String id;

    public Event() {
        date = Calendar.getInstance();
    }

    public Calendar getDate() {
        return date;
    }

    public void setDate(Calendar date) {
        this.date = date;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String toString() {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-d hh:mm:ss");
        return formatter.format(date.getTime()) + " Id: " + id;
    }

}