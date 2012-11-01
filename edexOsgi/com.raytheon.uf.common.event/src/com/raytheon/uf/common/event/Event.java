package com.raytheon.uf.common.event;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import javax.persistence.Column;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Provides logging and deletion services for camel
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012  #1305      bgonzale    Added LogLevel enum and transient attribute.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class Event implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    public enum LogLevel {
        DEBUG, INFO, WARN, ERROR, FATAL, ALL, OFF, TRACE
    }

    @Column
    @DynamicSerializeElement
    protected Calendar date;

    @Column
    @DynamicSerializeElement
    protected String id;

    @DynamicSerializeElement
    protected LogLevel logLevel;

    public Event() {
        this(LogLevel.DEBUG);
    }

    public Event(LogLevel logLevel) {
        date = Calendar.getInstance();
        this.logLevel = logLevel;
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

    /**
     * @return the logLevel
     */
    public LogLevel getLogLevel() {
        return logLevel;
    }

    /**
     * @param logLevel
     *            the logLevel to set
     */
    public void setLogLevel(LogLevel logLevel) {
        this.logLevel = logLevel;
    }

}