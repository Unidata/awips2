package com.raytheon.uf.viz.datadelivery.utils;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.datadelivery.system.Operator;

/** Enumeration to use for Dataset Name operations */
@XmlType(name = "nameOperationItems")
@XmlEnum
public enum NameOperationItems implements Operator<String> {
    /** Operation Like */
    @XmlEnumValue("Like")
    LIKE("Like");

    /** Dataset Name operation */
    private final String operation;

    private NameOperationItems(String operation) {
        this.operation = operation;
    }

    /**
     * Get dataset name operation.
     * 
     * @return operation
     */
    public String getOperation() {
        return operation;
    }

    @Override
    public String toString() {
        return operation;
    }

    @Override
    public boolean evaluate(String operandOne, String operandTwo) {
        if (operandOne.toLowerCase().contains(operandTwo.toLowerCase())) {
            return true;
        }
        return false;
    }
}